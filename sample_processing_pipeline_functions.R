#' Author: Steven Lauterwasser
#' 
#' This script contains the functions used in the text processing pipeline
#' in `processing_pipeline.R`.
#' 

library(tidyverse)
library(hunspell)


# ---- UTILITY FUNCTIONS ----

# function factory to allow the on demand creation
# of progressr compatible functions here.
progressr_func <- function(func, map_type) {
  #' Create a `progressr` tracked function
  #' 
  #' This is a function factory which takes an input
  #' function and a `future_map` function and creates 
  #' a mapped version of the input which can be run 
  #' using `with_progress()` to track the progress of 
  #' its execution.
  #' 
  #' @param func input function which will be mapped. 
  #' @param map_type a (future) mapping function to do
  #' the mapping. 
  #' 
  #' @return a function which can track its progress. 
  #' 
  #' The output function has two required arguments, 
  #' `docs` and `schedule` (both passed to the mapping
  #' function) and then an arbitrary number of named
  #' arguments (i.e., whatever is required for `func`
  #' to work). `docs` is the iterable to be mapped over
  #' and `schedule` is passed to `furrr_options()` 
  #' to set `furrr`'s scheduling.
  #' 

  function(docs, schedule, ...) {
    p <- progressor(steps = length(docs))
    
    out <- map_type(docs, function(x) {
      p()
      func(x, ...)
    }, 
    .options = furrr_options(seed = TRUE, 
                             scheduling = schedule))
    
    return(out)
  }
  
}

# Set paths to custom dictionaries, generated based on
# high quality text from the corpus itself. Means that
# words like "masculinities" or "heteronormative" 
# don't fail the spell check. 
custom_small_dict_path <- paste(file_path_prefix, script_folder,
                                "custom_dict/hunspell-en-small-custom/en-small-custom.dic", sep = "/")
custom_med_dict_path <- paste(file_path_prefix, script_folder, 
                              "custom_dict/hunspell-en-med-custom/en-med-custom.dic", sep = "/")

# a list of particles which appear as separate words in 
# some last names and should, in those instances, not be
# spell checked, but which we otherwise don't wish to be correct.
name_particles <- c("de", "del", "von", "van", "della", "des",
                    "du")



# ---- PRELIMINARY PROCESSING ----

prelim_process <- function(doc) {
  #' Perform several preliminary processing steps
  #' 
  #' @param doc a document given as a single string. 
  #' 
  #' @return a single string, potentially shorter than the input.
  #' 
  #' Currently preliminary processing includes: 
  #' 
  #' - canonizing unicode,
  #' - correcting some remaining unicode errors,
  #' - replacing smart single and double quotes,
  #' - removing HTML (or similarly structured) markup,
  #' - dealing with some observed common some errors.
  #' 
  
  doc <- stringi::stri_trans_nfkc(doc)
  doc <- str_replace_all(doc, "ˆı", "î") # correct glyph recognition error
  doc <- str_replace_all(doc, "[‘’]", "'") # replace smart single quotes
  doc <- str_replace_all(doc, "[“”]", '"') # replace smart double quotes
  doc <- str_replace_all(doc, "\\<[\\/]{0,1}[a-z0-9]+\\>", " ") # remove html tags
  doc <- str_remove_all(doc, "Ô") # I think these tend to replace quote marks or
  doc <- str_remove_all(doc, "Õ(?=\\P{L})") # apostrophes, but I'm mostly removing
  doc <- str_replace_all(doc, "Õ", "'") # them, for simplicity's sake
  
  return(doc)
  
}


# ---- TOKENIZING AND SECONDARY PROCESSING----

pipeline_tokenize <- function(doc) {
  #' Tokenize and further process a document
  #' 
  #' This is largely a convenience function, 
  #' binding together tokenizing and the 
  #' processing steps immediately after into 
  #' one package. It uses `spacy_tokenize()`
  #' for the actual tokenization and does not
  #' remove separators (i.e. spaces, newlines, 
  #' etc.). 
  #' 
  #' @param doc an untokenized document as a single string.
  #' 
  #' @return a character vector of tokens. All spaces
  #' are retained.
  #' 
  #' The two additional pieces of processing are, first, an 
  #' attempt to resolve hyphens, so, e.g., ensure that "pro-
  #' blem" becomes "problem" (details in documentation for  
  #' `resolve_hyphens()`). And second, an attempt to fix 
  #' the intra-word punctuation which occasionally shows up 
  #' as an extraction error (i.e., "er.rors like th.is"). 
  #' 
  #' Both of these should be relatively non-destructive and, 
  #' given that all spaces are retained, the documents at
  #' this point should maintain most of the original layout 
  #' (insofar as it was extracted to the text files at all).
  #' 
  #' The other convenience provided is the built in use of 
  #' `custom_split()`, which gets around a limit in spaCy.
  #' See the documentation on `custom_split()` for more. 
  
  doc <- spacy_tokenize(custom_split(doc), what = "word", remove_separators = FALSE, output = "data.frame")
  
  if (nrow(doc) == 0) {
    doc <- "FAILED: EMPTY ON TOKENIZATION"
    return(doc)
  }
  
  # resolve hyphens
  doc <- resolve_hyphens(doc, proper_dict = TRUE, speed = "slow")
  
  # deal with interpolated punctuation and related spacing issues: 
  doc <- map(doc[["token"]], ~if (str_detect(., "\\p{L}(?!')[[:punct:]]\\p{L}")) {
    check <- paste(str_split(., "[[:punct:]]")[[1]], collapse = "")
    if (hunspell_check(check)) {
      check
    } else { . }
  } else { . })
  
  doc <- unlist(doc)
  
  return(doc)
  
}

custom_split <- function(doc, n = 1000000) {
  #' Split a string into segments under n characters
  #' 
  #' Utility function to help workaround spaCy's limit
  #' on input documents to 1000000 characters (this limit
  #' is adjustable in Python, but not exposed in the `spacyr`
  #' wrapper). But can also be used for other purposes
  #' by setting `n` to an appropriate value.
  #' 
  
  if (nchar(doc) < n) {
    return(doc)
  } else {
    doc_split <- strsplit(doc, "")[[1]]
    
    doc <- character(0)
    
    n_lim <- min(n * .99, n - 100)
    
    while (length(doc_split) > 1) {
      max_end <- length(doc_split)
      doc_seg <- doc_split[1:min(n_lim, max_end)]
      doc_split <- doc_split[length(doc_seg):max_end]
      first_space <- str_locate(doc_split, "\\s")[1,1]
      if (!is.na(first_space)) {
        doc_seg <- c(doc_seg, doc_split[1:first_space]) 
        doc_split <- doc_split[first_space + 1:length(doc_split)]
      }
      
      doc <- c(doc, paste(doc_seg, collapse = ""))
      
    }
    return(doc)
  }
}

# ---- RESOLVING HYPHENS ----

# potential issue: they're actually not guaranteed to show up in order: 
# if we're having trouble parsing columned text (which is part of why this problem is 
# significant in the first place), then a hyphen from the bottom of one column might match
# to the top of the next, but the top of the next could precede it in the extracted text. 

resolve_hyphens <- function(doc_tok, proper_dict = FALSE, speed = "slow") {
  #' Attempts to rejoin hyphenated words
  #' 
  #' Takes a tokenized document as a dataframe, identifies first halves
  #' of hyphenated words and attempts to find their corresponding second halves
  #' through a simple (sane) brute force spell check of possible combinations of tokens.
  #' 
  #' @param doc_tok A tokenized document as a dataframe. Has only been tested on
  #' documents tokenized with spaCy (via `spacyr`).
  #' 
  #' @param proper_dict Logical value which determines whether to generate a 
  #' dictionary of proper nouns unique to the input document. This increases
  #' accuracy at the cost of time. 
  #' 
  #' @param speed Either "slow" or "fast". If "slow" (the default), then the algorithm
  #' checks every feasible pair of tokens. If "fast", then the algorithm only checks
  #' a combination if the second token fails a spell check. 
  #' 
  #' @return A dataframe of the same dimensions and columns as the input, 
  #' but likely with fewer rows (given that some hyphenated words have been
  #' recombined).
  #' 
  #' The approach here is straightforward: any token which ends with a letter
  #' followed by a hyphen is assumed to be the first half of a hyphenated word. 
  #' These halves are then combined with each plausible following token (dropping, 
  #' e.g., capitalized tokens, tokens with hyphens, etc.) until a combination 
  #' which passes a spell check is found. Then the first halves are replaced 
  #' and the second halves are dropped. If no combination was found, 
  #' the first half is also dropped. Note that this function relies on 
  #' some behavior of spaCy's tokenizer, namely keeping clause-level punctuation
  #' (e.g., periods, commas, etc.) as separate tokens and usually leaving 
  #' trailing hyphens in place.
  #' 
  
  # find tokens to check
  to_check <- which(str_detect(doc_tok$token, "\\p{L}-$"))
  
  if (length(to_check) == 0) {
    return(doc_tok)
  } else if (is.null(to_check)) {
    return(doc_tok)
  } 
  
  if (proper_dict) {
    proper_dict <- gen_entity_dict(doc_tok$token)
  } else {
    proper_dict <- NULL
  }
  
  if (speed == "slow") {
    resolutions <- map_dfr(to_check, ~resolve_hyphen_inner_slow(., doc_tok$token, proper_dict))
  } else if (speed == "fast") {
    resolutions <- map_dfr(to_check, ~resolve_hyphen_inner(., doc_tok$token, proper_dict))
  }
  
  doc_tok$token[to_check] <- resolutions$cand
  doc_tok <- doc_tok[-resolutions$index,]
  
  return(doc_tok)
  
}


resolve_hyphen_inner <- function(pos, tok_vec, proper_dict = NULL) {
  
  begin <- str_remove(tok_vec[pos], "-")
  
  candidates <- tibble("index" = seq_along(tok_vec), 
                       "token" = tok_vec)
  
  candidates <- candidates[pos:length(tok_vec), ]
  candidates <- candidates[nchar(candidates$token) > 1, ]
  candidates <- candidates[str_starts(candidates$token, "[^A-Z]"),]
  candidates <- candidates[!(hunspell_check(candidates$token) | str_detect(candidates$token, "\\W")), ]
  
  candidates$cand <- paste0(begin, candidates$token)
  
  if (is.null(proper_dict)) {
    resolve_pos <- match(TRUE, hunspell_check(candidates$cand, 
                                              dict = dictionary(custom_med_dict_path)))
  } else {
    resolve_pos <- match(TRUE, hunspell_check(candidates$cand, dict = dictionary(custom_med_dict_path)) | 
                           candidates$cand %in% proper_dict)
  }
  
  if (!is.na(resolve_pos)) {
    out <- candidates[resolve_pos, c("index", "cand")]
  } else {
    out <- tibble("index" = pos, 
                  "cand" = begin)
  }
  
  return(out)
}

resolve_hyphen_inner_slow <- function(pos, tok_vec, proper_dict = NULL) {
  
  begin <- str_remove(tok_vec[pos], "-")
  
  candidates <- tibble("index" = seq_along(tok_vec), 
                       "token" = tok_vec)
  
  candidates <- candidates[pos:length(tok_vec), ]
  candidates <- candidates[nchar(candidates$token) > 1, ]
  candidates <- candidates[str_starts(candidates$token, "[^A-Z]"),]
  candidates <- candidates[!str_detect(candidates$token, "\\W"), ]
  candidates$cand <- paste0(begin, candidates$token)
  
  if (is.null(proper_dict)) {
    resolve_pos <- match(TRUE, hunspell_check(candidates$cand, 
                                              dict = dictionary(custom_med_dict_path)))
  } else {
    resolve_pos <- match(TRUE, hunspell_check(candidates$cand, 
                                              dict = dictionary(custom_med_dict_path)) |
                           candidates$cand %in% proper_dict)
  }
  
  if (!is.na(resolve_pos)) {
    out <- candidates[resolve_pos, c("index", "cand")]
  } else {
    out <- tibble("index" = pos, 
                  "cand" = begin)
  }
  
  return(out)
}

gen_entity_dict <- function(tok_vec, lower_bound = 3) {
  #' Generate a dictionary of named entities
  #' 
  #' This function takes a tokenized document and uses 
  #' very basic heuristics to generate a dictionary of 
  #' named entities based on the contents of the document.
  #' The heuristics can be basic because the purpose of 
  #' the dictionary is only to aid in spell checking, 
  #' rather than for more involved NLP tasks. 
  #' 
  #' @param tok_vec a document as a vector of tokens. 
  #' @param lower_bound an integer setting the minimum
  #' number of times a capitalized token must appear 
  #' for it to be added to the output dictionary. 
  #' 
  #' @return a character vector of named entities to be
  #' used to assist in spell checking of the input document. 
  
  if (length(tok_vec) < lower_bound) {
    return(c(""))
  }
  
  # drop words following periods
  tok_vec <- tok_vec[-(which(tok_vec == ".") + 1)]
  
  # this currently misses words which start with a 
  # capitalized non-English character (e.g. "Á")
  tok_vec <- tok_vec[str_detect(tok_vec, "^[A-Z]\\p{L}+$")]
  
  # now get all the words appearing more
  # than lower_bound (default 3) times
  ent_dict <- table(tok_vec)[table(tok_vec) > lower_bound]
  
  ent_dict <- names(ent_dict)
  
  if (length(ent_dict) < lower_bound) {
    return(c(""))
  }
  
  return(ent_dict)
  
}


# ---- DOCUMENT TESTING ----
# note: doc_test() relies on some of the main processing functions, 
# defined later in this document. 


doc_test <- function(doc, mode = c("assess", "strict", "lax"), dict = "en_US", prop = .1, n = NULL, pass = .65) {
  #' Test the text quality of a document
  #' 
  #' Given a tokenized document, runs several tests
  #' to determine whether the source document is made up
  #' of English text which is or could be made legible.
  #' Used to assess whether a document needs further 
  #' processing, is good as is, or is not recoverable
  #' at all.
  #' 
  #' @param doc A character vector of word tokens.
  #' @param mode A string setting the mode, either:
  #' 
  #' - `"assess"`: performs a strict spell check and uses
  #'   `split_words()` on failures, assessing improvement.
  #' - `"strict"`: performs only a strict spell check.
  #' - `"lax"`: performs a spell check only after applying
  #'   `split_words()`.
  #'
  #' @param dict A hunspell dictionary object or string
  #' which can be passed to `dictionary`.
  #' @param prop,n either a double giving the proportion, or
  #' an integer giving the number, of alphabetic tokens to sample.
  #' The minimum is 50, under the assumption that sufficiently
  #' short texts should be excluded on that basis alone.
  #' @param pass double setting the proportion of tokens which 
  #' must pass in order for the document to pass. 
  #' 
  #' @return a one row dataframe with the columns:
  #' 
  #' - `"to_process"`: `TRUE` if the document should be processed, `FALSE` if not, 
  #'   `NA` if it failed due to insufficient number or length of tokens.
  #' - `"prop_pass"`: the proportion of sampled tokens which passed the spell check.
  #' - `"prop_split"`: the proportion of failing tokens which `split_words()` was
  #'   able to make passing.
  #' - `"prop_len"`: the proportion of sample tokens longer than a single character.
  #' 
  #' The input tokens are filtered to just alphabetic tokens and those
  #' are then filtered for named entities (identified in a rudimentary
  #' way using `gen_entity_dict()`).
  #' 
  #' A random sample of `prop` or `n` tokens is then drawn. If there are
  #' too few tokens, or more than 80% are shorter than 2 character, the 
  #' function returns a data from of `NA`s. 
  #' 
  #' Otherwise, the sampled tokens are (depending on the mode) 
  #' either simply spell checked, spell checked and failures are checked 
  #' with `split_words()`, or are only checked with `split_words()`.
  #' If the proportion of tokens which pass both these checks
  #' is greater than `pass` (default .9), then `to_process` will be `FALSE`.
  #' In essence, the assumption is that the function is testing for 
  #' whether processing is needed, rather than whether it is meaningfully
  #' feasible. However, the output is sufficiently detailed that it can
  #' subsequently be used for the latter purpose as well.
  #' 
  #' The `dict` argument is useful for further modulating the strictness
  #' of the test: use a smaller dictionary for a more difficult test.
  
  mode <- match.arg(mode, c("assess", "strict", "lax"))
  
  entity_dict <- gen_entity_dict(doc, lower_bound = 8)
  
  # label tokens and then subset to just alphabetic tokens
  doc <- doc[label_tokens(doc) == "word"]
  
  # then filter out named entities so they're not sampled
  doc <- doc[!(doc %in% entity_dict)]
  
  # getting a sample of tokens to test
  sample_size <- if (!is.null(n)) {
    n
  } else {
    max(round(length(doc) * prop), 50)
  }
  
  to_test <- tryCatch(sample(doc, size = sample_size), 
                      error = function(err) return("a")) # so it will fail the result test
  
  to_test <- to_test[nchar(to_test) < 32] # to make sure we're not dealing with any extremely long strings
  
  if (length(to_test) == 0) { # if that gets rid of the whole sample
    out <- tibble("to_process" = NA,
                  "prop_pass" = NA,
                  "prop_split" = NA,
                  "prop_len" = NA)
    return(out)
  }
  
  results <- tibble("tok" = to_test, 
                    "len_test" = if_else(nchar(to_test) < 2, FALSE, TRUE))
  
  if (mean(results$len_test) < .6) {
    out <- tibble("to_process" = NA,
                  "prop_pass" = NA,
                  "prop_split" = NA,
                  "prop_len" = mean(results$len_test))
    return(out)
  }
  
  if (mode == "assess" | mode == "strict") {
    results$spell_check <- map_lgl(to_test, ~hunspell_check(., dict = dict))
    
    if (mode == "assess") {
      results$split_check <- map_lgl(1:nrow(results), ~if (results[["spell_check"]][.]) {
        NA
      } else {
        split_check <- tryCatch(hunspell_check(str_split(split_words(to_test[.]), "\\s+")[[1]],
                                               dict = dict),
                                error = function(err) {
                                  out <- tibble("loc" = "doc_test",
                                                "string" = to_test[.], 
                                                "call" = paste(as.character(err$call), collapse = " "),
                                                "message" = err$message)
                                  write_csv(out, "error_log.csv", append = TRUE)
                                  return(FALSE)
                                })
        if (mean(split_check) > .7) {
          TRUE
        } else { FALSE }
      })
      
      out <- tibble("to_process" = if_else(mean(results$spell_check) < pass, TRUE, FALSE),
                    "prop_pass" = mean(results$spell_check), 
                    "prop_split" = mean(results$split_check, na.rm = TRUE), 
                    "prop_len" = mean(results$len_test))
      
    } else if (mode == "strict") {
      out <- tibble("to_process" = if_else(mean(results$spell_check) < pass, TRUE, FALSE),
                    "prop_pass" = mean(results$spell_check),
                    "prop_split" = NA,
                    "prop_len" = mean(results$len_test))
    }
    
  } else if (mode == "lax") {
    results$split_check <- 
      map_lgl(to_test, 
              ~if(mean(hunspell_check(str_split(split_words(.), "\\s+")[[1]], 
                                      dict = dict)) > .7) { TRUE } else { FALSE })
    
    out <- tibble("to_process" = if_else(mean(results$split_check) < pass, TRUE, FALSE),
                  "prop_pass" = mean(results$spell_check),
                  "prop_split" = mean(results$spell_check),
                  "prop_len" = mean(results$len_test))
  }
  
  return(out)
  
}

spacing_error_test <- function(doc, req_matches) {
  #' Check for spacing issues in text
  #' 
  #' Currently checks for two kinds of issues with spacing: 
  #' first, for "s e p a r a t e d l e t t e r s" (whether 
  #' separated by spaces or punctuation); and second, for
  #' "replacements'for'spaces", where instead of spaces
  #' you have symbols. 
  #' 
  #' @param doc a single string to be tested.
  #' @param req_matches the number of matches (counted
  #' separately for each test) required to fail.
  #' 
  #' @return a string, either "pass", "sep", "rep",
  #' or "sep rep", indicating which (if any) of the
  #' the tests the document failed.
  
  # First check if there are issues with separated letters
  sep_test <- str_match_all(doc, "(?:\\p{L} ){5,}")
  sep_result <- nrow(sep_test[[1]])
  out <- if (sep_result >= req_matches) {
    "sep"
  } else { "" }
  
  # Then check for replacements for spaces
  rep_test <- str_match_all(doc, "(?:\\p{L}+[^\\w\\s]){4,}")
  rep_result <- nrow(rep_test[[1]])
  out <- if (rep_result >= req_matches) {
    trimws(paste(out, "rep", sep = " "))
  } else { paste0(out, "") }
  
  if (nchar(out) == 0) {
    return("pass")
  } else {
    return(out)
  }
  
}




# ---- MAIN PROCESSING ----

process_doc <- function(doc) {
  #' Process a tokenized document
  #' 
  #' Takes a vector of tokens, including spaces, punctuation, 
  #' etc. and attempts to improve the quality of the text. 
  #' In particular, it is intended to deal with "combinedwords"
  #' and other spelling errors caused by poor quality pdf
  #' extraction/OCR. 
  #' 
  #' @param doc a character vector of tokens.
  #' 
  #' @return The processed and reassembled document as a single 
  #' string.
  #' 
  #' The workhouse funciton here is `doc_spelling_fix()` and, 
  #' within that, the algorithm in `split_words()`. See their 
  #' documentation for details on processing.
  
  # generate a vector indexing which tokens ought to be words
  # either "cont", "url", "word", "num", "open_parens", or "symbol"
  words <- label_tokens(doc)

  # then attempt to fix spelling errors and word spacing issues (i.e. combined words)
  doc <- tryCatch(doc_spelling_fix(doc, words = words), 
                  error = function(err) { 
                    out <- tibble("loc" = "process_doc",
                                  "string" = str_sub(doc, end = 300), 
                                  "call" = paste(as.character(err$call), collapse = " "),
                                  "message" = err$message)
                    write_csv(out, "error_log.csv", append = TRUE)
                    return(as.character(err)) })
  
  # checks if an error was returned (takes the form of a 
  # named list with elements "call" and "message"). if so, 
  # converts to a string and returns it as it
  if (is_list(doc)) {
    return(as.character(doc))
  }
  
  # remake words in case doc got longer
  words <- label_tokens(doc)

  # and then stick it back together
  return(reassemble_doc(doc, words))
  
}

# spaCy very intelligently tokenizes contractions into their constituent parts, 
# but hunspell (quite reasonably) does not consider them words. Thus, I create
# two vectors here to allow `label_tokens()` to label contraction related tokens
# which will then be excluded from spell checking and treated appropriately 
# with regard to spacing, etc.
contraction_endings <- c("n't", "'ve", "'en", "'er", "'s", "'d", "'ll", "'re", "'m")

contraction_beginnings <- c("wo", "ai")  # (both with "n’t". potentially others, but I don’t think they’ll really come up)

# regex which should match URLs, both those which include the "http" 
# and those which omit it and start with "www"
url_regex <- 
  "^(?:https?:\\/\\/(?:www\\.)?|(?:www\\.))[-a-zA-Z0-9@:\\%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b[-a-z0-9@:\\%_\\+.~#()?&\\/\\/=]*$"

label_tokens <- function(doc) {
  #' Labels tokens 
  #' 
  #' @param doc a character vector of tokens. 
  #' 
  #' @return a character vector of the same
  #' length as `doc`. 
  #' 
  #' The returned vector labels each corresponding
  #' token in the input as either a contraction, 
  #' the beginning of a contraction, a URL, 
  #' a word, a number, an opening parentheses or
  #' bracket, a closing parentheses or bracket, 
  #' a new line character, or a symbol of some kind 
  #' (including non new line spaces). These labels 
  #' are useful in various processing steps, e.g., 
  #' only spell checking words, not spell checking
  #' URLs, etc.
  
  # generate a vector indexing which tokens ought to be words
  # either "cont", "url", "word", "num", "open_parens", or "symbol"
  words <- case_when(doc %in% contraction_endings ~ "cont",
                     doc %in% contraction_beginnings ~ "cont_beg",
                     str_detect(doc, url_regex) ~ "url", 
                     str_detect(doc, "^[[:alpha:]]") ~ "word", 
                     str_detect(doc, "\\d") ~ "num", 
                     str_detect(doc, "\\(|\\[|\\{") ~ "open_parens",
                     str_detect(doc, "\\)|\\]|\\}") ~ "close_parens",
                     str_detect(doc, "\\n") ~ "new_line",
                     TRUE ~ "symbol") # should maybe test explicitly for non-alpha character
  
  return(words)
}

reassemble_doc <- function(doc, words) {
  #' Reassemble a tokenized and processed document
  #' 
  #' @param doc a character vector of tokens.
  #' @param words a character vector of the same length as
  #' `doc` which labels each token in `doc`.
  #' 
  #' @return the reassembled document as a single string.
  #' 
  #' The processing pipeline is written so as to preserve
  #' spacing as much as possible, so we can *almost* just
  #' paste the document back together. However, there are 
  #' two issues this function needs to handle: 
  #' 
  #' 1) `spaCy` will often correctly tokenize things like 
  #'    "word,word", but doesn't insert a space afterward
  #'    (which we want). 
  #' 2) `split_words()` puts spaces between each of its
  #'    outputs, which we sometimes don't want, e.g., 
  #'    when its output includes "(" or ")".
  
  doc <- map_chr(seq_along(doc), ~if (doc[.] %in% c(".", ",")) {
    if (any(words[. - 1] == "word" & words[. + 1] == "word", na.rm = TRUE)) {
      paste0(doc[.], " ")
    } else {
      doc[.]
    }
  } else if (any(words[. - 1] == "open_parens" & doc[.] == " ", na.rm = TRUE)) {
    ""
  } else if (any(words[. + 1] == "close_parens" & doc[.] == " ", na.rm = TRUE)) {
    ""
  } else {
    doc[.]
  })
  
  # and then collapse the whole thing back together
  doc <- paste(doc, collapse = "")
  
  # so we return the document as a single string
  return(doc)
  
}




doc_spelling_fix <- function(doc, words = NULL) {
  #' Attempts to fix spelling errors
  #' 
  #' Spell checks the input using hunspell (ignoring capitalized words)
  #' then attempts to fix misspelled words, either by splitting them into 
  #' valid English or by using suggestions from hunspell. The two possible
  #' solutions are compared on the basis of likelihood and distance from the 
  #' original.
  #' 
  #' @param doc A character vector of word tokens.
  #' @param words A character vector of the same length as doc, labeling
  #' its elements as "word", "symbol" or "cont" (contraction).
  #' 
  #' @return A character vector, likely with more elements than the input. 
  #' 
  #' If `words` is `NULL`, then all elements are assumed to be words
  #' and all capitalized elements are ignored. Otherwise, `words` is used 
  #' to allow the spell checking and fixing to ignore all capitalized 
  #' words except those preceded by a period. 
  
  # figure out which elements of doc we need to check
  # I had previously tried a more complicated logic in 
  # an attempt to exclude tokens which don't need to be checked, 
  # but currently we're just checking everything labeled as a "word",
  # except for name_particles followed by a capital.
  if (!is.null(words)) {
    to_check <- if_else(words == "word", TRUE, FALSE)
    
    not_ignore <- map_lgl(seq_along(words), ~if (doc[.] %in% name_particles & # not checking name particles preceding a name
                                                 str_starts(doc[. + 1], "[A-Z]")) {
      FALSE
    } else {
      NA
    })
    
    to_check <- coalesce(not_ignore, to_check)

  } else {
    to_check <- if_else(str_starts(words, "[A-Z]"), FALSE, TRUE)
  }
  
  # do the spell checking and collect the words to correct
  spelling <- hunspell_check(doc[to_check])
  to_correct <- doc[to_check][!spelling]
  
  # get a set of suggestions using split_words()
  # (note that hunspell suggesting occurs within split_words())
  suggs <- map_chr(to_correct, split_words)
  
  # add words which could not be corrected into the suggs vector (overwriting the NAs)
  suggs[is.na(suggs)] <- to_correct[is.na(suggs)]
  
  # now overwrite the incorrectly spelled words with the suggestions
  # (which now includes the original text where no suggestion could be found)
  doc[to_check][!spelling] <- suggs
  
  # split_words returns strings with words separated by spaces
  # so for consistency of input + output, we split those up
  doc <- unlist(map(doc, ~ if (str_detect(., "\\p{L}+ \\p{L}+")) {
    str_split(., "(?<!^)\\b(?!$)")[[1]] # this splits on word bounardies, but not start or end
  } else {
    .
  }))
  
  return(doc)
  
}




# ---- SPLIT_WORDS FUNCTION AND SUPPORT ----

split_words <- function(s, must_split = FALSE) {
  #' Correct input string via splitting or spell check
  #' 
  #' Attempts to provide the most probable corrected version
  #' of the input string. Considers both the top suggestion
  #' from `hunspell_suggest()` as well as the most probable 
  #' split of `s` into multiple words. For details on the 
  #' splitting algorithm, see the end of the documentation.
  #' 
  #' The suggestions are compared on the basis their
  #' probability, from `get_word_prob()`, adjusted by
  #' how different they are from the input, from `lev_diff()`. 
  #' 
  #' If the original suggestion from hunspell is sufficiently
  #' good, then no splitting is performed unless `must_split`
  #' is set to `TRUE`.
  #' 
  #' @param s the string to be corrected. 
  #' @param must_split a boolean determining whether 
  #' the input must be split regardless of the quality
  #' of the spell check suggestion. 
  #' 
  #' @return a corrected string. If the best correction
  #' involved splitting the string, then the components
  #' are returned as a single string with spaces in 
  #' between.
  #' 
  #' The splitting algorithm used within `split_words()` 
  #' evaluates how likely a given division of string `s`
  #' is based on uni- and bi-gram frequency data from 
  #' the Google Trillion Words corpus (with sensible 
  #' fallbacks when a word is missing, see `get_word_prob()`
  #' for details). I use a dynamic programming approach
  #' which iterates through `s` finding the most probable 
  #' division of `s` up to position `i`, recording it, 
  #' and then using that information to help find the 
  #' optimal split at position `i + 1`, e.g., when evaluating
  #' "areyou" we only ever need to figure out that "are" is the
  #' best division of "are" once, we won't have to check 
  #' possibilites like "a re yo u" (but we might still check
  #' "arey ou"). This strategy basically comes from this 
  #' stackoverflow question: 
  #' https://stackoverflow.com/questions/8870261/how-to-split-text-without-spaces-into-list-of-words
  #' as well as this book (referenced in one of the answers):
  #' http://norvig.com/ngrams/
  #' 
  
  if (!is_character(s)) { # ensure that we in fact have a string at all.
    return(NA_character_)
  } else if(is.na(s)) {
    return(NA_character_)
  } else if(nchar(s) == 0) {
    return("")
  }
  
  # first we see how good a spell check suggestion solution would be
  sugg <- hunspell_suggest(s, 
                           dict = dictionary(custom_med_dict_path))[[1]][1]
  if (!is.na(sugg)) {
    sugg_prob <- get_word_prob(sugg)
    sugg_lev_diff <- lev_diff(sugg, s)
    sugg_adj_prob <- sugg_prob * sugg_lev_diff
    
    # default behavior is to skip splitting if the suggestion is good enough
    # "good enough" is intended to catch typos and misspellings while 
    # still splitting combined words. Current boundary, via a decent amount of 
    # testing, is adj_prob >= -1.5/-2 or Lev Dist <= .1
    if (!must_split) {
      if (sugg_lev_diff <= .1 | sugg_adj_prob > -2) {
        return(sugg)
      } 
    }
  }
  
  
  
  # set up tibble to record the cost of 
  # the best possible division at each position
  # and the string which makes that division
  optimum <- tibble("best_match" = rep("", times = nchar(s) + 1), 
                    "total_prob" = rep(0, times = nchar(s) + 1))
  
  # find the best match up to position i, assuming
  # the string splits after i, record the match and 
  # its cost. repeat until the end of the string.
  for (i in 1:nchar(s)) {
    optimum_i <- best_match(i, s, optimum)
    
    optimum$total_prob[i + 1] <- optimum_i$total_prob
    optimum$best_match[i + 1] <- optimum_i$best_match
  }
  
  # assemble the components of the optimum division
  i <- nchar(s) + 1
  out <- character(0)
  
  while (i > 1) {
    out <- c(optimum$best_match[i], out)
    i <- i - nchar(out[1])
    
  }
  
  # paste back into a single string for
  # consistency input and output and to
  # facilitate the comparison which follows
  out <- paste(out, collapse = " ")
  
  # now compare with the solution spell check provides
  split_lev_diff <- lev_diff(out, s)
  split_adj_prob <- optimum$total_prob[nchar(s) + 1] * split_lev_diff
  
  if (is.na(sugg)) {
    return(out)
  } else if (split_adj_prob > sugg_adj_prob) {
    return(out)
  } else {
    return(sugg)
  }
  
  # is there something to be done about cases where the best answer
  # requires both a split and a spell check suggestion? cause hunspell
  # seems willing to suggest you split two concatenated words, but not 
  # to do that and also correct a type in one of them, and split_words 
  # obviously doesn't do that either. Maybe a stop gap like "if the 
  # solution is sufficiently bad/unlikely, then spell check split_word's
  # output?" but that doesn't solve the problem cause it rarely splits into
  # "misspelled word correct word", instead it will give you more splits 
  # than you'd want with like lone single letters &c.
  # could maybe add a step in split_words to between the "probability 
  # of a correctly spelled word which isn't in the frequency data" and
  # "probability of incorrectly spelled word" which would be "probability
  # of hunspell correction of misspelled word, adjusted so that every two 
  # nonsense syllable winds up corrected". Maybe only run it on candidates
  # longer than 3/4/5 letters or something. 
  # I started working on this at the end of the document, but I'm starting 
  # to think the collateral damage will be too high (i.e., it'll just be
  # screwing up names a lot; although I guess split_words() would do that
  # anyways? UGH).
  
  return(out)
  
}

lev_diff <- function(word_1, word_2) {
  #' Calculate a normalized Levenshtein distance
  #' 
  #' Takes two strings and calculates the Levenshtein distance
  #' between them, then divides by the number of characters
  #' in the longer string to normalize. 
  #' 
  #' @param word_1,word_2 two strings to compare.
  #' 
  #' @return The normalized Levenshtein distance, a double between 0 and 1.
  #' 
  #' The Levenshtein distance between two strings is equal to the number of 
  #' insertions, deletions, or substitutions it would take to transform one 
  #' string to another. It is normalized by dividing my the length of the longer
  #' string. Thus, the result will be 0 if the strings are identical 
  #' and 1 if every single character must be altered to get from the
  #' one string to the other (i.e., the number of changes is equal to
  #' the number of characters). 
  
  len <- max(nchar(word_1), nchar(word_2))
  lev <- adist(word_1, word_2)[1, 1]
  out <- (lev/len)
  return(out)
  
}

best_match <- function(i, s, so_far) {
  #' Find the most probably division of a string
  #' 
  #' Given a string `s`, `best_match()` returns
  #' the most probable division of the substring
  #' up to position `i`, based on the most probable
  #' divisions which have already been found `so_far`.
  #' 
  #' @param i an integer setting the size of the substring being considered.
  #' @param s the source string.
  #' @param so_far a dataframe containing the best matches and their 
  #' probabilities for all the positions considered so far. 
  #' 
  #' @return a single row dataframe containing the most probable 
  #' division and its probability. Only the latter part of the 
  #' division is returned, as the preceding portion is already 
  #' in the appropriate position in `so_far`. 
  
  # get the substring to test
  to_test <- str_sub(s, end = i)

  # enumerate the candidate divisions (word_2) and the 
  # words which precede them (to take advantage of bi-gram frequencies)
  candidates <- map_dfr(1:i, ~tibble("word_1" = so_far$best_match[.],
                                     "word_2" = str_sub(to_test, start = .)))
  
  # use get_word_prob() to get the probability of word_2 following word_1
  cand_prob <- map2_dbl(candidates$word_1, candidates$word_2, get_word_prob)
  
  optimum <- tibble("best_match" = candidates[["word_2"]], 
                    "total_prob" = cand_prob)
  
  # print(optimum)
  # print(so_far)
  
  # get the total probability so far
  # note: these are in fact log probabilities, hence addition rather than 
  # multiplication. 
  optimum$total_prob <- optimum$total_prob + so_far[["total_prob"]][1:i]
  
  # print(optimum)
  
  # select the optimum candidate based on maximizing total probability
  optimum <- optimum %>%
    slice_max(order_by = total_prob, with_ties = FALSE)
  
  return(optimum)
  
}

get_word_prob <- function(x, y = NULL) {
  #' Get the probability of given word in English text
  #' 
  #' This is the workhouse logic inside the `best_match()` function
  #' and thus is the core of the whole `split_words()` pipeline. Can
  #' be used on lone words, in which case `x` is the word being checked,
  #' or (ideally) bi-grams, in which case `y` is.
  #' 
  #' @param x the word (string), immediately preceding `y`, unless 
  #' `y` is `NULL`. Then, `x` is the word whose probability is to be 
  #' checked.
  #' @param y if present, the word whose probability is to be 
  #' checked.
  #' 
  #' @return The natural log of the probability of `x` appearing.
  #' The natural log is used for computational reasons (specifically
  #' the potential for underflow).
  #' 
  #' The probabilities here are based on uni- or bi-gram frequency 
  #' data. If the precise bi-gram being evaluated is not in the 
  #' reference data (the top ~200k bi-grams or top ~50k
  #' uni-grams of the Google Trillion Words corpus), then 
  #' sensible fall backs are used: the probability of a 
  #' word in the bottom 25% of the distribution adjusted 
  #' based on bi-gram length frequency data (e.g., if three letter
  #' words disproportionately follow two letter words, then a 
  #' if `x` is three letters and `y` is two, `x`'s probability will 
  #' be adjusted upwards, even though the specific bi-gram `y x` 
  #' is not in the data). 
  #' 
  #' Cautionary note: given the current word frequency data, 
  #' the maximum word length for which we have data is 
  #' 21/22 characters for bi-grams or 26 characters for 
  #' uni-grams. Probabilities will still be returned for 
  #' longer strings, but they are simply using the values
  #' at those maxima.
  
  # First, check whether we've been given one or two arguments. 
  # If two, then get the probability of the bi-gram. If one, reassign 
  # `x` to `y`, set `x` to be an empty string, and set `prob_2` to length
  # zero, matching its absence from the bi-gram frequency distribution.
  
  y_uni_max <- min(nchar(y), 26)
  x_uni_max <- min(nchar(x), 26)
  y_bi_max <- min(nchar(y), 22)
  x_bi_max <- min(nchar(x), 21)
  
  if (!is.null(y)) {
    x_orig <- x
    x <- str_to_lower(x)
    y_orig <- y
    y <- str_to_lower(y)
    
    prob_2 <- bi_freq[bi_freq[["word_1"]] == x & bi_freq[["word_2"]] == y,][["prob_log"]]
  } else if (is.null(y)) {
    y <- x
    x <- ""
    y_orig <- y
    y <- str_to_lower(y)
    prob_2 <- numeric(0)
  }
  
  if (length(prob_2) > 0) {
    prob_1 <- uni_freq[uni_freq[["word"]] == x,][["prob_log"]] # should I draw from the bi-gram dist for this?
    if (length(prob_1) > 0) {
      return(min(prob_2 - prob_1, -1))
    } else {
      prob_1 <- word_length_prob_log[word_length_prob_log[["word_length"]] == x_uni_max,][["prob_log"]]
      
      return(prob_2 - prob_1)
    }
    
  } else if (y %in% uni_freq[["word"]]) {
    prob_2 <- uni_freq[uni_freq[["word"]] == y,][["prob_log"]]
    
    # trying out using the length distribution again, basically attempting to get
    # P(y | nchar(x)) by getting P(y | nchar(y)) * P(nchar(y) | nchar(x)) 
    # (could also make the first term just P(y) if this turns out to bias again)
    
    if (nchar(x) == 0) {
      # print("cond 2")
      return(prob_2)
    } else {
      # print("cond 3")
      # P(y | nchar(y)) = P(y & nchar(y)) / P(nchar(y)); note that P(y & nchar(y)) = P(y)
      prob_2 <- prob_2 - word_length_prob_log[word_length_prob_log[["word_length"]] == y_uni_max,][["prob_log"]]
      # P(nchar(y) | nchar(x)) = P(nchar(y) & nchar(x)) / P(nchar(x))
      prob_1 <- bi_len_freq[bi_len_freq[["word_1"]] == x_bi_max & bi_len_freq[["word_2"]] == y_bi_max,][["prob_log"]]
      prob_1 <- prob_1 - word_length_prob_log[word_length_prob_log[["word_length"]] == x_uni_max,][["prob_log"]]
      return(prob_2 + prob_1)
    }
  } else if (hunspell::hunspell_check(y, dict = dictionary(custom_med_dict_path))) {
    # instead of a very low probability which increases exponentially with length,
    # using a somewhat higher probability (to try and make up for the difference 
    # between our corpus and the one the frequencies are drawn from) which increases
    # based on the distribution of bigram length frequencies
    # So we want P(nchar(y) | nchar(x)) and then make some adjustment for the
    # relative lack of frequency
    
    if (nchar(x) == 0) {
      # in the absence of word_1, just use word length frequency data for unigrams
      # this is saying "the probability of a word of this length, adjusting for the fact
      # that we know it's really uncommon" (prob_2 below is saying "the chance of this very uncommon word, 
      # given that we know the length", which is why they're different)
      prob_2 <- -13 + word_length_prob_log[word_length_prob_log[["word_length"]] == y_uni_max,][["prob_log"]]
      return(prob_2)
    } else {
      # -14 for P(y) assumes that these words are a little less common than the 25% quantile of words (-13.85)
      prob_2 <- -14 - word_length_prob_log[word_length_prob_log[["word_length"]] == y_uni_max,][["prob_log"]]
      prob_1 <- bi_len_freq[bi_len_freq[["word_1"]] == x_bi_max & bi_len_freq[["word_2"]] == y_bi_max,][["prob_log"]]
      prob_1 <- prob_1 - word_length_prob_log[word_length_prob_log[["word_length"]] == x_uni_max,][["prob_log"]]
      
      return(prob_2 + prob_1)
    }
    
  } else {
    #print("cond 5")
    # trying out some logic to treat segments in all caps differently, to try and let acronyms 
    # and stuff get through. what I'm currently doing is given them just the probability of
    # an average word of their length
    if (str_detect(y_orig, "^[A-Z]+$")) {
      return(word_length_prob_log[word_length_prob_log[["word_length"]] == y_uni_max,][["prob_log"]])
    } else {
      return(-14 - 2^nchar(y))
    }
  }
  # potential improvement by making the all-caps check a separate step at the end, 
  # comparing the probability of the standard process vs. either 1) a pretty high 
  # probability *if the all-caps is in the documents named entity dict*, or
  # 2) a probability which is just very slightly higher than the standard 
  # failed spell check probability. I need this kind of differentiation because
  # just using probability based on word length makes them way too likely. this is
  # relatively okay when we're considering only things which have gotten to the end, 
  # but it would do things like wind up preferring "MUSTDO" over "MUST DO", I think.
}


# ---- SETTING WORD FREQUENCY DATA----
# Creating the uni-gram and bi-gram (and uni/bi-gram length) frequency data

# may want to instead calculate this based on the 
# unigram counts from the bigram frequency table
uni_freq <- read_tsv(paste(file_path_prefix, script_folder, "count_1w.txt", sep = "/"), 
                     col_names = c("word", "freq"), na = character())
#uni_freq <- uni_freq %>% slice_head(prop = .5)
uni_freq$word <- uni_freq$word %>% str_to_lower()

N <- sum(uni_freq$freq)

uni_freq$prob <- uni_freq$freq/N
uni_freq$prob_log <- log(uni_freq$prob)

# adding P(word & nchar(word)) for use with the bi-gram length distribution
# also, could create this with the bi-gram distribution, or even make 
# a separate one for first vs. second words
word_length_prob_log <- map_dfr(unique(nchar(uni_freq$word)), 
                                ~tibble("word_length" = ., 
                                        "prob" = sum(uni_freq[["freq"]][nchar(uni_freq[["word"]]) == .])/N))

word_length_prob_log$prob_log <- log(word_length_prob_log$prob)

word_length_prob_log <- map_dfr(1:max(word_length_prob_log[["word_length"]]), 
                                ~if (. %in% word_length_prob_log[["word_length"]]) {
                                  word_length_prob_log[word_length_prob_log[["word_length"]] == ., ]
                                  } else {
                                    tibble("word_length" = ., 
                                           "prob" = min(word_length_prob_log[["prob"]][1:.], na.rm = TRUE),
                                           "prob_log" = min(word_length_prob_log[["prob_log"]][1:.], na.rm = TRUE))
                                    })


uni_freq <- uni_freq %>%
  add_row(word = "<s>", freq = 1, prob = 1, prob_log = 0)


bi_freq <- read_tsv(paste(file_path_prefix, script_folder, "count_2w.txt", sep = "/"), 
                    col_names = c("word", "freq"), na = character())
bi_freq <- bi_freq %>% # deal with, e.g., "EBay" v "Ebay" v "ebay"
  mutate(word = str_to_lower(word)) %>%
  group_by(word) %>%
  summarize(freq = sum(freq)) %>%
  ungroup()

bi_freq[, c("word_1", "word_2")] <- str_split_fixed(bi_freq$word, " ", 2)
bi_freq <- bi_freq %>% 
  filter(!str_detect(word_1, "\\d")) %>%
  filter(!str_detect(word_2, "\\d"))

N <- sum(bi_freq$freq)

bi_freq$prob <- bi_freq$freq/N
bi_freq$prob_log <- log(bi_freq$prob)

bi_len_freq <- bi_freq %>%
  mutate(word_1 = nchar(word_1),
         word_2 = nchar(word_2)) %>%
  group_by(word_1, word_2) %>%
  summarize(freq = sum(freq))

N <- sum(bi_len_freq$freq)

bi_len_freq$prob <- bi_len_freq$freq/N
bi_len_freq$prob_log <- log(bi_len_freq$prob)

# filling in missing combinations
bi_len_freq <- left_join(expand_grid("word_1" = 1:21, "word_2" = 1:22), bi_len_freq, 
          by = c("word_1", "word_2")) %>% 
  mutate(word = if_else(word_1 > word_2, word_1, word_2))

to_replace <- which(is.na(bi_len_freq$prob_log))

bi_len_freq[to_replace, ]$prob_log <- 
  word_length_prob_log[match(bi_len_freq[to_replace, ]$word, word_length_prob_log$word_length), ]$prob_log - 1.3

bi_len_freq <- bi_len_freq %>%
  select(-word)





# ---- experimenting with spell checking ----

best_match_sp <- function(i, s, so_far) {
  # not done yet, since I'm not sure if adding the spell checking is even a good idea.
  to_test <- str_sub(tolower(s), end = i)
  
  candidates <- map_dfr(1:i, ~tibble("word_1" = so_far$best_match[.],
                                     "word_2" = str_sub(to_test, start = .)))
  
  cand_prob <- map2_dbl(candidates$word_1, candidates$word_2, get_word_prob)
  
  optimum <- tibble("best_match" = candidates[["word_2"]], 
                    "total_prob" = cand_prob)
  
  optimum <- map2_dfr(candidates$word_1, candidates$word_2, get_word_prob)
  
  # print(optimum)
  # print(so_far)
  
  optimum$total_prob <- optimum$total_prob + so_far[["total_prob"]][1:i]
  
  # print(optimum)
  
  optimum <- optimum %>%
    slice_max(order_by = total_prob, with_ties = FALSE)
  
  return(optimum)
  
}

get_word_prob_sp <- function(x, y = NULL) {
  #' Get the probability of given word in English text
  #' 
  #' This is the workhorse logic inside the `best_match()` function
  #' and thus is the core of the whole `split_words()` pipeline. Can
  #' be used on lone words, in which case `x` is word being checked,
  #' or (ideally) bi-grams, in which case `y` is.
  #' 
  #' @param x the word (string), immediately preceding `y`, unless 
  #' `y` is `NULL`. Then, `x` is the word whose probability is to be 
  #' checked.
  #' @param y if present, the word whose probability is to be 
  #' checked.
  #' 
  #' @return A one-row dataframe with the word being checked (or, 
  #' potentially, a spell check corrected version thereof) and the 
  #' natural log of the probability of `x`/`y` appearing. The 
  #' natural log is used for computational reasons (specifically
  #' the potential for underflow).
  #' 
  #' The probabilities here are based on uni- or bi-gram frequency 
  #' data. If the precise bi-gram being evaluated is not in the 
  #' reference data (the top ~200k bi-grams or top ~50k
  #' uni-grams of the Google Trillion Words corpus), then 
  #' sensible fall backs are used: the probability of a 
  #' word in the bottom 25% of the distribution adjusted 
  #' based on bi-gram length frequency data (e.g., if three letter
  #' words disproportionately follow two letter words, then a 
  #' if `x` is three letters and `y` is two, `x`'s probability will 
  #' be adjusted upwards, even though the specific bi-gram `y x` 
  #' is not in the data). 
  #' 
  #' In addition, if the string being checked is longer than 3 
  #' characters and fails a spell check, then `hunspell_suggest()`
  #' is used to provide a potentially correct alternative. The 
  #' probability of this word is obtained by the same logic as the 
  #' original, but is only used if it `solution_compare()` judges
  #' it the better option. 
  
  # First, check whether we've been given one or two arguments. 
  # If two, then get the probability of the bi-gram. If one, reassign 
  # `x` to `y`, set `x` to be an empty string, and set `prob_2` to length
  # zero, matching its absence from the bi-gram frequency distribution.
  if (!is.null(y)) {
    prob_2 <- bi_freq[bi_freq[["word_1"]] == x & bi_freq[["word_2"]] == y,][["prob_log"]]
  } else if (is.null(y)) {
    y <- x
    x <- ""
    prob_2 <- numeric(0)
  }
  
  if (length(prob_2) > 0) {
    prob_1 <- uni_freq[uni_freq[["word"]] == x,][["prob_log"]] # should I draw from the bigram dist for this?
    return(prob_2 - prob_1)
  } else if (y %in% uni_freq[["word"]]) {
    prob_2 <- uni_freq[uni_freq[["word"]] == y,][["prob_log"]]
    
    # trying out using the length distribution again, basically attempting to get
    # P(y | nchar(x)) by getting P(y | nchar(y)) * P(nchar(y) | nchar(x)) 
    # (could also make the first term just P(y) if this turns out to bias again)
    
    if (nchar(x) == 0) {
      # print("cond 2")
      return(prob_2)
    } else {
      # print("cond 3")
      # P(y | nchar(y)) = P(y & nchar(y)) / P(nchar(y)); note that P(y & nchar(y)) = P(y)
      prob_2 <- prob_2 - word_length_prob_log[word_length_prob_log[["word_length"]] == nchar(y),][["prob_log"]]
      # P(nchar(y) | nchar(x)) = P(nchar(y) & nchar(x)) / P(nchar(x))
      prob_1 <- bi_len_freq[bi_len_freq[["word_1"]] == nchar(x) & bi_len_freq[["word_2"]] == nchar(y),][["prob_log"]]
      prob_1 <- prob_1 - word_length_prob_log[word_length_prob_log[["word_length"]] == nchar(x),][["prob_log"]]
      return(prob_2 + prob_1)
    }
  } else if (hunspell::hunspell_check(y)) {
    # instead of a very low probability which increases exponentially with length,
    # using a somewhat higher probability (to try and make up for the difference 
    # between our corpus and the one the frequencies are drawn from) which increases
    # based on the distribution of bigram length frequencies
    # So we want P(nchar(y) | nchar(x)) and then make some adjustment for the
    # relative lack of frequency
    
    if (nchar(x) == 0) {
      # in the absence of word_1, just use word length frequency data for unigrams
      # this is saying "the probability of a word of this length, adjusting for the fact
      # that we know it's really uncommon" (prob_2 below is saying "the chance of this very uncommon word, 
      # given that we know the length", which is why they're different)
      prob_2 <- -13 + word_length_prob_log[word_length_prob_log[["word_length"]] == nchar(y),][["prob_log"]]
      return(prob_2)
    } else {
      # -14 for P(y) assumes that these words are a little less common than the 25% quantile of words (-13.85)
      prob_2 <- -14 - word_length_prob_log[word_length_prob_log[["word_length"]] == nchar(y),][["prob_log"]]
      prob_1 <- bi_len_freq[bi_len_freq[["word_1"]] == nchar(x) & bi_len_freq[["word_2"]] == nchar(y),][["prob_log"]]
      prob_1 <- prob_1 - word_length_prob_log[word_length_prob_log[["word_length"]] == nchar(x),][["prob_log"]]
      
      return(prob_2 + prob_1)
    }
    
  } else {
    #print("cond 5")
    return(-14 - 2^nchar(y))
  }
}

