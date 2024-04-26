#' Author: Steven Lauterwasser
#' 
#' This script is the workhorse of the text processing pipeline 
#' called from `gen_pub_txts.R`. Details on inputs and outputs can be 
#' found there, details on process are discussed and should be made clear  
#' below, and details on the functions being used can be found in 
#' `sample_processing_pipeline_functions.R`.

#' Before this can be run, several things (listed below) must happen. 
#' All of them are accomplished in `gen_pub_txts.R`, and
#' the intent is for that to be used to set the global variables
#' which might vary from machine to machine when running the pipeline. 
#' Those include (descriptions in that script): 
#' - The location of `spaCy` for `spacyr`. The actual initialization
#'   happens there as well. 
#' - A number of file path related variables: 
#'   - `file_path_prefix`
#'   - `script_folder`
#'   - `input_folder`
#'   - `main_output_folder`
#'   - `prelim_output_folder`
#'   - `final_output_folder`
#' - The `furrr` multiprocessing variables `schedule` and `workers`. 
#' 
#' Overview of the pipeline: 
#' 
#' -- Setup --
#' 
#' There are two odd or complicated packages this script relies upon which
#' I want to call out: 
#' - Tokenization of documents is performed using `spaCy`, a Python NLP,
#'   via the R library `spacyr`, which acts as a wrapper. As a result, 
#'   this script also requires the `reticulate` package, an install of 
#'   Python with `--enable-shared` to make it available to `reticulate`, 
#'   an install of `spaCy` and one of its language backends, and finally
#'   (as mentioned above) the location/environment of that `spaCy` install
#'   so that `spacyr` can find it for initialization. Details on `spaCy`
#'   generally can be found [here](https://spacy.io) and documentation 
#'   for `spacyr` is available [here](https://spacyr.quanteda.io) (this
#'   includes clean and simple install instructions which should make all
#'   this straightforward).
#' - This pipeline makes extensive use of `furrr` for easy multiprocessing
#'   as well as `progressr` to display progress updates. Because `progressr`
#'   relies on internal changes to function language to work, I've written 
#'   `progressr_func()` as a function factory to allow for easy creation of
#'   `progressr` ready functions on the fly. It's basically a convenient
#'   wrapper for various kinds of `future_map()` statements, details in 
#'   `sample_processing_pipeline_functions.R`.

library(tidyverse)
library(furrr)
library(readtext)
library(progressr)
library(spacyr)

source(paste(file_path_prefix, script_folder, "sample_processing_pipeline_functions.R", sep = "/"))


# ---- SETUP AND PRELIMINARY PROCESSING----
# create error log
write_csv(tibble("loc" = character(0),
                 "string" = character(0), 
                 "call" = character(0),
                 "message" = character(0)), 
          paste(file_path_prefix, script_folder, "error_log.csv", sep = "/"))

start <- proc.time()

# load in the documents
raw_docs <- readtext(paste(file_path_prefix, input_folder, "*.txt", sep = "/"))
doc_ids <- raw_docs$doc_id

# set up multiprocessing
# Note: the use of `multicore` instead of `multisession` (which
# spins off separate R sessions instead of forking them from the 
# main one) is important here, because a number of functions rely
# on shared variables which will be out of scope and not carried
# into new sessions (while they are present in forked sessions). 
plan(multicore, workers = workers)


# perform preliminary processing: normalizing unicode, dealing with 
# obvious glyph confusions, etc.
in_process <- future_map(raw_docs$text, prelim_process, 
                         .options = furrr_options(scheduling = schedule, 
                                                  seed = TRUE))

# write the minimally processed documents out to disk. 
# additional processing from here will be necessarily destructive
# in a way the preliminary processing largely is not, so a record is kept. 
walk2(in_process, doc_ids, ~write_lines(.x, paste(file_path_prefix, main_output_folder, 
                                                  prelim_output_folder, .y, sep = "/")))

print("Preliminary processing and export complete.")

# ---- TOKENIZATION AND TESTING ----

# test for spacing issues
with_progress({
  spacing_test <- progressr_func(spacing_error_test, future_map_chr)(in_process, schedule,
                                                                     req_matches = 10)
  
})

print("Test for spacing errors complete.")

# spaCy is theoretically multicore already
# but I don't think it's acting that way,
# using furrr instead
with_progress({
  in_process <- progressr_func(pipeline_tokenize, future_map)(in_process, schedule)
  
})

print("Tokenization complete.")

# perform quality tests on the tokenized documents
# by spell checking a sample of tokens (10% of a 
# document's alphabetic tokens, minimum 50). 
# Documents which return better than 98% correct
# are assumed to not need further corrective processing.
with_progress({
  doc_test_results <- progressr_func(doc_test, future_map_dfr)(in_process, schedule, 
                                                               mode = "assess", 
                                                               dict = dictionary(custom_med_dict_path), 
                                                               prop = .1, pass = .98)
})

print("Document testing complete.")

# add spacing test results
doc_test_results$spacing <- spacing_test

# and add IDs back
doc_test_results$doc_id <- doc_ids

# and make a judgement call on which have "failed"
# currently saying pass rate below .4 *and* 
# split passing rate below .4
doc_test_results$failed <- case_when(is.na(doc_test_results$to_process) ~ TRUE,
                                     doc_test_results$prop_pass < .4 & 
                                       doc_test_results$prop_split < .4 ~ TRUE, 
                                     TRUE ~ FALSE)

# then save for later evaluation
saveRDS(doc_test_results, paste(file_path_prefix, main_output_folder, "doc_test_results.rds", sep = "/"))

# now sort out documents which we're not processing due to failure
failed_docs <- map_chr(in_process[doc_test_results$failed], paste, collapse = "")
failed_ids <- doc_ids[doc_test_results$failed]

in_process <- in_process[!doc_test_results$failed]
doc_ids <- doc_ids[!doc_test_results$failed]

doc_test_results_failed <- doc_test_results[doc_test_results$failed,]
doc_test_results <- doc_test_results[!doc_test_results$failed,]

print(str_glue("{nrow(doc_test_results_failed)} documents failed."))

# and now divert the ones which don't need processing
no_process <- map_chr(in_process[!doc_test_results$to_process], paste, collapse = "")
no_proc_ids <- doc_ids[!doc_test_results$to_process]

print(str_glue("{length(no_process)} documents diverted with no further processing."))

walk2(no_process, no_proc_ids, ~write_lines(.x, paste(file_path_prefix, main_output_folder, 
                                                      final_output_folder, .y, sep = "/")))

in_process <- in_process[doc_test_results$to_process]
doc_ids <- doc_ids[doc_test_results$to_process]

print(str_glue("{length(in_process)} documents remaining."))

end_pre_process <- proc.time()

pre_process_time <- end_pre_process - start

print(str_glue("Pre-processing took {hms::as_hms(pre_process_time[3])}."))

# ---- MAIN PROCESSING ----

# and now the actual processing
with_progress({
  processed_docs <- progressr_func(process_doc, future_map_chr)(in_process, schedule)
})

end_processing <- proc.time()

process_time <- end_processing - end_pre_process

print(str_glue("Processing took {hms::as_hms(process_time[3])}."))

plan(sequential)

processed_docs <- tibble("doc_id" = doc_ids,
                         "text" = processed_docs)

# ---- EVALUATION AND OUTPUT ----

# ---- see how much processing improved things ----
processed_docs_tok <- map(processed_docs$text, pipeline_tokenize)

with_progress({
  doc_test_results_post <- progressr_func(doc_test, future_map_dfr)(processed_docs_tok, schedule, 
                                                               mode = "assess", 
                                                               dict = dictionary(custom_med_dict_path), 
                                                               prop = .1, pass = .98)
})

# and add IDs back
doc_test_results_post$doc_id <- processed_docs$doc_id

saveRDS(doc_test_results_post, paste(file_path_prefix, main_output_folder, "doc_test_results_post.rds", sep = "/"))

# anything fail now? (under slightly stricter guidelines)
doc_test_results_post$failed <- case_when(is.na(doc_test_results_post$to_process) ~ TRUE,
                                          doc_test_results_post$prop_pass < .6 & 
                                            doc_test_results_post$prop_split < .6 ~ TRUE, 
                                          TRUE ~ FALSE)



# ---- save pipeline output ----
walk2(processed_docs$text, processed_docs$doc_id,
      ~write_file(.x, paste(file_path_prefix, main_output_folder, final_output_folder, .y, sep = "/")))

# reincorporate the diverted docs so that 
# everything ends up in one dataframe
final_docs <- tibble("pub_file_name" = c(doc_ids, no_proc_ids),
                     "text" = c(processed_docs$text, no_process)) %>%
  mutate(pub_file_name = str_remove(pub_file_name, "\\.txt$"))

failed_docs <- tibble("pub_file_name" = c(failed_ids, 
                                          doc_test_results_post$doc_id[doc_test_results_post$failed]),
                      "text" = c(failed_docs, processed_docs$text[doc_test_results_post$failed])) %>%
  mutate(pub_file_name = str_remove(pub_file_name, "\\.txt$"))

# filter out failed docs
final_docs <- final_docs %>% 
  filter(!(pub_file_name %in% failed_docs$pub_file_name))

# and save the output
saveRDS(final_docs, paste(file_path_prefix, main_output_folder, "pub_texts_clean.rds", sep = "/"))

saveRDS(failed_docs, paste(file_path_prefix, main_output_folder, "pub_texts_failed.rds", sep = "/"))


