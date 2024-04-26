#' Author: Steven Lauterwasser
#' 
#' Summary: This script inputs the raw text files from `Pubs_full_texts_txt`
#' and outputs processed and cleaned versions, both as text files to 
#' `Pubs_full_texts_txt` and `Pubs_full_texts_txt_proc` and as a dataframe
#' to `pub_txts_clean.rds`. Additional diagnostics are recorded as well and
#' are described below.
#' 
#' Note: the whole pipeline takes a significant amount of time to run, 
#' something on the order of 12 hours running on 12 cores, so plan 
#' accordingly.
#' 
#' Input: `Pubs_full_texts_txt`
#' Output: 
#' - `Pubs_full_texts_txt`
#' - `Pubs_full_texts_txt_proc`
#' - `pub_txts_clean.rds`
#' - `pub_txts_failed.rds`
#' - `doc_test_results.rds`
#' - `doc_test_results_post.rds`
#' 
#' 
#' This is a wrapper script for `processing_pipeline.R`, which
#' is where all of the actual work happens (except for function 
#' definitions, which are sourced in `processing_pipeline.R` and
#' live in `processing_pipeline_functions.R`).  
#' The purpose of this script is to set a number of global, 
#' machine-dependent, variables which the pipeline requires to run
#' and then to actually set the pipeline in motion. 
#' Those variables and their functions are listed below.
#' 
#' - `file_path_prefix`: sets a path to prefix the other path variables.
#' - `script_folder`: the folder the scripts reside in, in relation to `file_path_prefix`.
#' - `input_folder`: the folder containing the input text files.
#' - `main_output_folder`: where to place the output.Must contain
#'   `prelim_output_folder` and `final_output_folder`. 
#' - `prelim_output_folder`: where to place the output text files
#'   which received only basic cleaning. Must exist before the script 
#'   is run and must be inside `main_output_folder`. 
#' - `final_output_folder`: as `prelim_output_folder` but for the final, 
#'   fully processed files.
#'   
#' - `python_executable`, `virtualenv`, or `condaenv`: one of these must
#'   be set within the `spacy_initialize()` call so that `spacyr` can find
#'   where `spaCy` for Python has been installed. 
#'   
#' - `workers`: an integer which tells `furrr` how many cores to use
#'   (workers to create). 
#' - `schedule`: an integer which determines how `furrr` chunks input
#'   for multiprocessing. Specifically, it sets the number of chunks
#'   `furrr` attempts to give each worker. Thus, `schedule = 1` (the 
#'   default) results as many chunks as there are workers. In cases,
#'   like ours, where chunks may take wildly different amounts of time
#'   to process, this can be tremendously inefficient. I try to set it 
#'   such that each chunk is between 15 and 30 documents. 
#'   
#' Sourcing this script will create the following output: 
#' - in `script_folder`:
#'   - `error_log.csv`: dataframe recording any errors. Contains
#'     the location (in the pipeline) of the error, the string
#'     which caused it, and the message and call from the error
#'     itself.
#' - in `main_output_folder`: 
#'   - `pub_texts_clean_full.rds`: a dataframe containing the
#'     document names (without file type extension) and their 
#'     fully processed text. Omits documents which failed
#'     `doc_test()` and were not processed as well as those
#'     which were still inexcusably bad post-processing. 
#'   - `pub_texts_failed_full.rds`: a dataframe containing 
#'     document names and processed text for all document
#'     (which have text) which were excluded from 
#'     `pub_texts_clean_full.rds`.
#'   - `doc_test_results.rds`: a dataframe of test results
#'     from `doc_test()` and `spacing_error_test()` for every 
#'     document. Includes `to_process` and `failed`, booleans
#'     which describe whether the document received further
#'     processing or did sufficiently badly processing could
#'     not improve it.
#'   - `doc_test_results_post.rds`: test results from 
#'     processed documents, to allow for comparison, 
#'     assessment, and removal of documents still of 
#'     low quality. 
#' - in `prelim_output_folder`: a minimally processed version 
#'   of each and every input document. 
#' - in `final_output_folder`: a fully processed version of 
#'   each document which was not marked as `failed` in 
#'   `doc_test_results.rds`.
#'   
#' For details on the logic and functioning of the pipeline,
#' see the documentation in `processing_pipeline.R` and for
#' details on the functions used, see `process_pipeline_functions.R`.
#'   

library(tidyverse)
library(furrr)
library(readtext)
library(progressr)
library(spacyr)

# to set before running, will vary by machine/location
# currently set as with the assumption that the top level
# "ADVANCE" folder is the working directory 
file_path_prefix <- "data_text"
script_folder <- "scripts"
input_folder <- "publication_docs/Pubs_full_texts_txt"
main_output_folder <- "publication_docs"
prelim_output_folder <- "Pub_full_texts_txt"
final_output_folder <- "Pub_full_texts_txt_proc"

workers <- 12
schedule <- 6

# 
spacy_initialize(virtualenv = "~/.pyenv/versions/3.10.6/envs/text_analysis")

source(paste(file_path_prefix, script_folder, "processing_pipeline.R", sep = "/"))


