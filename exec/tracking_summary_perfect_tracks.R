#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(synapser))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(neurolincsscoring))

read_args <- function() {
  option_list <- list(
    make_option(c("--tracking_file"), type = "character",
                help = "Path or Synapse ID to tracking file.",
                dest = "tracking_file"),
    make_option(c("--curated_data_table"), type = "character",
                help = "Path or ID containing curated data.",
                dest = "curated_file",
                default = "syn18344955"),
    make_option(c("--per_well"), type = "logical",
                action = "store_true",
                help = "Report results per well instead of across all wells.",
                dest = "per_well",
                default = FALSE))
  opt <- parse_args(OptionParser(option_list = option_list))
  return(opt)
}

get_tracking_file_reader <- function(tracking_file_path) {
  tracking_file_reader <- list(isSynapseId = FALSE, read = NULL)
  if (stringr::str_detect(tracking_file_path, "^syn.*")) {
    tracking_file_reader$isSynapseId <- TRUE
    tracking_file_reader$read <- neurolincsscoring::syn_get_tracking_submission_file
  } else {
    tracking_file_reader$isSynapseId <- FALSE
    tracking_file_reader$read <- readr::read_csv
  }
  return(tracking_file_reader)
}

get_curated_file_reader <- function(curated_file_path) {
  curated_file_reader <- list(isSynapseId = FALSE, read = NULL)
  if (stringr::str_detect(curated_file_path, "^syn.*")) {
    curated_file_reader$isSynapseId <- TRUE
    curated_file_reader$read <- neurolincsscoring::syn_get_curated_data
  } else {
    curated_file_reader$isSynapseId <- FALSE
    curated_file_reader$read <- neurolincsscoring::read_curated_data
  }
  return(curated_file_reader)
}

conditional_synLogin <- function(syn_tracking_file, syn_curated_file) {
  if (syn_tracking_file | syn_curated_file) {
    synLogin(silent = TRUE)
  }
}

read_tracking_file <- function(tracking_file_path, read) {
  trackingResults <- tryCatch({
    readr::stop_for_problems(
      read(tracking_file_path))
  }, error = function(e) {
    return("The tracking file is not readable as a csv format.")
  })
  return(trackingResults)
}

read_curated_table <- function(curated_table_path, read) {
  curatedData <- tryCatch({
    readr::stop_for_problems(
      read(curated_table_path))
  }, error = function(e) {
    return("The curated data table is not readable as a csv format.")
  })
  return(curatedData)
}

cat_invalid_reasons <- function(invalid_reasons) {
  cat(jsonlite::toJSON(list(status = "INVALID", invalid_reasons = invalid_reasons)))
}

score_tracking_results <- function(trackingResults, curatedData, per_well) {
  curatedData <- curatedData %>%
    dplyr::filter(Experiment %in% unique(trackingResults$Experiment))
  message(sprintf("Curated data has %s rows\n", nrow(curatedData)))
  message(sprintf("Tracking submission has %s rows\n", nrow(trackingResults)))
  merged <- dplyr::full_join(curatedData,
                             trackingResults,
                             by = c("Experiment" = "Experiment",
                                    "Well" = "Well",
                                    "TimePoint" = "TimePoint",
                                    "ObjectLabelsFound" = "ObjectLabelsFound"))

  merged <- merged %>%
    dplyr::mutate(matched = ObjectTrackID.x == ObjectTrackID.y)

  merged$matched[is.na(merged$matched)] <- FALSE

  if (per_well) {
    res <- neurolincsscoring::score_perfect_tracks_per_well(merged)
  } else {
    res <- neurolincsscoring::score_perfect_tracks(merged)
    check_n_rows <- assertthat::assert_that(
      nrow(res) == 1, msg = "Number of rows in result is not equal to 1.")
  }
  cat(jsonlite::toJSON(
    list(
      status = "SCORED",
      invalid_reasons = NULL,
      results = as.list(res)),
    auto_unbox = TRUE))
}

main <- function() {
  opt <- read_args()
  invalid_reasons = NULL # If the tracking file is invalid, we will output this value as json
  tracking_file_reader <- get_tracking_file_reader(opt$tracking_file)
  curated_file_reader <- get_curated_file_reader(opt$curated_file)
  conditional_synLogin(tracking_file_reader$isSynapseId,
                       curated_file_reader$isSynapseId)

  trackingResults <- read_tracking_file(opt$tracking_file, tracking_file_reader$read)
  if (is.character(trackingResults)) { # returned invalid reason
    cat_invalid_reasons(trackingResults)
    return()
  }

  curatedData <- read_curated_table(opt$curated_file, curated_file_reader$read)
  if (is.character(curatedData)) { # returned invalid reason
    cat_invalid_reasons(curatedData)
    return()
  }

  invalid_reasons <- neurolincsscoring::validate_tracking_results(
                         trackingResults, curatedData)
  if (length(invalid_reasons)) {
    cat_invalid_reasons(invalid_reasons)
    return()
  }
  score_tracking_results(trackingResults, curatedData, opt$per_well)
}

main()
