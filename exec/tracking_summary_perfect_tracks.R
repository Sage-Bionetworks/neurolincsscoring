#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(synapser))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(neurolincsscoring))

option_list <- list(
  make_option(c("--tracking_file"), type = "character",
              help = "Path or Synapse ID to tracking file.",
              dest = "tracking_file"),
  make_option(c("--curated_data_table"), type = "character",
              help = "Path or ID containing curated data.",
              dest = "curated_file",
              default = "syn18344955"),
  make_option(c("--json"), type = "logical",
              action = "store_true",
              help = "Write output in JSON format.",
              dest = "json",
              default = FALSE)
)

opt <- parse_args(OptionParser(option_list = option_list))

if (stringr::str_detect(opt$tracking_file, "^syn.*")) {
  syn_tracking_file <- TRUE
  get_tracking_file_fxn <- neurolincsscoring::syn_get_tracking_submission_file
} else {
  syn_tracking_file <- FALSE
  get_tracking_file_fxn <- neurolincsscoring::read_tracking_submission_file
}

if (stringr::str_detect(opt$curated_file, "^syn.*")) {
  syn_curated_file <- TRUE
  get_curated_data_fxn <- neurolincsscoring::syn_get_curated_data
} else {
  syn_curated_file <- FALSE
  get_curated_data_fxn <- neurolincsscoring::read_curated_data
}

if (syn_tracking_file | syn_curated_file) {
  foo <- capture.output(synLogin())
}

## ----get-tracking-results, message = FALSE---------------------------------
trackingResults <- get_tracking_file_fxn(opt$tracking_file)

testexpts <- assertthat::assert_that(length(unique(trackingResults$Experiment)) == 1,
                                     msg = "Did not find a single experiment in tracking file.")

## ----get-curated-data----------------------------------------------------
curatedData <- get_curated_data_fxn(opt$curated_file)

trackingResults <- trackingResults %>%
  assertr::verify(trackingResults$Experiment %in% curatedData$Experiment)

curatedData <- curatedData %>%
  dplyr::filter(Experiment %in% unique(trackingResults$Experiment))

message(sprintf("Curated data has %s rows\n", nrow(curatedData)))
message(sprintf("Tracking submission has %s rows\n", nrow(trackingResults)))

## ----merge-curated-and-tracked-------------------------------------------
merged <- dplyr::full_join(curatedData,
                           trackingResults,
                           by = c("Experiment" = "Experiment",
                                  "Well" = "Well",
                                  "TimePoint" = "TimePoint",
                                  "ObjectLabelsFound" = "ObjectLabelsFound"))

merged <- merged %>%
  dplyr::mutate(matched = ObjectTrackID.x == ObjectTrackID.y)

merged$matched[is.na(merged$matched)] <- FALSE

## ----percentagetable-expt-well-object------------------------------------
res <- neurolincsscoring::score_perfect_tracks(merged)

check_n_rows <- assertthat::assert_that(nrow(res) == 1, msg = "Number of rows in result is not equal to 1.")

if (opt$json) {
  cat(jsonlite::toJSON(as.list(res), auto_unbox = TRUE))
} else {
  cat(readr::format_csv(res))
}
