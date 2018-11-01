#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(synapser))
suppressPackageStartupMessages(library(tidyverse))
library(neurolincsscoring)
suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option(c("--synapse_file_id"), type = "character",
              help = "File ID containing tracking information.",
              dest = "synapse_file_id",
              metavar = "synapseid")
)

opt <- parse_args(OptionParser(option_list = option_list))

foo <- capture.output(synLogin())

manuallyCuratedId <- 'syn11378063'

## ----get-tracking-results, message = FALSE---------------------------------
trackingResults <- neurolincsscoring::syn_get_tracking_submission_file(opt$synapse_file_id)

## ----get-curated-data----------------------------------------------------
curatedDataRaw <- neurolincsscoring::syn_get_curated_data(manuallyCuratedId)

trackingResults <- trackingResults %>%
  assertr::verify(trackingResults$Experiment %in% curatedDataRaw$Experiment)

curatedData <- curatedDataRaw %>%
  filter(!is.na(ObjectTrackID), !Lost_Tracking) %>%
  distinct() # This shouldn't be required

# This shouldn't be required
curatedData <- curatedData %>%
  count(Experiment, Well, TimePoint, ObjectLabelsFound, ObjectTrackID) %>%
  filter(n > 1) %>%
  anti_join(curatedData, .,
            by=c("Experiment", "Well", "TimePoint", "ObjectLabelsFound", "ObjectTrackID"))

curatedData <- curatedData %>%
  count(Experiment, Well, TimePoint, XCoordinate, YCoordinate) %>%
  filter(n > 1) %>%
  anti_join(curatedData, .,
            by=c("Experiment", "Well", "TimePoint", "XCoordinate", "YCoordinate"))

## ----remove-after-t-zero-------------------------------------------------
curatedData <- curatedData %>%
  group_by(Experiment, Well, ObjectTrackID) %>%
  summarize(mintime = min(TimePoint)) %>%
  ungroup() %>%
  filter(mintime > 0) %>%
  anti_join(curatedData, .,
            by=c("Experiment", "Well", "ObjectTrackID"))

trackingResults <- trackingResults %>%
  group_by(Experiment, Well, ObjectTrackID) %>%
  summarize(mintime = min(TimePoint)) %>%
  ungroup() %>%
  filter(mintime > 0) %>%
  anti_join(trackingResults, .,
            by=c("Experiment", "Well", "ObjectTrackID"))

message(sprintf("Curated data has %s rows\n", nrow(curatedData)))

curatedData <- curatedData %>%
  filter(Experiment %in% unique(trackingResults$Experiment))

message(sprintf("Tracking submission has %s rows\n", nrow(trackingResults)))

## ----merge-curated-and-tracked-------------------------------------------
merged <- full_join(curatedData,
                    trackingResults,
                    by = c("Experiment" = "Experiment",
                           "Well" = "Well",
                           "TimePoint" = "TimePoint",
                           "ObjectLabelsFound" = "ObjectLabelsFound"))

merged <- merged %>%
  mutate(matched = ObjectTrackID.x == ObjectTrackID.y)

merged$matched[is.na(merged$matched)] <- FALSE

## ----percentagetable-expt-well-object------------------------------------
res <- neurolincsscoring::score_perfect_tracks(merged) %>%
  mutate(submitted_file=opt$synapse_file_id) %>%
  select(submitted_file, everything())

cat(readr::format_csv(res))


