#!/usr/bin/env Rscript

library(synapser)
library(tidyverse)
library(neurolincsscoring)

foo <- synLogin()

manuallyCuratedId <- 'syn11378063'

curatedDataRaw <- neurolincsscoring::syn_get_curated_data(manuallyCuratedId)

curatedData <- curatedDataRaw %>%
  filter(!is.na(ObjectTrackID), !Lost_Tracking) %>%
  distinct()

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

# ## store the data
# table <- Table()
