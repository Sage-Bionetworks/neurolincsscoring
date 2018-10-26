library(synapser)
library(tidyverse)
library(neurolincsscoring)

foo <- capture.output(synLogin())

manuallyCuratedId <- 'syn11378063'

# Where things get stored to
parentId <- "syn11612119"

curatedDataRaw <- neurolincsscoring::syn_get_curated_data('syn11378063')

# Check for duplicates by curated labeling
duplicate_labels <- curatedDataRaw %>%
  count(Experiment, Well, TimePoint, ObjectLabelsFound, ObjectTrackID) %>%
  filter(n > 1) %>%
  left_join(., curatedDataRaw)

# Check for duplicates by position in the well at a timepoint
duplicate_positions <- curatedDataRaw %>%
  count(Experiment, Well, TimePoint, XCoordinate, YCoordinate) %>%
  filter(n > 1) %>%
  left_join(., curatedDataRaw)
