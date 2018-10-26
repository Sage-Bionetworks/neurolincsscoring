library(synapser)
library(tidyverse)
library(ggplot2)
library(neurolincsscoring)

foo <- capture.output(synLogin())

manuallyCuratedId <- 'syn11378063'

# fileSynId <- 'syn10878942'
# fileSynId <- 'syn10878998'
# fileSynId <- 'syn11678427'
# fileSynId <- 'syn11678505'
#fileSynId <- 'syn11956881'
fileSynId <- 'syn12084866'
# fileSynId <- 'syn16779108'

# Where things get stored to
parentId <- "syn11612119"

# synStore(File(rmarkdown::render("./tracking-summary.Rmd"), name="Tracking Summary", parentId="syn11612119", used=c(manuallyCuratedId, fileSynId)))

## ----get-tracking-results, message=FALSE---------------------------------
trackingResults <- neurolincsscoring::syn_get_tracking_submission_file(fileSynId)

## ----get-curated-data----------------------------------------------------
curatedDataRaw <- neurolincsscoring::syn_get_curated_data(manuallyCuratedId)

trackingResults <- trackingResults %>%
  assertr::verify(trackingResults$Experiment %in% curatedDataRaw$Experiment)

curatedData <- curatedDataRaw %>%
  filter(!is.na(ObjectTrackID), !Lost_Tracking) %>%
  select(Experiment, Well, TimePoint, ObjectLabelsFound, ObjectTrackID) %>%
  distinct() # This shouldn't be required

# This shouldn't be required
curatedData <- curatedData %>%
  count(Experiment, Well, TimePoint, ObjectLabelsFound, ObjectTrackID) %>%
  filter(n > 1) %>%
  anti_join(curatedData, .)

## ----remove-after-t-zero-------------------------------------------------
curatedData <- curatedData %>%
  group_by(Experiment, Well, ObjectTrackID) %>%
  summarize(mintime=min(TimePoint)) %>%
  ungroup() %>%
  filter(mintime > 0) %>%
  anti_join(curatedData, .)

trackingResults <- trackingResults %>%
  group_by(Experiment, Well, ObjectTrackID) %>%
  summarize(mintime=min(TimePoint)) %>%
  ungroup() %>%
  filter(mintime > 0) %>%
  anti_join(trackingResults, .)

## ----merge-curated-and-tracked-------------------------------------------
merged <- full_join(curatedData,
                    trackingResults,
                    by=c("Experiment"="Experiment",
                         "Well"="Well",
                         "TimePoint"="TimePoint",
                         "ObjectLabelsFound"="ObjectLabelsFound"))

merged <- merged %>%
  mutate(matched=ObjectTrackID.x == ObjectTrackID.y)

merged$matched[is.na(merged$matched)] <- FALSE

# merged %>%
#   readr::write_csv(path="./merge-curated-and-tracked.csv")
# mergedObj <- synStore(File("./merge-curated-and-tracked.csv", parentId=parentId),
#                            forceVersion=FALSE)

## ----percentage-table-expt-----------------------------------------------
pctTable <- merged %>%
  group_by(Experiment) %>%
  summarize(matches=sum(matched, na.rm=FALSE),
            total=n()) %>%
  mutate(percentage=matches/total) %>%
  arrange(Experiment)

## ----percentagetable-expt-well-object------------------------------------
# Some ObjectTrackID.x values are NA, meaning they were tracked automatically but not manually curated.
# These are treated as mismatches.
tblExptWellObj <- merged %>%
  group_by(Experiment, Well, ObjectTrackID.x) %>%
  summarize(matches=sum(matched, na.rm=TRUE),
            total=n()) %>%
  mutate(percentage=matches/total) %>%
  arrange(Experiment, Well, ObjectTrackID.x) %>%
  ungroup()%>%
  mutate(errors=total-matches)

## ----results-aggregate-experiment-well-----------------------------------
aggregateExperimentWell <- tblExptWellObj %>%
  group_by(Experiment, Well) %>%
  summarize(perfect=sum(percentage == 1), total=n()) %>%
  mutate(percentage=perfect/total) %>%
  arrange(Experiment, Well)

# aggregateExperimentWell %>%
#   readr::write_csv(path = "./results-aggregate-experiment-well.csv")
#
# aggExptWellObj <- synStore(File("./results-aggregate-experiment-well.csv", parentId=parentId),
#                            forceVersion=FALSE)
#
# aggExptWellObjId <- sprintf('%s.%s',
#                             aggExptWellObj$properties$id,
#                             aggExptWellObj$properties$versionNumber)

## ------------------------------------------------------------------------
tblExptWellObj %>%
  group_by(Experiment) %>%
  summarize(`perfect tracks`=sum(percentage == 1), total=n()) %>%
  mutate(percentage=`perfect tracks`/total) %>%
  arrange(Experiment)

## ------------------------------------------------------------------------
tblExptWellObj %>%
  filter(!is.na(ObjectTrackID.x), errors > 0) %>%
  ggplot(., aes(x=errors)) +
    geom_density() +
    facet_wrap(~ Experiment)

## ------------------------------------------------------------------------
tblExptWellObj %>%
  filter(!is.na(ObjectTrackID.x), errors > 0) %>%
  ggplot(aes(x=Experiment, y=total)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90))

# ## ------------------------------------------------------------------------
# duplicatesObj <- synStore(File("./duplicates.csv", parentId=parentId),
#                            forceVersion=FALSE)

