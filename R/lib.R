#' @export
find_min_timepoints <- function(d, group_col="Experiment", timepoint_col="TimePoint") {
  d %>%
    dplyr::group_by(Experiment) %>%
    dplyr::summarise(minTimePoint=min(TimePoint))
}

#' @export
score_perfect_tracks <- function(d) {
  # Some ObjectTrackID.x values are NA, meaning they were tracked automatically but not manually curated.
  # These are treated as mismatches.
  d <- d %>% assertr::verify(assertr::has_all_names("Experiment", "Well",
                                                    "ObjectTrackID.x", "matched"))
  tblExptWellObj <- d %>%
    dplyr::group_by(Experiment, Well, ObjectTrackID.x) %>%
    dplyr::summarize(matches = sum(matched, na.rm=TRUE),
                     total = dplyr::n()) %>%
    dplyr::mutate(percentage = matches/total) %>%
    dplyr::arrange(Experiment, Well, ObjectTrackID.x) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(errors = total - matches)

  res <- tblExptWellObj %>%
    dplyr::group_by(Experiment) %>%
    dplyr::summarize(`perfect tracks`=sum(percentage == 1), total=dplyr::n()) %>%
    dplyr::mutate(percentage=`perfect tracks`/total) %>%
    dplyr::arrange(Experiment)

  return(res)
}

#' Get censored wells from the censored well table.
#'
#' @param id Synapse ID of a censored well table.
#'
#' @return A data frame of censored well information.
#' @export
get_censored_wells <- function(id='syn11709601', ...) {
  censored_wells <- readr::read_csv(synapser::synTableQuery(sprintf(query='select * from %s', id), ...)$filepath)
}

#' Identify object labels that are not present in image masks.
#'
#' These are potentially manual mistakes during curation.
#'
#' @param experiment A list that has an `experiment` name and `id` as a Synapse ID for a file with the survival data in it.
#' @param objects_data_frame A data frame of potential objects.
#'
#' @return A data frame summary of the objects not present.
#' @export
find_manual_errors <- function(experiment, objects_data_frame) {
  orig <- objects_data_frame %>% dplyr::filter(Experiment == experiment$experiment)

  f <- synapser::synGet(experiment$id)
  d <- readr::read_csv(f$path, col_types = readr::cols(.default = "c"))

  manual_timepoint_columns <- colnames(d)[stringr::str_detect(colnames(d), "T[0-9]+")]

  # get manual time columns into a single column
  d2 <- d %>%
    tidyr::gather(key = "TimePointCol",
                  "TimePointObject",
                  dplyr::one_of(manual_timepoint_columns))

  d3 <- d2 %>%
    dplyr::mutate(TimePointCol=as.numeric(stringr::str_remove(TimePointCol, "T")),
                  TimePointObject=as.numeric(TimePointObject)) %>%
    dplyr::filter(Phenotype == "N") %>%
    dplyr::select(Sci_WellID, ObjectLabelsFound, Timepoint,
                  Time, TimePointCol, TimePointObject) %>%
    dplyr::filter(!is.na(TimePointObject),
                  !(TimePointObject %in% c("U")))

  d4 <- dplyr::anti_join(d3, orig,
                         by=c("Sci_WellID"="Well",
                              "TimePointObject"="ObjectLabelsFound"))

  d5 <- d4 %>%
    dplyr::mutate(Experiment=experiment$experiment) %>%
    dplyr::select(Experiment,
                  Well=Sci_WellID,
                  ObjectLabelsFoundT0=ObjectLabelsFound,
                  TimePointColFromSurvival=TimePointCol,
                  ObjectLabelFromSurvival=TimePointObject) %>%
    dplyr::arrange(Well)
  d5
}
