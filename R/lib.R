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
    group_by(Experiment, Well, ObjectTrackID.x) %>%
    summarize(matches = sum(matched, na.rm=TRUE),
              total = n()) %>%
    mutate(percentage = matches/total) %>%
    arrange(Experiment, Well, ObjectTrackID.x) %>%
    ungroup() %>%
    mutate(errors = total - matches)

  res <- tblExptWellObj %>%
    group_by(Experiment) %>%
    summarize(`perfect tracks`=sum(percentage == 1), total=n()) %>%
    mutate(percentage=`perfect tracks`/total) %>%
    arrange(Experiment)

  return(res)
}
