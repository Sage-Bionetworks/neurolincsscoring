#' @export
find_min_timepoints <- function(d, group_col="Experiment", timepoint_col="TimePoint") {
  d %>%
    dplyr::group_by(Experiment) %>%
    dplyr::summarise(minTimePoint=min(TimePoint))
}
