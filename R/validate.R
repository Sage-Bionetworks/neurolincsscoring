#' @export
validate_tracking_results <- function(trackingResults, curatedData) {
  invalid_reasons <- NULL
  mandatory_columns <- c("Experiment", "ObjectLabelsFound", "ObjectTrackID",
                         "Well", "TimePoint")
  present_columns <- mandatory_columns %in% names(trackingResults)
  if (!all(present_columns)) {
    invalid_reasons <- c(invalid_reasons,
                         paste("Not all required columns are present.",
                               "Did not find column(s):",
                               paste0(mandatory_columns[!present_columns],
                                      collapse = ", ")))
    return(invalid_reasons)
  }
  if (dplyr::n_distinct(trackingResults$Experiment) > 1) {
    invalid_reasons <- c(invalid_reasons,
                         "Only one unique Experiment value is allowed.")
  }
  largest_min_timepoint <- trackingResults %>%
    dplyr::group_by(Experiment, Well, ObjectTrackID) %>%
    dplyr::summarise(min_timepoint = min(TimePoint)) %>%
    ungroup() %>%
    select(min_timepoint) %>%
    max()
  if (largest_min_timepoint > 0) {
    invalid_reasons <- c(invalid_reasons,
                         paste("The TimePoint value of each tracked cell",
                               "must begin at 0."))
  }
  allowed_experiments <- curatedData %>% distinct(Experiment)
  inputted_experiments <- trackingResults %>% distinct(Experiment)
  correct_experiments <- inputted_experiments$Experiment %in% allowed_experiments$Experiment
  if (!all(correct_experiments)) {
    invalid_reasons <- c(invalid_reasons,
                         paste("The following experiments are not able to be",
                               "scored because they were not found in the",
                               "curated reference file:",
                               paste0(inputted_experiments[!correct_experiments],
                                      collapse = ", ")))
  }
  return(invalid_reasons)
}
