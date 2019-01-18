library(synapser)
library(tidyverse)

synLogin()

experiments <- list(list(experiment="LINCS092016B", id="syn17089669"),
                    list(experiment="KS-AB-iMN-TDP43-Survival", id="syn17089673"),
                    list(experiment="AB-CS47iTDP-Survival", id="syn17139503"),
                    list(experiment="LINCS062016A", id="syn17089659")
                    # list(experiment="AB7-SOD1-KW4-WTC11-Survival-exp3", id="syn17089671")
                    )

original_data <- readr::read_csv("neurolincsdreamchallenge/workflows/unique-objects-workflow/unique-objects.csv")

experiment <- experiments[[1]]

find_manual_errors <- function(experiment, original_data) {
  orig <- original_data %>% filter(Experiment == experiment$experiment)

  f <- synGet(experiment$id)
  d <- readr::read_csv(f$path)

  manual_timepoint_columns <- colnames(d)[stringr::str_detect(colnames(d), "T[0-9]+")]

  # get manual time columns into a single column
  d2 <- d %>%
    tidyr::gather(key = "TimePointCol",
                  "TimePointObject",
                  one_of(manual_timepoint_columns))

  d3 <- d2 %>%
    mutate(TimePointCol=as.numeric(stringr::str_remove(TimePointCol, "T")),
           TimePointObject=as.numeric(TimePointObject)) %>%
    filter(Phenotype == "N") %>%
    select(Sci_WellID, ObjectLabelsFound, Timepoint,
           Time, TimePointCol, TimePointObject) %>%
    filter(!is.na(TimePointObject),
           !(TimePointObject %in% c("U")))

  d4 <- anti_join(d3, orig,
                  by=c("Sci_WellID"="Well",
                       "TimePointObject"="ObjectLabelsFound"))

  d5 <- d4 %>%
    mutate(Experiment=experiment$experiment) %>%
    select(Experiment,
           Well=Sci_WellID,
           ObjectLabelsFoundT0=ObjectLabelsFound,
           TimePointColFromSurvival=TimePointCol,
           ObjectLabelFromSurvival=TimePointObject) %>%
    arrange(Well)
  d5
}

foo <- purrr::map_df(experiments, find_manual_errors, original_data=original_data)
readr::write_csv(foo, "potential-manual-errors.csv")
