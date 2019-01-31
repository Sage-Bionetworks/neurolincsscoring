library(synapser)
library(tidyverse)

synLogin()

experiment_table_id <- 'syn11817859'
experiment_table <- readr::read_csv(synTableQuery(sprintf('select Experiment,survival_file from %s WHERE survival_file is not NULL', experiment_table_id))$filepath) %>%
  select(-ROW_ID, -ROW_VERSION, experiment=Experiment, id=survival_file)

experiments <- purrr::pmap(experiment_table, list)

censored_wells_id <- 'syn11709601'
censored_wells <- readr::read_csv(synTableQuery('select * from syn11709601')$filepath) %>%
  select(-ROW_ID, -ROW_VERSION)

original_data <- readr::read_csv("/home/kdaily/repositories/neurolincsdreamchallenge/workflows/unique-objects-workflow/unique-objects.csv")

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
foo <- anti_join(foo, censored_wells)
readr::write_csv(foo, "/tmp/potential-manual-errors.csv")
synStore(File("/tmp/potential-manual-errors.csv", parentId="syn17079347"))
