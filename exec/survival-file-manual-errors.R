library(synapser)
library(tidyverse)
library(neurolincsscoring)

synLogin()

# Get experiment list from table
# This has survival file entity ids in it too
experiment_table_id <- 'syn11817859'
experiment_table <- readr::read_csv(synTableQuery(sprintf('select Experiment,survival_file from %s WHERE survival_file is not NULL', experiment_table_id))$filepath) %>%
  select(-ROW_ID, -ROW_VERSION, experiment=Experiment, id=survival_file)

experiments <- purrr::pmap(experiment_table, list)

# Get censored wells data
censored_wells_id <- 'syn11709601'
censored_wells <- neurolincsscoring::get_censored_wells(censored_wells_id)

# This is a 3GB file. Read into memory with caution.
objects_df <- readr::read_csv("/home/kdaily/repositories/neurolincsdreamchallenge/workflows/unique-objects-workflow/unique-objects.csv")

foo <- purrr::map_df(experiments, neurolincsscoring::find_manual_errors, objects_data_frame = objects_df)
foo <- anti_join(foo, censored_wells)
readr::write_csv(foo, "/tmp/potential-manual-errors.csv")
synStore(File("/tmp/potential-manual-errors.csv", parentId="syn17079347"))
