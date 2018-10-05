get_curated_data <- function(id) {
  query <- glue::glue("select * from {id}")
  curated_data_query <- synapser::synTableQuery(query)
  curated_data_query %>%
    as.data.frame() %>%
    tibble::tibble()
}

get_image_masks <- function(id) {
  query <- glue::glue("select id,Experiment,Well,PID,parentId from {id}")
  image_masks_query <- synapser::synTableQuery(query)
  image_masks <- image_masks_query %>%
    as.data.frame() %>%
    tibble::as.tibble() %>%
    select(-ROW_ID, -ROW_VERSION, -ROW_ETAG)
  return(imageMasks)
}
