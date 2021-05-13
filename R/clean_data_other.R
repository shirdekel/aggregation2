##' @title Clean other data

##' @return
##' @author Shir Dekel
##' @export
##' @param data_raw_prep
##' @param main_stage
clean_data_other <- function(data_raw_prep, main_stage) {
  data_other <-
    data_raw_prep %>%
    drop_na(responses) %>%
    filter(stage != main_stage) %>%
    nest_by(subject) %>%
    mutate(other = data %>%
      pull(responses) %>%
      map_dfc(fromJSON) %>%
      list()) %>%
    unnest(other) %>%
    ungroup() %>%
    select(-data)

  return(data_other)
}
