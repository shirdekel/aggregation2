##' @title Clean separate data

##' @param data_raw_prep
##'
##' @param names_to
##'
##' @return
##' @author Shir Dekel
##' @export
clean_data_separate <- function(data_raw_prep, names_to) {

  data_separate <-
    data_raw_prep %>%
    filter(stage == "project_choice", presentation == "separate") %>%
    group_by(subject) %>%
    mutate(project_order = 1:length(responses)) %>%
    rowwise() %>%
    mutate(responses %>%
             map_dfc(fromJSON) %>% # Doesn't work as a single step without pivot_longer()
             pivot_longer(cols = everything(),
                          names_to = names_to,
                          names_sep = "_",
                          values_to = "choice")
    )

  return(data_separate)

}
