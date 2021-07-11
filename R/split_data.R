##' @title Split data into effects
##' @param data
##' @return
##' @author Shir Dekel
##' @export
split_data <- function(data) {
  data_distribution <-
    data %>%
    dplyr::filter(presentation == "separate", awareness == "naive") %>%
    dplyr::mutate(dplyr::across(distribution, ~ .x %>%
      forcats::fct_relevel("present")))

  data_presentation <-
    data %>%
    dplyr::filter(distribution == "absent", awareness == "naive") %>%
    dplyr::mutate(dplyr::across(presentation, ~ .x %>%
      forcats::fct_relevel("joint")))

  data_awareness <-
    data %>%
    dplyr::filter(distribution == "absent", presentation == "separate") %>%
    dplyr::mutate(dplyr::across(awareness, ~ .x %>%
      forcats::fct_relevel("aware")))

  data_effects <-
    list(data_presentation, data_awareness, data_distribution) %>%
    rlang::set_names("presentation", "awareness", "distribution")

  return(data_effects)
}
