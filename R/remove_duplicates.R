##' @title Remove duplicates
##'
##' Group by Prolific ID, rank each ID's completion time, and only take the first one.
##'
##' @return
##' @author Shir Dekel
##' @export
remove_duplicates <- function(data) {

  duplicates_removed <-
    data %>%
    group_by(prolific) %>%
    mutate(date_rank = dense_rank(datetime)) %>%
    filter(date_rank == 1) %>%
    ungroup()

  return(duplicates_removed)

}
