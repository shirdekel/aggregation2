##' @title Get project description E2
##'
##' @param project_name
##' @param gambles
##' @param project_type
##'
##' @return
##' @author Shir Dekel
##' @export
get_project_description_experiment2 <- function(gambles,
                                                project_name,
                                                project_type) {


  outcome_negative <- gambles$outcome_dif - gambles$outcome_positive
  prob_positive <- gambles$prob_positive * 100
  prob_negative <- 100 - prob_positive

  bold <- str_c(
    "there is ",
    prob_positive,
    "% chance of gaining $",
    gambles$outcome_positive,
    " million and a ",
    prob_negative,
    "% chance of losing $",
    outcome_negative,
    " million on the investment."
  ) %>%
    map_chr(~tags$strong(.x) %>%
              as.character())

  project_description_experiment2 <- str_c(
    project_name,
    " is a business in your company that proposes to construct ",
    project_type %>%
      map_chr(shirthesis::get_indefinite),
    " ",
    project_type,
    " project, which they forecast will cost $",
    outcome_negative,
    " million. If the project succeeds, forecasts show the company would make $",
    gambles$outcome_dif,
    " million. Research suggests that there is a ",
    prob_positive,
    "% chance of the project succeeding. Therefore, ",
    bold
  )

  return(project_description_experiment2)

}
