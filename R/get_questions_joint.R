##' @title Joint questions

##' @param project_description
##'
##' @param form_options
##' @param project_input
##'
##' @return
##' @author Shir Dekel
##' @export
get_questions_joint <- function(project_description, form_options = c("Yes", "No"), project_input) {

  questions_joint <- project_description %>%
    map2(project_input, ~question_multi(
      prompt = .x,
      options = form_options,
      name = .y,
      required = TRUE
    ))

  return(questions_joint)

}
