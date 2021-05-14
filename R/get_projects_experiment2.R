##' @title Get projects with short descriptions

##' @param gambles
##'
##' @return
##' @author Shir Dekel
##' @export
get_projects_experiment2 <- function(gambles) {

  project_name <-
    c(
      "Refinera"
    )
  project_type <-
    c(
      "oil well"
    )
  project_description_experiment2 <-
    get_project_description_experiment2(
      gambles,
      project_name,
      project_type
    )
  project_input_experiment2 <-
    shirthesis::get_project_input(
      type = project_type,
      gambles = gambles
    )

  projects_experiment2 <-
    list(description = project_description_experiment2,
         input = project_input_experiment2)

  return(projects_experiment2)

}
