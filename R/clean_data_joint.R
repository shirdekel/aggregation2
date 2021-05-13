##' @title Clean joint data

##' @param data_raw_prep
##'
##' @param names_to
##'
##' @return
##' @author Shir Dekel
##' @export
clean_data_joint <- function(data_raw_prep, names_to) {

  data_joint <-
    data_raw_prep %>%
    filter(stage == "project_choice", presentation == "joint") %>%
    rowwise()  %>%
    mutate(responses %>%
             map_dfc(fromJSON)) %>%
    pivot_longer(cols = -(stage:thesis_project),
                 names_to = names_to,
                 names_sep = "_",
                 values_to = "choice") %>%
    nest_by(subject, question_order) %>%
    mutate(project_order = question_order %>%
             map(fromJSON)) %>%
    unnest(c(data, project_order)) %>%
    mutate(project_order = project_order + 1) %>%
    ungroup()

  return(data_joint)

}
