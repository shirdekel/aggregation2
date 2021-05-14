##' @title Get E2 screenshot file names

##' @return
##' @author Shir Dekel
##' @export
get_file_name_materials_experiment2 <- function() {

  project_choice <-
    list(presentation = c("joint", "separate"),
         projects = c(1, 10)) %>%
    pmap(
      ~ str_c(
        "project_choice",
        "oil-well",
        .x,
        "distribution",
        "present",
        1:.y,
        sep = "_"
      )
    ) %>%
    unlist()

  file_name_materials_experiment2 <-
    c(
      str_c(
        "instructions",
        1:3
      ),
      str_c(
        "awareness",
        c("naive", "aware"),
        sep = "_"
      ),
      project_choice,
      "project_number",
      str_c(
        c(
          "porfolio_binary",
          "porfolio_number"
        ),
        "distribution_present",
        sep = "_"
      )
    )

  return(file_name_materials_experiment2)

}
