##' @title Get condition allocation columns

##' @return
##' @author Shir Dekel
##' @export
get_columns <- function() {
  columns <-
    insert_property(
      subject = insert_javascript("jsPsych.randomization.randomID(15)"),
      experiment = "aggregation_exp2",
      sample = "prolific",
      awareness = insert_javascript("awareness_condition"),
      presentation = insert_javascript("presentation_condition"),
      distribution = insert_javascript("distribution_condition")
    )

  return(columns)
}
