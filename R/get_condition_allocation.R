##' @title Parse condition allocation JS code for E2

##' @return
##' @author Shir Dekel
##' @export
get_condition_allocation <- function() {
  c(
    "naive_separate_absent",
    "naive_joint_absent",
    "naive_separate_present",
    "aware_separate_absent"
  ) %>%
    toJSON() %>%
    str_c(
      "condition = jsPsych.randomization.sampleWithoutReplacement(",
      .,
      ", 1)[0]"
    ) %>%
    c(
      "regex_awareness = /(.*)_.*_.*/",
      "regex_presentation = /.*_(.*)_.*/",
      "regex_distribution = /.*_.*_(.*)/",
      # Add [1] to extract capture group
      "awareness_condition = condition.match(regex_awareness)[1]",
      "presentation_condition = condition.match(regex_presentation)[1]",
      "distribution_condition = condition.match(regex_distribution)[1]",
      "urlvar = jsPsych.data.urlVariables()",
      "if typeof urlvar.presentation != 'undefined' then presentation_condition = urlvar.presentation;",
      "if typeof urlvar.awareness != 'undefined' then awareness_condition = urlvar.awareness;",
      "if typeof urlvar.distribution != 'undefined' then distribution_condition = urlvar.distribution;"
    ) %>%
    js::coffee_compile(bare = TRUE)
}
