##' @title Get joint trial
##'
##' @param preamble
##' @param questions_joint
##' @param distribution
##' @param randomize_order
##'
##' @return
##' @author Shir Dekel
##' @export
get_trial_joint <- function(preamble = "<p>Indicate below whether you would invest in the following:</p>", questions_joint, distribution, randomize_order) {

  trial_joint <- trial_survey_multi_choice(
    preamble = preamble,
    questions = questions_joint,
    randomize_question_order = randomize_order,
    data = insert_property(stage = "project_choice")
  ) %>%
    build_timeline() %>%
    display_if(fn_data_condition(distribution == !!distribution)) %>%
    build_timeline() %>%
    display_if(fn_data_condition(presentation == "joint"))

  return(trial_joint)

}
