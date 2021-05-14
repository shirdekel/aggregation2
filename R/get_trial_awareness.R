##' @title Get awareness trial

##' @param stimulus
##'
##' @param awareness
##'
##' @return
##' @author Shir Dekel
##' @export
get_trial_awareness <- function(stimulus, awareness) {

  trial_awareness <- trial_html_button_response(
    stimulus = stimulus,
    choices = "Continue",
    data = insert_property(stage = "awareness")
  ) %>%
    build_timeline() %>%
    display_if(fn_data_condition(awareness == !!awareness))

  return(trial_awareness)

}
