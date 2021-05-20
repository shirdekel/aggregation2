##' @title Get post experiment trials E2

##' @return
##' @author Shir Dekel
##' @export
get_post_experiment <- function() {

  trial_project_number <-
    shirthesis::get_trial_project_number()

  preamble_portfolio_distribution_present <-
    get_preamble_distribution()

  trial_portfolio_binary_distribution_present <-
    shirthesis::get_trial_portfolio_binary(
      preamble = preamble_portfolio_distribution_present,
      distribution = "present"
    )

  trial_portfolio_binary_distribution_absent <-
    shirthesis::get_trial_portfolio_binary(
      distribution = "absent"
    )

  trial_portfolio_number_distribution_present <-
    shirthesis::get_trial_portfolio_number(
      preamble = preamble_portfolio_distribution_present,
      distribution = "present"
    )

  trial_portfolio_number_distribution_absent <-
    shirthesis::get_trial_portfolio_number(
      distribution = "absent"
    )

  ## debrief <-
  ##   get_debrief()

  trial_end <-
    shirthesis::get_trial_end()

  post_experiment <-
    build_timeline(
      trial_project_number,
      trial_portfolio_binary_distribution_present,
      trial_portfolio_binary_distribution_absent,
      trial_portfolio_number_distribution_present,
      trial_portfolio_number_distribution_absent,
      ## debrief,
      trial_end
    )

  return(post_experiment)

}
