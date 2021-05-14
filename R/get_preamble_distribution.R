##' @title Get distribution preamble
##' @return
##' @author Shir Dekel
##' @export
get_preamble_distribution <- function() {

  preamble_distribution <- str_c(
      div(
        p("Below is the probability distribution of final outcomes if all projects were chosen."),
        p("The numbers on the x-axis (labelled 'Outcome') represent the final amounts of money possible if you chose to invest in all the projects. The numbers on the y-axis (labelled 'Probability') represent the likelihoods of each of the possible outcomes. Negative final outcomes (losses) are shown in red, positive final outcomes (gains) are shown in green, and a final outcome of zero (no loss or gain) is shown in blue.")) %>%
        as.character(),
      div(
        img(src = insert_resource("gambles_plot_experiment2.png"),
            width = "600",
            height = "400"),
        .noWS = "inside"
      ) %>%
        as.character(),
      p("Indicate below whether you would invest in the following:") %>%
        as.character())

  return(preamble_distribution)

}
