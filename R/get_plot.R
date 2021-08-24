##' @title Get plot for Experiment 2
##' @param data
##' @return
##' @author Shir Dekel
##' @export
get_plot <- function(data = aggregation2::data) {
  dv_label <- "Mean proportions of project acceptance"

  data_split <-
    split_data(data)

  choice_proportion_omnibus <-
    list(
      data_split,
      names(data_split)
    ) %>%
    purrr::pmap(
      ~ .x %>%
        get_omnibus("proportion", .y)
    )

  choice_proportion_presentation <-
    choice_proportion_omnibus$presentation %>%
    afex::afex_plot(
      x = "presentation",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        presentation = c(
          separate = "Separate",
          joint = "Joint"
        )
      )
    ) +
    ggplot2::labs(
      x = "Presentation",
      y = dv_label
    ) +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none")

  choice_proportion_awareness <-
    choice_proportion_omnibus$awareness %>%
    afex::afex_plot(
      x = "awareness",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        awareness = c(
          naive = "Naive",
          aware = "Aware"
        )
      )
    ) +
    ggplot2::labs(
      x = "Awareness",
      y = dv_label
    ) +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none")

  choice_proportion_distribution <-
    choice_proportion_omnibus$distribution %>%
    afex::afex_plot(
      x = "distribution",
      mapping = c("shape", "color"),
      error_arg = list(width = 0.05),
      data_geom = ggbeeswarm::geom_quasirandom,
      point_arg = list(size = 3),
      factor_levels = list(
        distribution = c(
          absent = "Absent",
          present = "Present"
        )
      )
    ) +
    ggplot2::labs(
      x = "Distribution",
      y = dv_label
    ) +
    papaja::theme_apa() +
    ggplot2::theme(legend.position = "none")

  choice_proportion <-
    cowplot::plot_grid(
      choice_proportion_presentation,
      choice_proportion_awareness + ggplot2::ylab(NULL),
      choice_proportion_distribution + ggplot2::ylab(NULL),
      nrow = 1
    )

  choice_binary <-
    shirthesis::plot_choice(
      data_split,
      choice,
      "Mean choice of project acceptance"
    )

  portfolio_number <-
    shirthesis::plot_choice(
      data_split,
      portfolio_number,
      "Mean number of project acceptance"
    )

  portfolio_binary <-
    shirthesis::plot_choice(
      data_split,
      portfolio_binary,
      "Mean choice of complete project portfolio acceptance"
    )

  choice_trials <-
    data %>%
    shirthesis::plot_choice_trials(awareness) +
    ggplot2::labs(
      color = "Awareness",
      linetype = "Awareness"
    )

  project_number <-
    shirthesis::plot_project_number(data, condition)

  gamble_values <-
    shirthesis::plot_gamble_values(data)

  trials <-
    shirthesis::plot_trials(data)

  plot_aggregation_2 <-
    tibble::lst(
      choice_binary,
      choice_proportion,
      choice_proportion_presentation,
      choice_proportion_awareness,
      choice_proportion_distribution,
      portfolio_number,
      portfolio_binary,
      choice_trials,
      project_number,
      gamble_values,
      trials
    )

  return(plot_aggregation_2)
}
