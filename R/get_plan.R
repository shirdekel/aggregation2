#' @title `{drake}` plan

#' @return
#' @author Shir Dekel
#' @export
get_plan <- function() {
  drake_plan(
    restricted_values = target(
      get_restricted_values(),
      ## Seed needed to recreate old target with different name. Generated from
      ## `diagnose(restricted_values)$seed` in the original thesis project.
      seed = 2068659205
    ),
    gambles = target(
      shirthesis::get_gambles(
        restricted_values,
        10
      ),
      ## Seed needed to recreate old target with different name. Generated from
      ## `diagnose(restricted_values)$gambles` in the original thesis project.
      seed = 336030447
    ),
    experiment_resources = target(
      {
        file.path("inst", "experiment_resources")
      },
      format = "file"
    ),
    gambles_plot = target(
      plot_gambles(
        gambles,
        experiment_resources,
        experiment_number = 2
      )
    ),
    main = target(
      get_main(gambles)
    ),
    experiment = target(
      {
        shirthesis::get_experiment(
          gambles,
          experiment_directory = file.path("inst", "jspsych"),
          experiment_resources,
          main,
          post_experiment = get_post_experiment(),
          columns = get_columns(),
          condition_allocation = get_condition_allocation(),
          ethics = FALSE,
          on_finish = NULL
        )
        file.path("inst", "jspsych", "experiment")
      },
      target = "file"
    ),
    materials = get_screenshots(experiment)
  )
}
