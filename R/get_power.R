#' @title Get aggregation 2 power analysis

#' @return
#' @author Shir Dekel
#' @export
get_power <- function() {
  data_joint <-
    aggregation1::data %>%
    nest_by(id, presentation, similarity, awareness, proportion) %>%
    ungroup() %>%
    filter(
      similarity == "high",
      awareness == "naive",
      presentation == "joint"
    ) %>%
    pull(proportion)

  data_separate <-
    aggregation1::data %>%
    nest_by(id, presentation, similarity, awareness, proportion) %>%
    ungroup() %>%
    filter(
      similarity == "high",
      awareness == "naive",
      presentation == "separate"
    ) %>%
    pull(proportion)

  data_joint %>%
    t.test(data_separate, noncentral = TRUE, paired = TRUE) %>%
    .[[1]] %>%
    MOTE::d.dep.t.diff.t(n = length(data_joint)) %>%
    .[["d"]] %>%
    unname() %>%
    pwr::pwr.t.test(d = ., power = 0.8, alternative = "greater") %>%
    .[["n"]] %>%
    ceiling()
}
