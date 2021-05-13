##' @title Clean combined data

##' @param data_combined
##'
##' @return
##' @author Shir Dekel
##' @export
clean_data_combined <- function(data_combined) {

  data <-
    data_combined %>%
    janitor::remove_empty("cols") %>%
    group_by(subject) %>%
    mutate(choice = recode(choice, "Yes" = 1, "No" = 0),
           datetime = dateCreated %>%
             dmy_hms(tz = "Australia/Sydney"),
           total_time = max(time_elapsed)/60000, # Milliseconds to minutes
           proportion = sum(choice)/length(choice),
           across(c(distribution, awareness, presentation), as.factor),
           across(where(shirthesis::check_numeric), as.numeric),
           gamble = str_c(
             "(",
             probability_positive,
             ", ",
             outcome_positive,
             "; ",
             1 - probability_positive,
             ", ",
             outcome_positive - outcome_dif,
             ")"
           ) %>%
             as.factor(),
           get_restriction_values(probability_positive, outcome_positive) %>%
             .[c("expected_value", "gain_loss_ratio")] %>%
             as_tibble()) %>%
    select(-c(question_order, time_elapsed, dateCreated, responses)) %>%
    ungroup() %>%
    nest_by(subject, presentation, distribution, awareness, proportion) %>%
    unite("condition", presentation, distribution, awareness, remove = FALSE) %>%
    mutate(across(condition, as.factor)) %>%
    unnest(data)

  return(data)

}
