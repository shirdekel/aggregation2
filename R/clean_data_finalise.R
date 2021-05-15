##' Filter out relevant prolific participants, remove duplicates, and add column
##' ID
##'
##' Only remove duplicates now because sometimes the past study filtering is not
##' set right in Prolific and you still need to credit the duplicates
##'
##' Assign IDs only after all filtering
##'
##' Filter out testing data and data generated through Prolific preview (through
##' personal PID)
##'
##' @title Finalise data cleaning
##' @param data
##' @param test
##' @param prolific_filter
##' @param prolific_filter_label
##' @return
##' @author Shir Dekel
##' @export
clean_data_finalise <- function(data, test, prolific_filter,
                                prolific_filter_label) {

  if ("prolific" %in% colnames(data) & !test) {
      data <-
          data %>%
          filter(
              !str_detect(prolific, "test"),
              prolific != "5b878067600e3a000194db61"
          )

      ## list(prolific_filter, prolific_filter_label) %>%
      ##     pmap(
      ##         ~ data %>%
      ##             get_prolific_id(.x, .y)
      ##     )
  }

    data %>%
        shirthesis::remove_duplicates() %>%
        shirthesis::add_id_column(subject)

}
