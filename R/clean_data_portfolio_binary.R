##' @title clean portfolio binary data

##' @param data_raw_prep
##'
##' @return
##' @author Shir Dekel
##' @export
clean_data_portfolio_binary <- function(data_raw_prep) {

  data_portfolio_binary <-
    data_raw_prep %>%
    filter(stage == "portfolio_binary") %>%
    mutate(portfolio_binary = button_pressed %>%
             recode("0" = 1, # In the experiment, 0 corresponds to investing in all, so this reverses the coding to be more intuitive
                    "1" = 0)) %>%
    select(subject, portfolio_binary)

  return(data_portfolio_binary)

}
