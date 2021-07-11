#' @title Get aggregation omnibus
##' @param data
##' @param dv
##' @param iv
#' @return
#' @author Shir Dekel
#' @export
get_omnibus <- function(data, dv, iv) {
  data %>%
    afex::aov_ez(
      id = "id",
      dv = dv,
      data = .,
      between = iv,
      type = 2
    )
}
