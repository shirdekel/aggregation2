##' Need to convert stage from JSON. Making sure it comes out normal from
##' jaysire proved to be difficult because it unboxes also other elements.
##'
##' @title Convert string to JSON by column

##' @return
##' @author Shir Dekel
##' @export
##' @param data
##' @param columns
column_fromJSON <- function(data, columns) {
  data %>%
    rowwise() %>%
    mutate(
      across({{ columns }}, ~ .x %>%
        map_if(validate, fromJSON) %>%
        unlist())
    )
}
