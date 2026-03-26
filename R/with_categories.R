#' Returns variables from categories for [retrieve_aggregated()]
#' @description Takes a vector of one or more categories, as listed in the 
#' [retrieve_aggregated_vars()] data frame, and returns all variables associated
#'  with those categories. Primarily intended for use inside the 
#'  [retrieve_aggregated()] function parentheses.
#' @param categories Supply a vector of categories you wish to return all 
#' variables associated with, as listed in the [retrieve_aggregated_vars()] data
#'  frame.
#' @export

with_categories <- function(categories) {
  
  vars_df <- retrieve_aggregated_vars() |>
    dplyr::filter(category %in% categories)
  vars <- vars_df$variable
  
  return(vars)
}