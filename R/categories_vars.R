#' Returns variables from categories for [retrieve_aggregated()]
#' @description Takes a vector of one or more categories, as listed in the
#' [retrieve_aggregated_vars()] data frame, and returns all variables associated
#'  with those categories. Primarily intended for use inside the
#'  [retrieve_aggregated()] function parentheses.
#' @param categories Supply a vector of categories you wish to return all
#' variables associated with, as listed in the [retrieve_aggregated_vars()] data
#'  frame.
#' @export

categories_vars <- function(categories) {
  if (is.null(categories)) {
    print("`categories` cannot be `NULL`.")
  }

  tryCatch(
    {
      categories <- as.character(categories)
    },
    error = function(e) {
      stop("Must enter `categories` value that can be coerced to character.")
    }
  )

  retrieve_aggregated_vars <- retrieve_aggregated_vars()

  categories_test <- categories %in% retrieve_aggregated_vars$category

  if (FALSE %in% categories_test) {
    categories_vals <- which(categories_test == FALSE)

    categories_vec <- rep(NA, length(categories_vals))

    for (i in 1:length(categories_vals)) {
      categories_val <- categories_vals[i]
      categories_vec[i] <- categories[categories_val]
    }

    stop(paste0("`categories` contains the following invalid category names: ", paste(categories_vec, collapse = ", "), ". Use category names that are present in the `category` column of the dataset retrieved by `retrieve_aggregated_vars()`."))
  }

  vars_df <- retrieve_aggregated_vars |>
    dplyr::filter(category %in% categories)

  vars <- vars_df$variable

  return(vars)
}
