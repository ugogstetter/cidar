#' Establishes a U.S. Census Bureau API key for [retrieve_aggregated()]
#' @description Saves your Census Bureau API key so you won't have to reenter
#' it, even between R sessions. If you haven't yet registered for a Census
#' Bureau API key, follow the instructions on the Census API webpage
#' [here](https://api.census.gov/data/key_signup.html).
#' @param key Supply your Census Bureau API key.
#' @export

# some of this code is adapted from the census_api_key function from tidycensus
census_key <- function(key) {
  if (is.null(key)) {
    print("`key` cannot be `NULL`.")
  }

  if (length(key) != 1) {
    stop("`key` must have length 1.")
  }

  tryCatch(
    {
      key <- as.character(key)
    },
    error = function(e) {
      stop("Must enter `key` value that can be coerced to character.")
    }
  )

  renv <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (!file.exists(renv)) {
    file.create(renv)
  }
  oldenv <- readLines(renv)
  replaced <- FALSE
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, 16) == "census_key_value") {
      oldenv[i] <- paste0("census_key_value=\"", key, "\"")
      write(oldenv, renv, sep = "\n", append = FALSE)
      replaced <- TRUE
    }
  }
  if (replaced == FALSE) {
    write(paste0("census_key_value=\"", key, "\""), renv, sep = "\n", append = TRUE)
  }
}
