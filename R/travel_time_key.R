#' Establishes a TravelTime API ID and key for [travel_time()]
#' @description Saves your TravelTime API ID and key so you won't have to 
#' reenter it, even between R sessions. If you haven't yet registered for the 
#' TravelTime API, follow the instructions at https://account.traveltime.com.
#' @param id Supply your TravelTime application ID.
#' @param key Supply your TravelTime application key.
#' @export

# some of this code is adapted from the census_api_key function from tidycensus
travel_time_key <- function(id, key) {
  renv <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (!file.exists(renv)) {
    file.create(renv)
  }
  oldenv <- readLines(renv)
  replaced <- FALSE
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, 13) == "TRAVELTIME_ID") {
      oldenv[i] <- paste0("TRAVELTIME_ID=\"", id, "\"")
      write(oldenv, renv, sep = "\n", append = FALSE)
      replaced <- TRUE
    }
  }
  if (replaced == FALSE) {
    write(paste0("TRAVELTIME_ID=\"", id, "\""), renv, sep = "\n", append = TRUE)
  }
  oldenv <- readLines(renv)
  replaced <- FALSE
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, 14) == "TRAVELTIME_KEY") {
      oldenv[i] <- paste0("TRAVELTIME_KEY=\"", key, "\"")
      write(oldenv, renv, sep = "\n", append = FALSE)
      replaced <- TRUE
    }
  }
  if (replaced == FALSE) {
    write(paste0("TRAVELTIME_KEY=\"", key, "\""), renv, sep = "\n", append = TRUE)
  }
}