#' Establishes a U.S. Bureau of Economic Analysis API key for [retrieve_data()]
#' @description If you haven't yet registered for a Bureau of Economic Analysis API key, follow the instructions at http://www.bea.gov/API/signup/index.cfm.
#' @param key Supply your Bureau of Economic Analysis API key.
#' @export

bea_key <- function(key) {
  assign("bea_key_value", key, 1, envir = .GlobalEnv)
}