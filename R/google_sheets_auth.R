#' Authorizes user account for Google Sheets data writing and retrieval in 
#' [web_app()]
#' @description In order to collect and display data from app viewers in a 
#' Shiny app created using [web_app()], this function must first be executed so 
#' that data can be written to and read from Google Sheets. This requires 
#' a Google account. This function only needs to be run once, as your account
#' information will then be saved across R sessions.
#' @param email Supply the email address associated with your Google account.
#' @export

google_sheets_auth <- function(email) {
  
  renv <- file.path(Sys.getenv("HOME"), ".Renviron")
  if (!file.exists(renv)) {
    file.create(renv)
  }
  oldenv <- readLines(renv)
  replaced <- FALSE
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, 19) == "google_sheets_email") {
      oldenv[i] <- paste0("google_sheets_email=\"", email, "\"")
      write(oldenv, renv, sep = "\n", append = FALSE)
      replaced <- TRUE
    }
  }
  if (replaced == FALSE) {
    write(paste0("google_sheets_email=\"", email, "\""), renv, sep = "\n", append = TRUE)
  }
  
  options(gargle_oauth_cache = ".secrets", email = email)
  googlesheets4::gs4_auth(email = email)
}