auth_gs4_alternative <- function() {
  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googlesheets4::gs4_auth(cache = ".secrets")
}

auth_gs4 <- function() {
  options(gargle_oauth_cache = ".secrets", email = Sys.getenv("google_sheets_email"))
  googlesheets4::gs4_auth(email = Sys.getenv("google_sheets_email"))
  }