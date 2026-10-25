#' Retrieves Google Sheet ID for use in [web_app()]
#' @description Returns the sheet ID from your local R environment for a `project_name`
#'  with a corresponding Google Sheet already established using `web_app()`, or 
#'  informs you that you must create the desired sheet by running `web_app()` with 
#'  your desired `project_name` supplied.
#' @param project_name The project name for which you wish to retrieve the 
#' corresponding Google Sheet ID from your local R environment.
#' @export

get_sheet_id <- function(project_name = NULL) {
  
  id_exists <- FALSE
  
  renv <- file.path(Sys.getenv("HOME"), ".Renviron")
  
  if (file.exists(renv)) {
    
  oldenv <- readLines(renv)
  
  for (i in 1:length(oldenv)) {
    if (substr(oldenv[i], 1, nchar(project_name)) == project_name) {
      sheet_id <- substr(oldenv[i], (nchar(project_name) + 3), (nchar(oldenv[i]) - 1))
      id_exists <- TRUE
    }
  }
}
  
  if (id_exists == TRUE) {
    return(sheet_id)
    }
  if (id_exists == FALSE) {
    message("There is no Google Sheet ID currently associated with the given `project_name`. To create a Google Sheet corresponding to the given project name, run the `web_app()` function, supplying your desired `project_name` in the function arguments, and ensuring that you are specifying your desired number of questions and `select_on_click` values.")
    }
}