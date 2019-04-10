#' Run Shiny Application Demo
#' 
#' Function runs Shiny Application Demo
#' 
#' This function does not take any arguments and will run the Shiny Application.
#' If running from RStudio, will open the application in the viewer, 
#' otherwise will use the default internet browser.
#' 
#' @examples 
#' \donttest{
#' run_shiny()
#' }
#' 
#' @export
run_shiny <- function() {
  appDir <- system.file("shiny_example", package = "pdfsearch")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pdfsearch`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}