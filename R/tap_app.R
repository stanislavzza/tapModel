#' Launch Shiny App
#'
#' This function launches the Shiny app.
#' @export
launchApp <- function() {
  appDir <- system.file("shiny-app", package = "tapModel")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `yourPackageName`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
