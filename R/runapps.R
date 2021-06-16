#' @export
run_test <- function() {
  appDir <- system.file("apps", "test", package = "shinybrapps")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `shinybrapps`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}

