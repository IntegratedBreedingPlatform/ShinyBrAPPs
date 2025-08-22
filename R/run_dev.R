#' @export
run_dev <- function(app, launchBrowser = T, port = NULL) {
  devtools::load_all(".")
  appDir <- paste0("inst/apps/", app)
  shiny::runApp(appDir, display.mode = "normal",launch.browser = T, port = port)
}

#' @export
run_dev_stabrapp <- function(launchBrowser = T, port = NULL) {
  run_dev("stabrapp", launchBrowser, port)
}

#' @export
run_dev_decisionsupport <- function(launchBrowser = T, port = NULL) {
  run_dev("decisionsupport", launchBrowser, port)
}

#' @export
run_dev_trialdataxplor <- function(launchBrowser = T, port = NULL) {
  run_dev("trialdataxplor", launchBrowser, port)
}

#' @export
run_dev_samplr <- function(launchBrowser = T, port = NULL) {
  run_dev("samplr", launchBrowser, port)
}
