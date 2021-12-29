#' @export
run_test <- function() {
  appDir <- system.file("apps", "test", package = "shinybrapps")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `shinybrapps`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}

#' @export
run_stabrapp <- function() {
  appDir <- system.file("apps", "stabrapp", package = "shinybrapps")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `stabrapp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}

#' @export
run_decision <- function() {
  appDir <- system.file("apps", "decisionsupport", package = "shinybrapps")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `decisionsupport`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
}
