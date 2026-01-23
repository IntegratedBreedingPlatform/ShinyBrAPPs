get_global_config <- function(env = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  config::get(
    file = system.file("config", "config.yml", package = "shinybrapps"),
    config = env
  )
}

get_app_config <- function(app_folder, env = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  config::get(
    file = system.file("apps", app_folder, "config.yml", package = "shinybrapps"),
    config = env
  )
}

#' Get config
#'
#' @param env
#'
#' @returns config parameters
#' @import config
#' @export
get_config <- function(app_folder, env = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  modifyList(
    get_global_config(env),
    get_app_config(app_folder, env)
  )
}
