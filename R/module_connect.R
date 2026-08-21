#' @import shinyWidgets
#' @import bslib
#' @export
mod_connect_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("get_connect_params"),
    shinyOAuth::use_shinyOAuth(),
    tagList(
      ## UI for study selection (if no GET parameters)
      # tags$style(".modal-dialog
      #            {max-width: 80%;
      #            width: fit-content !important;}"),
      tags$style(HTML(
        ".accordion-header {
          background-color: #f8f9fa;
          font-size: 18px;
          border-bottom: 1px solid #dee2e6;
        }"
      )),
      div(
        id = "get_connect_params_by_ui",
        style = "display: none",
        accordion(
          id = ns("connectAcc"),
          open = T,
          accordion_panel(
            id = ns("connectAccPanel"),
            title = "Connection parameters",
            div(
              # div(id = "select_trialDbId_UI",style = "display:block",
              textInput(ns("apiURL"), "BrAPI Endpoint", placeholder = "E.g. https://test-server.brapi.org/", value = "", width = "100%"),
              textInput(ns("token"), "Token", placeholder = "Enter Token", value = "", width = "100%"),
              textInput(ns("cropDb"), "CropDb", value = "maize", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames", width = "100%"),
            )
          )
        )
      )
    )
  )
}

#' @importFrom DT renderDT
#' @importFrom varhandle check.numeric
#' @export
mod_connect_server <- function(id, rv, dataset_4_dev = NULL) { # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      rv$parse_GET_param <- NULL
      rv$connect_mode <- NULL
      rv$show_study_selection <- FALSE

      encoded <- isolate(cookies::get_cookie("shinybrapps_url_search"))
      url_search <- if (!is.null(encoded)) utils::URLdecode(encoded) else NULL
      auth <- NULL

      app_url <- isolate({
        url <- data.frame(
          scheme = sub(":$", "", session$clientData$url_protocol),
          domain = session$clientData$url_hostname,
          port = session$clientData$url_port,
          path = sub("/$", "", session$clientData$url_pathname),
          parameter = NA,
          fragment = NA
        )
        urltools::url_compose(url)
      })

      if (!is.null(url_search)) {
        query <- parseQueryString(url_search)
        apiURL <- query$apiURL
        client <- build_oauth_client(apiURL, redirect_uri = app_url)
        auth <- shinyOAuth::oauth_module_server("auth", client, auto_redirect = T)
      }

      observeEvent(session$clientData$url_search, {
        query <- parseQueryString(session$clientData$url_search)
        if (!is.null(query$apiURL)) {
          cookies::set_cookie("shinybrapps_url_search",
            utils::URLencode(session$clientData$url_search, reserved = T),
            expiration = 1
          )
          rv$apiURL <- query$apiURL
          rv$connect_mode <- "url"
          rv$query <- query
        } else if (!is.null(cookies::get_cookie("shinybrapps_url_search"))) {
          ### URL mode after oauth redirection
          rv$connect_mode <- "url"
        } else {
          #### UI MODE
          rv$connect_mode <- "UI"
          shinyjs::runjs("$('#get_connect_params_by_ui').css('display', 'block');")
          rv$show_study_selection <- T
        }
      })

      observeEvent(rv$apiURL, {
        client <- build_oauth_client(rv$apiURL, redirect_uri = app_url)
        auth <- shinyOAuth::oauth_module_server("auth", client, auto_redirect = T)
      })

      observeEvent(auth$authenticated, {
        if (is.null(rv$token)) {
          req(auth$authenticated)
          req(auth$token@access_token)
          showNotification("Connected successfully", type = "message", duration = notification_duration)
          expiration_seconds <- auth$token@expires_at - as.numeric(Sys.time())
          expiration_days <- expiration_seconds / (3600 * 24)
          rv$token <- auth$token@access_token
        }
      })

      observeEvent(rv$token, {
        if (!is.null(rv$query)) {
          query <- rv$query
        } else {
          encoded <- isolate(cookies::get_cookie("shinybrapps_url_search"))
          url_search <- if (!is.null(encoded)) utils::URLdecode(encoded) else NULL
          query <- parseQueryString(url_search)
        }

        req(query$apiURL, query$cropDb)
        parsed_url <- parse_api_url(query$apiURL)

        rv$con <- brapir::brapi_connect(
          secure = (parsed_url$brapi_protocol == "https://"),
          db = parsed_url$brapi_db,
          port = parsed_url$brapi_port,
          apipath = parsed_url$brapi_apipath,
          multicrop = TRUE,
          commoncropname = query$cropDb,
          token = rv$token
        )
        updateQueryString(url_search, mode = "replace", session = session)
        rv$parse_GET_param <- query

        # delete cookie
        cookies::remove_cookie("shinybrapps_url_search")
        rv$connect_mode <- "url"
      })

      ### BrAPI GET trials
      observeEvent(c(input$apiURL, input$token, input$cropDb), {
        req(input$apiURL)
        req(input$token)
        req(input$cropDb)
        # browser()
        rv$obs_unit_level <- input$picker_obs_unit_level

        updateSelectizeInput(
          session = session, inputId = "trials", choices = "",
          options = list(
            placeholder = "",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        ## set up connection
        parsed_url <- parse_api_url(input$apiURL)

        rv$con <- brapir::brapi_connect(
          secure = (parsed_url$brapi_protocol == "https://"),
          db = parsed_url$brapi_db,
          port = parsed_url$brapi_port,
          apipath = parsed_url$brapi_apipath,
          multicrop = TRUE,
          commoncropname = input$cropDb,
          token = input$token
        )
      })
    }
  )
}
