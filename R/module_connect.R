#' @import shinyWidgets
#' @import bslib
#' @export
mod_connect_ui <- function(id){
  ns <- NS(id)
  div(
    id = ns("get_connect_params"),
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
mod_connect_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){
      
      ns <- NS(id)
      rv_st <- reactiveValues(
        env_to_load = NULL,
        parse_GET_param = NULL,
        ui_mode = NULL
      )
        
        observeEvent(session$clientData$url_search, {
          rv_st$parse_GET_param <- parseQueryString(session$clientData$url_search)
        })
        
        
        observeEvent(rv_st$parse_GET_param,{
          if(!is.null(rv_st$parse_GET_param$pushOK)){
            rv$pushOK <- rv_st$parse_GET_param$pushOK
          }
          
          if(!is.null(rv_st$parse_GET_param$apiURL) &
             !is.null(rv_st$parse_GET_param$token) &
             !is.null(rv_st$parse_GET_param$cropDb)){
            
            ### set up connection
            parsed_url <- parse_api_url(rv_st$parse_GET_param$apiURL)
            
            # rv$con <- brapirv2::brapi_connect(
            #   secure = TRUE,
            #   protocol = parsed_url$brapi_protocol,
            #   db = parsed_url$brapi_db,
            #   port = parsed_url$brapi_port,
            #   apipath = parsed_url$brapi_apipath,
            #   multicrop = TRUE,
            #   commoncropname = rv_st$parse_GET_param$cropDb,
            #   token = rv_st$parse_GET_param$token,
            #   granttype = "token",
            #   clientid = "brapir",
            #   bms = TRUE
            # )
            
            rv$con <- brapir::brapi_connect(
              secure = (parsed_url$brapi_protocol == "https://"), 
              db = parsed_url$brapi_db,
              port = parsed_url$brapi_port,
              apipath = parsed_url$brapi_apipath,
              multicrop = TRUE, 
              commoncropname = rv_st$parse_GET_param$cropDb,
              token = rv_st$parse_GET_param$token
            )
            
            rv$connect_mode <- "url"
          } else {
              #### UI MODE
              rv$connect_mode <- "UI"
              shinyjs::runjs("$('#get_connect_params_by_ui').css('display', 'block');") 
            } 
          })
        ### BrAPI GET trials
        observeEvent(c(input$apiURL, input$token, input$cropDb),{
          req(input$apiURL)
          req(input$token)
          req(input$cropDb)
          #browser()
          rv$obs_unit_level <-  input$picker_obs_unit_level
          
          updateSelectizeInput(
            session = session, inputId = "trials", choices = "",
            options = list(
              placeholder = '',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
          
          ## set up connection
          parsed_url <- parse_api_url(input$apiURL)
          
          # rv$con <- brapirv2::brapi_connect(
          #   secure = TRUE,
          #   protocol = parsed_url$brapi_protocol,
          #   db = parsed_url$brapi_db,
          #   port = parsed_url$brapi_port,
          #   apipath = parsed_url$brapi_apipath,
          #   multicrop = TRUE,
          #   commoncropname = input$cropDb,
          #   token = input$token,
          #   granttype = "token",
          #   clientid = "brapir",
          #   bms = TRUE
          # )
          # 
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