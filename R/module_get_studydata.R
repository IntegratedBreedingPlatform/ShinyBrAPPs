#' @export
mod_get_studydata_ui <- function(id){
  ns <- NS(id)

  tagList(
    ## UI for study selection (if no GET parameters)
    bsCollapse(
      id = ns("dataImportCollapse"),
      open = "Data import",
      # style = "width : 50%",
      bsCollapsePanel(
        "Data import",
        fluidRow(
          column(
            6,id = ns("select_trialDbId_UI"),style = "display:none",
            textInput(ns("token"), "Token", placeholder = "Enter Token", width = "100%"),
            textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames", width = "100%"),
            selectizeInput(
              ns("trials"), label = "Study", choices = NULL, multiple = FALSE, width = "100%",
              options = list(
                placeholder = '',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          column(
            3,
            prettyCheckboxGroup(
              inputId = ns("environments"),
              label = "Available environments",
              choices = NULL,
              icon = icon("check-square-o"),
              status = "primary",
              outline = TRUE,
              width = "100%",
              animation = "jelly"
            ),
            actionButton(
              inputId = ns("load_env"),
              label = "Load Selected"
            ),
            actionButton(
              inputId = ns("load_all_env"),
              label = "Load All"
            )
          ),
          column(
            3,
            "Loaded Environments",
            uiOutput(ns("loaded_env"))
          )
        )
      )
    ),
  )
}

#' @export
mod_get_studydata_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)

      if(!is.null(dataset_4_dev)){ # XXX
        rv$data <- dataset_4_dev$data
        rv$study_names <- dataset_4_dev$study_names
      }else{
        parse_GET_param  <- reactive({
          pars <- parseQueryString(session$clientData$url_search)
        })

        observeEvent(parse_GET_param(),{

          if(!(is.null(parse_GET_param()$token) | is.null(parse_GET_param()$cropDb) | is.null(parse_GET_param()$trialDbId))){
            rv$con <- brapirv2::brapi_connect(
              secure = TRUE,
              protocol = brapi_protocol,
              db = brapi_db,
              port = brapi_port,
              apipath = brapi_apipath,
              multicrop = TRUE,
              token = parse_GET_param()$token,
              commoncropname = parse_GET_param()$cropDb,
              granttype = "token",
              clientid = "brapir",
              bms = TRUE
            )
            rv$trialDbId <- parse_GET_param()$trialDbId
          }else{
            shinyjs::show(id = "select_trialDbId_UI")
            if(!is.null(parse_GET_param()$token)){
              updateTextInput("token", session = session, value = parse_GET_param()$token)
            }
          }
        })

        observeEvent(c(input$token,input$cropDb),{
          req(input$token)
          req(input$cropDb)
          rv$con <- brapirv2::brapi_connect(
            secure = TRUE,
            protocol = brapi_protocol,
            db = brapi_db,
            port = brapi_port,
            apipath = brapi_apipath,
            multicrop = TRUE,
            commoncropname = input$cropDb,
            token = input$token,
            granttype = "token",
            clientid = "brapir",
            bms = TRUE
          )
          try({
            ## get the brapi::trials for the crop
            trials <- as.data.table(brapirv2::brapi_get_trials(con = rv$con))
            trial_choices <- trials[,trialDbId]
            names(trial_choices) <- trials[,trialName]
            updateSelectizeInput(
              inputId = "trials", session = session, choices = trial_choices,
              options = list(
                placeholder = 'Select a study',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
        })
      }

      observeEvent(input$trials,{
        req(input$trials)
        rv$trialDbId <- input$trials
      })

      observeEvent(rv$trialDbId,{
        req(rv$trialDbId)
        try({
          ## get all the studies of a trial
          trial_studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, trialDbId = rv$trialDbId))
          study_ids <- unique(trial_studies$studyDbId)

          ## make environment names
          study_names <- rbindlist(l = lapply(1:length(study_ids), function(k){
            try({
              study_id <- study_ids[k]

              location_name <- trial_studies[studyDbId == study_id,unique(locationName)]
              location_id <- trial_studies[locationName == location_name, unique(locationDbId)]

              location_name_abbrev <- NULL
              try({
                loc <- brapirv2::brapi_get_locations_locationDbId(con = rv$con, locationDbId = location_id)
                location_name_abbrev <- loc[,unique("abbreviation")]
              })
              maxchar <- 9
              location_name_abbrev <- ifelse(
                length(location_name_abbrev)==0,
                ifelse(
                  nchar(location_name)>maxchar,
                  paste0(
                    substr(location_name,1,4),
                    "...",
                    substr(location_name,nchar(location_name)-3,nchar(location_name))
                  ),
                  location_name
                ),
                location_name_abbrev
              )
              environment_number <- trial_studies[studyDbId == study_id & environmentParameters.parameterName == "ENVIRONMENT_NUMBER",environmentParameters.value]
              environment_number <- ifelse(length(environment_number)==0,k,environment_number)

              return(data.table(study_id,location_name,location_name_abbrev,environment_number))
            })
          }), use.names = T, fill = T)

          study_names[,study_name_BMS := paste0(
            environment_number, "-",
            location_name
          )]
          study_names[,study_name_app := paste0(
            environment_number, "-",
            location_name, " (",
            location_name_abbrev, ")",
          )]
          study_names[,study_name_abbrev_app := paste0(
            environment_number, "-",
            location_name_abbrev
          )]
          study_names[, loaded:=F]

          env_choices <- study_names[,study_id]
          names(env_choices) <- study_names[,study_name_app]

          updateCheckboxGroupInput(
            inputId = "environments",
            session = session,
            label = "Available environments",
            choices = env_choices
          )
          shinyjs::show(id = "load_env")
          shinyjs::show(id = "load_all_env")
          rv$study_names <- study_names
        })
      })

      observeEvent(input$load_env,{
        req(input$environments)
        withProgress(message = "Loading", value = 0, {
          n_studies <- length(input$environments)

          studies <- rbindlist(lapply(1:n_studies, function(k){
            id <- input$environments[k]

            incProgress(1/n_studies, detail = rv$study_names[study_id == id,study_name_app])

            study <- get_env_data(
              con = rv$con, studyDbId = id,
              env_number = rv$study_names[study_id == id,environment_number],
              loc_name = rv$study_names[study_id == id,location_name],
              loc_name_abbrev = rv$study_names[study_id == id,location_name_abbrev],
              stu_name_app = rv$study_names[study_id == id,study_name_app],
              stu_name_abbrev_app = rv$study_names[study_id == id,study_name_abbrev_app]
            )
            rv$study_names[study_id == id,loaded:=T]
            return(study)
          }), use.names = T,fill = T
          )

          env_choices <- rv$study_names[loaded==F,study_id]
          if(length(env_choices)==0){
            updateCheckboxGroupInput(session = session,inputId = "environments", label = "", choices = vector())
            shinyjs::hide(id = "load_env")
            shinyjs::hide(id = "load_all_env")
            shinyBS::updateCollapse(
              id = "dataImportCollapse",
              session = session,
              close = "Data import"
            )
          }else{
            names(env_choices) <- rv$study_names[loaded==F,study_name_app]
            updateCheckboxGroupInput(session = session,inputId = "environments", choices = env_choices)
          }
          output$loaded_env <- renderUI({
            tags$ul(
              lapply(rv$study_names[loaded == T,study_name_app], tags$li)
            )
          })
          if("trialDbId" %in% names(rv$data)){
            rv$data <- rbindlist(
              list(
                rv$data[trialDbId == rv$trialDbId],
                studies
              ),
              use.names = T, fill = T
            )
          }else{
            rv$data <- studies
          }
        })
      })

      observeEvent(input$load_all_env,{
        req(input$trials)
        withProgress(message = "Loading", value = 0, {
          n_studies <- rv$study_names[,.N]

          studies <- rbindlist(lapply(1:n_studies, function(k){

            incProgress(1/n_studies, detail = rv$study_names[k,study_name_app])

            study <- get_env_data(
              con = rv$con, studyDbId = rv$study_names[k,study_id],
              env_number = rv$study_names[k,environment_number],
              loc_name = rv$study_names[k,location_name],
              loc_name_abbrev = rv$study_names[k,location_name_abbrev],
              stu_name_app = rv$study_names[k,study_name_app],
              stu_name_abbrev_app = rv$study_names[k,study_name_abbrev_app]
            )

            rv$study_names[k,loaded:=T]
            return(study)
          }
          ))

          updateCheckboxGroupInput(session = session,inputId = "environments", label = "", choices = vector())
          output$loaded_env <- renderUI({
            tags$ul(
              lapply(rv$study_names[loaded == T,study_name_app], tags$li)
            )
          })
          shinyjs::hide(id = "load_env")
          shinyjs::hide(id = "load_all_env")

          rv$data <- studies

          shinyBS::updateCollapse(
            id = "dataImportCollapse",
            session = session,
            close = "Data import"
          )
        })
      })

      return(rv)
    }
  )
}
