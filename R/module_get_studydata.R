#' @export
mod_get_studydata_ui <- function(id){
  ns <- NS(id)
  div(
    id = ns("get_studydata"),
    tagList(
      ## UI for study selection (if no GET parameters)
      div(
        id = ns("get_studydata_by_ui"),
        style = "display:none",
        bsCollapse(
          id = ns("dataImportCollapse"),
          open = "Data import",
          # style = "",
          bsCollapsePanel(
            "Data import",
            fluidRow(
              column(
                6,id = ns("select_trialDbId_UI"),style = "display:block",
                textInput(ns("apiURL"), "BrAPI Endpoint", placeholder = "E.g. https://bms-uat-test.net/bmsapi", value = "https://bms-uat.ibp.services/bmsapi", width = "100%"),
                textInput(ns("token"), "Token", placeholder = "Enter Token", value = "", width = "100%"),
                textInput(ns("cropDb"), "CropDb", value = "maize", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames", width = "100%"),
                selectInput(ns("picker_obs_unit_level"), label = "Observation unit levels", choices = allowed_obs_unit_levels, selected = allowed_obs_unit_levels, multiple = T, width = "100%"),
                selectizeInput(
                  ns("trials"), label = "Study", choices = NULL, multiple = FALSE, width = "100%",
                  options = list(
                    placeholder = '',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                ),
                actionButton(ns("go_trial_metadata"), "Show study metadata", css.class = "btn btn-info"),
                bsModal(ns("modal_trial_metadata"), "Study Metadata", ns("go_trial_metadata"), size = "large",
                        dataTableOutput(ns("table_trial_metadata")))
              ),
              column(
                3,
                prettyCheckboxGroup(
                  inputId = ns("environments"),
                  label = "Available environments",
                  choices = NULL,
                  icon = icon("square-check"),
                  status = "primary",
                  outline = TRUE,
                  width = "100%",
                  animation = "jelly"
                ),
                actionButton(
                  inputId = ns("load_env"),
                  label = "Load Selected",
                  css.class = "btn btn-primary"
                ),
                actionButton(
                  inputId = ns("load_all_env"),
                  label = "Load All",
                  css.class = "btn btn-primary"
                ),
                actionButton(ns("go_study_metadata_ui"), "Show Environment Metadata", css.class = "btn btn-info")
                # bsModal(ns("modal_study_metadata"), "Environment Metadata", ns("go_study_metadata"), size = "large",
                #         dataTableOutput(ns("table_study_metadata")))
              ),
              column(
                3,
                "Loaded Environments",
                uiOutput(ns("loaded_env"))
              )
            )
          )
        )
      ),
      div(
        id = ns("get_studydata_by_url"), style = "float:right",
        shiny::actionButton(ns("go_study_metadata_url"), "Show Environment Metadata")
      )
    ),
    bsModal(ns("modal_study_metadata"), "Environment Metadata", ns("go_study_metadata_ui"), size = "large",
            uiOutput(ns("tables_study_metadata"))
    )
  )
}

#' @export
mod_get_studydata_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)
      rv
      env_to_load <- reactiveVal()

      parse_GET_param  <- reactive({
        pars <- parseQueryString(session$clientData$url_search)
      })

      if(!is.null(dataset_4_dev)){ # XXX
        rv$data <- dataset_4_dev$data
        rv$trial_metadata <- dataset_4_dev$trial_metadata
        rv$study_metadata <- dataset_4_dev$study_metadata
      }else{

        rv$trialDbId <- NULL
        rv$study_metadata <- NULL
        rv$data <- NULL
        rv$trial_metadata <- NULL
        rv$pushOK <- FALSE
        
        observeEvent(parse_GET_param(),{
          
          if(!is.null(parse_GET_param()$pushOK)){
            rv$pushOK <- parse_GET_param()$pushOK
          }
          
          if(!is.null(parse_GET_param()$apiURL) &
             !is.null(parse_GET_param()$token) &
             !is.null(parse_GET_param()$cropDb) &
             !is.null(parse_GET_param()$studyDbIds)){

            #### URL MODE

            ### set up connection
            parsed_url <- parse_api_url(parse_GET_param()$apiURL)
            
            rv$con <- brapirv2::brapi_connect(
              secure = TRUE,
              protocol = parsed_url$brapi_protocol,
              db = parsed_url$brapi_db,
              port = parsed_url$brapi_port,
              apipath = parsed_url$brapi_apipath,
              multicrop = TRUE,
              commoncropname = parse_GET_param()$cropDb,
              token = parse_GET_param()$token,
              granttype = "token",
              clientid = "brapir",
              bms = TRUE
            )
            
            # get study_metadata
            tryCatch({
              study_metadata <- make_study_metadata(con = rv$con, studyDbIds = parse_GET_param()$studyDbIds)
            }, error = function(e)({
              showNotification("Could not get environment metadata", type = "error", duration = notification_duration)
            }))

            req(exists("study_metadata"))
            req(study_metadata[,.N]>0)

            ## set environments to load
            env_to_load(study_metadata[,unique(studyDbId)])

            rv$study_metadata <- study_metadata

            if (isTruthy(can_filter_obs_unit_level_in_url)) {
              chosen_levels <- parse_GET_param()$obs_unit_level
              if (!is.null(chosen_levels)) {
                rv$obs_unit_level <- intersect(allowed_obs_unit_levels, unlist(strsplit(chosen_levels, ",")))
              } else {
                rv$obs_unit_level <- NULL
              }
            } else {
              rv$obs_unit_level <- allowed_obs_unit_levels
            }

          }else{

            #### UI MODE
            shinyjs::show(id = "get_studydata_by_ui")
            shinyjs::hide(id = "get_studydata_by_url")

            ### BrAPI GET trials
            observeEvent(c(input$apiURL, input$token, input$cropDb, input$picker_obs_unit_level),{
              req(input$apiURL)
              req(input$token)
              req(input$cropDb)
              req(input$picker_obs_unit_level)
              
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

              rv$con <- brapirv2::brapi_connect(
                secure = TRUE,
                protocol = parsed_url$brapi_protocol,
                db = parsed_url$brapi_db,
                port = parsed_url$brapi_port,
                apipath = parsed_url$brapi_apipath,
                multicrop = TRUE,
                commoncropname = input$cropDb,
                token = input$token,
                granttype = "token",
                clientid = "brapir",
                bms = TRUE
              )

              ## get trials
              withProgress(message = "Reaching studies", value = 0, {
                incProgress(1)
                tryCatch({
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
                  rv$trial_metadata <- trials
                },
                error=function(e){
                  showNotification("Check url, token and/or cropDb", type = "error", duration = notification_duration)
                })
              })
            })

            observeEvent(input$trials,{
              req(input$trials)
              rv$data <- NULL
              rv$trialDbId <- input$trials
            })

            ### BrAPI GET studies and GET locations
            observeEvent(rv$trialDbId,{
              req(rv$trialDbId)

              # get study_metadata
              tryCatch({
                study_metadata <- make_study_metadata(con = rv$con, trialDbId = rv$trialDbId)
              }, error = function(e)({
                showNotification("Could not get environment metadata", type = "error", duration = notification_duration)
              }))

              req(study_metadata[,.N]>0)

              env_choices <- study_metadata[,unique(studyDbId)]
              names(env_choices) <- study_metadata[,unique(study_name_app)]

              updateCheckboxGroupInput(
                inputId = "environments",
                session = session,
                label = "Available environments",
                choices = env_choices
              )
              shinyjs::show(id = "load_env")
              shinyjs::show(id = "load_all_env")
              rv$study_metadata <- study_metadata
            })
          } # (end UI MODE)
        }) # (end observer for parse_GET_param() )
      } # (end is.null(dataset_4_dev)) XXX

      ## load environment data
      observeEvent(input$load_env,{
        req(input$environments)
        req(rv$study_metadata)
        req(rv$study_metadata)
        env_to_load(input$environments)
      })
      observeEvent(input$load_all_env,{
        req(rv$study_metadata)
        env_to_load(rv$study_metadata[loaded==F,unique(studyDbId)])
      })
      observeEvent(env_to_load(),{
        req(env_to_load())
        req(rv$study_metadata)
        study_metadata <- rv$study_metadata
        withProgress(message = "Loading", value = 0, {
          n_studies <- length(env_to_load())

          studies <- rbindlist(lapply(1:n_studies, function(k){
            id <- env_to_load()[k]

            incProgress(1/n_studies, detail = rv$study_metadata[studyDbId == id,unique(study_name_app)])
            study <- get_env_data(
              con = rv$con, studyDbId = id,
              env_number = rv$study_metadata[studyDbId == id,unique(environment_number)],
              loc_name = rv$study_metadata[studyDbId == id,unique(locationName)],
              loc_name_abbrev = rv$study_metadata[studyDbId == id,unique(location_name_abbrev)],
              stu_name_app = rv$study_metadata[studyDbId == id,unique(study_name_app)],
              stu_name_abbrev_app = rv$study_metadata[studyDbId == id,unique(study_name_abbrev_app)],
              obs_unit_level =rv$obs_unit_level
            )
            if (!is.null(study)) { rv$study_metadata[studyDbId == id,loaded:=T] }
            return(study)
          }), use.names = T,fill = T
          )

          ## convert variables from text to numeric (when variable is numeric)
          studies <- studies[,lapply(.SD, function(x){
            if(all(check.numeric(x))){
              as.numeric(x)
            }else{
              x
            }
          })]
          
          env_choices <- rv$study_metadata[loaded==F,unique(studyDbId)]
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
            names(env_choices) <- rv$study_metadata[loaded==F,unique(study_name_app)]
            updateCheckboxGroupInput(session = session,inputId = "environments", choices = env_choices)
          }
          output$loaded_env <- renderUI({
            tags$ul(
              lapply(rv$study_metadata[loaded == T,unique(study_name_app)], tags$li)
            )
          })

          if("trialDbId" %in% names(rv$data)){
            rv$data <- unique(rbindlist(
              list(
                rv$data[trialDbId == rv$trialDbId],
                studies
              ),
              use.names = T, fill = T
            ))
          }else{
            rv$data <- studies
          }
        })
      })

      output$table_trial_metadata <- renderDT({
        req(input$trials)
        trial_metadata <- data.table(
          metadata = names(rv$trial_metadata[trialDbId==input$trials]),
          value = unlist(rv$trial_metadata[trialDbId==input$trials])
        )
        datatable(
          trial_metadata,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            # scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })

      observeEvent(input$go_study_metadata_url, {
        toggleModal(session, "modal_study_metadata", toggle = "open")
      })

      output$tables_study_metadata <- renderUI({
        req(rv$study_metadata)
        #browser()
        panels <- lapply(
          rv$study_metadata[,unique(studyDbId)],
          function(id){
            bsCollapsePanel(
              title = rv$study_metadata[studyDbId == id, unique(study_name_app)],
              datatable(
                rv$study_metadata[studyDbId == id],
                rownames = F,
                width = "100%",
                options = list(
                  paging = F,
                  scrollX = T,
                  scrollY = 500,
                  scrollCollapse = T,
                  dom = 't'
                )
              )
            )
          }
        )
        do.call(
          what = "bsCollapse",
          args = append(list(id = NULL, multiple = FALSE, open = NULL), panels),
          quote = F
        )
      })
      return(rv)
    }
  )
}
