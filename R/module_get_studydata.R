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
            6,id = ns("select_trialDbId_UI"),style = "display:block",
            textInput(ns("apiURL"), "BrAPI Endpoint", placeholder = "E.g. https://brapi.bms-uat-test.net:80/bmsapi", value = "https://brapi.bms-uat-test.net:80/bmsapi", width = "100%"),
            textInput(ns("token"), "Token", placeholder = "Enter Token", width = "100%"),
            textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames", width = "100%"),
            selectizeInput(
              ns("trials"), label = "Study", choices = NULL, multiple = FALSE, width = "100%",
              options = list(
                placeholder = '',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            shiny::actionButton(ns("go_trial_metadata"), "Show study metadata"),
            bsModal(ns("modal_trial_metadata"), "Study Metadata", ns("go_trial_metadata"), size = "large",
                    dataTableOutput(ns("table_trial_metadata")))
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
            shiny::actionButton(
              inputId = ns("load_env"),
              label = "Load Selected"
            ),
            shiny::actionButton(
              inputId = ns("load_all_env"),
              label = "Load All"
            ),
            shiny::actionButton(ns("go_study_metadata"), "Show Environment Metadata"),
            bsModal(ns("modal_study_metadata"), "Environment Metadata", ns("go_study_metadata"), size = "large",
                    dataTableOutput(ns("table_study_metadata")))
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

        observeEvent(parse_GET_param()$apiURL,{
          req(parse_GET_param()$apiURL)
          updateTextInput(session = session, inputId = "apiURL", value = parse_GET_param()$apiURL)
        })
        observeEvent(parse_GET_param()$token,{
          req(parse_GET_param()$token)
          updateTextInput(session = session, inputId = "token", value = parse_GET_param()$token)
        })
        observeEvent(parse_GET_param()$cropDb,{
          req(parse_GET_param()$cropDb)
          updateTextInput(session = session, inputId = "cropDb", value = parse_GET_param()$cropDb)
        })

        ### BrAPI GET trials
        observeEvent(c(input$apiURL, input$token, input$cropDb),{
          req(input$apiURL)
          req(input$token)
          req(input$cropDb)

          ### parse URL
          ## get protocol (default = "https://")
          protocolless_url <- gsub("(^http://|^https://)(.*)$", "\\2",input$apiURL)
          brapi_protocol <- gsub("(^http://|^https://)(.*)$", "\\1",input$apiURL)
          brapi_protocol <- ifelse(brapi_protocol == input$apiURL, "https://", brapi_protocol)

          ## get base url and port (default = 80)
          db_split <- strsplit(gsub("([^/]*).*", "\\1",protocolless_url), ":")
          brapi_db <- db_split[[1]][1]
          brapi_port <- ifelse(is.na(db_split[[1]][2]),80,as.numeric(db_split[[1]][2]))

          ## brapi api path (default = "/")
          brapi_apipath <- ifelse(grepl("/.*",protocolless_url),gsub("[^/]*/(.*)", "\\1", protocolless_url),"/")

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
          ## get the brapi::trials
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

              ## set trialDbId if in http query string
              if(length(parse_GET_param()$trialDbId%in%trial_choices)>0){
                if(parse_GET_param()$trialDbId%in%trial_choices){
                  updateSelectizeInput(
                    inputId = "trials", session = session, selected = parse_GET_param()$trialDbId
                  )
                }else{
                  showNotification("trialDbId not in cropDb", type = "error", duration = notification_duration)
                }
              }
            },
            error=function(e){
              showNotification("check url, token and/or cropDb", type = "error", duration = notification_duration)
            })
          })
        })
      }

      observeEvent(input$trials,{
        req(input$trials)
        rv$trialDbId <- input$trials
      })

      ### BrAPI GET studies and GET locations
      observeEvent(rv$trialDbId,{
        req(rv$trialDbId)

        ## get all the studies of a trial
        tryCatch({
          study_metadata <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, trialDbId = rv$trialDbId))
          study_ids <- unique(study_metadata$studyDbId)
          location_ids <- unique(study_metadata$locationDbId)
        },
        error=function(e){
          showNotification("study metadata not found", type = "error", duration = notification_duration)
        })

        if(exists("location_ids")){
          ## get location abbreviations
          withProgress(message = "Reaching location metadata", value = 0, {
            req(location_ids)
            loc_names <- rbindlist(l = lapply(1:length(location_ids), function(k){
              incProgress(
                1/length(location_ids),
                detail = study_metadata[locationDbId==location_ids[k], unique(locationName)]
              )
              try({
                brapirv2::brapi_get_locations_locationDbId(con = rv$con, locationDbId = location_ids[k])
              })
            }), fill = T, use.names = T)
          })
          req(loc_names)
          maxchar <- 9
          loc_names[,location_name_abbrev := lapply(abbreviation, function(x){
            if(is.na(x)){
              if(nchar(locationName)>maxchar){
                paste0(
                  substr(locationName,1,4),
                  "...",
                  substr(locationName,nchar(locationName)-3,nchar(locationName))
                )
              }else{
                locationName
              }
            }else{
              x
            }
          })]
          study_metadata <- merge.data.table(
            x = study_metadata[,-intersect(names(study_metadata)[names(study_metadata)!="locationDbId"], names(loc_names)), with = F],
            # x = study_metadata[,-"locationName", with = F],
            y = loc_names,
            by = "locationDbId", all.x = T)

          ## environment number
          if("environmentParameters.parameterName"%in%names(study_metadata)){
          env_number <- merge.data.table(
            x = study_metadata[,.(studyDbId = unique(studyDbId))],
            y = study_metadata[environmentParameters.parameterName == "ENVIRONMENT_NUMBER",.(studyDbId, environment_number = environmentParameters.value)],
            by = "studyDbId", all.x = T)
          env_number[is.na(environment_number), environment_number:=studyDbId]
          }else{
            env_number <- study_metadata[,.(studyDbId = unique(studyDbId))]
            env_number[, environment_number:=studyDbId]
          }
          study_metadata <- merge.data.table(
            x = study_metadata,
            y = env_number,
            by = "studyDbId", all.x = T)

          ## environment names
          study_metadata[,study_name_BMS := paste0(
            environment_number, "-",
            locationName
          )]
          study_metadata[,study_name_app := paste0(
            study_name_BMS, " (",
            location_name_abbrev, ")"
          )]
          study_metadata[,study_name_abbrev_app := paste0(
            environment_number, "-",
            location_name_abbrev
          )]
          study_metadata[, loaded:=F]
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

          ## load environments if indicated in url
          req(parse_GET_param()$studyDbIds)
          ids <- unlist(strsplit(parse_GET_param()$studyDbIds, ","))
          env_to_load(rv$study_metadata[loaded==F & studyDbId %in% ids,unique(studyDbId)])
          if(any(!(ids%in%rv$study_metadata[,unique(studyDbId)]))){
            showNotification(
              paste0(
                "studyDbId ",
                paste(ids[!(ids%in%rv$study_metadata[,unique(studyDbId)])], collapse = ", "),
                " not in trialDbId"
              ),
              type = "error", duration = notification_duration
            )
          }
        }
      })

      ### load environment data
      observeEvent(input$load_env,{
        req(input$environments)
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
              stu_name_abbrev_app = rv$study_metadata[studyDbId == id,unique(study_name_abbrev_app)]
            )

            rv$study_metadata[studyDbId == id,loaded:=T]
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

      output$table_study_metadata <- renderDT({
        req(rv$study_metadata)
        dtable <- datatable(
          rv$study_metadata,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't',
            rowsGroup = as.list(c(0,(1:length(names(rv$study_metadata)))[unlist(rv$study_metadata[,lapply(.SD, function(x){length(unique(x))}),studyDbId][,lapply(.SD,function(x){all(x==1)})])] - 1)) # indices of the columns with duplicated values per studyDbId
          ))
        path <- "www/js/datatables-rowsgroup/"
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0",
          path, script = "dataTables.rowsGroup.js"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      })

      return(rv)
    }
  )
}
