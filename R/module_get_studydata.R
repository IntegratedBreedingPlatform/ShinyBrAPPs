#' @import shinyWidgets
#' @import bslib
#' @export
mod_get_studydata_ui <- function(id){
  ns <- NS(id)
  div(
    id = ns("get_studydata"),
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
        id = "get_studydata_by_ui",
        style = "display: none",
        accordion(
          id = "dataImportAcc",
          open = T,
          accordion_panel(
            id = "dataImportAccPanel",
            title = "Data import",
            layout_columns(
              col_widths = c(6,3,3),
              div(
                selectInput(ns("picker_obs_unit_level"), label = "Observation unit levels", choices = allowed_obs_unit_levels, selected = allowed_obs_unit_levels, multiple = T, width = "100%"),
                selectizeInput(
                  ns("trials"), label = "Study", choices = NULL, multiple = FALSE, width = "100%",
                  options = list(
                    placeholder = '',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                ),
                actionButton(ns("go_trial_metadata"), "Show study metadata", class = "btn btn-info"),
                # bsModal("modal_trial_metadata"), "Study Metadata", ns("go_trial_metadata"), size = "large",
                #         dataTableOutput(ns("table_trial_metadata")))
              ),
              div(
                awesomeCheckboxGroup(
                  inputId = ns("environments"),
                  label = "Available environments",
                  choices = NULL,
                  # status = "primary",
                  width = "100%",
                ),
                actionButton(
                  inputId = ns("load_env"),
                  label = "Load Selected",
                  class = "btn btn-primary"
                ),
                actionButton(
                  inputId = ns("load_all_env"),
                  label = "Load All",
                  class = "btn btn-primary"
                ),
                actionButton(ns("go_study_metadata_ui"), "Show Environment Metadata", class = "btn btn-info")
              ),
              div(
                "Loaded Environments",
                uiOutput(ns("loaded_env"))
              )
            )
          )
        )
      ) 
      ,
      div(
        id = "get_studydata_by_url", style = "float:right",
        #shiny::actionButton(ns("go_study_metadata_url"), "Show Environment Metadata")
      )
    )
  )
}

#' @importFrom DT renderDT
#' @importFrom varhandle check.numeric
#' @export
mod_get_studydata_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)
      rv_st <- reactiveValues(
        env_to_load = NULL,
        parse_GET_param = NULL,
        ui_mode = NULL
      )
      
      if(!is.null(dataset_4_dev)){ # XXX
        rv$data <- dataset_4_dev$data
        rv$trial_metadata <- dataset_4_dev$trial_metadata
        rv$study_metadata <- dataset_4_dev$study_metadata
        
      } else {

        observeEvent(session$clientData$url_search, {
          rv_st$parse_GET_param <- parseQueryString(session$clientData$url_search)
        })
  
  
        observeEvent(rv_st$parse_GET_param,{
          if(!is.null(rv_st$parse_GET_param$pushOK)){
            rv$pushOK <- rv_st$parse_GET_param$pushOK
          }
          
          if(!is.null(rv_st$parse_GET_param$studyDbIds)){
            
            ### set up connection
            parsed_url <- parse_api_url(rv_st$parse_GET_param$apiURL)
            
            # get study_metadata
            tryCatch({
              print(rv$con$token)
              print(rv$con$commoncropname)
              study_metadata <- make_study_metadata(con = rv$con, studyDbIds = rv_st$parse_GET_param$studyDbIds)
            }, error = function(e)({
              showNotification("Could not get environment metadata", type = "error", duration = notification_duration)
            }))
            
            req(exists("study_metadata"))
            req(study_metadata[,.N]>0)
            
            ## set environments to load
            rv_st$env_to_load <- study_metadata[,unique(studyDbId)]
            
            rv$study_metadata <- study_metadata
            
            if (isTruthy(can_filter_obs_unit_level_in_url)) {
              chosen_levels <- rv_st$parse_GET_param$obs_unit_level
              if (!is.null(chosen_levels)) {
                rv$obs_unit_level <- intersect(allowed_obs_unit_levels, unlist(strsplit(chosen_levels, ",")))
              } else {
                rv$obs_unit_level <- NULL
              }
            } else {
              rv$obs_unit_level <- allowed_obs_unit_levels
            }
  
          } else {
            #### UI MODE
            shinyjs::runjs("$('#get_studydata_by_ui').css('display', 'block');") 
          }
        })
        observeEvent(rv$con,{
          ## get trials
          
          withProgress(message = "Reaching studies", value = 0, {
            incProgress(1)
            tryCatch({
              trials <- as.data.table(brapirv2::brapi_get_trials(con = rv$con))
              rv$trial_metadata <- trials
              trial_choices <- trials[,trialDbId]
              names(trial_choices) <- trials[,trialName]
              updateSelectizeInput(
                inputId = "trials", session = session, choices = trial_choices,
                options = list(
                  placeholder = 'Select a study',
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
              if (rv$connect_mode=="UI"){
                showNotification("Connection successful", type = "message", duration = notification_duration)
                shinyjs::runjs('var accordionBody = $("#connect-connectAccPanel");
                                var accordionPanel = accordionBody.parent();
                                accordionPanel.collapse("hide");')
              }              
            },
            error=function(e){
              showNotification("Check url, token and/or cropDb", type = "error", duration = notification_duration)
            })
          })
          
        })
  
        observeEvent(input$trials,{
          req(input$trials)
          rv$data <- NULL
          rv_st$trialDbId <- input$trials
        })
  
        
        ### BrAPI GET studies and GET locations
        observeEvent(rv_st$trialDbId,{
          req(rv_st$trialDbId)
          # get study_metadata
          tryCatch({
            study_metadata <- make_study_metadata(con = rv$con, trialDbId = rv_st$trialDbId)
          }, error = function(e)({
            showNotification("Could not get environment metadata", type = "error", duration = notification_duration)
          }))
  
          req(study_metadata[,.N]>0)
  
          env_choices <- study_metadata[,unique(studyDbId)]
          names(env_choices) <- study_metadata[,unique(study_name_app)]
  
          updateAwesomeCheckboxGroup(
            inputId = "environments",
            session = session,
            label = "Available environments",
            choices = env_choices
          )
          shinyjs::show(id = "load_env")
          shinyjs::show(id = "load_all_env")
          rv$study_metadata <- study_metadata
        })
  
        ## load environment data
        observeEvent(input$load_env,{
          req(input$environments)
          req(rv$study_metadata)
          req(rv$study_metadata)
          rv_st$env_to_load <- input$environments
        })
        observeEvent(input$load_all_env,{
          req(rv$study_metadata)
          rv_st$env_to_load <- rv$study_metadata[loaded==F,unique(studyDbId)]
        })
        observeEvent(rv_st$env_to_load,{
          req(rv$study_metadata)
          study_metadata <- rv$study_metadata
          withProgress(message = "Loading", value = 0, {
            n_studies <- length(rv_st$env_to_load)
  
            studies <- rbindlist(lapply(1:n_studies, function(k){
              id <- rv_st$env_to_load[k]
  
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
              updateAwesomeCheckboxGroup(session = session,inputId = "environments", label = "", choices = vector())
              shinyjs::hide(id = "load_env")
              shinyjs::hide(id = "load_all_env")
              accordion_panel_close(id = "dataImportAcc", values = "dataImportAccPanel")
              
            }else{
              names(env_choices) <- rv$study_metadata[loaded==F,unique(study_name_app)]
              updateAwesomeCheckboxGroup(session = session,inputId = "environments", choices = env_choices)
            }
            output$loaded_env <- renderUI({
              tags$ul(
                lapply(rv$study_metadata[loaded == T,unique(study_name_app)], tags$li)
              )
            })
  
            if("trialDbId" %in% names(rv$data)){
              rv$data <- unique(rbindlist(
                list(
                  rv$data[trialDbId == rv_st$trialDbId],
                  studies
                ),
                use.names = T, fill = T
              ))
            }else{
              rv$data <- studies
            }
            
            if("observationDbId" %in% names(rv$data)){
              rv$data[, observationDbId := as.character(observationDbId)]
            }
          })
        })
  
        output$table_trial_metadata <- renderDT({
          req(input$trials)
          trial_metadata <- data.table(
            metadata = names(rv$trial_metadata[trialDbId==input$trials]),
            value = unlist(rv$trial_metadata[trialDbId==input$trials])
          )[!is.na(value),]
          
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
  
        #observeEvent(input$go_study_metadata_url, {
        #  showModal(
        #    modalDialog(
        #      title = "Environments metadata",
        #      fade = F,
        #      uiOutput(ns("tables_study_metadata")),
        #      footer = tagList(
        #        modalButton("Close")
        #      )
        #    )
        #  )
        #})
        
        observeEvent(input$go_trial_metadata, {
          showModal(
            modalDialog(
              title = "Study Metadata",
              easyClose = TRUE,
              footer = NULL,
              fade = F,
              size = "l",
              dataTableOutput(ns("table_trial_metadata"))
            )
          )
        })
        
        observeEvent(input$go_study_metadata_ui, {
          showModal(
            modalDialog(
              title = "Environment Metadata",
              easyClose = TRUE,
              footer = NULL,
              fade = F,
              size = "l",
              uiOutput(ns("tables_study_metadata"))
            )
          )
        })
  
        output$tables_study_metadata <- renderUI({
          req(rv$study_metadata)
  
          panels <- lapply(
            rv$study_metadata[,unique(studyDbId)],
            function(id){
              accordion_panel(
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
            what = "accordion",
            args = append(list(id = NULL, open = FALSE), panels),
            quote = F
          )
        })
      }
    }
  )
}
