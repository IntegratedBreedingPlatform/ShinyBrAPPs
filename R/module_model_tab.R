#' @import bslib
#' @export
mod_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 3, 3, 2),
      ## Select inputs ####
      pickerInput(
        ns("select_environments"), 
        label = "Select Environments", 
        multiple = TRUE, 
        choices = "", 
        width = "100%", 
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        ns("select_traits"), 
        label = "Select Traits", 
        multiple = TRUE, 
        choices = "", 
        width = "100%", 
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        ns("model_design"),
        label = actionLink(ns("model_design_metadata_button"),"Select Model Design", style ="color:inherit", icon = icon("info-circle", style = "float:right; font-size:large;margin-left:10px")),
        choices = "",
        selected = NULL, 
        multiple = F,
        options = list(
          title = "Select Model Design",
          onInitialize = I('function() { this.setValue(""); }'),
          container = "body"
        ),
        width = "100%"
      ),
      pickerInput(
        ns("model_engine"), 
        label = "Select Modelling Engine",
        choices = c("SpATS", "lme4"),
        # choices = c("SpATS", "lme4", "asreml"),
        selected = "lme4",
        width = "100%"
      )
    ),
    ## Modal with model designs ####
    # bsModal(
    #   ns("modal_model_design"), title = "Metadata for model designs", trigger = ns("model_design_metadata_button"), size = "large",
    #   dataTableOutput(ns("table_model_design_metadata"))
    # ),
    br(),
    
    ## Advanced options accordion ####
    accordion(id = ns("advanced_options"),
      open = F,
      accordion_panel(
        title = "Advanced fitting options", 
        fluidRow(
          awesomeCheckboxGroup(
            ns("what"),
            label = tooltip(
              trigger = list(
                "Genotype effect (what)",
                icon("info-circle")#, style = "float:right; font-size:large;margin-left:10px")
              ),
              "Specify whether 'genotype' should be fitted as a fixed or random effect. If not specified, both models are fitted."
            ),
            choices = c("random", "fixed"), 
            selected = c("random", "fixed"), 
            inline = T, 
            width = "300"
          ),
          
          pickerInput(
            ns("covariates"),
            label = tooltip(
              trigger = list(
                "Covariates",
                icon("info-circle")
              ),
              "Specify covariates to be fitted as extra fixed effects in the model."
            ),
            choices = NULL, 
            multiple = T
          )
        ),
        fluidRow(
          prettySwitch(
            ns("spatial_opt"),
            label = tooltip(
              trigger = list(
                "Spatial",
                icon("info-circle")
              ),
              "Should spatial models be tried? Spatial models can only be fitted with SpATS and asreml. \n
              If SpATS is used for modeling, only spatial models can be fitted and spatial is always set to TRUE. \n
              If asreml is used, fitting spatial models is optional."
            ),
            value = T
          ),
          prettySwitch(ns("display_psanova_opt"),label = "Set up PSANOVA", value = F)
        ),
        uiOutput(ns("psanova_opt"))
     )
    ),
    br(),
    ## Fit model buttons ####
    layout_columns(
      col_widths = c(2, 2, 4),
      disabled(actionButton(ns("go_fit_model"), "Fit model", class = "btn btn-info")),
      shiny::downloadButton(ns("STA_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary"),
      prettySwitch(ns("report_toc"),label = "Include TOC in report", value = TRUE)
      #hidden(shiny::actionButton(ns("go_fit_no_outlier"), "Refit without outliers", class = "btn btn-info")),
      #h4(textOutput(ns("fit_outliers_output")))
    ),
    br(),
    navset_tab(
      ## Results panel ####
      nav_panel(
        "Results",
        layout_columns(
          col_widths = c(6,6),
          div(
            # tags$h4("Metrics ~ Environment x Trait"),
            # pickerInput(ns("select_metrics_A"), "Statistics", multiple = F, choices = NULL, width = "40%", inline = T),
            downloadButton(ns("export_metrics_A"), "CSV Export", class = "btn btn-info", style = "float:right; margin:5px"),
            shiny::actionButton(
              inputId = ns("push_metrics_to_BMS_B"), 
              label = "Push BLUES/BLUPS to BMS", 
              icon = icon("leaf"), 
              class = "btn btn-primary", 
              style = "float:right; margin:5px"
            ) |>
              tooltip("You can select the traits you want to push by selecting raws in the table below"),
            dataTableOutput(ns("metrics_A_table"))
          ),
          div(
            # tags$h4("Metrics ~ Environment x Trait x Genotype"),
            downloadButton(ns("export_metrics_B"), "CSV Export", class = "btn btn-info", style = "float:right; margin:5px"),
            #disabled(shiny::actionButton(ns("push_metrics_to_BMS_B"), "Push to BMS", icon = icon("leaf"), class="btn btn-primary", style = "float:right; margin:5px")),
            #br(),
            pickerInput(ns("select_metrics_B"), "BLUPs/BLUEs", multiple = F, choices = c("BLUPs","seBLUPs","BLUEs","seBLUEs"), width = "40%", inline = T, options = list(`style` = "margin-bottom: 0;")),
            pickerInput(ns("select_environment_metrics"), "Filter by Environment", multiple = F, choices = NULL, width = "40%", inline = T, options = list(`style` = "margin-bottom: 0;")),
            dataTableOutput(ns("metrics_B_table"))
          )
        )
      ),
      ## Outliers panel ####
      nav_panel(
        "Outliers",
        fluidRow(
          column(6, pickerInput(
            ns("select_trait_outliers"),"Trait", multiple = F, choices = NULL
          )),
          column(6, actionButton(ns("mark_outliers"), "Mark all outliers as excluded observation", class = "btn btn-info", disabled = T, style = "float:right; margin:5px"))
          #,
          #column(
          #  4,
          #  sliderInput(ns("limit_residual"), label = "Threshold for standardized residuals", min = 0, max = 0, value = 0, width = "100%")
          #)
        ),
        fluidRow(
          column(
            12,
            dataTableOutput(ns("table_outliers"))
          )
        )
      ),
      ## Fitted models panel ####
      nav_panel(
        title = "Fitted models",
        layout_columns(
          col_widths = c(3, 3),
          pickerInput(
            ns("select_environment_fit"),"Environments", multiple = T, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            ns("select_trait_fit"),"Trait", multiple = F, choices = NULL, width = "100%"
          )
        ),      
        layout_columns(
          col_widths = c(4, 4, 4),
          verbatimTextOutput(ns("fit_summary")),
          plotOutput(ns("fit_residuals")),
          plotOutput(ns("fit_spatial"))
        )
      )
    )
  )
}

#' @import statgenSTA
#' @importFrom DT formatRound JS
#' @importFrom gridExtra arrangeGrob
#' @export
mod_model_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns
      
      # store blues before confirmation modal
      bluesToPush <- NULL
      methodIds <- NULL
      obs_units <- NULL
      brapir_con <- NULL

      rv_mod <- reactiveValues(
        fit = NULL,
        obsUnit_outliers = NULL
      )
      shinyjs::disable("STA_report")
      
      ## observe rv$data ####
      observeEvent(rv$data, {
        req(rv$data)
        validate(
          need(
            "observationVariableName" %in% names(rv$data),
            "There is no trait data for this study"
          )
        )
        
        choices_env <- rv$data[!is.na(study_name_app)][,unique(study_name_app)]
        updatePickerInput(
          session,"select_environments",
          choices = choices_env,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        choices_traits <- unique(rv$data[scale.dataType == "Numerical"]$observationVariableName)
        updatePickerInput(
          session,"select_traits",
          choices = choices_traits,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
        shinyjs::hide("go_fit_no_outlier")
        shinyjs::hide("fit_outliers_output")
        rv_mod$selected_env <- NULL
        rv_mod$selected_traits <- NULL
        
        brapir_con <<- brapir::brapi_connect(
          secure = rv$con$secure, 
          db = rv$con$db, 
          port = rv$con$port, 
          apipath = rv$con$apipath, 
          multicrop = rv$con$multicrop, 
          commoncropname = rv$con$commoncropname,
          token = rv$con$token
        )
        
        # Get methodDbIds 
        #TODO use /search/methodDbIds
        programDbId <- unique(rv$study_metadata$programDbId)
        methodIds <<- tryCatch({
            get_BLUES_methodsDbIds(rv$con, programDbId)
          },
          error=function(e){
            showNotification(conditionMessage(e), type = "error", duration = notification_duration)
            return(NULL)
          }
        )
        
      })
      
      ## observe select_environments_open ####
      observeEvent(input$select_environments_open, {
        req(rv$data)
        #update traits dropdown when closing environment dropdown and only if the environments selection has changed
        if (!isTRUE(input$select_environments_open) && !identical(input$select_environments, rv_mod$selected_env)) {
          rv_mod$selected_env <- input$select_environments
          shinyjs::hide("go_fit_no_outlier")
          # Update traits dropdown
          if (is.null(input$select_environments)) {            
            choices_traits <- unique(rv$data[scale.dataType == "Numerical"]$observationVariableName)            
            if (is.null(input$select_traits)) {
              selected_traits <- NULL
            } else {
              selected_traits <- input$select_traits
            }
  
          } else {
            ## only traits found in all selected environments can be selected
            trait_by_studyDbIds <- rv$data[scale.dataType == "Numerical"][study_name_app %in% input$select_environments, .(trait = unique(observationVariableName)), .(studyDbId)]
            choices_traits <- trait_by_studyDbIds[, .N, trait][N == length(trait_by_studyDbIds[, unique(studyDbId)]), trait]
            if (is.null(input$select_traits)) {
              selected_traits <- choices_traits
            } else {
              if (all(input$select_traits %in% choices_traits)) {
                selected_traits <- input$select_traits
              } else {
                selected_traits <- choices_traits
              }
            }
          }
          updatePickerInput(
            session, "select_traits",
            choices = choices_traits,
            selected = selected_traits,
            options = list(
              placeholder = 'Select 1 or more traits',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
      }, ignoreNULL = FALSE)
      
      ## observe select_environments ####
      observeEvent(input$select_environments, {
        ## restrict the design choices to the available column in the environment datasets
        # based in the following rules (https://biometris.github.io/statgenSTA/articles/statgenSTA.html#modeling-1)
        # - ibd 		    => 	subBlocks are defined
        # - res.ibd 	  => 	subBlocks and repIds are defined
        # - rcbd		    =>	repIds are defined
        # - rowcol		  =>	rowId and colId are defined
        # - res.rowcol	=>	repIds, rowId and colId are defined
        #
        # NB: choices_model_design is defined in inst/apps/stabrapp/config.R
        # For the following code to work, the item order in choices_model_design has to be: "ibd","res.ibd", "rcbd", "rowcol", "res.rowcol"
        data_filt <- rv$data[!(observationDbId %in% rv$excluded_obs) & (study_name_app %in% input$select_environments)]
        has_subBlocks <- data_filt[,.N,.(blockNumber)][,.N]>1
        has_repIds <- data_filt[,.N,.(replicate)][,.N]>1
        has_coords <- data_filt[,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1
        
        possible_designs <- choices_model_design[c(
          has_subBlocks, # matches "ibd",
          has_subBlocks & has_repIds, # matches "res.ibd"
          has_repIds, # matches  "rcbd"
          has_coords, # matches "rowcol"
          has_coords & has_repIds # matches "res.rowcol"
        )]
        
        ## set default experimental design
        design_pui <- NA
        if("experimentalDesign.pui"%in%names(rv$study_metadata)){
          design_pui <- rv$study_metadata[study_name_app %in% input$select_environments,unique(experimentalDesign.pui)]
        }
        StatGenSTA_code <- exp_designs_corresp[BMS_pui %in% design_pui, StatGenSTA_code]
        
        if(length(StatGenSTA_code)==1){
          updatePickerInput(
            session, "model_design",
            choices = possible_designs,
            selected = StatGenSTA_code
          )
        }else{
          selected_model_design <- ""
          #keep selected model design when changing environment
          if (!is.null(input$model_design)) {
            if (input$model_design %in% possible_designs) {
              selected_model_design <- input$model_design
            }
          } 
          updatePickerInput(
            session, "model_design",
            choices = possible_designs,
            selected = selected_model_design,
            options = list(
              title = "Select Model Design",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
        
        rv_mod$data_checks <- list(
          has_subBlocks = has_subBlocks,
          has_repIds = has_repIds,
          has_coords = has_coords
        )
        shinyjs::disable("STA_report")
      })
      
      ## observe select_traits_open ####
      observeEvent(c(input$select_traits_open, req(!is.null(rv$data))), {
        #update environmentq dropdown when closing traits dropdown and only if the traits selection has changed
        if (!isTRUE(input$select_traits_open) && !identical(input$select_traits, rv_mod$selected_traits)) {
          rv_mod$selected_traits <- input$select_traits
          shinyjs::hide("go_fit_no_outlier")
          # Update environments dropdown
          if (is.null(input$select_traits)) {
            req("observationVariableName"%in%names(rv$data))
            choices_env <- rv$data[,unique(study_name_app)]
            if (is.null(input$select_environments)) {
              selected_env <- NULL
            } else {
              selected_env <- input$select_environments
            }
          } else {
            ## only environment with all selected traits can be selected
            env_by_traits <- rv$data[observationVariableName %in% input$select_traits, .(env = unique(study_name_app)), .(observationVariableName)]
            choices_env <- env_by_traits[, .N, env][N == length(env_by_traits[, unique(observationVariableName)]), env]
            if (is.null(input$select_environments)) {
              selected_env <- choices_env
            } else {
              if (all(input$select_environments %in% choices_env)) {
                selected_env <- input$select_environments
              } else {
                selected_env <- choices_env
              }
            }
          }
          updatePickerInput(
            session, "select_environments",
            choices = choices_env,
            selected = selected_env,
            options = list(
              placeholder = 'Select 1 or more traits',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
          
          ## the possible covariates
          # - have to be numerical
          # - must not be some columns (like ids)
          # - can be traits
          all_traits <- rv$data[,unique(observationVariableName)]
          remaining_traits <- setdiff(all_traits, input$select_traits)
          choices_cov <- c(names(rv$data)[unlist(rv$data[,lapply(.SD, is.numeric)])], remaining_traits)
          not_cov <- c(
            "studyDbId", "trialDbId","observationDbId",
            "environment_number",
            "observationVariableDbId",
            "observationValue",
            "programDbId"
          )
          choices_cov <- choices_cov[!(choices_cov%in%not_cov)]
          updatePickerInput(
            session, "covariates", choices = choices_cov, selected = NULL
          )
        }
        shinyjs::disable("STA_report")
        
      })
      
      observeEvent(input$model_design, {
        if (input$model_design != "") {
          enable("go_fit_model")
        }
      })
      
      ## observe model_design_metadata_button ####
      observeEvent(input$model_design_metadata_button, {
        showModal(
          modalDialog(
            title = "Metadata for model designs",
            easyClose = TRUE,
            footer = NULL,
            fade = F,
            size = "l",
            dataTableOutput(ns("table_model_design_metadata"))
          )
        )
      })

      ## observe model_engine ####
      observeEvent(input$model_engine,{
        if (input$model_engine=="SpATS") {
          shinyjs::hide("spatial_opt")
          shinyjs::show("display_psanova_opt")
        }else if (input$model_engine=="lme4") {
          shinyjs::hide("spatial_opt")
          shinyjs::hide("display_psanova_opt")
        } else {
          shinyjs::show("spatial_opt")
        }
        shinyjs::disable("STA_report")
        
      })

      ## output$psanova_opt ####
      output$psanova_opt <- renderUI({ ## based on RAPWeb code
        req(input$select_environments, rv$TD, input$model_engine == "SpATS", input$display_psanova_opt==T)

        ## Variable selection. Cov vars have to be a factor column.
        nRows <- min(sapply(X = input$select_environments, FUN = function(trial) {
          nlevels(droplevels(rv$TD[[trial]]$rowId))
        }))
        nCols <- min(sapply(X = input$select_environments, FUN = function(trial) {
          nlevels(droplevels(rv$TD[[trial]]$colId))
        }))
        tagList(
          sliderInput(inputId = ns("spRowSeg"),
                      label = "Number of row segments in PSANOVA",
                      value = floor(nRows / 2), min = 1, max = nRows, step = 1),
          sliderInput(inputId = ns("spColSeg"),
                      label = "Number of column segments in PSANOVA",
                      value = floor(nCols / 2), min = 1, max = nCols, step = 1),
          sliderInput(inputId = ns("spNestDiv"),
                      label = "Divisor of segments in PSANOVA",
                      value = 2, min = 1, max = 5, step = 1)
        )
      })

      ## output$table_model_design_metadata ####
      output$table_model_design_metadata <- renderDT({
        req(input$select_environments)
        req(rv$study_metadata)
        if("experimentalDesign.pui"%in%names(rv$study_metadata)){
          designs <- rv$study_metadata[study_name_app%in%input$select_environments,.(experimentalDesign.pui = unique(experimentalDesign.pui), experimentalDesign.description = unique(experimentalDesign.description)),study_name_app]
        }else{
          designs <- rv$study_metadata[study_name_app%in%input$select_environments,.(unique(study_name_app))]
          designs[,experimentalDesign.pui:=NA]
          designs[,experimentalDesign.description:=NA]
        }
        datatable(
          designs,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            # scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })

      ## observe go_fit_model ####
      observeEvent(input$go_fit_model,{
        req(rv_mod$data_checks)
        ## create TD without the excluded observations
        ## exclude observations
        rv$data[,observationValue:=as.numeric(observationValue)]
        data_filtered <- rv$data[!(observationDbId %in% rv$excluded_obs$observationDbId)]
        rv_mod$fitted_data <- data_filtered
        fitModel(data_filtered)
        enable("push_metrics_to_BMS_B")
        enable("STA_report")
      })
      
      ## observe button mark outliers ####
      observeEvent(input$mark_outliers, {
        new_excluded_obs <-  rv$data[observationDbId %in% rv_mod$obs_outliers, .(observationDbId)]
        new_excluded_obs[, reason := "model outlier"]
        rv$excluded_obs <- rbind(rv$excluded_obs, new_excluded_obs)
      })
      observeEvent(c(rv$excluded_obs,input$model_design),{
        shinyjs::disable("STA_report")
      })
      ## observe go_fit_no_outlier ####
      # observeEvent(input$go_fit_no_outlier,{
      #   req(rv_mod$data_checks)
      #   req(rv_mod$obsUnit_outliers)
      #   ## create TD without the excluded observations
      #   ## exclude observations
      #   rv$data[,observationValue:=as.numeric(observationValue)]
      #   data_filtered <- rv$data[!(observationDbId %in% rv$excluded_obs)]
      #   data_filtered <- data_filtered[!(observationUnitDbId %in% rv_mod$obsUnit_outliers)]
      #   rv_mod$fitted_data <- data_filtered
      #   fitModel(data_filtered)
      # })
      
      fitModel <- function(data_filtered) {
        
        ## parametrization
        createTD_args <- list(
          genotype = "genotype",
          trial = "trial",
          loc = "loc"
        )
        
        data <- data_filtered[,.(
          observationUnitDbId,
          genotype = germplasmName,
          trial = study_name_app, 
          loc = locationDbId,
          observationVariableName, 
          observationValue
        )]
        
        formula <- "observationUnitDbId + genotype + trial + loc"
        
        if(rv_mod$data_checks$has_subBlocks){
          createTD_args$subBlock <- "subBlock"
          data[,subBlock := data_filtered$blockNumber]
          formula <- paste0(formula, " + subBlock")
        }
        if(rv_mod$data_checks$has_repIds){
          createTD_args$repId <- "repId"
          data[,repId := data_filtered$replicate]
          formula <- paste0(formula, " + repId")
        }
        if(rv_mod$data_checks$has_coords){
          createTD_args$rowCoord <- "rowCoord"
          createTD_args$colCoord <- "colCoord"
          data[,rowCoord := data_filtered$positionCoordinateY]
          data[,colCoord := data_filtered$positionCoordinateX]
          formula <- paste0(formula, " + rowCoord + colCoord")
        }
        
        formula <- paste0(formula, " ~ observationVariableName")
        
        ## make 1 column per trait
        data_filtered_casted <- dcast(
          data = data,
          formula = formula,
          value.var = "observationValue"
        )
        
        createTD_args <- c(
          list(data = data_filtered_casted),
          createTD_args
        )
        
        ## create TD
        rv$TD <- do.call(what = createTD, args = createTD_args)
        rv$TD <- rv$TD
        
        
        
        if (input$model_engine == "SpATS" && input$display_psanova_opt==T) {
          cntrl <- list(nSeg = c(input$spColSeg, input$spRowSeg),
                        nestDiv = input$spNestDiv)
        } else {
          cntrl <- NULL
        }
        
        ### fit TD
        a <- tryCatch({
          withProgress(message = "Fitting model", min=1, max=1, {
            rv_mod$fit <- fitTD(
              TD = rv$TD,
              trials = input$select_environments,
              design = input$model_design,
              traits = input$select_traits,
              engine = input$model_engine,
              covariates = input$covariates,
              what = input$what,
              # useCheckId = FALSE,
              spatial = ifelse(input$model_engine=="SpATS", input$spatial_opt, F),
              control = cntrl
            )})
          withProgress(message = "Extracting statistics from fitted models", min=1, max=1,{ 
            rv_mod$fitextr <- extractSTA(rv_mod$fit)
          })
          withProgress(message = "Looking for outliers", min=1, max=1,{
            rv_mod$outliers <-  outlierSTA(rv_mod$fit, 
                                       what = "random",
                                       commonFactors = "genotype")
          })
          },
          error=function(e){ e })
        mess <- a$message
        if(!is.null(mess)){
          showNotification(mess, type = "error", duration = notification_duration)
        }
        
        req(rv_mod$fit)
        
        ## SPATs does not make prediction when genotypes are in the fixed part of the model
        # It causes the summary.TD and plot.TD functions to throw error when trying to compute the predictions
        # temporary fix: if there is no "fixed" modelling, then this list item is removed from the fitTD object
        # example: ?cropDb=rice&token=jhjlkj&apiURL=https://test-server.brapi.org/&studyDbIds=2705,2706
        for(trial in names(rv_mod$fit)){
          if(all(unlist(lapply(rv_mod$fit[[trial]]$mFix, is.null)))){
            rv_mod$fit[[trial]][["mFix"]] <- NULL
            showNotification(paste0(trial,':\nno modelling for what=fixed'), type = "default", duration = notification_duration)
          }
        }
        
        ## update selectors
        updatePickerInput(
          session, "select_environment_fit",
          choices = input$select_environments,
          selected = input$select_environments
        )
        updatePickerInput(
          session, "select_trait_fit",
          choices = input$select_traits,
          selected = input$select_traits[1]
        )
        updatePickerInput(
          session, "select_trait_outliers",
          choices = input$select_traits,
          selected = input$select_traits[1]
        )
        
        # if(input$model_engine%in%c("lme4")){
        #   choices_metrics_A <- c("Wald"="wald", "Heritability"="heritability", "CV"="CV")
        # }else{
        #   choices_metrics_A <- c("Heritability"="heritability")
        # }
        # updatePickerInput(
        #   session, "select_metrics_A",
        #   choices = choices_metrics_A
        # )
        
        updatePickerInput(
          session,"select_trait_metrics",
          choices = input$select_traits
          # selected = input$select_traits[1]
        )
        updatePickerInput(
          session, "select_environment_metrics",
          choices = input$select_environments
        )
        
        # assign input$model_engine to a reactive variable (isolated in the "observeEvent-go fit model") to prevent the app from changing model results unless "go fit model" is clicked
        rv_mod$model_engine <- input$model_engine
      }

      ## show fitted models per trait and environments ####
      observeEvent(c(input$select_environment_fit, input$select_trait_fit),{
        req(input$select_environment_fit)
        req(input$select_trait_fit)
        req(rv_mod$fit)
          output$fit_summary <- renderPrint({
            # s_all <- summary(
            #   rv_mod$fit,
            #   trait = input$select_trait_fit,
            #   trials = input$select_environment_fit
            # )
            s <- lapply(input$select_environment_fit, function(env){
              summary(
                rv_mod$fit,
                trait = input$select_trait_fit,
                trials = env
              )
            })
            names(s) <- input$select_environment_fit
            # s["all environments"] <- s_all
            s
          })
          #browser()
          output$fit_residuals <- renderPlot({
            req(rv_mod$fit)
            req(input$select_environment_fit)
            plots_envs <- lapply(input$select_environment_fit, function(trial){
              plot(
                rv_mod$fit,
                traits = input$select_trait_fit,
                trials = trial,
                output = F
                # output = F,
                # what = c("random","fixed")[c(
                #   !is.null(rv_mod$fit[[trial]]$mRand),
                #   !is.null(rv_mod$fit[[trial]]$mFixed)
                # )]
              )
            })
            
            plot_envs <- lapply(1:length(input$select_environment_fit), function(k){
              do.call("arrangeGrob",
                      c(
                        plots_envs[[k]][[input$select_environment_fit[k]]][[input$select_trait_fit]],
                        ncol=2,
                        top = paste0("Trial: ", input$select_environment_fit[k],
                                     "\nTrait: ", input$select_trait_fit)
                      )
              )
            })
            plot_multi_env <- do.call("arrangeGrob", c(plot_envs, ncol=1))
            plot(plot_multi_env)
          },
          height=length(input$select_environment_fit)*500
          )
          
          output$fit_spatial <- renderPlot({
            req(rv_mod$fit)
            isolate(req(rv_mod$data_checks$has_coords))
            req(input$select_environment_fit)
            plots_envs <- lapply(input$select_environment_fit, function(trial){
              if(rv$data[observationVariableName == input$select_trait_fit & study_name_app == trial,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1){
                p <- plot(
                  rv_mod$fit,
                  plotType = "spatial",
                  traits = input$select_trait_fit,
                  trials = trial,
                  output = F
                  # output = F,
                  # what = c("random","fixed")[c(
                  #   !is.null(rv_mod$fit[[trial]]$mRand),
                  #   !is.null(rv_mod$fit[[trial]]$mFixed)
                  # )]
                )
                p
              }else{
                a_STATgen_like_list <- list()
                a_STATgen_like_list[[trial]][[input$select_trait_fit]][["p1"]] <- ggplot() + geom_text(aes(x = 0, y = 0), label = "no spatial data") + theme_void()
                a_STATgen_like_list
              }
            })
            plot_envs <- lapply(1:length(input$select_environment_fit), function(k){
              do.call("arrangeGrob",
                      c(
                        plots_envs[[k]][[input$select_environment_fit[k]]][[input$select_trait_fit]],
                        ncol=2,
                        top = paste0("Trial: ", input$select_environment_fit[k],
                                     "\nTrait: ", input$select_trait_fit)
                      )
              )
            })
            plot_multi_env <- do.call("arrangeGrob", c(plot_envs, ncol=1))
            plot(plot_multi_env)
          },
          height=length(input$select_environment_fit)*500
          )          

      })

      ## output$table_outliers ####
      output$table_outliers <- renderDataTable({
        req(rv_mod$fit)
        req(input$select_trait_outliers)
        req(rv_mod$outliers)
        outliers_all <- as.data.table(rv_mod$outliers$outliers) #as.data.table(outliersSTA_all$outliers)
        req(outliers_all[,.N] > 0)
        
        updateActionButton(
          inputId = "mark_outliers",
          disabled = F
        )
        
        # outliers for the selected trait
        outliers <- outliers_all[trait == input$select_trait_outliers]
        
        validate(need(outliers[,.N]>0 , "No outlier on this trait"))
        
        req(rv_mod$fitted_data)

        which(colnames(outliers_all) == "subBlock")
        outliers_nb <- outliers_all[, .(`#outliers` = sum(outlier)), by = observationUnitDbId]
        
        variables_index <- which(colnames(outliers_all) %in% unique(rv_mod$fitted_data$observationVariableName))
        first_var_index <- variables_index[1]
        last_var_index <- variables_index[length(variables_index)]

        obs_nb <- unique(outliers_all[, .(observationUnitDbId,  `#observations` = rowSums(!is.na(.SD[, (first_var_index):(last_var_index)])))])
        outliers_by_obsUnit <- merge(outliers_nb, obs_nb, by="observationUnitDbId")   
        
        rv$obsUnit_outliers <- outliers_by_obsUnit[`#outliers` > 0]$observationUnitDbId
        shinyjs::show("go_fit_no_outlier")
        shinyjs::show("fit_outliers_output")
        
        rv_mod$obs_outliers <- merge(
          as.data.table(rv_mod$outliers$outliers)[outlier == T,], 
          rv$data, 
          by.x = c("trial", "observationUnitDbId", "trait"), 
          by.y = c("study_name_app", "observationUnitDbId", "observationVariableName")
          )[, observationDbId]
        
        if(outliers[,.N]>0){
          setnames(outliers, "trial", "environment")
        }

        outliers <- merge(outliers, outliers_by_obsUnit, by="observationUnitDbId")
        
        #Sort outliers on genotype, environment and repetition
        outliers <- outliers[order(genotype, environment, repId)]
        
        # change columns order (move variables columns at the end)
        cols <- colnames(outliers)
        first_var_index <- which(colnames(outliers_all) %in% unique(rv_mod$fitted_data$observationVariableName))[1]
        outlier_index <- which(cols == "outlier")
        new_cols_order <- c(cols[1:first_var_index-1], cols[outlier_index:length(cols)], cols[(first_var_index):(outlier_index-1)])
        setcolorder(outliers, new_cols_order)
        
        # set a color for each environment/genotype couple
        # 2 colors depending on even or odd row
        group_colors <- unique(outliers[ , .(environment, genotype)])
        group_colors <- lapply(seq_len(nrow(group_colors)), function(i) {
          if (i %% 2 == 0) {
            color = "'#d9edf7'"  
          } else {
            color = "'#e3e3e3'"  
          }
          return(paste0("'", paste0(group_colors[i, environment], "|", group_colors[i, genotype]),"':", color))
        })
        group_colors <- paste(group_colors, collapse = ",")
        
        genotype_col_index = which(colnames(outliers) == "genotype") - 1
        env_col_index = which(colnames(outliers) == "environment") - 1
        outlier_col_index = which(colnames(outliers) == "outlier") - 1
        dec_cols <- names(which(apply(data.frame(outliers)[,which(!apply(outliers,2,function(a) all(is.na(as.numeric(a)))))],2,function(a) sum(abs(round(as.numeric(a),0)-as.numeric(a)),na.rm = T))>0))
        formatRound(datatable(
          outliers,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't',
            rowCallback = JS(
              sprintf(
                "function(row, data) {
                  var groups = {%s};
                  $('td', row).css('background-color', groups[data[%i].concat('|', data[%i])]);
                  if (data[%i]) {
                    $('td', row).css('font-weight', 'bold');
                  }
                }",
                group_colors,
                env_col_index,
                genotype_col_index,
                outlier_col_index
              )
            )
          )
        ),digits = 2, columns = dec_cols)
      })

      ## output$metrics_A_table ####
      output$metrics_A_table <- renderDT({
        # req(rv_mod$fit)
        # req(rv_mod$fitextr)

        validate(
          need(rv_mod$fitextr, "you must fit a model")
        )
        
        ## heritability
        allex <- rv_mod$fitextr
        if(rv_mod$model_engine%in%c("lme4")){
          ### CV
          metrics <- rbindlist(Map(function(f,t) data.table(Environment=t,Trait=names(f$heritability),Heritability=f$heritability, CV=f$CV, `Wald p.value`=unlist(lapply(f$wald,function(a) a$`p.value`))),allex, names(allex)))
          
        }else{
          metrics <- rbindlist(Map(function(f,t) data.table(Environment=t,Trait=names(f$heritability),Heritability=f$heritability),allex, names(allex)))
          
        }
        rv_mod$metrics_A <- metrics

        setkey(metrics, "Environment")
        dtable <- formatRound(datatable(
          metrics,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't',
            rowsGroup = as.list(c(0))
          )),columns=names(which(unlist(lapply(metrics,is.numeric))==TRUE)), digits=2)
        path <- "www/js/datatables-rowsgroup/"
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0",
          path, script = "dataTables.rowsGroup.js"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      })

      ## output$metrics_B_table ####
      output$metrics_B_table <- renderDataTable({
        req(input$select_metrics_B)
        req(input$select_environment_metrics)
        req(rv_mod$fitextr)
        #browser()
        #metrics_table <- as.data.table(extractSTA(STA = rv_mod$fit, what = input$select_metrics_B))
        metrics_table <- as.data.table(rv_mod$fitextr[[input$select_environment_metrics]][[input$select_metrics_B]])
        
        entry_types <- unique(rv$data[,.(genotype=germplasmName, entryType)])
        setkey(metrics_table, genotype)
        setkey(entry_types, genotype)
        metrics_table <- entry_types[metrics_table]
        #setnames(metrics_table, "trial", "environment")

        #metrics_table_filt <- metrics_table[environment==input$select_environment_metrics, -c("environment"), with = F]
        metrics_table_filt <- metrics_table
        
        # selected_rows <- input$metrics_A_table_rows_selected
        # print("selected rows: ")
        # print(selected_rows)
        # traits <- rv_mod$metrics_A[selected_rows]$Trait
        # colnames(metrics_table_filt)
        # selected <- match(traits, colnames(metrics_table_filt)) - 1
        # print("selected columns: ")
        # print(selected)
        dtable <- formatRound(datatable(
          metrics_table_filt,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ),
          #selection = list(mode = 'multiple', selected = selected, target = 'column', selectable = T)
        ), columns = colnames(metrics_table_filt)[!colnames(metrics_table_filt)%in%c("genotype","entryType")],
        digits = 2)
        path <- "www/js/datatables-rowsgroup/"
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0",
          path, script = "dataTables.rowsGroup.js"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      })

      ## output$export_metrics_A ####
      output$export_metrics_A <- downloadHandler(
        filename = function() {"statistics.csv"},
        content = function(file) {
          req(rv_mod$metrics_A)
          metrics <- melt(rv_mod$metrics_A, id.vars = c("Environment", "Trait"), variable.name = "statistics")
          setnames(metrics, "Environment", "environment")
          setnames(metrics, "Trait", "trait")
          write.csv(metrics, file, row.names = FALSE)
        }
      )
      
      extract_all_BLUEs_BLUPs <- reactive({
        req(rv_mod$fitextr)
        
        table_metrics <- NULL
        for (i in 1:length(rv_mod$fitextr)) {
          for (j in c("BLUEs", "seBLUEs","BLUPs", "seBLUPs")) {
            dt <- as.data.table(rv_mod$fitextr[[i]][[j]])[, environment:=names(rv_mod$fitextr)[i]][,result:=j]
            if (is.null(table_metrics)) {
              table_metrics <- dt
            } else {
              table_metrics <- rbindlist(list(table_metrics, dt))
            }
          }
        }

        table_metrics <- melt(
          data = table_metrics,
          measure.vars = names(table_metrics)[!(names(table_metrics)%in%c("genotype","environment","result"))],
          variable.name = "trait"
        )
        
        # table_metrics <- table_metrics[, genotype:=as.character(genotype)]
        # table_metrics <- table_metrics[, trait:=as.character(trait)]
        
        return(table_metrics)
      })

      ## output$export_metrics_B ####
      output$export_metrics_B <- downloadHandler(
        filename = function() {"BLUPs_and_BLUEs.csv"},
        content = function(file) {
          withProgress(message = "Generating csv", min=1, max=1, {
            req(rv_mod$fitextr)
            
            table_metrics <- NULL
            for (i in 1:length(rv_mod$fitextr)) {
              dt_BLUPs <- as.data.table(rv_mod$fitextr[[i]][["BLUPs"]])
              dt_seBLUPs <- as.data.table(rv_mod$fitextr[[i]][["seBLUPs"]])
              dt_BLUEs <- as.data.table(rv_mod$fitextr[[i]][["BLUEs"]])
              dt_seBLUEs <- as.data.table(rv_mod$fitextr[[i]][["seBLUEs"]])
              for (j in 2:length(colnames(dt_BLUPs))) {
                colnames(dt_BLUPs)[j] <- paste0(colnames(dt_BLUPs)[j], "_BLUPs")
                colnames(dt_seBLUPs)[j] <- paste0(colnames(dt_seBLUPs)[j], "_seBLUPs")
                colnames(dt_BLUEs)[j] <- paste0(colnames(dt_BLUEs)[j], "_BLUEs")
                colnames(dt_seBLUEs)[j] <- paste0(colnames(dt_seBLUEs)[j], "_seBLUEs")
              }

              table_metrics_env <- merge(dt_BLUPs, dt_seBLUPs, by=c("genotype"))
              table_metrics_env <- merge(table_metrics_env, dt_BLUEs, by=c("genotype"))
              table_metrics_env <- merge(table_metrics_env, dt_seBLUEs, by=c("genotype"))
              
              #add environment column
              table_metrics_env[, environment:=names(rv_mod$fitextr)[i]]

              #concatenate table_metrics for each environment
              if (is.null(table_metrics_env)) {
                table_metrics <- table_metrics_env
              } else {
                table_metrics <- rbindlist(list(table_metrics, table_metrics_env))
              }
            }
            
            #change col name genotype
            setnames(table_metrics, "genotype", "germplasm")
            
            #put environment column in first position (environment, germplasm, var1_BLUPs, var2_BLUPs,var1_seBLUPs, var2_seBLUPs, etc)
            cols <- colnames(table_metrics)
            new_col_order <- append(c(cols[length(table_metrics)], cols[1]), cols[2:(length(cols)-1)]) 
            table_metrics <- setcolorder(table_metrics, new_col_order)
            
            write.csv(table_metrics, file, row.names = FALSE)
          })
        }
      )
      
      ## observe  push_metrics_to_BMS_A####
      observeEvent(input$push_metrics_to_BMS_A,{
        print("push metrics!")
      })
      
      ### PUSH METRICS_B TO BMS
      pushModal <- function() {
        modalDialog(
          title = "Confirmation",
          "The heritability is 0 for some traits and environment. Are you sure you still want to push all BLUEs/BLUPs ? You can select for which traits end environments you want to push BLUEs/BLUPs by clicking on lines in the statistics table",
          footer = tagList(
            modalButton("Cancel"),
            shiny::actionButton(ns("ok"), "Push BLUEs/BLUPs anyway", class = "btn btn-primary")
          ),
          fade = F
        )
      }
      
      ## observe button OK in push modal####
      observeEvent(input$ok, {
        removeModal()
        pushBlues()
      })

      ## observe push_metrics_to_BMS_B####
      observeEvent(input$push_metrics_to_BMS_B,{
        # check if heritability > 0 before pushing
        if (!is.null(input$metrics_A_table_rows_selected)) {
          selected_rows <- input$metrics_A_table_rows_selected
          heritabilities <- rv_mod$metrics_A[selected_rows]$Heritability
        } else {
          heritabilities <- rv_mod$metrics_A$Heritability
          selected_traits <- NULL
        }
        
        if (all(heritabilities>0)) {
          pushBlues()
        } else {
          showModal(pushModal())
        }
      })
      
      existingBluesModal <- function() {
        modalDialog(
          title = "Confirmation",
          "Some BLUEs/BLUPs have already been pushed to BMS. They will be erased by these new values. Are you sure you still want to push those BLUEs/BLUPs ?",
          footer = tagList(
            modalButton("Cancel"),
            shiny::actionButton(ns("push_erase_ok"), "Push BLUEs/BLUPs anyway", class = "btn btn-primary")
          ),
          fade = F
        )
      }
      
      ## observe button OK in existingBluesModal####
      observeEvent(input$push_erase_ok, {
        removeModal()
        postObservations()
      })
      
      postObservations <- function() {  
        req(bluesToPush)
        tryCatch({
          origin_variable_names <- unique(bluesToPush[,originVariableName])
          methods <- data.table(result = names(methodIds), methodDbId = unname(unlist(methodIds)))
        
          ## push BLUES per variable ####
          withProgress(message = "Pushing BLUES/BLUPS for", value = 0, {
            for (i in 1:length(origin_variable_names)) {
              var_name <- as.character(origin_variable_names[i])
              print(var_name)
              incProgress(1/length(origin_variable_names), detail = var_name)
              variable <- unique(rv$data[observationVariableName==var_name, .(observationVariableDbId)])
              variableDbId <- as.character(variable[1, observationVariableDbId])
              #filter table_metrics on variable
              data_to_push <- bluesToPush[originVariableName==var_name,]
              
              ## push missing variables ####
              # Get variable scale and trait
              resp_variable <- brapir::phenotyping_variables_get(con = brapir_con, observationVariableDbId = variableDbId)
              
              if (resp_variable$status_code != 200) {
                showNotification(paste0("An error occured"), type = "error", duration = notification_duration)
                return(NULL)
              } else {
                # Checking if relative BLUES/BLUPS variables already exist
                # which means looking for variables with the same scaleDbId, traitDbId but with BLUEs/BLUPs methodDbIds 
                scaleDbId <- resp_variable$data$scale.scaleDbId #"6085"
                variableName <- resp_variable$data$observationVariableName
                traitDbId <-  resp_variable$data$trait.traitDbId #"20454"
                #variableDbId <- origin_variables$originVariableDbId[i]
                
                resp_search_variables <- brapir::phenotyping_variables_post_search(
                  con = brapir_con,
                  methodDbIds = c(methodIds$BLUEs, methodIds$BLUPs, methodIds$seBLUEs, methodIds$seBLUPs),
                  scaleDbIds = scaleDbId,
                  traitDbIds = traitDbId
                )
                if (resp_search_variables$status_code == 200 | resp_search_variables$status_code == 202) {
                  resp_get_search_variables <- brapir::phenotyping_variables_get_search_searchResultsDbId(
                    con = brapir_con,
                    searchResultsDbId = resp_search_variables$data$searchResultsDbId
                  )
                  if (resp_get_search_variables$status_code == 200) {
                    existing_variables <- NULL
                    if (resp_get_search_variables$metadata$pagination$totalCount > 0) {
                      existing_variables <- data.table(resp_get_search_variables$data)[,.(observationVariableName, observationVariableDbId, 
                                                                                          methodDbId = method.methodDbId, scaleDbId = scale.scaleDbId,
                                                                                          traitDbId = trait.traitDbId, originVariableDbId = variableDbId,
                                                                                          originVariableName = variableName)]
                      #[,result := names(methodIds)[which(unlist(methodIds) == methodDbId)]]
                      existing_variables <- merge(existing_variables, methods, by="methodDbId")
                      missing_methods <- unlist(methodIds)[!(unlist(methodIds) %in% existing_variables$methodDbId)]
                    } else {
                      missing_methods <- unlist(methodIds)
                    }
                    
                    if (length(missing_methods) > 0) {
                      #some variables are missing
                      missing_variables_dt <- data.table(
                        methodDbId = unname(missing_methods),
                        methodName = names(missing_methods)
                      )[, observationVariableName := paste0(variableName, "_", methodName)
                      ][, contextOfUse := "MEANS"
                      ][, scaleDbId := scaleDbId
                      ][, traitDbId := traitDbId]
                      
                      # Create missing variables
                      if (nrow(missing_variables_dt) > 0) {
                        print("Creating missing BLUES/BLUPS variables")
                        print(paste0("Creating ", nrow(missing_variables_dt) , " new variables"))
                        print(missing_variables_dt$observationVariableName)
                        
                        body <- apply(missing_variables_dt,1,function(a){
                          list(
                            contextOfUse = c("MEANS"),
                            method = list(methodDbId = jsonlite::unbox(a["methodDbId"])),
                            observationVariableName = jsonlite::unbox(a["observationVariableName"]),
                            scale = list(scaleDbId = jsonlite::unbox(a["scaleDbId"])),
                            trait = list(traitDbId = jsonlite::unbox(a["traitDbId"]))
                          )
                        })
                        
                        resp_post_variables <- brapir::phenotyping_variables_post_batch(con = brapir_con, data = body)
                        
                        if (resp_post_variables$status_code == 200) {
                          created_variables_dt <- data.table(resp_post_variables$data)[
                            , .(observationVariableDbId, observationVariableName,
                                methodDbId = method.methodDbId,
                                scaleDbId = scale.scaleDbId,
                                traitDbId = trait.traitDbId,
                                originVariableName = variableName,
                                originVariableDbId = variableDbId
                            )]
                          created_variables_dt <- merge(created_variables_dt, methods, by="methodDbId")
                          
                          print("Created variables:")
                          print(created_variables_dt)
                          # Add new variables to the existing variables
                          if (is.null(existing_variables)) {
                            existing_variables <- created_variables_dt
                          } else {
                            existing_variables <- rbind(existing_variables, created_variables_dt)
                          }
                        }
                        print("All variables:")
                        print(existing_variables)
                      }
                    }
                    
                    # add variableDbIds to data table
                    data_to_push <- merge(data_to_push, existing_variables, by=c("originVariableName","result"))
                    
                    ## push observations ####
                    print("Posting observations")
                    data_to_push$studyDbId = as.character(data_to_push$studyDbId)
                    data_to_push <- merge(data_to_push, obs_units, by=c("germplasmDbId","studyDbId"))
                    
                    # Building body POST request
                    body <- apply(data_to_push,1,function(a){
                      list(
                        germplasmDbId = jsonlite::unbox(as.character(a["germplasmDbId"])),
                        observationUnitDbId = jsonlite::unbox(as.character(a["observationUnitDbId"])),
                        studyDbId = jsonlite::unbox(as.character(a["studyDbId"])),
                        observationVariableDbId = jsonlite::unbox(as.character(a["observationVariableDbId"])),
                        value = jsonlite::unbox(as.numeric(a["value"]))
                      )
                    })
                    
                    resp <- brapir::phenotyping_observations_post_batch(con = brapir_con, data = body)
                    if (resp$status_code == 200) {
                      created_observations_df <- resp$data
                      showNotification(paste0(var_name, " BLUES/BLUPS were pushed to BMS (",nrow(created_observations_df), " data)"), type = "message", duration = notification_duration)
                    } else {
                      showNotification(paste0("An error occured while creating BLUES/BLUPS observations for ", var_name), type = "error", duration = notification_duration)
                      showNotification(paste0(resp$metadata), type = "error", duration = notification_duration)
                    }
                  }
                }
              }
            }
          })
        },
        error = function(e) {
          showNotification(paste0("An error occured: ", e), type = "error", duration = notification_duration)
          print(e)
          return(NULL)
        })
      }
        
      pushBlues <- function() {
        tryCatch({
          #Get all BLUEs/BLUPs data
          table_metrics <- extract_all_BLUEs_BLUPs()
          #filter traits and env to push
          if (!is.null(input$metrics_A_table_rows_selected)) {
            selected_rows <- input$metrics_A_table_rows_selected
            all_lines_to_keep <- data.table(matrix(nrow = 0, ncol = ncol(table_metrics)))
            colnames(all_lines_to_keep) <- colnames(table_metrics)
            for (i in 1:length(selected_rows)) {
              row_trait <- rv_mod$metrics_A[selected_rows[i]]$Trait
              row_env <- rv_mod$metrics_A[selected_rows[i]]$Environment
              lines_to_keep <- table_metrics[trait == row_trait & environment == row_env]
              all_lines_to_keep <- rbind(all_lines_to_keep, lines_to_keep)
            }
            table_metrics <- all_lines_to_keep
          }
          
          print("PUSH BLUEs/BLUPs")
          colnames(table_metrics) = c("germplasmName", "environment", "result", "originVariableName", "value")
          bluesToPush <<- table_metrics
          
          #exit the function if missing method ids
          if (is.null(methodIds)) {
            return(NULL)
          }
          
          methods <- data.table(result = names(methodIds), methodDbId = unname(unlist(methodIds)))
          
          # Get the ids of variables that were used in the model
          origin_variable_names <- unique(bluesToPush[,originVariableName])

          ## push blues variables ####
          # Get variables ids from data
          variables <- unique(rv$data[observationVariableName %in% origin_variable_names, .(observationVariableDbId)])
          
          # Search variables on ids to get scale and trait
          resp <- brapir::phenotyping_variables_post_search(
            con = rv$con,
            observationVariableDbIds = as.character(variables$observationVariableDbId)
          )
          if (resp$status_code == 200 | resp$status_code == 202) {
            if ("searchResultsDbId" %in% names(resp$data)) {
              searchResultsDbId = resp$data$searchResultsDbId
              resp2 <- brapir::phenotyping_variables_get_search_searchResultsDbId(con = brapir_con, searchResultsDbId = searchResultsDbId)
              origin_variables = data.table(resp2$data)
            } else {
              origin_variables = data.table(resp$data)
            }
          }
          origin_variables <- unique(origin_variables[
            ,.(
              originVariableDbId = observationVariableDbId,
              originVariableName = observationVariableName, 
              traitDbId = `trait.traitDbId`, 
              scaleDbId = `scale.scaleDbId`
            )
          ])
          
          print("origin_variables:")
          print(origin_variables)
          
          # Checking if BLUES/BLUPS variables already exist
          print("Checking if BLUES/BLUPS variables already exist")
          missing_variables_df <- data.frame(matrix(nrow = 0, ncol = 5))
          metrics_variables_df <- data.frame(matrix(nrow = 0, ncol = 8))
          # 
          # for each origin variable, we are looking for corresponding BLUPs/BLUEs variables
          # which means looking for variables with the same scaleDbId, traitDbId but with BLUEs/BLUPs methodDbIds 
          missing_variables_list <- list()
          existing_variables_list <- list()
          for (i in 1:nrow(origin_variables)) {
            # Checking if relative BLUES/BLUPS variables already exist
            # which means looking for variables with the same scaleDbId, traitDbId but with BLUEs/BLUPs methodDbIds 
            scaleDbId <- origin_variables$scaleDbId[i] #"6085"
            variableName <- origin_variables$originVariableDbId[i]
            traitDbId <-  origin_variables$traitDbId[i] #"20454"
            #variableDbId <- origin_variables$originVariableDbId[i]
            
            resp_search_variables <- brapir::phenotyping_variables_post_search(
              con = brapir_con,
              methodDbIds = c(methodIds$BLUEs, methodIds$BLUPs, methodIds$seBLUEs, methodIds$seBLUPs),
              scaleDbIds = scaleDbId,
              traitDbIds = traitDbId
            )
            if (resp_search_variables$status_code == 200 | resp_search_variables$status_code == 202) {
              resp_get_search_variables <- brapir::phenotyping_variables_get_search_searchResultsDbId(
                con = brapir_con,
                searchResultsDbId = resp_search_variables$data$searchResultsDbId
              )
              if (resp_get_search_variables$status_code == 200) {
                existing_variables <- NULL
                if (resp_get_search_variables$metadata$pagination$totalCount > 0) {
                  existing_variables <- data.table(resp_get_search_variables$data)[,.(observationVariableName, observationVariableDbId, 
                                                                                      methodDbId = method.methodDbId, scaleDbId = scale.scaleDbId,
                                                                                      traitDbId = trait.traitDbId, originVariableDbId = observationVariableDbId,
                                                                                      originVariableName = observationVariableName)]
                  #[,result := names(methodIds)[which(unlist(methodIds) == methodDbId)]]
                  existing_variables <- merge(existing_variables, methods, by="methodDbId")
                  missing_methods <- unlist(methodIds)[!(unlist(methodIds) %in% existing_variables$methodDbId)]
                } else {
                  missing_methods <- unlist(methodIds)
                }
                
                if (length(missing_methods) > 0) {
                  #some variables are missing
                  missing_variables_dt <- data.table(
                    methodDbId = unname(missing_methods),
                    methodName = names(missing_methods)
                  )[, observationVariableName := paste0(origin_variables$originVariableName[i], "_", methodName)
                  ][, contextOfUse := "MEANS"
                  ][, scaleDbId := scaleDbId
                  ][, traitDbId := traitDbId]
                  
                  missing_variables_list <- append(missing_variables_list, list(missing_variables_dt))
                }
                
                if (!is.null(existing_variables)) {
                  existing_variables_list <- append(existing_variables_list, list(existing_variables))
                }
              }
            }
          }
          
          # Create missing variables
          if (length(missing_variables_list) > 0) {
            missing_variables <- rbindlist(missing_variables_list)
            print("Creating missing BLUES/BLUPS variables")
            print(paste0("Creating ", nrow(missing_variables) , " new variables"))
            print(missing_variables$observationVariableName)
            
            body <- apply(missing_variables,1,function(a){
              list(
                contextOfUse = c("MEANS"),
                method = list(methodDbId = jsonlite::unbox(a["methodDbId"])),
                observationVariableName = jsonlite::unbox(a["observationVariableName"]),
                scale = list(scaleDbId = jsonlite::unbox(a["scaleDbId"])),
                trait = list(traitDbId = jsonlite::unbox(a["traitDbId"]))
              )
            })
            
            resp_post_variables <- brapir::phenotyping_variables_post_batch(con = brapir_con, data = body)
            
            if (resp_post_variables$status_code == 200) {
              created_variables_dt <- data.table(resp_post_variables$data)[
                , .(observationVariableDbId, observationVariableName,
                    methodDbId = method.methodDbId,
                    scaleDbId = scale.scaleDbId,
                    traitDbId = trait.traitDbId,
                    originVariableName = observationVariableName,
                    originVariableDbId = observationVariableDbId
                )]
              created_variables_dt <- merge(created_variables_dt, methods, by="methodDbId")
              
              print("Created variables:")
              print(created_variables_dt)
              
              # Add new variables to the existing variables
              existing_variables_list <- append(existing_variables_list, list(created_variables_dt))
            }
          } 
          existing_variables <- rbindlist(existing_variables_list, use.names = T)
          print("All variables:")
          print(existing_variables)
          
          ## push observationunits ####
          withProgress(message = "Looking for existing observationUnits", min=1, max=1, {
            # Getting existing observationunits
            print("Checking if observationUnits already exist")
            needed_env <- unique(bluesToPush[,environment])
            needed_observation_units <- unique(rv$data[study_name_app %in% needed_env,.(germplasmDbId, germplasmName, studyDbId, study_name_app, programDbId, trialDbId, entryType)])
            needed_observation_units$studyDbId <- as.character(needed_observation_units$studyDbId)
            needed_observation_units$trialDbId <- as.character(needed_observation_units$trialDbId)
            setnames(needed_observation_units, "study_name_app","environment")
            bluesToPush <<- merge(needed_observation_units, bluesToPush, by=c("germplasmName", "environment"))
            
            studyDbIds <- as.character(unique(bluesToPush[, studyDbId]))
            print(studyDbIds)
            resp_post_search_obsunit <- brapir::phenotyping_observationunits_post_search(con = brapir_con, 
                                                             observationLevels = data.frame(levelName = c("MEANS")),
                                                             studyDbIds = studyDbIds)
            print(resp_post_search_obsunit$status_code)
            if (resp_post_search_obsunit$status_code == 200 | resp_post_search_obsunit$status_code == 202) {
              resp_get_search_obsunit <- brapir::phenotyping_observationunits_get_search_searchResultsDbId(con = brapir_con, searchResultsDbId = resp_post_search_obsunit$data$searchResultsDbId)
              if (resp_post_search_obsunit$status_code == 200) {
                existing_obs_units <- resp_get_search_obsunit$data
                pagination <- resp_get_search_obsunit$metadata$pagination
                page = 0
                while (pagination$totalCount > (pagination$currentPage + 1)*pagination$pageSize) {
                  page = page + 1
                  resp_get_search_obsunit <- brapir::phenotyping_observationunits_get_search_searchResultsDbId(con = brapir_con, searchResultsDbId = resp_post_search_obsunit$data$searchResultsDbId, page = page)
                  pagination <- resp_get_search_obsunit$metadata$pagination
                  existing_obs_units <- rbindlist(list(existing_obs_units, resp_get_search_obsunit$data))
                }
              }
            } 
          })

          observation_units <- NULL
          if (nrow(existing_obs_units)==0) {
            print("no existing_obs_units")
            missing_observation_units <- needed_observation_units
          } else {
            print("existing_obs_units:")
            print(head(existing_obs_units))
            existing_obs_units <- data.table(existing_obs_units)
            existing_obs_units <- existing_obs_units[,.(observationUnitDbId, 
              germplasmDbId, germplasmName, studyDbId, programDbId, trialDbId, 
              entryType = observationUnitPosition.entryType)]
            # COMPARE EXISTING OBSERVATION UNITS GERMPLASM TO DATA GERMPLASM
            merge <- merge(needed_observation_units, existing_obs_units, by = c("studyDbId", "germplasmDbId", "germplasmName", "programDbId", "trialDbId", "entryType"), all = TRUE)
            observation_units <- merge[!is.na(observationUnitDbId)] 
            missing_observation_units <- merge[is.na(observationUnitDbId)] 
          }
          
          print("missing_observation_units:")
          print(missing_observation_units)
          
          # POSTING MISSING OBSERVATION UNITS
          if (!is.null(missing_observation_units) && nrow(missing_observation_units) > 0) {
            withProgress(message = "Creating new observationUnits", min=1, max=1, {
              print("Posting observationUnits")
              
              #TODO Remove this step when not necessary anymore
              #Create dataset MEANS with PUT variables before posting observationUnits
              # var <- apply(metrics_variables_df,1,function(a){
              #   list(
              #     contextOfUse = c("MEANS"),
              #     observationVariableDbId = jsonlite::unbox(a["observationVariableDbId"]),
              #     method = list(methodDbId = jsonlite::unbox(a["methodDbId"])),
              #     observationVariableName = jsonlite::unbox(a["observationVariableName"]),
              #     scale = list(scaleDbId = jsonlite::unbox(a["scaleDbId"])),
              #     trait = list(traitDbId = jsonlite::unbox(a["traitDbId"])),
              #     studyDbIds = as.character(studyDbIds)
              #   )
              # })
              # resp <- brapi_put_variable(rv$con, jsonlite::toJSON(var), metrics_variables_df[1, "observationVariableDbId"])
              #TODO end
              
              # Building body POST request
              body <- apply(missing_observation_units,1,function(a){
                list(
                  observationUnitPosition = list(
                    entryType =jsonlite::unbox(a["entryType"]), 
                    observationLevel = list(levelName = jsonlite::unbox("MEANS"))),
                  germplasmDbId = jsonlite::unbox(as.character(a["germplasmDbId"])),
                  programDbId = jsonlite::unbox(as.character(a["programDbId"])),
                  studyDbId = jsonlite::unbox(as.character(a["studyDbId"])),
                  trialDbId = jsonlite::unbox(as.character(a["trialDbId"]))
                )
              })
              
              resp <- brapir::phenotyping_observationunits_post_batch(con = brapir_con, body)
              print(resp$status_code)
              
              new_observation_units <- resp$data
              new_observation_units <- data.table(new_observation_units)
              new_observation_units <- new_observation_units[,.(observationUnitDbId, 
              germplasmDbId, germplasmName, studyDbId, programDbId, trialDbId, 
              entryType = observationUnitPosition.entryType)]
              
              print("created observation_units:")
              print(new_observation_units)

              if (!is.null(observation_units)) {
                observation_units[,environment:=NULL]
                observation_units <- rbind(observation_units, new_observation_units)
              } else { #no existing observation_units
                observation_units <- new_observation_units
              }              
            })
          }
          
          observation_units <- observation_units[,.(observationUnitDbId, germplasmDbId, studyDbId)] 
          print("all observation_units:")
          print(observation_units)
          obs_units <<- observation_units

          ## check if existing BLUEs/BLUPs ####          
          withProgress(message = "check if BLUEs/BLUPs are already stored in the database", min=1, max=1, {
            print("Look for existing BLUEs/BLUPs")
            resp <- brapir::phenotyping_observations_post_search(
              con = brapir_con, 
              observationUnitDbIds = observation_units$observationUnitDbId, 
              observationVariableDbIds = existing_variables$observationVariableDbId,
              pageSize = 1)
            if (resp$status_code == 200) {
              if ("searchResultsDbId" %in% names(resp$data)) {
                searchResultsDbId = resp$data$searchResultsDbId
                resp2 <- brapir::phenotyping_observations_get_search_searchResultsDbId(con = brapir_con, searchResultsDbId = searchResultsDbId)
                existing_obs_count = resp2$metadata$pagination$totalCount
              } else {
                existing_obs_count = resp$metadata$pagination$totalCount
              }
            }
          })          
          if (existing_obs_count > 0) {
            showModal(existingBluesModal())
          } else {
            postObservations()
          }
        },
        error = function(e) {
          showNotification(paste0("An error occured: ", e), type = "error", duration = notification_duration)
          print(e)
          return(NULL)
        })
      }
      ## STA Report ####
      output$STA_report <- downloadHandler(
        filename = function() {
          paste("STA-", Sys.Date(), ".docx", sep="")
        },
        content = function(file) {
          if (is.null(rv_mod$fit)){
            showNotification("Please fit a model first", type = "error", duration = notification_duration)
            return(NULL)
          } else {
            withProgress(message = "Building report", value = 0,max = (length(rv_mod$fit[[1]]$traits)*length(rv_mod$fit))+1, {
            stareport(fit=rv_mod$fit,
                      file=file,
                      template="reports/STA_Model.docx",
                      trialName=unique(rv$study_metadata$trialName),
                      trialdesc = rv$trial_metadata[trialDbId==unique(rv$study_metadata$trialDbId),trialDescription],
                      crop = rv$trial_metadata[trialDbId==unique(rv$study_metadata$trialDbId),commonCropName],
                      spatial = rv_mod$data_checks$has_coords,
                      outliers = rv_mod$outliers$outliers,
                      excluded = if(nrow(rv$excluded_obs)>0) rv$data[observationDbId %in% rv$excluded_obs$observationDbId] else NULL,
                      toc = input$report_toc)
            })
          }
        }
      )
      
      
    }
  )
}
