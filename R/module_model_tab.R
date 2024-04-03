#' @export
mod_model_ui <- function(id){
  ns <- NS(id)
  column(
    12,
    fluidRow(
      column(
        4,
        pickerInput(
          ns("select_environments"), "Select Environments", multiple = TRUE, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
        )
      ),
      column(
        3,
        pickerInput(
          ns("select_traits"), label = "Select Traits", multiple = TRUE, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
        )
      ),
      column(
        3,
        pickerInput(ns("model_design"),
                    label = actionLink(ns("model_design_metadata_button"),"Select Model Design", style ="color:inherit", icon = icon("info-circle", style = "float:right; font-size:large;margin-left:10px")),
                    choices = NULL,
                    selected = NULL, multiple = F,
                    options = list(
                      title = "Select Model Design",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    width = "100%"),
        bsModal(
          ns("modal_model_design"), title = "Metadata for model designs", trigger = ns("model_design_metadata_button"), size = "large",
          dataTableOutput(ns("table_model_design_metadata"))
        )
      ),
      column(
        2,
        pickerInput(ns("model_engine"), "Select Modelling Engine",
                    choices = c("SpATS", "lme4"),
                    # choices = c("SpATS", "lme4", "asreml"),
                    selected = "lme4",
                    width = "100%")
      )
    ),
    fluidRow(
      column(
        12,
        bsCollapse(
          open = NULL,
          bsCollapsePanel(
            title = "Advanced fitting options",
            pickerInput(ns("covariates"),label = "Covariates", choices = NULL, multiple = T),
            checkboxGroupInput(ns("what"),label = "Genotype effect (what)", choices = c("random", "fixed"), selected = c("random", "fixed"), inline = T),
            prettySwitch(ns("spatial_opt"),label = "Spatial", value = T),
            prettySwitch(ns("display_psanova_opt"),label = "Set up PSANOVA", value = F),
            uiOutput(ns("psanova_opt"))
          )
        )
      )
    ),
    fluidRow(
      column(
        2,
        shiny::actionButton(ns("go_fit_model"), "Fit model", class = "btn btn-info"),
        hidden(shiny::actionButton(ns("go_fit_no_outlier"), "Refit without outliers", class = "btn btn-info"))
      ),
      column(
        4,
        h4(textOutput(ns("fit_outliers_output")))
      )
    ),
    fluidRow(
      column(
        12,
        tabsetPanel(
          tabPanel(
            "Fitted models",
            fluidRow(
              column(
                3,
                pickerInput(
                  ns("select_environment_fit"),"Environments", multiple = T, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
                )
              ),
              column(
                3,
                pickerInput(
                  ns("select_trait_fit"),"Trait", multiple = F, choices = NULL, width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                4,
                verbatimTextOutput(ns("fit_summary"))
              ),
              column(
                4,
                plotOutput(ns("fit_residuals"))
              ),
              column(
                4,
                plotOutput(ns("fit_spatial"))
              )
            )
          ),
          tabPanel(
            "Outliers",
            fluidRow(
              column(
                4,
                pickerInput(
                  ns("select_trait_outliers"),"Trait", multiple = F, choices = NULL, width = "100%"
                )
              ),
              column(
                4,
                sliderInput(ns("limit_residual"), label = "Threshold for standardized residuals", min = 0, max = 0, value = 0, width = "100%")
              )
            ),
            fluidRow(
              column(
                12,
                dataTableOutput(ns("table_outliers"))
              )
            )
          ),
          tabPanel(
            "Results",
            fluidRow(
              column(
                6,
                # tags$h4("Metrics ~ Environment x Trait"),
                # pickerInput(ns("select_metrics_A"), "Statistics", multiple = F, choices = NULL, width = "40%", inline = T),
                downloadButton(ns("export_metrics_A"), "CSV Export", class = "btn btn-info", style = "float:right; margin:5px"),
                shiny::actionButton(ns("push_metrics_to_BMS_B"), "Push BLUES/BLUPS to BMS", icon = icon("leaf"), class = "btn btn-primary", style = "float:right; margin:5px"),
                bsTooltip(id = ns("push_metrics_to_BMS_B"), title = "You can select the traits you want to push by selecting raws in the table below", placement = "left", trigger = "hover"),
                dataTableOutput(ns("metrics_A_table"))
              ),
              column(
                6,
                # tags$h4("Metrics ~ Environment x Trait x Genotype"),
                downloadButton(ns("export_metrics_B"), "CSV Export", class = "btn btn-info", style = "float:right; margin:5px"),
                #disabled(shiny::actionButton(ns("push_metrics_to_BMS_B"), "Push to BMS", icon = icon("leaf"), class="btn btn-primary", style = "float:right; margin:5px")),
                tags$br(),
                pickerInput(ns("select_metrics_B"), "BLUPs/BLUEs", multiple = F, choices = c("BLUPs","seBLUPs","BLUEs","seBLUEs"), width = "40%", inline = T),
                pickerInput(ns("select_environment_metrics"), "Filter by Environment", multiple = F, choices = NULL, width = "40%", inline = T),
                dataTableOutput(ns("metrics_B_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' @export
mod_model_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      rv_mod <- reactiveValues()
      
      

      observe({
        ## initialization
        updatePickerInput(
          session, "select_environment_fit",
          choices = ""
        )
        updatePickerInput(
          session, "select_trait_fit",
          choices = ""
        )
        updatePickerInput(
          session, "select_trait_outliers",
          choices = ""
        )
        updatePickerInput(
          session, "select_environment_metrics",
          choices = ""
        )
        rv$fit <- NULL
        rv$obsUnit_outliers <- NULL

        req(rv$data_dq)
        req("observations.observationVariableName"%in%names(rv$data_dq))
        choices_env <- rv$data_dq[,unique(study_name_app)]
        updatePickerInput(
          session,"select_environments",
          choices = choices_env,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        choices_traits <- unique(rv$data_dq[scale.dataType == "Numerical"]$observations.observationVariableName)
        updatePickerInput(
          session,"select_traits",
          choices = choices_traits,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
        # req(rv$pushOK)
        # if (rv$pushOK == TRUE) {
          shinyjs::enable("push_metrics_to_BMS_B")
        # }
        shinyjs::hide("go_fit_no_outlier")
        shinyjs::hide("fit_outliers_output")
        rv_mod$selected_env <- NULL
        rv_mod$selected_traits <- NULL
        
      })
      
      observeEvent(c(input$select_environments_open, req(!is.null(rv$data_dq))), {
        #update traits dropdown when closing environment dropdown and only if the environments selection has changed
        if (!isTRUE(input$select_environments_open) && !identical(input$select_environments, rv_mod$selected_env)) {
          rv_mod$selected_env <- input$select_environments
          shinyjs::hide("go_fit_no_outlier")
          # Update traits dropdown
          if (is.null(input$select_environments)) {            
            choices_traits <- unique(rv$data_dq[scale.dataType == "Numerical"]$observations.observationVariableName)            
            if (is.null(input$select_traits)) {
              selected_traits <- NULL
            } else {
              selected_traits <- input$select_traits
            }
  
          } else {
            ## only traits found in all selected environments can be selected
            trait_by_studyDbIds <- rv$data_dq[scale.dataType == "Numerical"][study_name_app %in% input$select_environments, .(trait = unique(observations.observationVariableName)), .(studyDbId)]
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
        
        data_filt <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs) & (study_name_app %in% input$select_environments)]
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
      })
      
      observeEvent(c(input$select_traits_open, req(!is.null(rv$data_dq))), {
        #update environmentq dropdown when closing traits dropdown and only if the traits selection has changed
        if (!isTRUE(input$select_traits_open) && !identical(input$select_traits, rv_mod$selected_traits)) {
          rv_mod$selected_traits <- input$select_traits
          shinyjs::hide("go_fit_no_outlier")
          # Update environments dropdown
          if (is.null(input$select_traits)) {
            req("observations.observationVariableName"%in%names(rv$data_dq))
            choices_env <- rv$data_dq[,unique(study_name_app)]
            if (is.null(input$select_environments)) {
              selected_env <- NULL
            } else {
              selected_env <- input$select_environments
            }
          } else {
            ## only environment with all selected traits can be selected
            env_by_traits <- rv$data_dq[observations.observationVariableName %in% input$select_traits, .(env = unique(study_name_app)), .(observations.observationVariableName)]
            choices_env <- env_by_traits[, .N, env][N == length(env_by_traits[, unique(observations.observationVariableName)]), env]
            if (is.null(input$select_environments)) {
              selected_env <- choices_env
            } else {
              if (all(input$select_environments %in% choices_env)) {
                selected_env <- input$select_environments
              } else {
                selected_env <- choices_env
              }
            }
            print(selected_env)
            print(choices_env)
            #choices_env <- unique(rv$data_dq[observations.observationVariableName %in% input$select_traits, study_name_app])
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
          all_traits <- rv$data_dq[,unique(observations.observationVariableName)]
          remaining_traits <- setdiff(all_traits, input$select_traits)
          choices_cov <- c(names(rv$data_dq)[unlist(rv$data_dq[,lapply(.SD, is.numeric)])], remaining_traits)
          not_cov <- c(
            "studyDbId", "trialDbId","observations.observationDbId",
            "environment_number",
            "observations.observationVariableDbId",
            "observations.value",
            "programDbId"
          )
          choices_cov <- choices_cov[!(choices_cov%in%not_cov)]
          updatePickerInput(
            session, "covariates", choices = choices_cov, selected = NULL
          )
          
        }
      }, 
      ignoreNULL = FALSE)

      observeEvent(input$model_engine,{
        if(input$model_engine=="SpATS"){
          shinyjs::show("spatial_opt")
          shinyjs::show("display_psanova_opt")
        }else{
          shinyjs::hide("spatial_opt")
          shinyjs::hide("display_psanova_opt")
        }
      })

      output$psanova_opt <- renderUI({ ## based on RAPWeb code
        req(input$select_environments, rv_mod$TD, input$model_engine == "SpATS", input$display_psanova_opt==T)

        ## Variable selection. Cov vars have to be a factor column.
        nRows <- min(sapply(X = input$select_environments, FUN = function(trial) {
          nlevels(droplevels(rv_mod$TD[[trial]]$rowId))
        }))
        nCols <- min(sapply(X = input$select_environments, FUN = function(trial) {
          nlevels(droplevels(rv_mod$TD[[trial]]$colId))
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

      observeEvent(input$go_fit_model,{
        req(rv_mod$data_checks)
        ## create TD without the excluded observations
        ## exclude observations
        rv$data_dq[,observations.value:=as.numeric(observations.value)]
        data_filtered <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs)]
        fitModel(data_filtered)
        output$fit_outliers_output = renderText({
          ""
        })
      })
      
      observeEvent(input$go_fit_no_outlier,{
        req(rv_mod$data_checks)
        req(rv$obsUnit_outliers)
        ## create TD without the excluded observations
        ## exclude observations
        rv$data_dq[,observations.value:=as.numeric(observations.value)]
        data_filtered <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs)]
        data_filtered <- data_filtered[!(observationUnitDbId %in% rv$obsUnit_outliers)]
        fitModel(data_filtered)
        output$fit_outliers_output = renderText({
          "The outliers were removed before fitting the model"
        })
      })
      
      fitModel <- function(data_filtered) {
        ## make 1 column per trait
        data_filtered_casted <- dcast(
          data = data_filtered[,.(
            observationUnitDbId,
            genotype = germplasmName, trial = study_name_app, loc = studyLocationDbId,
            repId = replicate,
            subBlock = blockNumber,
            rowCoord = positionCoordinateY, colCoord = positionCoordinateX,
            observations.observationVariableName, observations.value
          )],
          formula = "observationUnitDbId + genotype + trial + loc + repId + subBlock + rowCoord + colCoord ~ observations.observationVariableName",
          value.var = "observations.value"
        )
        
        ## parametrization
        createTD_args <- list(
          genotype = "genotype",
          trial = "trial",
          loc = "loc",
          repId = "repId",
          subBlock = "subBlock",
          rowCoord = "rowCoord",
          colCoord = "colCoord"
        )
        if(!rv_mod$data_checks$has_subBlocks){
          data_filtered_casted[,subBlock:=NULL]
          createTD_args$subBlock <- NULL
        }
        if(!rv_mod$data_checks$has_repIds){
          data_filtered_casted[,repId:=NULL]
          createTD_args$repId <- NULL
        }
        if(!rv_mod$data_checks$has_coords){
          data_filtered_casted[,rowCoord:=NULL]
          data_filtered_casted[,colCoord:=NULL]
          createTD_args$rowCoord <- NULL
          createTD_args$colCoord <- NULL
        }
        createTD_args <- c(
          list(data = data_filtered_casted),
          createTD_args
        )
        
        ## create TD
        rv_mod$TD <- do.call(what = createTD, args = createTD_args)
        
        
        rv$fit <- NULL
        
        if (input$model_engine == "SpATS" && input$display_psanova_opt==T) {
          cntrl <- list(nSeg = c(input$spColSeg, input$spRowSeg),
                        nestDiv = input$spNestDiv)
        } else {
          cntrl <- NULL
        }
        
        ### fit TD
        a <- tryCatch({
            rv$fit <- fitTD(
              TD = rv_mod$TD,
              trials = input$select_environments,
              design = input$model_design,
              traits = input$select_traits,
              engine = input$model_engine,
              covariates = input$covariates,
              what = input$what,
              # useCheckId = FALSE,
              spatial = ifelse(input$model_engine=="SpATS", input$spatial_opt, F),
              control = cntrl
            )
          },
          error=function(e){ e })
        mess <- a$message
        if(!is.null(mess)){
          showNotification(mess, type = "error", duration = notification_duration)
        }
        
        req(rv$fit)
        
        ## SPATs does not make prediction when genotypes are in the fixed part of the model
        # It causes the summary.TD and plot.TD functions to throw error when trying to compute the predictions
        # temporary fix: if there is no "fixed" modelling, then this list item is removed from the fitTD object
        # example: ?cropDb=rice&token=jhjlkj&apiURL=https://www.bms-uat-test.net/bmsapi&studyDbIds=2705,2706
        for(trial in names(rv$fit)){
          if(all(unlist(lapply(rv$fit[[trial]]$mFix, is.null)))){
            rv$fit[[trial]][["mFix"]] <- NULL
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

      ## show fitted models per trait and environments
      observeEvent(c(input$select_environment_fit, input$select_trait_fit),{
        req(input$select_environment_fit)
        req(input$select_trait_fit)
        req(rv$fit)

        output$fit_summary <- renderPrint({
          # s_all <- summary(
          #   rv$fit,
          #   trait = input$select_trait_fit,
          #   trials = input$select_environment_fit
          # )
          s <- lapply(input$select_environment_fit, function(env){
            summary(
              rv$fit,
              trait = input$select_trait_fit,
              trials = env
            )
          })
          names(s) <- input$select_environment_fit
          # s["all environments"] <- s_all
          s
        })

        output$fit_residuals <- renderPlot({
          req(rv$fit)
          req(input$select_environment_fit)
          plots_envs <- lapply(input$select_environment_fit, function(trial){
            plot(
              rv$fit,
              trait = input$select_trait_fit,
              trials = trial,
              output = F
              # output = F,
              # what = c("random","fixed")[c(
              #   !is.null(rv$fit[[trial]]$mRand),
              #   !is.null(rv$fit[[trial]]$mFixed)
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
          req(rv$fit)
          isolate(req(rv_mod$data_checks$has_coords))
          req(input$select_environment_fit)
          plots_envs <- lapply(input$select_environment_fit, function(trial){
            if(rv$data_dq[observations.observationVariableName == input$select_trait_fit & study_name_app == trial,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1){
              p <- plot(
                rv$fit,
                plotType = "spatial",
                trait = input$select_trait_fit,
                trials = trial,
                output = F
                # output = F,
                # what = c("random","fixed")[c(
                #   !is.null(rv$fit[[trial]]$mRand),
                #   !is.null(rv$fit[[trial]]$mFixed)
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

      ### Outliers
      observeEvent(input$select_trait_outliers,{
        req(rv$fit)
        req(input$select_trait_outliers)
        
        stdResR <- as.data.table(extractSTA(STA = rv$fit, traits = input$select_trait_outliers, what = "stdResR"))
        res_q <- quantile(abs(stdResR[[input$select_trait_outliers]]), probs = seq(0,1,0.01), na.rm = T)
        updateSliderInput(
          session = session, "limit_residual",
          max = as.numeric(res_q[101]),
          value = as.numeric(res_q[100]) # by default, the 1% extreme residuals are returned
        )
      })
      
      output$table_outliers <- renderDataTable({
        req(rv$fit)
        req(input$select_trait_outliers)
        req(input$limit_residual>0)

        # outliers on all traits (used to get number of outliers for each observationUnit)
        outliersSTA_all <- outlierSTA(
          rv$fit,
          what = "random",
          rLimit = input$limit_residual,
          commonFactors = "genotype",
          verbose = F
        )
        outliers_all <- as.data.table(outliersSTA_all$outliers)
        validate(need(outliers_all[,.N]>0 , "No outlier on this trait"))
        
        # outliers for the selected trait
        outliers <- outliers_all[trait == input$select_trait_outliers]
        
        validate(need(outliers[,.N]>0 , "No outlier on this trait"))
        
        outliers_nb <- outliers_all[, .(`#outliers` = sum(outlier)), by = observationUnitDbId]
        subBlock_index <- which(colnames(outliers_all) == "subBlock")
        obs_nb <- unique(outliers_all[, .(observationUnitDbId,  `#observations` = rowSums(!is.na(.SD[, (subBlock_index+1):(length(colnames(outliers_all))-1)])))])
        outliers_by_obsUnit <- merge(outliers_nb, obs_nb, by="observationUnitDbId")   
        
        rv$obsUnit_outliers <- outliers_by_obsUnit[`#outliers` > 0]$observationUnitDbId
        shinyjs::show("go_fit_no_outlier")
        shinyjs::show("fit_outliers_output")
        
        if(outliers[,.N]>0){
          setnames(outliers, "trial", "environment")
        }
        
        outliers <- merge(outliers, outliers_by_obsUnit, by="observationUnitDbId")
        
        #Sort outliers on genotype, environment and repetition
        outliers <- outliers[order(genotype, environment, repId)]
        
        # change columns order (move variables columns at the end)
        cols <- colnames(outliers)
        subBlock_index <- which(cols == "subBlock")
        outlier_index <- which(cols == "outlier")
        new_cols_order <- c(cols[1:subBlock_index], cols[outlier_index:length(cols)], cols[(subBlock_index+1):(outlier_index-1)])
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
        
        datatable(
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
        )
      })

      ### Metrics
      output$metrics_A_table <- renderDT({
        req(rv$fit)

        ## heritability
        heritability <- as.data.table(extractSTA(STA = rv$fit, what = "heritability"))

        if(rv_mod$model_engine%in%c("lme4")){
          ## CV
          cv <- as.data.table(extractSTA(STA = rv$fit, what = "CV"))

          ## wald test
          wald_raw <- extractSTA(STA = rv$fit, what = "wald")
          wald <- rbindlist(lapply(names(wald_raw), function(env){
            data.table(
              trial = env,
              rbindlist(lapply(names(wald_raw[[env]]$wald), function(trait){
                data.table(
                  trait = trait,
                  wald_raw[[env]]$wald[[trait]]
                )
              }))
            )
          }))

          cv_melt <- melt(cv, id.vars = "trial", variable.name = "trait", value.name = "CV")
          heritability_melt <- melt(heritability, id.vars = "trial", variable.name = "trait", value.name = "Heritability")

          metrics <- heritability_melt[cv_melt, on = .(trial, trait)][wald[,.(trial, trait, "Wald p.value" = p.value)], on = .(trial, trait)]

        }else{
          metrics <- melt(heritability, id.vars = "trial", variable.name = "trait", value.name = "Heritability")
        }
        setnames(metrics, "trial", "Environment")
        setnames(metrics, "trait", "Trait")

        rv_mod$metrics_A <- metrics

        setkey(metrics, "Environment")
        dtable <- datatable(
          metrics,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't',
            rowsGroup = as.list(c(0))
          ))
        path <- "www/js/datatables-rowsgroup/"
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0",
          path, script = "dataTables.rowsGroup.js"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      })

      output$metrics_B_table <- renderDataTable({
        req(rv$fit)
        req(input$select_metrics_B)
        req(input$select_environment_metrics)

        metrics_table <- as.data.table(extractSTA(STA = rv$fit, what = input$select_metrics_B))
        entry_types <- unique(rv$data_dq[,.(genotype=germplasmName, entryType)])
        setkey(metrics_table, genotype)
        setkey(entry_types, genotype)
        metrics_table <- entry_types[metrics_table]
        setnames(metrics_table, "trial", "environment")

        metrics_table_filt <- metrics_table[environment==input$select_environment_metrics, -c("environment"), with = F]
        rv_mod$metrics_B <- metrics_table_filt
        
        # selected_rows <- input$metrics_A_table_rows_selected
        # print("selected rows: ")
        # print(selected_rows)
        # traits <- rv_mod$metrics_A[selected_rows]$Trait
        # colnames(metrics_table_filt)
        # selected <- match(traits, colnames(metrics_table_filt)) - 1
        # print("selected columns: ")
        # print(selected)

        dtable <- datatable(
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
        )
        path <- "www/js/datatables-rowsgroup/"
        dep <- htmltools::htmlDependency(
          "RowsGroup", "2.0.0",
          path, script = "dataTables.rowsGroup.js"
        )
        dtable$dependencies <- c(dtable$dependencies, list(dep))
        dtable
      })

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
        req(rv$fit)
        table_BLUPs <- as.data.table(extractSTA(STA = rv$fit, what = c("BLUPs")))
        table_BLUPs[,result:="BLUPs"]
        table_seBLUPs <- as.data.table(extractSTA(STA = rv$fit, what = c("seBLUPs")))
        table_seBLUPs[,result:="seBLUPs"]
        table_BLUEs <- as.data.table(extractSTA(STA = rv$fit, what = c("BLUEs")))
        table_BLUEs[,result:="BLUEs"]
        table_seBLUEs <- as.data.table(extractSTA(STA = rv$fit, what = c("seBLUEs")))
        table_seBLUEs[,result:="seBLUEs"]
        
        table_metrics <- rbindlist(l = list(table_BLUPs,table_seBLUPs,table_BLUEs,table_seBLUEs), use.names = T, fill = T)
        table_metrics <- melt(
          data = table_metrics,
          measure.vars = names(table_metrics)[!(names(table_metrics)%in%c("genotype","trial","result"))],
          variable.name = "trait"
        )
        setnames(table_metrics, "trial", "environment")
        
        return(table_metrics)
      })

      output$export_metrics_B <- downloadHandler(
        filename = function() {"BLUPs_and_BLUEs.csv"},
        content = function(file) {
          table_BLUPs <- as.data.table(extractSTA(STA = rv$fit, what = c("BLUPs")))
          table_seBLUPs <- as.data.table(extractSTA(STA = rv$fit, what = c("seBLUPs")))
          table_BLUEs <- as.data.table(extractSTA(STA = rv$fit, what = c("BLUEs")))
          table_seBLUEs <- as.data.table(extractSTA(STA = rv$fit, what = c("seBLUEs")))
          
          for (i in 3:length(colnames(table_BLUPs))) {
            colnames(table_BLUPs)[i] <- paste0(colnames(table_BLUPs)[i], "_BLUPs")
            colnames(table_seBLUPs)[i] <- paste0(colnames(table_seBLUPs)[i], "_seBLUPs")
            colnames(table_BLUEs)[i] <- paste0(colnames(table_BLUEs)[i], "_BLUEs")
            colnames(table_seBLUEs)[i] <- paste0(colnames(table_seBLUEs)[i], "_seBLUEs")
          }
          
          table_metrics <- merge(table_BLUPs, table_seBLUPs, by=c("genotype", "trial"))
          table_metrics <- merge(table_metrics, table_BLUEs, by=c("genotype", "trial"))
          table_metrics <- merge(table_metrics, table_seBLUEs, by=c("genotype", "trial"))
          
          #rename columns trial and genotype
          colnames(table_metrics)[colnames(table_metrics) == "trial"] <- "environment"
          colnames(table_metrics)[colnames(table_metrics) == "genotype"] <- "germplasm"
          cols <- colnames(table_metrics)
          
          #put environment column in first position
          new_col_order <- append(c(cols[2], cols[1]), cols[3:length(cols)]) 
          table_metrics <- setcolorder(table_metrics, new_col_order)
          
          write.csv(table_metrics, file, row.names = FALSE)
        }
      )
      
      ### PUSH METRICS_A TO BMS
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
          )
        )
      }
      
      # When OK button is pressed, push BLUES/BLUPS.
      observeEvent(input$ok, {
        removeModal()
        pushBlues()
      })

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
  
          # Get methodDbIds 
          #TODO use /search/methodDbIds
          programDbId <- unique(rv$study_metadata$programDbId)
          methodIds <- tryCatch({
            get_BLUES_methodsDbIds(rv$con, programDbId)
            },
            error=function(e){
              showNotification(conditionMessage(e), type = "error", duration = notification_duration)
              return(NULL)
            }
          )
          #exit the function if missing method ids
          if (is.null(methodIds)) {
            return(NULL)
          }
          
          methods <- data.table(result = names(methodIds), methodDbId = unname(unlist(methodIds)))
          
          # GET AND/OR CREATE BLUES/BLUPS VARIABLES
          # Get the ids of variables that were used in the model
          origin_variable_names <- select(table_metrics, c("originVariableName"))
          origin_variable_names <- origin_variable_names[!duplicated(origin_variable_names), ]
          
          # Get variables ids from data
          variables_df <- select(rv$data, c("observations.observationVariableDbId", "observations.observationVariableName"))
          variables_df <- variables_df[!duplicated(variables_df), ]
          variables_df <- na.omit(variables_df)
          colnames(variables_df) <- c("originVariableDbId","originVariableName")
          
          # Merge variables from table_metrics with variables from data on their name to only keep variables actually used in model
          variables_df <- merge(origin_variable_names, variables_df, by="originVariableName")
  
          # Search variables on ids to get scale and trait
          search_variables <- brapirv2::brapi_post_search_variables(
            con = rv$con,
            observationVariableDbIds = as.character(variables_df$originVariableDbId))
  
          origin_variables <- brapirv2::brapi_get_search_variables_searchResultsDbId(
            con = rv$con,
            searchResultsDbId = search_variables$searchResultsDbId)
  
          origin_variables <- data.frame(
            originVariableDbId = origin_variables$observationVariableDbId, 
            originVariableName = origin_variables$observationVariableName, 
            traitDbId = origin_variables$trait.traitDbId, 
            scaleDbId=origin_variables$scale.scaleDbId)
          origin_variables <- origin_variables[!duplicated(origin_variables), ]
          print("origin_variables:")
          print(origin_variables)
          
          # Checking if BLUES/BLUPS variables already exist
          print("Checking if BLUES/BLUPS variables already exist")
          missing_variables_df <- data.frame(matrix(nrow = 0, ncol = 5))
          metrics_variables_df <- data.frame(matrix(nrow = 0, ncol = 8))
          # 
          # for each origin variable, we are looking for corresponding BLUPs/BLUEs variables
          # which means looking for variables with the same scaleDbId, traitDbId but with BLUEs/BLUPs methodDbIds 
          for (i in 1:nrow(origin_variables)) {
            scaleDbId <- origin_variables$scaleDbId[i]  #"6085"
            variableName <- origin_variables$originVariableName[i]
            traitDbId <- origin_variables$traitDbId[i]  #"20454"
            variableDbId <- origin_variables$originVariableDbId[i]
            
            print(origin_variables)
            search_variables <- brapirv2::brapi_post_search_variables(
              con = rv$con,
              methodDbIds = c(methodIds$BLUEs, methodIds$BLUPs, methodIds$seBLUEs, methodIds$seBLUPs),
              scaleDbIds = scaleDbId,
              traitDbIds = traitDbId
            )
            nbExistingVariables = 0
            
  
            res <-brapi_get_variable_searchResultsDbId(
              con = rv$con,
              searchResultsDbId = search_variables$searchResultsDbId)
  
            if (res$content$metadata$pagination$totalCount > 0) {
              existing_variables <- res$content$result$data
              for (j in 1:length(methodIds)) {
                var <- existing_variables[existing_variables$method$methodDbId == methodIds[[j]], ]
                if (nrow(var) == 0) { # missing variable
                  new_variable_name <- paste0(variableName, "_", names(methodIds[j]))
                  missing_variables_df <- rbind(missing_variables_df, c(new_variable_name, "MEANS", methodIds[[j]], scaleDbId, traitDbId))
                } else { #existing variable
                  metrics_variables_df <- rbind(metrics_variables_df, c(variableDbId, variableName, var[1, "observationVariableName"], var[1, "observationVariableDbId"], names(methodIds[j]), methodIds[[j]], traitDbId, scaleDbId))
                }
              }
              nbExistingVariables = nrow(existing_variables)
            } else {  #none existing variables, so adding them all
              for (i in 1:length(methodIds)) {
                new_variable_name <- paste0(variableName, "_", names(methodIds[i]))
                missing_variables_df <- rbind(missing_variables_df, c(new_variable_name, "MEANS", methodIds[[i]], scaleDbId, traitDbId))
              }
            }
          }
          colnames(missing_variables_df) =  c("observationVariableName", "contextOfUse", "methodDbId", "scaleDbId", "traitDbId")
          colnames(metrics_variables_df) =  c("originVariableDbId", "originVariableName", "observationVariableName", "observationVariableDbId", "result", "methodDbId", "traitDbId", "scaleDbId")
  
          metrics_variables_df <- metrics_variables_df[!duplicated(metrics_variables_df), ]
          print("Existing Variables:")
          print(metrics_variables_df)
  
          print("Missing Variables:")
          print(missing_variables_df)
          
          # Create missing variables
          if (nrow(missing_variables_df) > 0) {
            print("Creating missing BLUES/BLUPS variables")
            print(paste0("Creating ", nrow(missing_variables_df) , " new variables"))
            print(missing_variables_df$observationVariableName)
  
            # Building body POST request
            # body <- list()
            # for (i in 1:nrow(missing_variables_df)) {
            #   var <- list(
            #     contextOfUse = c("MEANS"),
            #     method = list(methodDbId = jsonlite::unbox(missing_variables_df[i, "methodDbId"])),
            #     observationVariableName = jsonlite::unbox(missing_variables_df[i, "observationVariableName"]),
            #     scale = list(scaleDbId = jsonlite::unbox(missing_variables_df[i, "scaleDbId"])),
            #     trait = list(traitDbId = jsonlite::unbox(missing_variables_df[i, "traitDbId"]))
            #   )
            #   body <- c(body, list(var))
            # }
            body <- apply(missing_variables_df,1,function(a){
              list(
                contextOfUse = c("MEANS"),
                method = list(methodDbId = jsonlite::unbox(a["methodDbId"])),
                observationVariableName = jsonlite::unbox(a["observationVariableName"]),
                scale = list(scaleDbId = jsonlite::unbox(a["scaleDbId"])),
                trait = list(traitDbId = jsonlite::unbox(a["traitDbId"]))
              )
            })
            
            resp <- brapi_post_several_variables(rv$con, jsonlite::toJSON(body))
  
            created_variables_df <- resp$content$result$data
  
            created_variables_df <- data.frame(
              observationVariableDbId = created_variables_df$observationVariableDbId,
              observationVariableName = created_variables_df$observationVariableName,
              methodDbId = created_variables_df$method$methodDbId,
              traitDbId = created_variables_df$trait$traitDbId,
              scaleDbId = created_variables_df$scale$scaleDbId)
  
            created_variables_df <- created_variables_df[!duplicated(created_variables_df), ]
            print("Created variables:")
            print(created_variables_df)
  
            created_variables_df <- merge(created_variables_df, methods, by="methodDbId")
  
            # Adding originVariableName and originVariableDbId columns to created_variables_df
            merge <- merge(created_variables_df, origin_variables, by=c("traitDbId", "scaleDbId"))
            print(merge)
  
            # Add new variables to the existing variables dataframe
            metrics_variables_df <- rbind(metrics_variables_df, merge)
            print("All variables:")
            print(metrics_variables_df)
          }
          
          # add variableDbIds to data table
          table_metrics <- merge(table_metrics, metrics_variables_df, by=c("originVariableName","result"))
          
          print("Checking if observationUnits already exist")
          
          # GETTING EXISTING OBSERVATION UNITS
          needed_env <- unique(table_metrics[,environment])
          needed_observation_units <- unique(rv$data[study_name_app %in% needed_env,.(germplasmDbId, germplasmName, studyDbId, study_name_app, programDbId, trialDbId, entryType)])
          needed_observation_units$studyDbId <- as.character(needed_observation_units$studyDbId)
          needed_observation_units$trialDbId <- as.character(needed_observation_units$trialDbId)
          setnames(needed_observation_units, "study_name_app","environment")
          table_metrics <- merge(needed_observation_units, table_metrics, by=c("germplasmName", "environment"))
          
          studyDbIds <- as.character(unique(table_metrics[, studyDbId]))
          body <- list(observationLevels = list(list(levelName = jsonlite::unbox("MEANS"))), studyDbIds = studyDbIds)
          res <- brapi_post_search_obsUnits(
            con = rv$con,
            jsonlite::toJSON(body)
          )
          
          existing_obs_units <- NULL
          try(existing_obs_units <- brapi_get_search_observationunits_searchResultsDbId(con = rv$con, searchResultsDbId = res$content$result$searchResultsDbId))
  
          missing_observation_units <- NULL
          observation_units <- NULL
          if (is.null(existing_obs_units)) {
            print("no existing_obs_units")
            missing_observation_units <- needed_observation_units
          } else {
            print("existing_obs_units:")
            print(existing_obs_units)
            existing_obs_units <- data.table(existing_obs_units)
            existing_obs_units <- existing_obs_units[,.(observationUnitDbId, germplasmDbId, germplasmName, studyDbId, programDbId, trialDbId, observationUnitPosition.entryType)]
            setnames(existing_obs_units, "observationUnitPosition.entryType", "entryType")
            # COMPARE EXISTING OBSERVATION UNITS GERMPLASM TO DATA GERMPLASM
            merge <- merge(needed_observation_units, existing_obs_units, by = c("studyDbId", "germplasmDbId", "germplasmName", "programDbId", "trialDbId", "entryType"), all = TRUE)
            observation_units <- merge[!is.na(observationUnitDbId)] 
            missing_observation_units <- merge[is.na(observationUnitDbId)] 
          }
          
          print("missing_observation_units:")
          print(missing_observation_units)
          
          # POSTING MISSING OBSERVATION UNITS
          if (!is.null(missing_observation_units) && nrow(missing_observation_units) > 0) {
            print("Posting observationUnits")
  
            #TODO Remove this step when not necessary anymore
            #Create dataset MEANS with PUT variables before posting observationUnits
            var <- apply(metrics_variables_df,1,function(a){
              list(
                contextOfUse = c("MEANS"),
                observationVariableDbId = jsonlite::unbox(a["observationVariableDbId"]),
                method = list(methodDbId = jsonlite::unbox(a["methodDbId"])),
                observationVariableName = jsonlite::unbox(a["observationVariableName"]),
                scale = list(scaleDbId = jsonlite::unbox(a["scaleDbId"])),
                trait = list(traitDbId = jsonlite::unbox(a["traitDbId"])),
                studyDbIds = as.character(studyDbIds)
              )
            })
            resp <- brapi_put_variable(rv$con, jsonlite::toJSON(var), metrics_variables_df[1, "observationVariableDbId"])
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
            
            resp <- brapi_post_several_observationUnits(rv$con, jsonlite::toJSON(body))
            print(resp$content$metadata$status)
    
            new_observation_units <- resp$content$result$data
            new_observation_units <- data.table(new_observation_units)
            new_observation_units <- new_observation_units[,.(observationUnitDbId, germplasmDbId, germplasmName, studyDbId, programDbId, trialDbId, observationUnitPosition.entryType)]
            setnames(new_observation_units, "observationUnitPosition.entryType", "entryType")
  
            if (!is.null(observation_units)) {
              observation_units[,environment:=NULL]
              observation_units <- rbind(observation_units, new_observation_units)
            } else { #no existing observation_units
              observation_units <- new_observation_units
            }
            print("created observation_units:")
            print(observation_units)
          }
          
          observation_units <- observation_units[,.(observationUnitDbId, germplasmDbId, studyDbId)] 
            
          print("all observation_units:")
          print(observation_units)
    
          # POSTING OBSERVATIONS
          print("Posting observations")
          
          table_metrics$studyDbId = as.character(table_metrics$studyDbId)
          table_metrics <- merge(table_metrics, observation_units, by=c("germplasmDbId","studyDbId"))
  
          # Building body POST request
          body <- apply(table_metrics,1,function(a){
            list(
              germplasmDbId = jsonlite::unbox(as.character(a["germplasmDbId"])),
              observationUnitDbId = jsonlite::unbox(as.character(a["observationUnitDbId"])),
              studyDbId = jsonlite::unbox(as.character(a["studyDbId"])),
              observationVariableDbId = jsonlite::unbox(as.character(a["observationVariableDbId"])),
              value = jsonlite::unbox(as.numeric(a["value"]))
            )
          })
          
          resp <- brapi_post_several_observations(rv$con, jsonlite::toJSON(body))
          created_observations_df <- resp$content$result$data
          print(resp$content$metadata$status)
  
          if (resp$status_code == 200) {
            showNotification(paste0("BLUES/BLUPS were pushed to BMS (",nrow(created_observations_df), " data)"), type = "message", duration = notification_duration)
          } else {
            showNotification(paste0("An error occured while creating new observations"), type = "error", duration = notification_duration)
          }
        },
        error = function(e) {
          showNotification(paste0("An error occured"), type = "error", duration = notification_duration)
          return(NULL)
        }
        )
      }
    }
  )
}
