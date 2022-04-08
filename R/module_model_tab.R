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
    fluidRow(column(12,shiny::actionButton(ns("go_fit_model"), "Fit model", class = "btn btn-info"))),
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
                shiny::actionButton(ns("push_metrics_to_BMS_A"), "Push to BMS", icon = icon("leaf"), class = "btn btn-primary", style = "float:right; margin:5px"),
                dataTableOutput(ns("metrics_A_table"))
              ),
              column(
                6,
                # tags$h4("Metrics ~ Environment x Trait x Genotype"),
                downloadButton(ns("export_metrics_B"), "CSV Export", class = "btn btn-info", style = "float:right; margin:5px"),
                shiny::actionButton(ns("push_metrics_to_BMS_B"), "Push to BMS", icon = icon("leaf"), class="btn btn-primary", style = "float:right; margin:5px"),
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
          session,"select_environments",
          choices = ""
        )
        updatePickerInput(
          session,"select_traits",
          choices = ""
        )
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

        req(rv$data_dq)
        req("observations.observationVariableName"%in%names(rv$data_dq))
        choices_env <- rv$data_dq[,unique(study_name_app)]
        updatePickerInput(
          session,"select_environments",
          choices = choices_env,
          selected = choices_env,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })

      observeEvent(c(input$select_environments, rv$excluded_obs),{
        ## only traits found in all environments can be selected
        trait_by_studyDbIds <- rv$data_dq[study_name_app %in% input$select_environments,.(trait = unique(observations.observationVariableName)), .(studyDbId)]
        choices_traits <- trait_by_studyDbIds[,.N,trait][N==length(trait_by_studyDbIds[,unique(studyDbId)]), trait]
        updatePickerInput(
          session,"select_traits",
          choices = choices_traits,
          selected = choices_traits,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

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
          updatePickerInput(
            session, "model_design",
            choices = possible_designs,
            selected = "",
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

        ###  create TD without the excluded observations
        ## exclude observations
        data_filtered <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs)]
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

      })

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

      observeEvent(input$select_traits,{
        # req(input$select_traits)
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
        rv$fit <- NULL

        if (input$model_engine == "SpATS" && input$display_psanova_opt==T) {
          cntrl <- list(nSeg = c(input$spColSeg, input$spRowSeg),
                        nestDiv = input$spNestDiv)
        } else {
          cntrl <- NULL
        }

        ### fit TD
        a <- tryCatch(
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
          ),
          error=function(e){ e })
        mess <- a$message
        if(!is.null(mess)){
          showNotification(mess, type = "error", duration = notification_duration)
        }
        req(rv$fit)

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
      })

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
        outliersSTA <- outlierSTA(
          rv$fit,
          traits = input$select_trait_outliers,
          what = "random",
          rLimit = input$limit_residual,
          commonFactors = "genotype",
          verbose = F
        )
        outliers <- as.data.table(outliersSTA$outliers)
        setnames(outliers, "trial", "environment")
        datatable(
          outliers,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
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

        datatable(
          metrics_table_filt,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
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

      output$export_metrics_B <- downloadHandler(
        filename = function() {"BLUPs_and_BLUEs.csv"},
        content = function(file) {
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
          setcolorder(table_metrics, c("environment", "genotype", "trait", "result", "value"))

          write.csv(table_metrics, file, row.names = FALSE)
        }
      )
    }
  )
}
