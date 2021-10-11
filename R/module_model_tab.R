#' @export
mod_model_ui <- function(id){
  ns <- NS(id)
  column(
    12,
    fluidRow(
      column(
        3,
        pickerInput(
          ns("select_traits"), label = "Select Traits", multiple = TRUE, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
        )
      ),
      column(
        3,
        pickerInput(
          ns("select_environments"), "Environments", multiple = TRUE, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
        )
      ),
      column(
        3,
        pickerInput(ns("model_design"), "Select Model Design",
                    choices = choices_model_design,
                    selected = NULL, multiple = F,
                    options = list(
                      title = "Select Model Design",
                      onInitialize = I('function() { this.setValue(""); }')
                    ),
                    width = "100%")
      ),
      column(
        2,
        pickerInput(ns("model_engine"), "Select Modelling Engine",
                    choices = c("SpATS", "lme4", "asreml"),
                    selected = "SpATS",
                    width = "100%")
      ),
      column(
        1,
        actionButton(ns("go_fit_model"), "Fit model")
      )
    ),
    fluidRow(
      column(
        12,
        bsCollapse(
          open = NULL,
          bsCollapsePanel(
            title = "Advanced fitting options",
            pickerInput(ns("covariates"),label = "covariates", choices = NULL, multiple = T)
          )
        )
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
                  ns("select_trait_fit"),"Trait", multiple = F, choices = NULL, width = "100%"
                )
              ),
              column(
                3,
                pickerInput(
                  ns("select_environment_fit"),"Environments", multiple = T, choices = NULL, width = "100%", options = list(`actions-box` = TRUE)
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
            "Results",
            fluidRow(
              column(
                6,
                tags$label("Heritabilities"),
                dataTableOutput(ns("herits")),
                actionButton("export_herits", "Export")
              ),
              column(
                6,
                # tags$label("Other metrics"),
                pickerInput(ns("select_metrics"), "Metric", multiple = F, choices = c("BLUPs","seBLUPs","BLUEs","seBLUEs"), width = "40%", inline = T),
                pickerInput(ns("select_environment_metrics"), "Environment", multiple = F, choices = NULL, width = "50%", inline = T),
                # pickerInput(ns("select_trait_metrics"), label = "Trait", multiple = F, choices = NULL, width = "50%"),
                dataTableOutput(ns("metrics_table")),
                actionButton("export_metrics", "Export")
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
      observe({
        req(rv$data)
        choices_env <- rv$data[,unique(study_name_app)]
        updatePickerInput(
          session,"select_environments",
          choices = choices_env,
          selected = choices_env,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        choices_traits <- rv$data[,unique(observations.observationVariableName)]
        updatePickerInput(
          session,"select_traits",
          choices = choices_traits,
          selected = choices_traits,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        ## the possible covariates
        # - have to be numerical
        # - must not be some columns (like ids)
        choices_cov <- names(rv$data)[unlist(rv$data[,lapply(.SD, is.numeric)])]
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

      observeEvent(input$select_environments, {
        ## update experimental design
        design_pui <- rv$study_names[study_name_app %in% input$select_environments,unique(exp_design_pui)]
        StatGenSTA_code <- exp_designs_corresp[BMS_pui == design_pui, StatGenSTA_code]
        if(length(StatGenSTA_code)==1 & length(StatGenSTA_code)>0){
          updatePickerInput(
            session, "model_design",
            selected = StatGenSTA_code
          )
        }else{
          updatePickerInput(
            session, "model_design",
            selected = "",
            options = list(
              title = "Select Model Design",
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
      })

      observeEvent(input$go_fit_model,{

        ###  create TD without the excluded observations

        ## exclude observations
        data_filtered <- rv$data[!(observations.observationDbId %in% rv$excluded_observations)]

        ## make 1 column per trait
        cols <- names(data_filtered)[!(names(data_filtered) %in% c("observations.observationVariableName", "observations.value"))]
        data_filtered_casted <- dcast(
          data = data_filtered,
          formula =  paste0(
            paste(cols, collapse = " + "), " ~ observations.observationVariableName"
          ),
          value.var = "observations.value"
        )

        ## duplicate and renames the columns used to create the TD
        data_filtered_casted[,genotype := germplasmName] # XXX using germplasmName instead of germplasmDbId makes the output easier to read but is it always OK?
        data_filtered_casted[,trial := study_name_app]
        data_filtered_casted[,loc := studyLocationDbId] # XXX
        data_filtered_casted[,repId := replicate]
        data_filtered_casted[,subBlock := observationUnitDbId]
        data_filtered_casted[,rowCoord := positionCoordinateY]
        data_filtered_casted[,colCoord := positionCoordinateX]

        TD <- createTD(
          data = data_filtered_casted,
          genotype = "genotype",
          trial = "trial",
          loc = "loc", # XXX
          repId = "repId",
          subBlock = "subBlock",
          rowCoord = "rowCoord",
          colCoord = "colCoord"
        )

        ### run the model
        a <- tryCatch(
          rv$fit <- fitTD(
            TD = TD,
            trials = input$select_environments,
            design = input$model_design,
            traits = input$select_traits,
            engine = input$model_engine,
            covariates = input$covariates
            # useCheckId = FALSE,
            # spatial = FALSE,
            # control = NULL
            # what = c("fixed", "random"),
          ),
          warning=function(w) { w },
          error=function(e){ e })
        mess <- a$message
        if(!is.null(mess)){
          showNotification(mess, type = "error", duration = NULL)
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
          session,"select_trait_metrics",
          choices = input$select_traits
          # selected = input$select_traits[1]
        )
        updatePickerInput(
          session, "select_environment_metrics",
          choices = input$select_environments
        )
      })

      ## show fitted models per trait and environments
      observeEvent(c(input$select_environment_fit, input$select_trait_fit),{
        req(input$select_environment_fit)
        req(input$select_trait_fit)

        output$fit_summary <- renderPrint({
          s_all <- summary(
            rv$fit,
            trait = input$select_trait_fit,
            trials = input$select_environment_fit
          )
          s <- lapply(input$select_environment_fit, function(env){
            summary(
              rv$fit,
              trait = input$select_trait_fit,
              trials = env
            )
          })
          names(s) <- input$select_environment_fit
          s["all environments"] <- s_all
          s
        })

        output$fit_residuals <- renderPlot({
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
                                   " Trait: ", input$select_trait_fit)
                    )
            )
          })
          plot_multi_env <- do.call("arrangeGrob", c(plot_envs, ncol=1))
          plot(plot_multi_env)
        },
        height=length(input$select_environment_fit)*500
        )

        output$fit_spatial <- renderPlot({
          plots_envs <- lapply(input$select_environment_fit, function(trial){
            plot(
              rv$fit,
              plotType = "spatial",
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
                                   " Trait: ", input$select_trait_fit)
                    )
            )
          })
          plot_multi_env <- do.call("arrangeGrob", c(plot_envs, ncol=1))
          plot(plot_multi_env)
        },
        height=length(input$select_environment_fit)*500
        )
      })

      output$herits <- renderDT({
        req(rv$fit)
        herits <- extractSTA(STA = rv$fit, what = "heritability")
        datatable(
          herits,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })
      metrics_table <- eventReactive(input$select_metrics,{
        req(input$select_metrics)
        metrics_table <- as.data.table(extractSTA(STA = rv$fit, what = input$select_metrics))
        entry_types <- unique(rv$data[,.(genotype=germplasmName, entryType)])
        setkey(metrics_table, genotype)
        setkey(entry_types, genotype)
        metrics_table <- entry_types[metrics_table]
        return(metrics_table)

      })
        output$metrics_table <- renderDataTable({
          req(input$select_metrics)
          req(input$select_environment_metrics)
          # req(input$select_trait_metrics)
          # metrics_table_filt <- metrics_table()[trial==input$select_environment_metrics, c("genotype", "entryType", input$select_trait_metrics), with = F]
          metrics_table_filt <- metrics_table()[trial==input$select_environment_metrics, c("genotype", "entryType"), with = F]
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
    }
  )
}
