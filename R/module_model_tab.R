#' @export
mod_model_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    fluidRow(
      column(
        3,
        selectInput(ns("select_traits"), label = "Select Traits", multiple = TRUE, choices = NULL, width = "100%")
      ),
      column(
        3,
        selectizeInput(
          ns("select_environments"), "Environments", multiple = TRUE, choices = NULL, width = "100%"
        )
      ),
      column(
        3,
        selectInput(ns("model_design"), "Select Model Design",
                    choices = c("incomplete block design" = "ibd",
                                "resolvable incomplete block design" = "res.ibd",
                                "randomized complete block design" = "rcbd",
                                "row column design" = "rowcol",
                                "resolvable row column design" = "res.rowcol"),
                    width = "100%")
      ),
      column(
        2,
        selectInput(ns("model_engine"), "Select Modelling Engine",
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
      bsCollapse(open = NULL,
                 bsCollapsePanel(
                   title = "Advanced fitting options",
                   # XXX
                   tags$p("NOT FUNCTIONNAL"),
                   pickerInput(inputId = 'xcol2',
                               label = 'other param 1',
                               choices = names(iris),
                               options = list(`style` = "btn-info")),

                   pickerInput(inputId = 'ycol2',
                               label = 'other param 2',
                               choices = names(iris),
                               selected = names(iris)[[2]],
                               options = list(`style` = "btn-warning")),

                   sliderInput(inputId = 'clusters2',
                               label = 'numeric param 1',
                               value = 3,
                               min = 1, max = 9)
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
                selectizeInput(
                  ns("select_trait_fit"),"Trait", multiple = F, choices = NULL, width = "100%"
                )
              ),
              column(
                3,
                selectizeInput(
                  ns("select_environment_fit"),"Environments", multiple = T, choices = NULL, width = "100%"
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
            tags$label("Heritabilities"),
            dataTableOutput(ns("herits")),
            tags$label("BLUPs"),
            dataTableOutput(ns("BLUPs")),
            tags$label("BLUEs"),
            dataTableOutput(ns("BLUEs")),
            tags$label("seBLUPs"),
            dataTableOutput(ns("seBLUPs")),
            tags$label("seBLUEs"),
            dataTableOutput(ns("seBLUEs"))
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
        updateSelectizeInput(
          session,"select_environments",
          choices = choices_env,
          selected = choices_env,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        choices_traits <- rv$data[,unique(observations.observationVariableName)]
        updateSelectizeInput(
          session,"select_traits",
          choices = choices_traits,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })

      observeEvent(input$go_fit_model,{

        ###  create TD without the excluded observations

        ## exclude observations
        data_filtered <- rv$data[!(observations.observationDbId %in% rv$excluded_observations)]

        ## make 1 column per trait
        data_filtered_casted <- dcast(
          data_filtered,
          germplasmDbId + studyDbId + studyLocationDbId + study_name_app + germplasmName +
            replicate + observationUnitDbId + positionCoordinateY +
            positionCoordinateX ~ observations.observationVariableName,
          value.var = "observations.value"
        )

        TD <- createTD(
          data = data_filtered_casted,
          genotype = "germplasmName", # XXX using germplasmName instead of germplasmDbId makes the output easier to read but is it always OK?
          trial = "study_name_app",
          loc = "studyLocationDbId", # XXX
          repId = "replicate",
          subBlock = "observationUnitDbId",
          rowCoord = "positionCoordinateY",
          colCoord = "positionCoordinateX"
        )

        ### run the model

        a <- tryCatch(
          rv$fit <- fitTD(
            TD = TD,
            trials = input$select_environments,
            design = input$model_design,
            traits = input$select_traits,
            engine = input$model_engine
          ),
          warning=function(w) { w },
          error=function(e){ e })
        mess <- a$message
        if(!is.null(mess)){
          showNotification(mess, type = "error", duration = NULL)
        }
        req(rv$fit)

        ## update selectors

        updateSelectizeInput(
          session, "select_environment_fit",
          choices = input$select_environments
          # options = list(
          #   placeholder = 'Select 1 environment',
          #   onInitialize = I('function() { this.setValue(""); }')
          # )
        )
        updateSelectizeInput(
          session, "select_trait_fit",
          choices = input$select_traits,
          selected = input$select_traits[1]
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
      output$BLUPs <- renderDT({
        req(rv$fit)
        BLUPs <- extractSTA(STA = rv$fit, what = "BLUPs")
        datatable(
          BLUPs,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })
      output$BLUEs <- renderDT({
        req(rv$fit)
        BLUEs <- extractSTA(STA = rv$fit, what = "BLUEs")
        datatable(
          BLUEs,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })
      output$seBLUEs <- renderDT({
        req(rv$fit)
        seBLUEs <- extractSTA(STA = rv$fit, what = "seBLUEs")
        datatable(
          seBLUEs,
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "500px",
            scrollCollapse = T,
            dom = 't'
          ))
      })
      output$seBLUPs <- renderDT({
        req(rv$fit)
        seBLUPs <- extractSTA(STA = rv$fit, what = "seBLUPs")
        datatable(
          seBLUPs,
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
