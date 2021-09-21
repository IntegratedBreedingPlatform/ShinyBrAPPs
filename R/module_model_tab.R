#' @export
mod_model_ui <- function(id){
  ns <- NS(id)
  column(12,
         fluidRow(
           column(
             3,
             selectizeInput(
               ns("select_environments"), "Environments", multiple = TRUE, choices = NULL, width = "100%"
             )
           ),
           column(
             3,
             selectInput(ns("select_traits"), label = "Select Traits", multiple = TRUE, choices = NULL, width = "100%")
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
           tabsetPanel(
             tabPanel(
               "Fitted models",
               fluidRow(
                 column(
                   3,
                   selectizeInput(
                     ns("select_environment_fit"),"Environments", multiple = T, choices = NULL, width = "100%"
                   ),
                   selectizeInput(
                     ns("select_trait_fit"),"Environments", multiple = F, choices = NULL, width = "100%"
                   )
                 ),
                 column(
                   4,
                   verbatimTextOutput(ns("fit_summary"))
                 ),
                 column(
                   5,
                   plotOutput(ns("fit_spatial"))
                 )
               )
             ),
             tabPanel("Results")
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
        choices_env <- rv$data[,unique(environment_label)]
        updateSelectizeInput(
          session,"select_environments",
          choices = choices_env,
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
          germplasmDbId + studyDbId + studyLocationDbId + environment_label + germplasmName +
            replicate + observationUnitDbId + positionCoordinateY +
            positionCoordinateX ~ observations.observationVariableName,
          value.var = "observations.value"
        )

        TD <- createTD(
          data = data_filtered_casted,
          genotype = "germplasmName", # XXX using germplasmName instead of germplasmDbId makes the output easier to read but is it always OK?
          trial = "environment_label", # XXX environment_label is the concatenation of studyDbId and location
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
          summary(
            rv$fit,
            trait = input$select_trait_fit,
            trials = input$select_environment_fit
          )
        })

        output$fit_spatial <- renderPlot({
          plot(
            rv$fit,
            plotType = "spatial",
            trait = input$select_trait_fit,
            trials = input$select_environment_fit
          )
        })
      })
    }
  )
}
