#' @export
mod_gxe_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Data preparation", 
        bslib::layout_sidebar(
          width = 1/3,
          height = 800,
          sidebar=bslib::sidebar(#bslib::card(
            # bslib::card_header(
            #   h4('Options ', icon('screwdriver-wrench'))
            # ),
            width = 350,
            pickerInput(ns("picker_trait"), label = "Trait", choices = c()),
            pickerInput(ns("picker_env"),
                        label = "Environments",
                        choices = c(),
                        multiple = T),
                        #options  = pickerOptions(actionsBox = TRUE)),
            pickerInput(ns("picker_location"), label = "Choose environment detail to use as location", choices = c()),
            pickerInput(ns("picker_year"), label = "Choose environment detail to use as year", choices = c()),
            pickerInput(ns("picker_scenario"), label = "Choose environment details to use as scenario", choices = c(), multiple = T),
            materialSwitch(ns("check_combine_scenario"),label = "Create a full scenario column defined as the combination of selected variables",
                           value = FALSE, inline = T, status = "info"),
            pickerInput(ns("picker_germplasm_attr"), label = "Select germplasm attributes to display in GxE plots", choices = c(), multiple = T)
          ),#),
          #bslib::layout_column_wrap(
          #  width = 1/2,
            bslib::card(full_screen = TRUE,#height = "33%",
              plotOutput(ns("TD_boxplot"))
            ),
            bslib::card(full_screen = TRUE,
             plotOutput(ns("TD_scatterplots"))
            )
          #)
        )
      ),
      bslib::nav_panel(
        title = "Mixed model",
        bslib::layout_sidebar(
          sidebar=bslib::sidebar(          # bslib::card_header(
          #   h4('Options ', icon('screwdriver-wrench'))
          # ),
            width = 350,
            pickerInput(ns("picker_gxe_mm_env_struct"),
                      label = "Environment structure",
                      options = list(container = "body"), 
                      choices = c("Environments correspond to trials",
                                  "Trials form a factorial structure of locations x years",
                                  "Trials are nested within year",
                                  "Trials are nested within locations",
                                  "Trials correspond to locations within regions across years",
                                  "Trials are nested within scenarios")),
          actionBttn(ns("mm_run_model"),"Run model")
        ),
        bslib::card(full_screen = TRUE,
                    verbatimTextOutput(ns("MM_text_output"))
        )
        )
      )
      #bslib::nav_panel(
      #  title = "Finlay-Wilkinson",
      #  bslib::card(
      #    NULL
      #  )
      #),
      #bslib::nav_panel(
      #  title = "GGE",
      #  bslib::card(
      #    NULL
      #  )
      #)
    )
  )
}

#' @export
mod_gxe_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        #update trait dropdown
        trait_choices <- rv$column_datasource[source %in% c("GxE","Means"), .(cols)]
        updatePickerInput(
          session, "picker_trait",
          choices = trait_choices,
          selected = character(0)
        )
      })
      
      observeEvent(input$picker_trait,{
        ## update environments dropdown
        envs <- unique(rv$data_plot[!is.na(get(input$picker_trait)),.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]

        updatePickerInput(
          session, "picker_env",
          choices = env_choices
        )
       
      })
      
      observeEvent(input$picker_env,{
        ## update env details dropdowns
        colnames(rv$data_plot)
        env_details <- rv$column_datasource[source == "environment" & visible == T,]$cols
        
        updatePickerInput(
          session, "picker_scenario",
          choices = env_details,
          selected = character(0)
        )
        
        updatePickerInput(
          session, "picker_location",
          choices = env_details,
          selected = character(0)
        )
        
        updatePickerInput(
          session, "picker_year",
          choices = env_details,
          selected = character(0)
        )
        
        germplasm_attr <- rv$column_datasource[source == "germplasm" & visible == T,]$cols
        updatePickerInput(
          session, "picker_germplasm_attr",
          choices = germplasm_attr,
          selected = character(0)
        )
      })
      observe({
        if (length(input$picker_scenario)>1){
          shinyjs::enable(id = "check_combine_scenario")
        } else {
          shinyjs::disable(id = "check_combine_scenario")
          updateMaterialSwitch(session, "check_combine_scenario", value = FALSE)
        }
      })
      
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        req(input$picker_trait)
        req(input$picker_env)
        #browser()
        data2TD <- copy(rv$data_plot)
        if (length(input$picker_scenario)>1 & input$check_combine_scenario){
          data2TD[, scenarioFull:= do.call(paste0, .SD), .SDcols = input$picker_scenario]
        }
        rv$TD <- statgenSTA::createTD(data = data2TD[studyDbId%in%input$picker_env],
                                      genotype = "germplasmDbId",
                                      trial = "study_name_app")
        output$TD_boxplot <- renderPlot({
          if (!is.null(input$picker_scenario)){
            if ("scenarioFull"%in%names(data2TD)){
              plot(rv$TD,
                   plotType = "box",
                   traits = input$picker_trait,
                   colorTrialBy = "scenarioFull",
                   orderBy = "descending")
            } else {
              plot(rv$TD,
                   plotType = "box",
                   traits = input$picker_trait,
                   colorTrialBy = input$picker_scenario[1],
                   orderBy = "descending")
            }
          } else {
            plot(rv$TD,
                 plotType = "box",
                 traits = input$picker_trait,
                 orderBy = "descending") 
          }
        })
        if (length(input$picker_env)>1){
          shinyjs::show("TD_scatterplots")
          output$TD_scatterplots <- renderPlot({
            if (!is.null(input$picker_germplasm_attr)){
              if (!is.null(input$picker_scenario)){
                if ("scenarioFull"%in%names(data2TD)){
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorGenoBy = input$picker_germplasm_attr, 
                       colorTrialBy = "scenarioFull")
                } else {
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorGenoBy = input$picker_germplasm_attr, 
                       colorTrialBy = input$picker_scenario[1])
                }
              } else {
                plot(rv$TD, plotType = "scatter",
                     traits = input$picker_trait,
                     colorGenoBy = input$picker_germplasm_attr)
              }            
            } else {
              if (!is.null(input$picker_scenario)){
                if ("scenarioFull"%in%names(data2TD)){
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorTrialBy = "scenarioFull")
                } else {
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorTrialBy = input$picker_scenario[1])
                }
              } else {
                plot(rv$TD, plotType = "scatter",
                     traits = input$picker_trait)
              }            
            }
          })          
        } else {
          shinyjs::hide("TD_scatterplots")
        }
      })
    }
  )
}