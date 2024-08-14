#' @export
# UI ####
mod_gxe_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::navset_tab(
      bslib::nav_panel(
        ## Data prep panel ####
        title = "Data preparation", 
        ### Sidebar ####
        bslib::layout_sidebar(
          width = 1/3,
          height = 800,
          sidebar=bslib::sidebar(#bslib::card(
            # bslib::card_header(
            #   h4('Options ', icon('screwdriver-wrench'))
            # ),
            width = 350,
            pickerInput(ns("picker_trait"), label = "Trait", choices = c()),
            pickerInput(ns("picker_germplasm_level"), label = "Germplasm level", choices = c("germplasmDbId","germplasmName"), selected = "GermplasmDbId"),
            pickerInput(ns("picker_env"),
                        label = "Environments",
                        choices = c(),
                        multiple = T),
                        #options  = pickerOptions(actionsBox = TRUE)),
            pickerInput(ns("picker_location"), label = "Choose environment detail to use as location", choices = c()),
            pickerInput(ns("picker_region"), label = "Choose environment detail to use as region", choices = c()),
            pickerInput(ns("picker_year"), label = "Choose environment detail to use as year", choices = c()),
            pickerInput(ns("picker_scenario"), label = "Choose environment details to use as scenario", choices = c(), multiple = T),
            p("NB: a scenario column will be defined as the combination of selected variables"),
            #materialSwitch(ns("check_combine_scenario"),label = "Create a full scenario column defined as the combination of selected variables",
            #               value = FALSE, inline = T, status = "info"),
            pickerInput(ns("picker_germplasm_attr"), label = "Select germplasm attributes to display in GxE plots", choices = c(), multiple = T)
          ),#),
          #bslib::layout_column_wrap(
          #  width = 1/2,
          ### Plots ####
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
        ## Mixed model panel ####
        title = "Mixed model",
        bslib::layout_sidebar(
          ### Sidebar ####
          sidebar=bslib::sidebar(          # bslib::card_header(
          #   h4('Options ', icon('screwdriver-wrench'))
          # ),
            width = 350,
            pickerInput(ns("picker_gxe_mm_env_struct"),
                      label = "Environment structure",
                      options = list(container = "body"), 
                      choices = c(`Environments correspond to trials`=1,
                                  `Trials form a factorial structure of locations x years`=2,
                                  `Trials are nested within year`=3,
                                  `Trials are nested within locations`=4,
                                  `Trials correspond to locations within regions across years`=5,
                                  `Trials are nested within scenarios`=6)),
          actionBttn(ns("mm_run_model"),"Run model")
        ),
        # bslib::layout_columns(
        #   bslib::card(full_screen = TRUE,
        #               bslib::card_header(
        #                 class = "bg-dark",
        #                 "Model output"
        #               ),
        #               bslib::card_body(
        #                 verbatimTextOutput(ns("MM_text_output")) 
        #               )
        #   ),
        #   bslib::card(full_screen = TRUE,
        #               bslib::card_header(
        #                 class = "bg-dark",
        #                 "Diagnostics"
        #               ),
        #               bslib::card_body(
        #                 verbatimTextOutput(ns("MM_diagnostics"))
        #               )
        #   ),
        #   bslib::card(full_screen = TRUE,
        #               bslib::card_header(
        #                 class = "bg-dark",
        #                 "Variance components"
        #               ),
        #               bslib::card_body(
        #                 verbatimTextOutput(ns("MM_vc"))
        #               )
        #               
        #   )
        # ),
        ### Results ####
        bslib::layout_columns(
          #### Accordion textual results ####
          bslib::accordion(id = ns("MM_accord1"),
            #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),
            bslib::accordion_panel(title = "Model output", 
                          verbatimTextOutput(ns("MM_text_output")) 
            ),
            bslib::accordion_panel(title = "Diagnostics", 
                          verbatimTextOutput(ns("MM_diagnostics"))
            ),
            bslib::accordion_panel(title = "Variance components and heritability", 
                          verbatimTextOutput(ns("MM_vc"))
            )
          ),
          #### Accordion plot and predict results ####
          bslib::accordion(id = ns("MM_accord2"),
                           #actionButton(ns("expand_MM_accord2"),label = "Open all", class = "btn btn-info"),
                           bslib::accordion_panel(title = "Model plot", 
                                                  plotOutput(ns("MM_plot_output")) 
                           ),
                           bslib::accordion_panel(title = "Predictions",
                                                  pickerInput(ns("MM_predict_level"), label = "Prediction level", choices = c("genotype")),
                                                  DT::dataTableOutput(ns("MM_predictions"))
                           )
          )
          
        )
        )
      ),
      bslib::nav_panel(
        ## FW panel ####
        title = "Finlay Wilkinson",
        bslib::layout_sidebar(
          ### Sidebar ####
          sidebar=bslib::sidebar(
            width = 350,
            actionBttn(ns("FW_run"), "Run FW analysis"),
            pickerInput(ns("FW_picker_plot_type"),
                        label="Plot type",
                        choices = c("scatter", "line", "trellis", "scatterFit"), selected = "line"),
            pickerInput(ns("FW_picker_color_by"), label="Color by", choices = c("sensitivity clusters")),
            #materialSwitch(ns("FW_cluster_sensitivity"), "Color by sensitivity clusters", value = FALSE, status = "info"),
            numericInput(ns("FW_cluster_sensitivity_nb"),"Number of clusters", min = 1, max = 10, step = 1, value = 1),
            pickerInput(ns("FW_picker_cluster_on"), label="Cluster on", choices = c(sensitivity="sens", `genotype means`="genMean"), multiple = TRUE, selected = "sens")
          ),
          bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("FW_accord1"),
                             #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("FW_text_output")) 
                             ),
                             bslib::accordion_panel(title = "FW plot",
                                                    bslib::layout_columns(
                                                    bslib::card(full_screen = T,
                                                      #plotlyOutput(ns("FW_plot"))
                                                      plotOutput(ns("FW_plot"))
                                                    ),
                                                    bslib::card(DT::dataTableOutput(ns("FW_sens_clusters_DT")))
                                                    )
                             )
            )
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

# SERVER ####
#' @export
mod_gxe_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ## observe data and update Trit picker ####
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        #update trait dropdown
        trait_choices <- rv$column_datasource[source %in% c("GxE","Means")]$cols
        if ("startDate"%in%colnames(rv$study_metadata)){
          rv$data_gxe <- rv$data_plot[unique(rv$study_metadata[,.(studyDbId, study_startYear = year(as.POSIXct(startDate)))]), on=.(studyDbId)]
        } else {
          rv$data_gxe <- rv$data_plot
        }
        
        updatePickerInput(
          session, "picker_trait",
          choices = trait_choices,
          selected = character(0)
        )
      })
      ## update env picker when trait is chosen ####
      observeEvent(input$picker_trait,{
        ## update environments dropdown
        envs <- unique(rv$data_gxe[!is.na(get(input$picker_trait)),.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]

        updatePickerInput(
          session, "picker_env",
          choices = env_choices
        )
       
      })
      ## update pickers when environment is chosen ####
      observeEvent(input$picker_env,{
        ## update env details dropdowns
        colnames(rv$data_gxe)
        env_details <- c(rv$column_datasource[source == "environment" & visible == T,]$cols, "study_startYear")
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
          session, "picker_region",
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
      ## update pickers location, year, region, when one of them is chosen ####
      observeEvent(input$picker_location, {
        env_details <- c(rv$column_datasource[source == "environment" & visible == T,]$cols, "study_startYear")
        updatePickerInput(
          session, "picker_region",
          choices = env_details[!env_details==input$picker_location],
          selected = ifelse(is.null(input$picker_region),character(0),input$picker_region)
        )
        updatePickerInput(
          session, "picker_year",
          choices = env_details[!env_details==input$picker_location],
          selected = ifelse(is.null(input$picker_year),character(0),input$picker_year)
        )
      })
      
      observeEvent(input$picker_region, {
        env_details <- c(rv$column_datasource[source == "environment" & visible == T,]$cols, "study_startYear")
        updatePickerInput(
          session, "picker_location",
          choices = env_details[!env_details==input$picker_region],
          selected = ifelse(is.null(input$picker_location),character(0),input$picker_location)
        )
        updatePickerInput(
          session, "picker_year",
          choices = env_details[!env_details==input$picker_region],
          selected = ifelse(is.null(input$picker_year),character(0),input$picker_year)
        )
      })
      
      observeEvent(input$picker_year, {
        env_details <- c(rv$column_datasource[source == "environment" & visible == T,]$cols, "study_startYear")
        updatePickerInput(
          session, "picker_location",
          choices = env_details[!env_details==input$picker_year],
          selected = ifelse(is.null(input$picker_location),character(0),input$picker_location)
        )
        updatePickerInput(
          session, "picker_region",
          choices = env_details[!env_details==input$picker_year],
          selected = ifelse(is.null(input$picker_region),character(0),input$picker_region)
        )
      })
      observeEvent(input$picker_germplasm_attr, {
        updatePickerInput(
          session, "FW_picker_color_by",
          choices = c(input$picker_germplasm_attr,"sensitivity clusters"),
          selected = character(0)
        )
      }
      )
      ## 
      #observe({
      #  if (length(input$picker_scenario)>1){
      #    shinyjs::enable(id = "check_combine_scenario")
      #  } else {
      #    shinyjs::disable(id = "check_combine_scenario")
      #    updateMaterialSwitch(session, "check_combine_scenario", value = FALSE)
      #  }
      #})
      ## main observer to build TD object and output basic plots ####
      observe({
        req(rv$data_gxe)
        req(rv$column_datasource)
        req(input$picker_trait)
        req(input$picker_env)
        #browser()
        data2TD <- copy(rv$data_gxe)
        data2TD[, germplasmDbId:=paste0(germplasmDbId," (",germplasmName,")")]
        if (length(input$picker_scenario)>1){ #& input$check_combine_scenario){
          data2TD[, scenario:= do.call(paste0, .SD), .SDcols = input$picker_scenario]
        } else {
          if (length(input$picker_scenario)==1){
            setnames(data2TD, old=input$picker_scenario, new="scenario")
          }
        }
        if (!is.null(input$picker_year)){
          setnames(data2TD, old=input$picker_year, new="year")
        }
        if (!is.null(input$picker_location)){
          setnames(data2TD, old=input$picker_location, new="loc")
        }
        if (!is.null(input$picker_region)){
          setnames(data2TD, old=input$picker_region, new="region")
        }
        rv$TD <- statgenSTA::createTD(data = data2TD[studyDbId%in%input$picker_env],
                                      genotype = input$picker_germplasm_level,
                                      trial = "study_name_app")
        output$TD_boxplot <- renderPlot({
          if (!is.null(input$picker_scenario)){
            #if ("scenarioFull"%in%names(data2TD)){
              plot(rv$TD,
                   plotType = "box",
                   traits = input$picker_trait,
                   colorTrialBy = "scenario",
                   orderBy = "descending")
            #} else {
            #  plot(rv$TD,
            #       plotType = "box",
            #       traits = input$picker_trait,
            #       colorTrialBy = input$picker_scenario[1],
            #       orderBy = "descending")
            #}
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
                #if ("scenarioFull"%in%names(data2TD)){
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorGenoBy = input$picker_germplasm_attr, 
                       colorTrialBy = "scenario")
                #} else {
                #  plot(rv$TD, plotType = "scatter",
                #       traits = input$picker_trait,
                #       colorGenoBy = input$picker_germplasm_attr, 
                #       colorTrialBy = input$picker_scenario[1])
                #}
              } else {
                plot(rv$TD, plotType = "scatter",
                     traits = input$picker_trait,
                     colorGenoBy = input$picker_germplasm_attr)
              }            
            } else {
              if (!is.null(input$picker_scenario)){
                #if ("scenarioFull"%in%names(data2TD)){
                  plot(rv$TD, plotType = "scatter",
                       traits = input$picker_trait,
                       colorTrialBy = "scenario")
                #} else {
                #  plot(rv$TD, plotType = "scatter",
                #       traits = input$picker_trait,
                #       colorTrialBy = input$picker_scenario[1])
                #}
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
      
      ## MM model ####
      ### Run MM model ####
      observeEvent(input$mm_run_model,{
        #browser()
        rv$TDVarComp <- switch(input$picker_gxe_mm_env_struct,
               `1`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait), error=function(e) e)},
               `2`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, locationYear = TRUE), error=function(e) e)},
               `3`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, nestingFactor = "year"), error=function(e) e)},
               `4`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, nestingFactor = "loc"), error=function(e) e)},
               `5`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, regionLocationYear = TRUE), error=function(e) e)},
               `6`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, nestingFactor = "scenario"), error=function(e) e)})
        output$MM_text_output <- renderPrint({
          if ("varComp"%in%class(rv$TDVarComp)){
            summary(rv$TDVarComp)
          } else {
            rv$TDVarComp
          }
        })
        output$MM_diagnostics <- renderPrint({
          if ("varComp"%in%class(rv$TDVarComp)){
            diagnostics(rv$TDVarComp)
          } else {
            "Model failed"
          }
        })
        output$MM_vc <- renderPrint({
          if ("varComp"%in%class(rv$TDVarComp)){
            list(`Variance components`=vc(rv$TDVarComp), heritability=herit(rv$TDVarComp))
          } else {
            "Model failed"
          }
        })
        output$MM_plot_output <- renderPlot({
          if ("varComp"%in%class(rv$TDVarComp)){
            plot(rv$TDVarComp)
          } else {
            "Model failed"
          }
        })
        output$MM_predictions <- DT::renderDataTable({
          if ("varComp"%in%class(rv$TDVarComp)){
            predict(rv$TDVarComp)
          } else {
            data.table()[]
          }
        })
        bslib::accordion_panel_set(id="MM_accord1", values=TRUE)
        bslib::accordion_panel_set(id="MM_accord2", values=TRUE)
      })
      observe({
        if (is.null(rv$TDVarComp$nestingFactor)){
          predict_levels <- "genotype"
          if (dropsVarComp$useLocYear){
            predict_levels <- c("genotype","trial")
          }
        } else {
          predict_levels <- c("genotype",rv$TDVarComp$nestingFactor)
        }
        updatePickerInput(session, "MM_predict_level", choices = predict_levels, selected = "genotype")
      })
      ### Change prediction level ####
      observeEvent(input$MM_predict_level,{
        output$MM_predictions <- DT::renderDataTable({
          if ("varComp"%in%class(rv$TDVarComp)){
            predict(rv$TDVarComp, predictLevel=input$MM_predict_level)
          } else {
            data.table()[]
          }
        })
        
      })
      ### Expand all accordion panels ####
      #observeEvent(input$expand_MM_accord1,{
      #  bslib::accordion_panel_set(id="MM_accord1", values=TRUE)
      #})
      #observeEvent(input$expand_MM_accord2,{
      #  bslib::accordion_panel_set(id="MM_accord2", values=TRUE)
      #})
      ## FW ####
      ### Run FW ####
      observeEvent(input$FW_run,{
        rv$TDFW <- tryCatch(gxeFw(TD = rv$TD, trait = input$picker_trait), error=function(e) e)
        output$FW_text_output <- renderPrint({
          if ("FW"%in%class(rv$TDFW)){
            summary(rv$TDFW)
          } else {
            rv$TDFW
          }
        })
      })
      ### FW plot ####
      observe({
        output$FW_plot <- renderPlot({
        #output$FW_plot <- renderPlotly({
          TDFWplot <- rv$TDFW
          if (is.null(input$FW_picker_color_by)){
            #ggplotly(plot(TDFWplot, plotType = input$FW_picker_plot_type))
            plot(TDFWplot, plotType = input$FW_picker_plot_type)
          } else {
            if (input$FW_picker_color_by=="sensitivity clusters"){
              #browser()
              sensclust <- data.table(rv$TDFW$estimates)
              sensclust <- sensclust[!is.na(sens)]
              sensclust <- sensclust[,sensitivity_cluster:=kmeans(.SD,centers = input$FW_cluster_sensitivity_nb)$cluster, .SDcols = input$FW_picker_cluster_on]
              rv$sensclust <- sensclust
              TDFWplot$TD <- lapply(TDFWplot$TD, function(a) data.table(a)[sensclust, on=.(genotype)])
              #ggplotly(plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy="sensitivity_cluster"))
              plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy="sensitivity_cluster")
            } else {
              #ggplotly(plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy=input$FW_picker_color_by))
              plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy=input$FW_picker_color_by)
            }
          }
        })
      })
      observe({
        output$FW_sens_clusters_DT <- DT::renderDataTable(rv$sensclust[,.(genotype, sensitivity_cluster, sens, genMean)])
      })
    }
  )
}