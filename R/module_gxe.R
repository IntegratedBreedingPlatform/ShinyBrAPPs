#' @export
# UI ####
mod_gxe_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    chooseSliderSkin("Flat", color = "#1b95b2"),
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
            pickerInput(ns("picker_trait"), label = tags$span(style="color: red;","Trait"), choices = c()),
            materialSwitch(ns("use_weights"),label = "Use weights", value = FALSE, inline = T, status = "info"),
            pickerInput(ns("weight_var"), label = "Weight variable", choices = c()),
            pickerInput(ns("picker_germplasm_level"), label = tags$span(style="color: red;","Germplasm level"), choices = c("germplasmDbId","germplasmName"), selected = "GermplasmDbId"),
            pickerInput(ns("picker_env_variable"), label = tags$span(style="color: red;","Variable to use as Environment Name"), choices = c()),
            pickerInput(ns("picker_env"),
                        label = "Environments",
                        choices = c(),
                        multiple = T),
                        #options  = pickerOptions(actionsBox = TRUE)),
            uiOutput(ns("sliderUI_exclude_geno_nb_env")),
            #sliderInput(ns("exclude_geno_nb_env"),label = "Exclude genotypes that are present in less than X environments", value = 1, min = 1, max = 10, step = 1 ),
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
          bslib::layout_columns(
            bslib::card(full_screen = TRUE,
             plotOutput(ns("TD_scatterplots"))
            ),
            bslib::card(full_screen = FALSE,
                        bslib::card_header("Excluded genotypes"),
                        DT::dataTableOutput(ns("TD_excluded_geno"))
            )
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
            div(style="display: flex;",
                actionBttn(ns("mm_run_model"),"Run model", block = TRUE),
                a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#vcomp",icon("fas fa-question-circle"), target="_blank")),
            pickerInput(ns("picker_gxe_mm_env_struct"),
                      label = "Environment structure",
                      options = list(container = "body"), 
                      choices = c(`Trials correspond to environments`=1,
                                  `Trials form a factorial structure of locations x years`=2,
                                  `Trials are nested within year`=3,
                                  `Trials are nested within locations`=4,
                                  `Trials correspond to locations within regions across years`=5,
                                  `Trials are nested within scenarios`=6)),
          shiny::downloadButton(ns("MM_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
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
            div(style="display: flex;",
            actionBttn(ns("FW_run"), "Run FW analysis", block = TRUE),
            a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#fw",icon("fas fa-question-circle"), target="_blank")),
            
            pickerInput(ns("FW_picker_plot_type"),
                        label="Plot type",
                        choices = c("scatter", "line", "trellis", "scatterFit"), selected = "line"),
            pickerInput(ns("FW_picker_color_by"), label="Color by", multiple = F, choices = c("Nothing","sensitivity clusters"), selected = "Nothing"),
            #materialSwitch(ns("FW_cluster_sensitivity"), "Color by sensitivity clusters", value = FALSE, status = "info"),
            numericInput(ns("FW_cluster_sensitivity_nb"),"Number of clusters", min = 2, max = 8, step = 1, value = 2),
            pickerInput(ns("FW_picker_cluster_on"), label="Cluster on", choices = c(sensitivity="Sens", `genotype means`="GenMean"), multiple = TRUE, selected = "Sens"),
            shiny::downloadButton(ns("FW_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
          ),
          bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("FW_accord1"),
                             #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),

                             bslib::accordion_panel(title = "FW plot",
                             #                       bslib::layout_columns(
                                                    bslib::card(full_screen = T, height = "800",
                                                      #plotlyOutput(ns("FW_plot"))
                                                      #plotOutput(ns("FW_plot"))
                                                      #materialSwitch(ns("FW_plot_interact"),"Interactive plot", value=FALSE, status="info"),
                                                      plotOutput(ns("FW_plot"))
                                                    )),
                             bslib::accordion_panel(title = "Germplasm list and clusters",
                                                    #bslib::card(DT::dataTableOutput(ns("FW_selected_obs_DT"))),
                                                    bslib::card(
                                                      bslib::card_body(DT::dataTableOutput(ns("FW_sens_clusters_DT"))),
                                                      bslib::card_footer(div(style="display: flex;gap: 10px;",
                                                                             shiny::actionButton(ns("create_groups_from_sensclusters"), "Create groups from clusters", icon = icon(NULL), class = "btn btn-info"),
                                                                             shiny::actionButton(ns("create_groups_from_selgeno"),label = "Create group from selected genotypes", icon = icon(NULL), class = "btn btn-info")
                                                                             )
                                                                         )
                                                      )
                             ),
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("FW_text_output")) 
                             )
            )
          )
        )
      ),
      bslib::nav_panel(
        ## GGE panel ####
        title = "GGE",
        bslib::layout_sidebar(
          ### Sidebar ####
          sidebar=bslib::sidebar(
            width = 350,
            div(style="display: flex;",
                actionBttn(ns("GGE_run"), "Run GGE analysis", block = TRUE),
                a(href="https://tiagoolivoto.github.io/metan/articles/vignettes_gge.html",icon("fas fa-question-circle"), target="_blank")),
            bslib::accordion(id = ns("GGE_adv_settings_acc"),
                             bslib::accordion_panel(title = "Advanced settings", value = "advs",
                                                    pickerInput(ns("GGE_advs_centering"), label = "centering", options = list(container = "body"), choices = c(none=0, global=1, environment=2, double=3), selected = 2),
                                                    pickerInput(ns("GGE_advs_scaling"), label = "scaling", options = list(container = "body"), choices = c(none=0, sd=1), selected = 0),
                                                    pickerInput(ns("GGE_advs_svp"), label = "svp", options = list(container = "body"), choices = c(genotype=1, environment=2, symmetrical=3), selected = 2))),
            pickerInput(ns("GGE_picker_plot_type"),
                        label="Plot type",
                        choices = c(`1-A basic biplot`=1,
                                    `2-Mean performance vs. stability`=2,
                                    `3-Which-won-where`=3,
                                    `4-Discriminativeness vs. representativeness`=4,
                                    `5-Examine an environment`=5,
                                    `6-Ranking environments`=6,
                                    `7-Examine a genotype`=7,
                                    `8-Ranking genotype`=8,
                                    `9-Compare two genotypes`=9,
                                    `10- Relationship among environments`=10),
                        selected = 1),
            pickerInput(ns("GGE_picker_env_select"), label="Plot type 5 - Select an environment", multiple = F, choices = c()),
            pickerInput(ns("GGE_picker_gen_select"), label="Plot type 7&9 - Select a genotype", multiple = F, choices = c()),
            pickerInput(ns("GGE_picker_gen2_select"), label="Plot type 9 - Select a second genotypes", multiple = F, choices = c()),
            shiny::downloadButton(ns("GGE_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
            
          ),
          bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("GGE_accord1"),
                             bslib::accordion_panel(title = "GGE plot",
                                                    bslib::layout_sidebar(
                                                      bslib::card(full_screen = T,height = "800",max_height = "800",
                                                                  plotOutput(ns("GGE_plot"))
                                                      ),
                                                      sidebar=bslib::sidebar(position = "right", title = "Advanced plot settings", open = FALSE,#full_screen = F,
                                                                             bslib::card(full_screen = F,height = "735",max_height = "735",
                                                                                         sliderInput(ns("GGE_plot_title_size"), label = "plot_title_size", min = 10, max = 30, value = 12, step = 1),
                                                                  sliderInput(ns("GGE_plot_size.text.gen"), label = "size.text.gen", min = 1, max = 8, value = 3.5, step = 0.5),
                                                                  sliderInput(ns("GGE_plot_repulsion"), label="plot_repulsion", value = 1, min=1, max=10, step=0.5),
                                                                  sliderInput(ns("GGE_plot_max_overlaps"), label="plot_max_overlaps", value = 20, min=5, max=50, step=1),
                                                                  sliderInput(ns("GGE_plot_size.shape"), label="plot_size.shape", value = 2.2, min=1, max=10, step=0.1),
                                                                  sliderInput(ns("GGE_plot_size.shape.win"), label="plot_size.shape.win", value = 3.2, min=1, max=10, step=0.1),
                                                                  sliderInput(ns("GGE_plot_size.stroke"), label="plot_size.stroke", value = 0.3, min=1, max=5, step=0.1),
                                                                  sliderInput(ns("GGE_plot_col.alpha"), label="plot_col.alpha", value = 1, min=0, max=1, step=0.1),
                                                                  sliderInput(ns("GGE_plot_col.alpha.circle"), label="plot_col.alpha.circle", value = 0.5, min=0, max=1, step=0.1),
                                                                  sliderInput(ns("GGE_plot_size.text.env"), label="plot_size.text.env", value = 3.5, min=1, max=10, step=0.5),
                                                                  sliderInput(ns("GGE_plot_size.text.lab"), label="plot_size.text.lab", value = 12, min=1, max=20, step=0.5),
                                                                  sliderInput(ns("GGE_plot_size.text.win"), label="plot_size.text.win", value = 4.5, min=1, max=10, step=0.5),
                                                                  sliderInput(ns("GGE_plot_size.line"), label="plot_size.line", value = 0.5, min=0, max=5, step=0.1),
                                                                  sliderInput(ns("GGE_plot_axis_expand"), label="plot_axis_expand", value = 1.2, min=0, max=2, step=0.1),
                                                                  colorPickr(ns("GGE_plot_col.stroke"), label="plot_col.stroke", selected = "black", theme = "monolith", update = "changestop", opacity = TRUE),
                                                                  colorPickr(ns("GGE_plot_col.gen"), label="plot_col.gen", selected = "blue", theme = "monolith", update = "changestop", opacity = TRUE),
                                                                  colorPickr(ns("GGE_plot_col.env"), label="plot_col.env", selected = "forestgreen", theme = "monolith", update = "changestop", opacity = TRUE),
                                                                  colorPickr(ns("GGE_plot_col.line"), label="plot_col.line", selected = "forestgreen", theme = "monolith", update = "changestop", opacity = TRUE),
                                                                  colorPickr(ns("GGE_plot_col.circle"), label="plot_col.circle", selected = "gray", theme = "monolith", update = "changestop"), opacity = TRUE))
                                                      
                                                    )
                             ),
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("GGE_text_output")) 
                             )
            )
          )
        )
      ),
      bslib::nav_panel(
        ## AMMI panel ####
        title = "AMMI",
        bslib::layout_sidebar(
          ### Sidebar ####
          sidebar=bslib::sidebar(
            width = 350,
            div(style="display: flex;",
                actionBttn(ns("AMMI_run"), "Run AMMI analysis", block = TRUE),
                a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#am",icon("fas fa-question-circle"), target="_blank")),
            pickerInput(ns("AMMI_nPC"),
                        label="Number of PC",
                        choices = c("Auto"), selected = "Auto"),
            materialSwitch(ns("AMMI_center"), "center", value = TRUE, status = "info"),
            pickerInput(ns("AMMI_excludeGeno"), label="Exclude genotypes", multiple = T, choices = c(), options = pickerOptions(liveSearch = TRUE)),
            materialSwitch(ns("AMMI_byYear"), "Run by year", value = FALSE, status = "info"),
            hr(style = "border-top: 1px solid #000000;"),
            pickerInput(ns("AMMI_plotType"), label="Plot type", choices = c("AMMI1", "AMMI2"), selected = "AMMI2"),
            pickerInput(ns("AMMI_primAxis"), label="Primary axis", choices = c()),
            pickerInput(ns("AMMI_secAxis"), label="Second axis", choices = c()),
            materialSwitch(ns("AMMI_plotGeno"), "Plot genotypes", value = TRUE, status = "info"),
            sliderInput(ns("AMMI_scale"), label = "scale", min = 0, max = 1, value = 1, step=0.1),
            materialSwitch(ns("AMMI_plotConvHull"), "Plot convex hull around the genotypes", value = FALSE, status = "info"),
            pickerInput(ns("AMMI_colorGenoBy"), label="Color genotypes by", choices = "Nothing", selected = "Nothing"),
            pickerInput(ns("AMMI_colorEnvBy"), label="Color environments by", choices = c()),
            pickerInput(ns("AMMI_rotatePC"), label="Rotate PC", choices = c()),
            shiny::downloadButton(ns("AMMI_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
            
          ),
          bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("AMMI_accord1"),
                             bslib::accordion_panel(title = "AMMI plot",
                                                   bslib::layout_sidebar(
                                                     bslib::card(full_screen = T,height = "800",max_height = "800",
                                                                 plotOutput(ns("AMMI_plot"))
                                                     ),
                                                     sidebar=bslib::sidebar(position = "right", title = "Advanced plot settings", open = FALSE,
                                                                            bslib::card(full_screen = F,height = "735",max_height = "735",
                                                                                        sliderInput(ns("AMMI_plot_sizeGeno"), label = "sizeGeno", min = 0, max = 10, value = 0, step = 1),
                                                                                        materialSwitch(ns("AMMI_plot_repel"), label = "use_ggrepel", value = FALSE, status = "info"),
                                                                                        sliderInput(ns("AMMI_plot_repulsion"), label="plot_repulsion", value = 1, min=1, max=10, step=0.5),
                                                                                        sliderInput(ns("AMMI_plot_max_overlaps"), label="plot_max_overlaps", value = 20, min=5, max=50, step=1),
                                                                                        sliderInput(ns("AMMI_plot_sizeEnv"), label = "sizeEnv", min = 0, max = 10, value = 3, step = 1),
                                                                                        sliderInput(ns("AMMI_plot_envFactor"), label = "envFactor", min = 0, max = 5, value = 1, step = 0.1),
                                                                                        textInput(ns("AMMI_plot_title"),label = "Title",value = NULL))
                                                                            )
                                                     )
                             ),
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("AMMI_text_output")) 
                             )
            )
          )
        )
      )
    )
  )
}

# SERVER ####
#' @export
mod_gxe_server <- function(id, rv, parent_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session){
      
      bslib::accordion_panel_close("GGE_adv_settings_acc", values="advs", session = session)
      ## observe data and update Trit picker ####
      rv$selected_genotypes <- NULL
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        #update trait dropdown
        trait_choices <- rv$column_datasource[source %in% c("GxE","Means") & grepl(variable_regexp,cols)]$cols
        weight_choices <- rv$column_datasource[source %in% c("GxE","Means") & grepl(variable_wt_regexp,cols)]$cols
        if ("startDate"%in%colnames(rv$study_metadata)){
          rv$study_metadata[, studyDbId := as.numeric(studyDbId)]
          rv$data_gxe <- rv$data_plot[unique(rv$study_metadata[studyDbId%in%unique(rv$data_plot$studyDbId),.(studyDbId, study_startYear = year(as.POSIXct(startDate)))]), on=.(studyDbId)]
        } else {
          rv$data_gxe <- rv$data_plot
        }
        #attempt to identify variables that are redundant with studyDbId to use as choices for picker_env_variable
        datagef <- lapply(rv$data_gxe, function(a) as.factor(a))
        env_vars <- names(which(unlist(lapply(datagef, function(a) length(levels(as.factor(paste(a, datagef$studyDbId))))==length(levels(datagef$studyDbId)) & length(levels(as.factor(paste(a, datagef$studyDbId))))==length(levels(a))))==TRUE))
        if (!is.null(input$picker_env_variable)){
          updatePickerInput(
            session, "picker_env_variable",
            choices = env_vars,
            selected = input$picker_env_variable
          )
        } else {
          updatePickerInput(
            session, "picker_env_variable",
            choices = env_vars,
            selected = character(0)
          )
        }
        
        if (!is.null(input$picker_trait)){
          updatePickerInput(
            session, "picker_trait",
            choices = trait_choices,
            selected = input$picker_trait
          )
        } else {
          updatePickerInput(
            session, "picker_trait",
            choices = trait_choices,
            selected = character(0)
          )
        }
        if (!is.null(input$weight_var)){
          updatePickerInput(
            session, "weight_var",
            choices = weight_choices,
            selected = input$weight_var
          )
        } else {
          updatePickerInput(
            session, "weight_var",
            choices = weight_choices,
            selected = character(0)
          )
        }
      })
      
      ## update env picker when trait is chosen ####
      #observeEvent(input$picker_trait,{
      observe({
        req(input$picker_trait)
        req(input$picker_env_variable)
        ## update environments dropdown
        envs <- unique(rv$data_gxe[!is.na(get(input$picker_trait)),.SD,.SDcols = c("studyDbId", input$picker_env_variable)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[[input$picker_env_variable]]

        updatePickerInput(
          session, "picker_env",
          choices = env_choices,
          selected = env_choices
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
        updatePickerInput(
          session, "AMMI_colorEnvBy",
          choices = env_details,
          selected = character(0)
        )
        
        germplasm_attr <- rv$column_datasource[source == "germplasm" & visible == T,]$cols
        updatePickerInput(
          session, "picker_germplasm_attr",
          choices = germplasm_attr,
          selected = character(0)
        )
        output$sliderUI_exclude_geno_nb_env <- renderUI({
          sliderInput(ns("exclude_geno_nb_env"),label = "Exclude genotypes that are present in less than X environments", value = 1, min = 1, max = length(input$picker_env), step = 1 )
        })
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
          choices = c("Nothing",input$picker_germplasm_attr,"sensitivity clusters"),
          selected = "Nothing"
        )
        updatePickerInput(
          session, "AMMI_colorGenoBy",
          choices = c("Nothing",input$picker_germplasm_attr),
          selected = "Nothing"
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
        data2TD <- copy(rv$data_gxe[observationLevel=="MEANS"])
        if (input$picker_germplasm_level=="germplasmDbId"){
          data2TD[, genotype:=paste0(germplasmDbId," (",germplasmName,")")]
        } else {
          data2TD[, genotype:=germplasmName]
        }
        
        #
          genot_to_excl <- data2TD[!is.na(get(input$picker_trait)),.N,genotype][N<input$exclude_geno_nb_env]
          data2TD <- data2TD[!genotype%in%genot_to_excl$genotype]
          #if (exists("genot_to_excl")){
            if (nrow(genot_to_excl)>1){
              #showNotification(paste0("Excluding ", nrow(genot_to_excl)," genotypes"), type = "message")
              output$TD_excluded_geno <- DT::renderDataTable(data.table(genot_to_excl), rownames= FALSE)
            } else {
              #showNotification(paste0("Using all genotypes"), type = "message")
              output$TD_excluded_geno <- DT::renderDataTable(data.table()[0L], rownames= FALSE)
              
            }
            
          #}
          
        #}
        if (length(input$picker_scenario)>=1){ #& input$check_combine_scenario){
          data2TD[, scenario:= do.call(paste0, .SD), .SDcols = input$picker_scenario]
        } #else {
        #  if (length(input$picker_scenario)==1){
        #    data2TD[, scenario:= do.call(paste0, .SD), .SDcols = input$picker_scenario]
        #    #setnames(data2TD, old=input$picker_scenario, new="scenario")
        #  }
        #}
        if (!is.null(input$picker_year)){
          data2TD[, year:= .SD, .SDcols = input$picker_year]
          #setnames(data2TD, old=input$picker_year, new="year")
        }
        if (!is.null(input$picker_location)){
          data2TD[, loc:= .SD, .SDcols = input$picker_location]
          #setnames(data2TD, old=input$picker_location, new="loc")
        }
        if (!is.null(input$picker_region)){
          data2TD[, region:= .SD, .SDcols = input$picker_region]
          #setnames(data2TD, old=input$picker_region, new="region")
        }
        if (input$use_weights){
          if (!is.null(input$weight_var)){
            data2TD[,wt:=(1/.SD)^2, .SDcols=input$weight_var]
          }
        }        
        #browser()
        rv$TD <- statgenSTA::createTD(data = data2TD[studyDbId%in%input$picker_env],
                                      genotype = "genotype",
                                      trial = input$picker_env_variable)

        
        #Update number of AMMI PCs picker as the smallest of the number of genotypes or environments minus one
        updatePickerInput(
          session, "AMMI_nPC",
          choices = c("Auto",c(2:min(length(rv$TD)-1, length(unique(rbindlist(rv$TD)$genotype))-1))),
          selected = "Auto"
        )
        #Update excludeGeno AMMI picker
        updatePickerInput(
          session, "AMMI_excludeGeno",
          choices = as.character(unique(rbindlist(rv$TD)$genotype)),
          selected = character(0)
        )
        #Update rotatePC AMMI picker
        updatePickerInput(
          session, "AMMI_rotatePC",
          choices = as.character(unique(rbindlist(rv$TD)$trial)),
          selected = character(0)
        )
        
        
        
        
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
        if (any(c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level)))){
          misspicks <- c("'Trait'","'Variable to use as Environment'", "'Germplasm level'")[c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level))] 
          showNotification(stringmagic::string_magic("{enum ? misspicks}  should be selected first on Data preparation Tab"), type = "error", duration = notification_duration)
        } else {
          rv$TDVarComp <- switch(input$picker_gxe_mm_env_struct,
                                 `1`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)},
                                 `2`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, locationYear = TRUE), error=function(e) e)},
                                 `3`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "year"), error=function(e) e)},
                                 `4`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "loc"), error=function(e) e)},
                                 `5`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, regionLocationYear = TRUE), error=function(e) e)},
                                 `6`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "scenario"), error=function(e) e)})
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
          }, rownames= FALSE)
          bslib::accordion_panel_set(id="MM_accord1", values=TRUE)
          bslib::accordion_panel_set(id="MM_accord2", values=TRUE)
        }
      })
      
      observe({
        req(rv$TDVarComp)
        if (is.null(rv$TDVarComp$nestingFactor)){
          predict_levels <- "genotype"
          if (!is.null(rv$TDVarComp$useLocYear)){
            if (rv$TDVarComp$useLocYear){
              predict_levels <- c("genotype","trial")
            }
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
        },rownames= FALSE)
        
      })
      
      
      ## FW ####
      ### Run FW ####
      observeEvent(input$FW_run,{
        if (any(c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level)))){
          misspicks <- c("'Trait'","'Variable to use as Environment'", "'Germplasm level'")[c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level))] 
          showNotification(stringmagic::string_magic("{enum ? misspicks}  should be selected first on Data preparation Tab"), type = "error", duration = notification_duration)
        } else {
          rv$TDFW <- tryCatch(gxeFw(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
          rv$TDFWplot <- rv$TDFW
          output$FW_text_output <- renderPrint({
            if ("FW"%in%class(rv$TDFW)){
              summary(rv$TDFW)
            } else {
              rv$TDFW
            }
          })
        }
      })
      ### FW plot ####
      #### renderPlot observer ####
      observe({
        req(rv$TDFW)
          output$FW_plot <- renderPlot({
            TDFWplot <- rv$TDFWplot
            #browser()
            if (is.null(input$FW_picker_color_by)){
              p <- plot(TDFWplot, plotType = input$FW_picker_plot_type)
            } else {
              if (input$FW_picker_color_by=="sensitivity clusters"){
                #browser()
                p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy="sensitivity_cluster")
                if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                  rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                  #browser()
                  p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                      ggnewscale::new_scale_color() + 
                      ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                      geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3)
                  }
              } else {
                if (input$FW_picker_color_by=="Nothing"){
                  p <- plot(TDFWplot, plotType = input$FW_picker_plot_type)
                  if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                    rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                    p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                        ggnewscale::new_scale_color() + 
                        ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                        geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3)
                  }
                  
                } else {
                  p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy=input$FW_picker_color_by)
                  if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                    rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                    p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                        ggnewscale::new_scale_color() + 
                        ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                        geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3)
                  }
                  
                }
              }
            }
            #Following is required because statgenGxE:::plot.FW return a list of
            #three ggplots in case of plotType="scatter"
            #need to restore legend on first plot and capture it
            #and to grid.arrange the 3 plots
            if (input$FW_picker_plot_type=="scatter"){
              if (input$FW_picker_color_by!="Nothing") {
                p1Gtable <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p$p1 + ggplot2::theme(legend.position = "right")))
                legendPos <- sapply(X = p1Gtable$grobs, FUN = `[[`, 
                                    "name") == "guide-box"
                legend <- p1Gtable$grobs[[which(legendPos)]]
              }
              else {
                legend <- NULL
              }
              pEmpty <- ggplot2::ggplot() + ggplot2::theme(panel.background = ggplot2::element_blank())
              p1Gr <- ggplot2::ggplotGrob(p$p1)
              p2Gr <- ggplot2::ggplotGrob(p$p2)
              p3Gr <- ggplot2::ggplotGrob(p$p3)
              pEmpty <- ggplot2::ggplotGrob(pEmpty)
              c1 <- gridExtra::gtable_rbind(p1Gr, p2Gr)
              c2 <- gridExtra::gtable_rbind(pEmpty, p3Gr)
              tot <- gridExtra::gtable_cbind(c1, c2)
                p <- gridExtra::grid.arrange(tot, right = legend, 
                                             top = paste("Finlay & Wilkinson analysis for", input$picker_trait))
            }
              p
          })
      })

      #### compute sensitivity_clusters whenever a picker changes ####
      observeEvent(  eventExpr = {
        input$FW_picker_color_by
        input$FW_cluster_sensitivity_nb
        input$FW_picker_cluster_on
        input$FW_picker_plot_type
        rv$TDFW
      }, handlerExpr = {
        req(rv$TDFW)
        #browser()
        if (input$FW_picker_color_by=="sensitivity clusters"){
          sensclust <- data.table(rv$TDFW$estimates)
          sensclust <- sensclust[!is.na(Sens)]
          sensclust[,sensitivity_cluster:=kmeans(scale(.SD),centers = input$FW_cluster_sensitivity_nb)$cluster, .SDcols = input$FW_picker_cluster_on]
          rv$sensclust <- sensclust
          rv$TDFWplot <- rv$TDFW
          rv$TDFWplot$TD <- lapply(rv$TDFWplot$TD, function(a) data.table(a)[sensclust, on=.(genotype=Genotype)])
        } else {
          sensclust <- data.table(rv$TDFW$estimates)
          rv$sensclust <- sensclust
        }
      })
      
      observeEvent(input$FW_sens_clusters_DT_rows_selected, ignoreNULL = FALSE, {
        #browser()
        if (!is.null(input$FW_sens_clusters_DT_rows_selected)){
          shinyjs::enable("create_groups_from_selgeno")
          #shinyjs::addClass("create_groups_from_selgeno", "active")
          
        } else {
          shinyjs::disable("create_groups_from_selgeno")
          #shinyjs::removeClass("create_groups_from_selgeno", "active")
        }
      })
      
      observeEvent(input$create_groups_from_selgeno,{
        #browser()
        if(length(rv$selected_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, parent_session = parent_session, modal_title = "Create new group", group_description = "Group manually created from selected genotypes in Finlay Wilkinson analysis"))
        }
      })
      
      #### Render FW sensclusters DT ####
      observe({
        req(rv$sensclust)
        output$FW_sens_clusters_DT <- DT::renderDataTable(DT::datatable(rv$sensclust, filter = "top"),rownames= FALSE, selection = 'multiple')
        if (any(colnames(rv$sensclust)=="sensitivity_cluster")){
          shinyjs::enable("create_groups_from_sensclusters")
          #shinyjs::addClass("create_groups_from_sensclusters", "active")
        } else {
          shinyjs::disable("create_groups_from_sensclusters")
        }
        
      })
      
      #### Handle FW group creation ####
      observeEvent(input$create_groups_from_sensclusters,{
        shinyjs::disable("create_groups_from_sensclusters")
        shinyjs::addClass("create_groups_from_sensclusters", "active")
        #browser()
        clusters <- unique(rbindlist(rv$TD)[,.(genotype,germplasmDbId,germplasmName)])[rv$sensclust, on=.(genotype=Genotype)][order(sensitivity_cluster)][,.(
          group_name = paste0("FW_cluster.", sensitivity_cluster),
          group_desc = paste0(
            "Clustering method: FW_clusters", tags$b(input$cluster_algo), tags$br(),
            "Cluster: ", tags$b(sensitivity_cluster,"/", input$n_clusters), tags$br(),
            "Timestamp: ", tags$b(Sys.time())
          ),
          germplasmDbIds = list(germplasmDbId),
          germplasmNames = list(germplasmName),
          .N),sensitivity_cluster]
        clusters[N>6 ,germplasmNames := paste(
          paste(unlist(germplasmNames)[1:5], collapse = ", "),
          paste("and", N - 5, "others")
        )]
        clusters[N <= 6, germplasmNames := paste(unlist(germplasmNames), collapse = ", ")]
        
        group_id_start <- ifelse(length(rv$groups$group_id)==0, 1, max(rv$groups$group_id) + 1)
        group_ids <- group_id_start:(group_id_start+clusters[,.N] -1)
        clusters[, group_id := group_ids]
        rv$groups <- rbindlist(list(
          rv$groups,
          clusters
        ), fill = T, use.names = T)
        
        ## update selectors (shape, colour)
        data_plot <- copy(rv$data_plot) # to avoid reactivity issues related to assignment by reference
        for(id in clusters[,unique(group_id)]){
          group_name <- clusters[group_id == id,group_name]
          data_plot[germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)], eval(group_name) := paste0('In "', group_name,'"')]
          data_plot[!(germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)]), eval(group_name) := paste0('Not in "', group_name,'"')]
        }
        rv$column_datasource <- rbindlist(
          list(
            rv$column_datasource,
            data.table(cols = clusters[,unique(group_name)], source = "group", type = "Text", visible = T)
          )
        )
        rv$data_plot <- data_plot
        
      })
      
      #output$FW_selected_obs_DT <- renderTable({
      #  req(input$observ_fwplot_brush)
      #  browser()
      #  rv$obs_fwtable <- brushedPoints(rv$fwp$data,
      #                                 input$observ_fwplot_brush)
      #  rv$obs_fwtable
      #}, width='50%')
      
      ## GGE ####
      ### Run GGE ####
      observeEvent(input$GGE_run,{
        req(rv$TD)
        #browser()
        rv$TD.metangge <- rbindlist(rv$TD)[,.SD, .SDcols=c("trial","genotype",input$picker_trait)]

        rv$TDGGEmetan <- tryCatch(metan::gge(rv$TD.metangge,
                                             env=trial,
                                             gen=genotype,
                                             resp = input$picker_trait,
                                             centering = input$GGE_advs_centering,
                                             scaling = input$GGE_advs_scaling,
                                             svp = input$GGE_advs_svp,), error=function(e) e)
        rv$TDGGE <- tryCatch(gxeGGE(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
        #browser()
        
        updatePickerInput(
          session, "GGE_picker_gen_select",
          choices = rv$TDGGEmetan[[1]]$labelgen
          #selected = character(0)
        )
        updatePickerInput(
          session, "GGE_picker_gen2_select",
          choices = rv$TDGGEmetan[[1]]$labelgen[2]
          #selected = character(0)
        )
        updatePickerInput(
          session, "GGE_picker_env_select",
          choices = rv$TDGGEmetan[[1]]$labelenv,
          selected = rv$TDGGEmetan[[1]]$labelenv
        )
        
        
        
        output$GGE_text_output <- renderPrint({
          if ("AMMI"%in%class(rv$TDGGE)){
            summary(rv$TDGGE)
          } else {
            rv$TDGGE
          }
        })
      })
      ### GGE plot ####
      observe({
        req(rv$TDGGEmetan)
        output$GGE_plot <- renderPlot({
          #output$FW_plot <- renderPlotly({
          #browser()
          TDGGEplot <- rv$TDGGEmetan
          #browser()
          if (input$GGE_picker_plot_type==5){
            metan:::plot.gge(TDGGEplot,
                             type = input$GGE_picker_plot_type,
                             sel_env = input$GGE_picker_env_select,
                             size.text.gen = input$GGE_plot_size.text.gen,
                             repulsion = input$GGE_plot_repulsion,
                             max_overlaps = input$GGE_plot_max_overlaps,
                             size.shape = input$GGE_plot_size.shape,
                             size.shape.win = input$GGE_plot_size.shape.win,
                             size.stroke = input$GGE_plot_size.stroke,
                             col.alpha = input$GGE_plot_col.alpha,
                             col.alpha.circle = input$GGE_plot_col.alpha.circle,
                             size.text.env = input$GGE_plot_size.text.env,
                             size.text.lab = input$GGE_plot_size.text.lab,
                             size.text.win = input$GGE_plot_size.text.win,
                             size.line = input$GGE_plot_size.line,
                             axis_expand = input$GGE_plot_axis_expand,
                             col.stroke = input$GGE_plot_col.stroke,
                             col.gen = input$GGE_plot_col.gen,
                             col.env = input$GGE_plot_col.env,
                             col.line = input$GGE_plot_col.line,
                             col.circle = input$GGE_plot_col.circle,
                             plot_theme = metan:::theme_metan() %+replace% theme(plot.title = element_text(size = input$GGE_plot_title_size, face = "bold", hjust = 0, vjust = 3),
                                                                                 plot.subtitle = element_text(size = input$GGE_plot_title_size-2, face = "italic", hjust = 0, vjust = 2))
            )            
            
          } else if (input$GGE_picker_plot_type == 7) {
            metan:::plot.gge(TDGGEplot,
                             type = input$GGE_picker_plot_type,
                             sel_gen = input$GGE_picker_gen_select,
                             size.text.gen = input$GGE_plot_size.text.gen,
                             repulsion = input$GGE_plot_repulsion,
                             max_overlaps = input$GGE_plot_max_overlaps,
                             size.shape = input$GGE_plot_size.shape,
                             size.shape.win = input$GGE_plot_size.shape.win,
                             size.stroke = input$GGE_plot_size.stroke,
                             col.alpha = input$GGE_plot_col.alpha,
                             col.alpha.circle = input$GGE_plot_col.alpha.circle,
                             size.text.env = input$GGE_plot_size.text.env,
                             size.text.lab = input$GGE_plot_size.text.lab,
                             size.text.win = input$GGE_plot_size.text.win,
                             size.line = input$GGE_plot_size.line,
                             axis_expand = input$GGE_plot_axis_expand,
                             col.stroke = input$GGE_plot_col.stroke,
                             col.gen = input$GGE_plot_col.gen,
                             col.env = input$GGE_plot_col.env,
                             col.line = input$GGE_plot_col.line,
                             col.circle = input$GGE_plot_col.circle,
                             plot_theme = metan:::theme_metan() %+replace% theme(plot.title = element_text(size = input$GGE_plot_title_size, face = "bold", hjust = 0, vjust = 3),
                                                                                 plot.subtitle = element_text(size = input$GGE_plot_title_size-2, face = "italic", hjust = 0, vjust = 2))
            )
          } else if (input$GGE_picker_plot_type == 9) {
            metan:::plot.gge(TDGGEplot,
                             type = input$GGE_picker_plot_type,
                             sel_gen1 = input$GGE_picker_gen_select,
                             sel_gen2 = input$GGE_picker_gen2_select,
                             size.text.gen = input$GGE_plot_size.text.gen,
                             repulsion = input$GGE_plot_repulsion,
                             max_overlaps = input$GGE_plot_max_overlaps,
                             size.shape = input$GGE_plot_size.shape,
                             size.shape.win = input$GGE_plot_size.shape.win,
                             size.stroke = input$GGE_plot_size.stroke,
                             col.alpha = input$GGE_plot_col.alpha,
                             col.alpha.circle = input$GGE_plot_col.alpha.circle,
                             size.text.env = input$GGE_plot_size.text.env,
                             size.text.lab = input$GGE_plot_size.text.lab,
                             size.text.win = input$GGE_plot_size.text.win,
                             size.line = input$GGE_plot_size.line,
                             axis_expand = input$GGE_plot_axis_expand,
                             col.stroke = input$GGE_plot_col.stroke,
                             col.gen = input$GGE_plot_col.gen,
                             col.env = input$GGE_plot_col.env,
                             col.line = input$GGE_plot_col.line,
                             col.circle = input$GGE_plot_col.circle,
                             plot_theme = metan:::theme_metan() %+replace% theme(plot.title = element_text(size = input$GGE_plot_title_size, face = "bold", hjust = 0, vjust = 3),
                                                                                 plot.subtitle = element_text(size = input$GGE_plot_title_size-2, face = "italic", hjust = 0, vjust = 2))
            )
          } else {
            metan:::plot.gge(TDGGEplot,
                             type = input$GGE_picker_plot_type,
                             size.text.gen = input$GGE_plot_size.text.gen,
                             repulsion = input$GGE_plot_repulsion,
                             max_overlaps = input$GGE_plot_max_overlaps,
                             size.shape = input$GGE_plot_size.shape,
                             size.shape.win = input$GGE_plot_size.shape.win,
                             size.stroke = input$GGE_plot_size.stroke,
                             col.alpha = input$GGE_plot_col.alpha,
                             col.alpha.circle = input$GGE_plot_col.alpha.circle,
                             size.text.env = input$GGE_plot_size.text.env,
                             size.text.lab = input$GGE_plot_size.text.lab,
                             size.text.win = input$GGE_plot_size.text.win,
                             size.line = input$GGE_plot_size.line,
                             axis_expand = input$GGE_plot_axis_expand,
                             col.stroke = input$GGE_plot_col.stroke,
                             col.gen = input$GGE_plot_col.gen,
                             col.env = input$GGE_plot_col.env,
                             col.line = input$GGE_plot_col.line,
                             col.circle = input$GGE_plot_col.circle,
                             plot_theme = metan:::theme_metan() %+replace% theme(plot.title = element_text(size = input$GGE_plot_title_size, face = "bold", hjust = 0, vjust = 3),
                                                                                 plot.subtitle = element_text(size = input$GGE_plot_title_size-2, face = "italic", hjust = 0, vjust = 2))
            )
          }

        })
          
      })
      
      ## AMMI ####
      ### Make sur year exists in TD if byYear is chosen ####
      observeEvent(input$AMMI_byYear,{
        if(input$AMMI_byYear & !any(colnames(rbindlist(rv$TD))=="year"))
          isolate({
            showNotification("Select an env. detail to use as year in Data preparation Tab", type = "warning", duration = notification_duration)
            updatePickerInput(
              session, "AMMI_byYear",
              selected = FALSE
            )
          })
      })
      
      ### Run AMMI ####
      observeEvent(input$AMMI_run,{
        req(rv$TD)
        #browser()
        rv$TDAMMI <- tryCatch(gxeAmmi(TD = rv$TD,
                                      trait = input$picker_trait,
                                      nPC = switch((input$AMMI_nPC=="Auto")+1,  as.numeric(input$AMMI_nPC,NULL)),
                                      byYear = input$AMMI_byYear,
                                      center = input$AMMI_center,
                                      excludeGeno = input$AMMI_excludeGeno,
                                      useWt = input$use_weights), error=function(e) e)
        #browser()
        output$AMMI_text_output <- renderPrint({
          if ("AMMI"%in%class(rv$TDAMMI)){
            summary(rv$TDAMMI)
          } else {
            rv$TDAMMI
          }
        })
      })

      ### AMMI plot ####
      observe({
        req(rv$TDAMMI)
        #browser()
        #Update rotatePC AMMI picker
        updatePickerInput(
          session, "AMMI_primAxis",
          choices = colnames(rv$TDAMMI$envScores),
          selected = colnames(rv$TDAMMI$envScores)[1]
        )
        updatePickerInput(
          session, "AMMI_secAxis",
          choices = colnames(rv$TDAMMI$envScores),
          selected = colnames(rv$TDAMMI$envScores)[2]
        )
        
        output$AMMI_plot <- renderPlot({
          #browser()
          if (input$AMMI_plotGeno & input$AMMI_plot_sizeGeno>1 & input$AMMI_plot_repel){
            p <- statgenGxE:::plot.AMMI(rv$TDAMMI,
                                        plotType = input$AMMI_plotType,
                                        scale = input$AMMI_scale,
                                        plotGeno = input$AMMI_plotGeno,
                                        colorGenoBy = switch((input$AMMI_colorGenoBy=="Nothing")+1,  input$AMMI_colorGenoBy, NULL),
                                        plotConvHull = input$AMMI_plotConvHull,
                                        colorEnvBy = input$AMMI_colorEnvBy,
                                        rotatePC = input$AMMI_rotatePC,
                                        primAxis = input$AMMI_primAxis,
                                        secAxis = input$AMMI_secAxis,
                                        envFactor = input$AMMI_plot_envFactor,
                                        sizeGeno = input$AMMI_plot_sizeGeno,
                                        sizeEnv = input$AMMI_plot_sizeEnv,
                                        title = switch((input$AMMI_plot_title=="")+1,  input$AMMI_plot_title, NULL))
            p$layers[[1]] <- NULL
            p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]])) +
              geom_text(data=p$data[p$data$type=="env",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="env",]))) +
              ggrepel::geom_text_repel(data =p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="geno",]), size=input$AMMI_plot_sizeGeno), max.overlaps = input$AMMI_plot_max_overlaps, force = input$AMMI_plot_repulsion)
          } else {
            p <- statgenGxE:::plot.AMMI(rv$TDAMMI,
                                        plotType = input$AMMI_plotType,
                                        scale = input$AMMI_scale,
                                        plotGeno = input$AMMI_plotGeno,
                                        colorGenoBy = switch((input$AMMI_colorGenoBy=="Nothing")+1,  input$AMMI_colorGenoBy, NULL),
                                        plotConvHull = input$AMMI_plotConvHull,
                                        colorEnvBy = input$AMMI_colorEnvBy,
                                        rotatePC = input$AMMI_rotatePC,
                                        primAxis = input$AMMI_primAxis,
                                        secAxis = input$AMMI_secAxis,
                                        envFactor = input$AMMI_plot_envFactor,
                                        sizeGeno = input$AMMI_plot_sizeGeno,
                                        sizeEnv = input$AMMI_plot_sizeEnv,
                                        title = switch((input$AMMI_plot_title=="")+1,  input$AMMI_plot_title, NULL))
          }
        print(p)
        })
      })
    ## MM Report ####
        output$MM_report <- downloadHandler(
          filename = function() {
            paste("GxE_MM-", Sys.Date(), ".html", sep="")
          },
          content = function(file) {
            rmarkdown::render(
              input="reports/GxE_MM.Rmd", output_file = file,
              params = list(data=rv$TD, trait=input$picker_trait, env_struct=input$picker_gxe_mm_env_struct)
            )
          }
        )
      ## FW Report ####
      output$FW_report <- downloadHandler(
        filename = function() {
          paste("GxE_FW-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
          rmarkdown::render(
            input="reports/GxE_FW.Rmd", output_file = file,
            params = list(data=rv$TD, trait=input$picker_trait, colorby=input$FW_picker_color_by)
          )
        }
      )
      ## GGE Report ####
      output$GGE_report <- downloadHandler(
        filename = function() {
          paste("GxE_GGE-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
          if (is.null(rv$TD.metangge)){
            showNotification("Please Run analysis once first", type = "warning", duration = notification_duration)
          } else {
            rmarkdown::render(
              input="reports/GxE_GGE.Rmd", output_file = file
            )
          }
        }
      )
      ## AMMI Report ####
      output$AMMI_report <- downloadHandler(
        filename = function() {
          paste("GxE_AMMI-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
          #if (is.null(rv$TDAMMI)){
          #  showNotification("Please Run analysis once first", type = "warning", duration = notification_duration)
          #} else {
            rmarkdown::render(
              input="reports/GxE_AMMI.Rmd", output_file = file
            )
          #}
        }
      )
    }
  )
}