#' @import shiny
#' @import shinyWidgets
#' @import bslib
#' @importFrom DT dataTableProxy
#' @export
# UI ####
mod_gxe_ui <- function(id){
  ns <- NS(id)
  tagList(
    rclipboard::rclipboardSetup(),
    shinyjs::useShinyjs(),
    chooseSliderSkin("Flat", color = "#1b95b2"),
    tags$head(
      tags$style(HTML("
      .console pre {
        color: #d40000;
        background-color: #f5f5f5;
        font-weight: bold;
        overflow-y:scroll;
        max-height: 250px;
      }
                      
      .btn:focus {
        color: black;
      }"))
    ),
    navset_tab(
      nav_panel(
        ## Data prep panel ####
        title = "Data preparation", 
        ### Sidebar ####
        layout_sidebar(
          width = 1/3,
          #height = 800,
          sidebar=sidebar(#card(
            # card_header(
            #   h4('Options ', icon('screwdriver-wrench'))
            # ),
            width = 350,
            pickerInput(ns("picker_trait"), label = tags$span(style="color: red;","Trait"), choices = c())|>
              tooltip("Select a single trait to work on", options = list(trigger="hover")),
            materialSwitch(ns("use_weights"),label = "Use weights", value = FALSE, inline = T, status = "info"),
            div(style="display: flex;gap: 10px;",
                pickerInput(ns("weight_var"), label = "Weight variable", choices = c()),
                materialSwitch(ns("transf_weights"),label = "Weight variable is the standard error of BLUE or BLUP", value = TRUE, inline = T, status = "info"))|>
                tooltip("When this option is selected weights will be calculated as 1/x^2, x being the selected weight variable", options = list(trigger="hover")),
            pickerInput(ns("picker_germplasm_level"), label = tags$span(style="color: red;","Germplasm level"), choices = c("germplasmDbId","germplasmName"), selected = "GermplasmName")|>
              tooltip("Select how genotypes will be identified (either germplasmDbId or germplasmName). In the second case, germplasm that have different DbIds in different environments but sharing a common preferred name will be considered as the same.", options = list(trigger="hover")),
            pickerInput(ns("picker_env_variable"), label = tags$span(style="color: red;","Variable to use as Environment Name"), choices = c())|>
              tooltip("Select an environment detail variable that will be used to identify environments", options = list(trigger="hover")),
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
          #layout_column_wrap(
          #  width = 1/2,
          ### Plots & tables ####
            card(full_screen = TRUE,#height = "33%",
              plotOutput(ns("TD_boxplot"))
            ),
          bslib::layout_columns(
            bslib::card(full_screen = FALSE, height = "650px",
                        bslib::card_header("Included genotypes"),
                        DT::dataTableOutput(ns("TD_included_geno")),
                        card_footer(div(style="display: flex;gap: 10px;",
                                        shiny::actionButton(ns("inclgeno_excl"), label = "Exclude selected genotypes", class = "btn btn-info"),
                                        uiOutput(ns("copy_incgeno_table"))))
                        #plotOutput(ns("TD_scatterplots"))
            ),
            bslib::card(full_screen = FALSE, height = "650px",
                        bslib::card_header("Excluded genotypes"),
                        DT::dataTableOutput(ns("TD_excluded_geno")),
                        card_footer(div(style="display: flex;gap: 10px;",
                                        shiny::actionButton(ns("exclgeno_incl"), label = "Include selected genotypes", class = "btn btn-info"),
                                        uiOutput(ns("copy_excgeno_table"))))
            )
          )
          #)
        )
      ),
      nav_panel(
        ## Mixed model panel ####
        title = "Mixed model",
        layout_sidebar(
          ### Sidebar ####
          sidebar=sidebar(          # card_header(
          #   h4('Options ', icon('screwdriver-wrench'))
          # ),
            width = 350,
            pickerInput(ns("picker_gxe_mm_env_struct"),
                        label = "Environment structure",
                        options = list(container = "body"), 
                        choices = c(`Trials correspond to environments`=1,
                                    `Trials form a factorial structure of locations x years`=2,
                                    `Trials are nested within year`=3,
                                    `Trials are nested within locations`=4,
                                    `Trials correspond to locations within regions across years`=5,
                                    `Trials are nested within scenarios`=6)),
            div(style="display: flex;",
                actionBttn(ns("mm_run_model"),"Run model", block = TRUE),
                a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#vcomp",icon("fas fa-question-circle"), target="_blank")),
          shiny::downloadButton(ns("MM_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
        ),
        # layout_columns(
        #   card(full_screen = TRUE,
        #               card_header(
        #                 class = "bg-dark",
        #                 "Model output"
        #               ),
        #               card_body(
        #                 verbatimTextOutput(ns("MM_text_output")) 
        #               )
        #   ),
        #   card(full_screen = TRUE,
        #               card_header(
        #                 class = "bg-dark",
        #                 "Diagnostics"
        #               ),
        #               card_body(
        #                 verbatimTextOutput(ns("MM_diagnostics"))
        #               )
        #   ),
        #   card(full_screen = TRUE,
        #               card_header(
        #                 class = "bg-dark",
        #                 "Variance components"
        #               ),
        #               card_body(
        #                 verbatimTextOutput(ns("MM_vc"))
        #               )
        #               
        #   )
        # ),
        ### Results ####
        layout_columns(
          #### Accordion textual results ####
          accordion(id = ns("MM_accord1"),
            #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),
            accordion_panel(title = "Model output", 
                          verbatimTextOutput(ns("MM_text_output")) 
            ),
            accordion_panel(title = "Diagnostics", 
                          verbatimTextOutput(ns("MM_diagnostics"))
            ),
            accordion_panel(title = "Variance components and heritability", 
                          verbatimTextOutput(ns("MM_vc"))
            )#,
            #accordion_panel(title = "Console", 
            #                       div(class = "console",verbatimTextOutput(ns("MM_console")))
            #)
          ),
          #### Accordion plot and predict results ####
          accordion(id = ns("MM_accord2"),
                           #actionButton(ns("expand_MM_accord2"),label = "Open all", class = "btn btn-info"),
                           accordion_panel(title = "Model plot", 
                                                  plotOutput(ns("MM_plot_output")) 
                           ),
                           accordion_panel(
                             title = "Predictions",
                             div(
                               style = "display: flex; align-items: flex-end;",
                               pickerInput(ns("MM_predict_level"), label = "Prediction level", choices = c("genotype")),
                               downloadButton(ns("MM_predict_download"), "CSV export", class = "btn btn-primary", style = "margin-bottom: 15px; margin-left: auto;")
                             ),
                             div(
                               style="display: flex;gap: 10px;",
                               actionButton(ns("MM_predict_select_all"), label = "Select all", class = "btn")|>
                                 tooltip("Select all filtered rows", options = list(trigger="hover")),
                               shinyjs::disabled(actionButton(ns("MM_predict_unselect"), "Deselect all", class = "btn")),
                               shinyjs::disabled(actionButton(ns("MM_create_group"), label = "Create group from selection", class = "btn btn-info"))
                             ),
                             DT::dataTableOutput(ns("MM_predictions"))
                           )
          )
          
        )
        )
      ),
      nav_panel(
        ## FW panel ####
        title = "Finlay Wilkinson",
        layout_sidebar(
          ### Sidebar ####
          sidebar=sidebar(
            width = 350,
            div(style="display: flex;",
            actionBttn(ns("FW_run"), "Run FW analysis", block = TRUE),
            a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#fw",icon("fas fa-question-circle"), target="_blank")),
            
            pickerInput(ns("FW_picker_plot_type"),
                        label="Plot type",
                        choices = c("scatter", "line", "trellis", "scatterFit"), selected = "line"),
            pickerInput(ns("FW_picker_color_by"), label="Color genotypes by", multiple = F, choices = c("Nothing","sensitivity clusters"), selected = "Nothing"),
            #materialSwitch(ns("FW_cluster_sensitivity"), "Color by sensitivity clusters", value = FALSE, status = "info"),
            bslib::accordion(id = ns("FW_clustering_acc"),
              bslib::accordion_panel(title = "Clustering", value = "fwclust",
                numericInput(ns("FW_cluster_sensitivity_nb"),"Number of clusters", min = 2, max = 8, step = 1, value = 2),
                pickerInput(ns("FW_picker_cluster_on"), label="Cluster on", choices = c(sensitivity="Sens", `genotype means`="GenMean"), multiple = TRUE, selected = "Sens"),
                prettyRadioButtons( 
                  inputId = ns("FW_picker_cluster_meth"),
                  label = "Clustering method",
                  choices = c("Kmeans", "hclust"),
                  inline = TRUE,
                  shape = "round",
                  status = "primary",
                  fill = TRUE,
                  bigger = TRUE),
                div(style="display: flex;gap: 10px;",
                    shiny::actionButton(inputId = ns("FW_cluster"), label = "Make clusters", icon = icon(NULL), class = "btn btn-info")
                    )
                
            )),
            shiny::downloadButton(ns("FW_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
          ),
          #layout_columns(
            #### Accordion results ####
            accordion(id = ns("FW_accord1"),
                             #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),
                             open = c("FW plot","Germplasm list and clusters"),

                             bslib::accordion_panel(title = "FW plot",
                                                    bslib::layout_columns(col_widths = c(9,3),
                                                                          bslib::layout_sidebar(
                                                                            bslib::card(full_screen = T, height = "800",
                                                                                        bslib::card_body(
                                                                                          #uiOutput(ns("FW_trellis_genot_select_ui")),
                                                                                          plotOutput(ns("FW_plot"), hover = hoverOpts(id =ns("FWplot_hover"),delay = 50), click = clickOpts(id=ns("FWplot_click")), dblclick = dblclickOpts(id=ns("FWplot_dblclick"))),
                                                                                          #htmlOutput(ns("FWhover_info")),
                                                                                          uiOutput(ns("FW_sens_clust_select"))
                                                                                        ),
                                                                                        #div(style=".bslib-card .card-footer.font-size: 1.2rem;",
                                                                                            card_footer(
                                                                                              div(style = "display: flex; margin-top: 1rem;",
                                                                                                  materialSwitch(ns("FW_coord_equal"), "Equal axes on line plot", value = TRUE, status = "info"),
                                                                                                  materialSwitch(ns("FW_display_raw_data"), "Plot also non fitted values on selected genotypes", value = FALSE, status = "info")
                                                                                              )
                                                                                            )#)
                                                                                        ),
                                                                            sidebar=bslib::sidebar(position = "right", title = "Advanced plot settings", open = FALSE,
                                                                                                 bslib::card(full_screen = F,height = "735",max_height = "735",
                                                                                                             sliderInput(ns("FW_axis.text.x.size"), label = "X-axis labels size", min = 8, max = 20, value = 10, step = 1),
                                                                                                             sliderInput(ns("FW_axis.text.y.size"), label = "Y-axis labels size", min = 8, max = 20, value = 10, step = 1)
                                                                                                 ))),
                                                                          value_box(title = "",
                                                                                    max_height = 100,
                                                                                    style = 'background-color: #EEEEEE!important;',
                                                                                    #value = tags$p(textOutput(ns("FWhover_vinfo")), style = "font-size: 100%;")
                                                                                    value = textOutput(ns("FWhover_vinfo"))
                                                                          )
                                                    )
                                                    ),
                             bslib::accordion_panel(title = "Germplasm list and clusters",
                                                    #bslib::card(DT::dataTableOutput(ns("FW_selected_obs_DT"))),
                                                    bslib::card(
                                                      bslib::card_header(div(style="display: flex;gap: 10px;",
                                                                             shiny::actionButton(inputId = ns("sens_clusters_DT.clearsel"), label = "Deselect all", icon = icon(NULL), class = "btn btn-info"),
                                                                             shiny::actionButton(ns("create_groups_from_sensclusters"), "Create groups from clusters", icon = icon(NULL), class = "btn btn-info"),
                                                                             shiny::actionButton(ns("create_groups_from_selgeno"),label = "Create group from selected genotypes", icon = icon(NULL), class = "btn btn-info")
                                                      )
                                                      ),
                                                      bslib::card_body(DT::dataTableOutput(ns("FW_sens_clusters_DT")))
                                                      )
                             ),
                             accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("FW_text_output")) 
                             )
            )
          #)
        )
      ),
      nav_panel(
        ## GGE panel ####
        title = "GGE",
        layout_sidebar(
          ### Sidebar ####
          sidebar=sidebar(
            width = 350,
            bslib::accordion(id = ns("GGE_adv_settings_acc"),
                             bslib::accordion_panel(title = "Analysis settings", value = "advs",
                                                    pickerInput(ns("GGE_advs_centering"), label = "centering", options = list(container = "body"), choices = c(none=0, global=1, environment=2, double=3), selected = 2),
                                                    pickerInput(ns("GGE_advs_scaling"), label = "scaling", options = list(container = "body"), choices = c(none=0, sd=1), selected = 0),
                                                    pickerInput(ns("GGE_advs_svp"), label = "svp", options = list(container = "body"), choices = c(genotype=1, environment=2, symmetrical=3), selected = 2))),
            div(style="display: flex;",
                actionBttn(ns("GGE_run"), "Run GGE analysis", block = TRUE),
                a(href="https://tiagoolivoto.github.io/metan/articles/vignettes_gge.html",icon("fas fa-question-circle"), target="_blank")),
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
            pickerInput(ns("GGE_picker_gen2_select"), label="Plot type 9 - Select a second genotype", multiple = F, choices = c()),
            pickerInput(ns("GGE_colorGenoBy"), label="Color genotypes by", choices = "Nothing", selected = "Nothing"),
            pickerInput(ns("GGE_colorEnvBy"), label="Color Environments by", choices = "Nothing", selected = "Nothing"),
            shiny::downloadButton(ns("GGE_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
            
          ),
          layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("GGE_accord1"),
                             bslib::accordion_panel(title = "GGE plot",
                                                    bslib::layout_sidebar(
                                                      bslib::card(full_screen = T,height = "800",max_height = "800",
                                                                  bslib::card_body(plotOutput(ns("GGE_plot"), click = clickOpts(id=ns("GGEplot_click")), dblclick = dblclickOpts(id=ns("GGEplot_dblclick")))),
                                                                  bslib::card_footer(div(style="display: flex;gap: 10px;",
                                                                    shiny::actionButton(ns("create_groups_from_GGEsel"), "Create group from selection", icon = icon(NULL), class = "btn btn-info")#,
                                                                    #shiny::actionButton(ns("create_group_from_wWw"), "Create groups from which won where", icon = icon(NULL), class = "btn btn-info")
                                                                  ))
                                                      ),
                                                      sidebar=bslib::sidebar(position = "right", title = "Advanced plot settings", open = FALSE,#full_screen = F,
                                                                             bslib::card(full_screen = F,height = "735",max_height = "735",
                                                                                         sliderInput(ns("GGE_plot_size.text.gen"), label = "Genotype labels size", min = 1, max = 8, value = 3.5, step = 0.5),
                                                                                         sliderInput(ns("GGE_plot_size.text.win"), label="Winners labels size", value = 4.5, min=1, max=10, step=0.5),
                                                                                         sliderInput(ns("GGE_plot_size.text.env"), label="Environement labels size", value = 3.5, min=1, max=10, step=0.5),
                                                                                         sliderInput(ns("GGE_plot_repulsion"), label="Labels repulsion", value = 1, min=1, max=10, step=0.5),
                                                                                         sliderInput(ns("GGE_plot_max_overlaps"), label="Labels max overlaps", value = 20, min=5, max=50, step=1),
                                                                                         sliderInput(ns("GGE_plot_size.shape"), label="Points size", value = 2.2, min=1, max=10, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_size.shape.win"), label="Winner points size", value = 3.2, min=1, max=10, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_size.stroke"), label="Points stroke width", value = 0.3, min=0, max=5, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_size.line"), label="Line width", value = 0.5, min=0, max=5, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_axis_expand"), label="Plot expansion", value = 1.2, min=0, max=2, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_col.alpha"), label="Transparency", value = 1, min=0, max=1, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_col.alpha.circle"), label="Circle transparency", value = 0.5, min=0, max=1, step=0.1),
                                                                                         sliderInput(ns("GGE_plot_size.text.lab"), label="plot_size.text.lab", value = 12, min=1, max=20, step=0.5),
                                                                                         sliderInput(ns("GGE_plot_title_size"), label = "Title text size", min = 10, max = 30, value = 12, step = 1),
                                                                                         spectrumInput(ns("GGE_plot_col.stroke"), label="Stroke Color", selected = "black", update_on = "dragstop", options=list(`show-alpha`=TRUE)),
                                                                                         spectrumInput(ns("GGE_plot_col.gen"), label="Genotype labels color", selected = "blue", update_on = "dragstop", options=list(`show-alpha`=TRUE)),
                                                                                         #colorPickr(ns("GGE_plot_col.gen"), label="Genotype labels color", selected = "blue", theme = "monolith", update = "changestop", opacity = TRUE),
                                                                                         spectrumInput(ns("GGE_plot_col.env"), label="Environment labels color", selected = "forestgreen", update_on = "dragstop", options=list(`show-alpha`=TRUE)),
                                                                                         spectrumInput(ns("GGE_plot_col.line"), label="Line color", selected = "forestgreen", update_on = "dragstop", options=list(`show-alpha`=TRUE)),
                                                                                         spectrumInput(ns("GGE_plot_col.circle"), label="Circle color", selected = "gray", update_on = "dragstop", options=list(`show-alpha`=TRUE))
                                                                             )
                                                    )
                             )),
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("GGE_text_output")) 
                             )
            )
          )
        )
      ),
      nav_panel(
        ## AMMI panel ####
        title = "AMMI",
        layout_sidebar(
          ### Sidebar ####
          sidebar=sidebar(
            width = 350,
            bslib::accordion(id = ns("AMMI_adv_settings_acc"),
                             bslib::accordion_panel(title = "Analysis settings", value = "advs",
                                                    pickerInput(ns("AMMI_nPC"),
                                                                label="Number of PC",
                                                                choices = c("Auto"), selected = "Auto"),
                                                    materialSwitch(ns("AMMI_center"), "center", value = TRUE, status = "info"),
                                                    pickerInput(ns("AMMI_excludeGeno"), label="Exclude genotypes", multiple = T, choices = c(), options = pickerOptions(liveSearch = TRUE))
                             )
            ),
            div(style="display: flex;",
                actionBttn(ns("AMMI_run"), "Run AMMI analysis", block = TRUE),
                a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#am",icon("fas fa-question-circle"), target="_blank")),
            #materialSwitch(ns("AMMI_byYear"), "Run by year", value = FALSE, status = "info"),
            #hr(style = "border-top: 1px solid #000000;"),
            #pickerInput(ns("AMMI_plotType"), label="Plot type", choices = c("AMMI1", "AMMI2"), selected = "AMMI2"),
            pickerInput(ns("AMMI_primAxis"), label="Primary axis", choices = c()),
            pickerInput(ns("AMMI_secAxis"), label="Second axis", choices = c()),
            #materialSwitch(ns("AMMI_plotGeno"), "Plot genotypes", value = TRUE, status = "info"),
            sliderInput(ns("AMMI_scale"), label = "scale", min = 0, max = 1, value = 0.5, step=0.1),
            materialSwitch(ns("AMMI_plotConvHull"), "Plot convex hull around the genotypes", value = FALSE, status = "info"),
            pickerInput(ns("AMMI_colorGenoBy"), label="Color genotypes by", choices = "Nothing", selected = "Nothing"),
            pickerInput(ns("AMMI_colorEnvBy"), label="Color environments by", choices = c()),
            pickerInput(ns("AMMI_rotatePC"), label="Rotate PC", choices = c()),
            shiny::downloadButton(ns("AMMI_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
            
          ),
          layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("AMMI_accord1"),
                             bslib::accordion_panel(title = "AMMI plot",
                                                   bslib::layout_sidebar(
                                                     bslib::card(full_screen = T,height = "800",max_height = "800",
                                                                 bslib::card_body(
                                                                   plotOutput(ns("AMMI_plot"), hover = hoverOpts(id =ns("AMMIplot_hover"),delay = 50), click = clickOpts(id=ns("AMMIplot_click")), dblclick = dblclickOpts(id=ns("AMMIplot_dblclick"))) 
                                                                 ),
                                                                 bslib::card_footer(
                                                                   div(style="display: flex;gap: 10px;",
                                                                       shiny::actionButton(ns("create_groups_from_AMMIsel"), "Create group from selection", icon = icon(NULL), class = "btn btn-info")
                                                                   )
                                                                 )
                                                     ),
                                                     sidebar=bslib::sidebar(position = "right", title = "Advanced plot settings", open = FALSE,
                                                                            bslib::card(full_screen = F,height = "735",max_height = "735",
                                                                                        sliderInput(ns("AMMI_plot_sizeGeno"), label = "Genotype labels size", min = 0, max = 10, value = 1, step = 1),
                                                                                        #materialSwitch(ns("AMMI_plot_repel"), label = "use_ggrepel", value = FALSE, status = "info"),
                                                                                        sliderInput(ns("AMMI_plot_sizePoint"), label="Points size", value = 2, min=1, max=10, step=1),
                                                                                        #sliderInput(ns("AMMI_plot_max_overlaps"), label="plot_max_overlaps", value = 20, min=5, max=50, step=1),
                                                                                        sliderInput(ns("AMMI_plot_sizeEnv"), label = "Env labels size", min = 0, max = 10, value = 3, step = 1),
                                                                                        sliderInput(ns("AMMI_plot_envFactor"), label = "Env blow up factor", min = 0.1, max = 5, value = 0.5, step = 0.1),
                                                                                        textInput(ns("AMMI_plot_title"),label = "Title",value = NULL))
                                                                            )
                                                     )
                             ),
                             accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("AMMI_text_output")) 
                             )
            )
          )
        )
      ),
      nav_panel(
        ## Stability panel ####
        title = "Stability",
        layout_sidebar(
          ### Sidebar ####
          sidebar=sidebar(
            width = 350,
            div(style="display: flex;",
                a(href="https://biometris.github.io/statgenGxE/articles/statgenGxE.html#st",icon("fas fa-question-circle"), target="_blank")),
            materialSwitch(ns("use_predict"),label = "Use BLUPs as genotype means", value = FALSE, inline = T, status = "info")|>
              tooltip("With this option, BLUPs computed in the mixed model tab will be used as genotype means in the Stability table and in the plots", options = list(trigger="hover")),
            pickerInput(ns("STAB_plots_colorby"),"Color Genotypes by", choices = c()),
            shiny::downloadButton(ns("Stab_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
          ),
          ### Results ####
          accordion(
            id = ns("STAB_accordsup"),
            accordion_panel(
              title = "S: Static stability (envt. variance), W: Wricke's ecovalence, and Sup: Superiority measure of Lin and Binns",
              layout_columns(
                col_widths = 6,
                card(
                  dataTableOutput(ns("STAB_sup")),
                  card_footer(
                    div(style="display: flex;gap: 10px;",
                        uiOutput(ns("copy_STABsup_table")),
                        actionButton(ns("STAB_select_all"), label = "Select all", class = "btn"),
                        shinyjs::disabled(actionButton(ns("STAB_unselect"), "Deselect all", class = "btn")),
                        shiny::actionButton(ns("create_groups_from_STABsel"), "Create group from selection", icon = icon(NULL), class = "btn btn-info")
                    )
                  )
                ),
                layout_columns(
                  col_widths = 12,
                  card(full_screen = TRUE,
                    plotOutput(ns("STAB_static_plot"),
                                #hover = hoverOpts(id =ns("STAB_static_plot_hover"),delay = 50),
                                click = clickOpts(id=ns("STAB_static_plot_click")),
                                #brush = brushOpts(id=ns("STAB_static_plot_brush")),
                                dblclick = dblclickOpts(id=ns("STAB_static_plot_dblclick"), delay = 1000))
                  ),
                  card(full_screen = TRUE,
                    plotOutput(ns("STAB_wricke_plot"),
                                #hover = hoverOpts(id =ns("STAB_wricke_plot_hover"),delay = 50),
                                click = clickOpts(id=ns("STAB_wricke_plot_click")),
                                #brush = brushOpts(id=ns("STAB_wricke_plot_brush")),
                                dblclick = dblclickOpts(id=ns("STAB_wricke_plot_dblclick"), delay = 1000))
                  ),
                  card(full_screen = TRUE,
                       plotOutput(ns("STAB_sup_plot"),
                                  #hover = hoverOpts(id =ns("STAB_sup_plot_hover"),delay = 50),
                                  click = clickOpts(id=ns("STAB_sup_plot_click")),
                                  #brush = brushOpts(id=ns("STAB_sup_plot_brush")),
                                  dblclick = dblclickOpts(id=ns("STAB_sup_plot_dblclick"), delay = 1000))
                  )
                )
              )
            )
          )
        )
      )#,
      #nav_panel(
      #  ## Mega-env panel ####
      #  title = "Mega-envs",
      #  layout_sidebar(
      #  )
      #)      
    ),
    accordion(
      id = ns("console_accord"),
      accordion_panel(title = "Console", 
      div(class = "console",verbatimTextOutput(ns("console")))
    )
    )###
  )
}

# SERVER ####
#' @import statgenGxE
#' @export
mod_gxe_server <- function(id, rv, parent_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session){
      #global TD reactiveValue needed for dowloading data in module_banner
      rv$TD = NULL
      
      #local reactive values
      rv_gxe <- reactiveValues(
        selected_genotypes = NULL,
        console = NULL,
        data = NULL,
        FWclicked_genotypes = NULL,
        TDVarComp = NULL,
        TDFW = NULL,
        TDFW_plot = NULL,
        TDGGEmetan = NULL,
        TDGGE = NULL,
        TDAMMI = NULL,
        TDStab = NULL,
        sensclust = NULL,
        STSclicked_genotypes = NULL
      )
      
      dtproxy <<- dataTableProxy('FW_sens_clusters_DT')
      predictDTproxy <<- dataTableProxy('MM_predictions')
      STAB_supproxy <<- dataTableProxy('STAB_sup')
      
      accordion_panel_close("GGE_adv_settings_acc", values="advs", session = session)
      accordion_panel_close("AMMI_adv_settings_acc", values="advs", session = session)

      #lastclick_stabsup <- Sys.time()
      #lastclick_stabsta <- Sys.time()
      #lastclick_stabwri <- Sys.time()
      #lastsel_stabtab <- Sys.time()

      ## initialize all inputs ####
      observe({
        req(rv$extradata)
        req(rv$column_datasource)
        req(!isTRUE(input$picker_germplasm_attr_open))
        #update trait dropdown
        trait_choices <- rv$column_datasource[source %in% c("GxE","Means") & grepl(variable_regexp,cols)]$cols
        weight_choices <- rv$column_datasource[source %in% c("GxE","Means") & grepl(variable_wt_regexp,cols)]$cols
        if ("startDate"%in%colnames(rv$study_metadata)){
          rv$study_metadata[, studyDbId := as.numeric(studyDbId)]
          rv_gxe$data <- rv$extradata[unique(rv$study_metadata[studyDbId%in%unique(rv$extradata$studyDbId),.(studyDbId, study_startYear = year(as.POSIXct(startDate)))]), on=.(studyDbId)]
        } else {
          rv_gxe$data <- rv$extradata
        }
        #attempt to identify variables that are redundant with studyDbId to use as choices for picker_env_variable
        datagef <- lapply(rv_gxe$data[,.SD, .SDcols = c("studyDbId",rv$column_datasource[source=="environment", cols])], function(a) as.factor(a))
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
        if (!is.null(input$picker_trait)){
          if (grepl("BLUEs",input$picker_trait) || grepl("BLUPs",input$picker_trait)){
            # is any weight variable 
            trait_name <- gsub(paste0("(.*)",variable_regexp),"\\1",input$picker_trait)
            blueorblup <- gsub(paste0(".*(",variable_regexp,")"),"\\1",input$picker_trait)
            if(any(grepl(trait_name,weight_choices))){
              
              updateMaterialSwitch(
                session, "use_weights",
                value = TRUE
              )
              updateMaterialSwitch(
                session, "transf_weights",
                value = TRUE
              )
              updatePickerInput(
                session, "weight_var",
                choices = weight_choices,
                selected = grep(paste0(trait_name,gsub("_","_se",blueorblup)),weight_choices,value = T)
              )
            }
          }          
        }

        #browser()
        #colorbychoices <- input$picker_germplasm_attr
        #colorbychoices <- c(colorbychoices,rv$column_datasource[source %in% "group"]$cols)
        colorbychoices <- list(Nothing=c("Nothing"))
        if (nrow(rv$column_datasource[source %in% "group"])>0){
          colorbychoices <- c(colorbychoices, list(`Groups`=as.list(rv$column_datasource[source %in% "group"]$cols)))
        }
        if (!is.null(input$picker_germplasm_attr)){
          colorbychoices <- c(colorbychoices, list(`Germplasm attributes`=as.list(input$picker_germplasm_attr)))
        }
        updatePickerInput(
          session, "AMMI_colorGenoBy",
          choices = colorbychoices,
          selected = "Nothing"
        )
        updatePickerInput(
          session, "GGE_colorGenoBy",
          choices = colorbychoices,
          selected = "Nothing"
        )
        updatePickerInput(
          session, "STAB_plots_colorby",
          choices = colorbychoices,
          selected = "Nothing"
        )
        colorbychoices <- c(colorbychoices, list(`Clusters`=list("sensitivity clusters")))
        updatePickerInput(
          session, "FW_picker_color_by",
          choices = colorbychoices,
          #choices = c("Nothing",colorbychoices, "sensitivity clusters"),
          selected = "Nothing"
        )          
      })
      
      ## update env picker when trait or env_variable is chosen ####
      observeEvent(c(input$picker_trait, input$picker_env_variable),{
        req(input$picker_trait)
        req(input$picker_env_variable)
        ## update environments dropdown
        envs <- unique(rv_gxe$data[!is.na(get(input$picker_trait)),.SD,.SDcols = c("studyDbId", input$picker_env_variable)])
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
        colnames(rv_gxe$data)
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
          choices = c("Nothing", env_details),
          selected = "Nothing"
        )
        updatePickerInput(
          session, "GGE_colorEnvBy",
          choices = c("Nothing", env_details),
          selected = "Nothing"
        )
        
        germplasm_attr <- rv$column_datasource[source == "germplasm" & visible == T,]$cols
        updatePickerInput(
          session, "picker_germplasm_attr",
          choices = germplasm_attr,
          selected = character(0)
        )
        output$sliderUI_exclude_geno_nb_env <- renderUI({
          sliderInput(ns("exclude_geno_nb_env"),label = "Set the minimum number of environments in which each germplasm must be present", value = 1, min = 1, max = length(input$picker_env), step = 1 )
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
        #updatePickerInput(
        #  session, "FW_picker_color_by",
        #  choices = c("Nothing",input$picker_germplasm_attr,"sensitivity clusters"),
        #  selected = "Nothing"
        #)
        #updatePickerInput(
        #  session, "AMMI_colorGenoBy",
        #  choices = c("Nothing",input$picker_germplasm_attr),
        #  selected = "Nothing"
        #)
        
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
      observeEvent(input$picker_scenario,{
        env_details <- c(rv$column_datasource[source == "environment" & visible == T,]$cols, "study_startYear")
        env_details <- c("Nothing", env_details, "scenario")
        updatePickerInput(
          session, "AMMI_colorEnvBy",
          choices = env_details,
          selected = "Nothing"
        )
        updatePickerInput(
          session, "GGE_colorEnvBy",
          choices = env_details,
          selected = "Nothing"
        )
      })
      observeEvent(input$inclgeno_excl,{
        if (!is.null(input$TD_included_geno_rows_selected)){
          rv_gxe$genot_manexcl <- rbind(rv_gxe$genot_manexcl,
                                        rv_gxe$genot_incl[input$TD_included_geno_rows_selected,])
        }
      })
      observeEvent(input$exclgeno_incl,{
        if (!is.null(input$TD_excluded_geno_rows_selected)){
          rv_gxe$genot_manexcl <- rv_gxe$genot_manexcl[!genotype%in%rv_gxe$genot_to_excl[input$TD_excluded_geno_rows_selected,genotype]]
        }
      })
      ## main observer to build TD object and output basic plots ####
      observe({
        req(rv_gxe$data)
        req(rv$column_datasource)
        req(input$picker_trait)
        req(input$picker_env)
        req(!isTRUE(input$picker_germplasm_attr_open))
        #browser()
        data2TD <- copy(rv_gxe$data[observationLevel=="MEANS"])
        if (input$picker_germplasm_level=="germplasmDbId"){
          data2TD[, genotype:=paste0(germplasmDbId," (",germplasmName,")")]
        } else {
          if(any(length(data2TD[, .N,.(germplasmName,studyDbId)][N>1, germplasmName]))){
            namedups <- unique(data2TD[, .N,.(germplasmName,studyDbId)][N>1, germplasmName])
            data2TD[germplasmName%in%unique(data2TD[, .N,.(germplasmName,studyDbId)][N>1, germplasmName]),germplasmName:=paste0(germplasmName,".",1:.N),.(germplasmName, studyDbId)]
            data2TD[, genotype:=germplasmName]
            showNotification(paste0("Duplicates found in germplasmName for germplasm(s) ",paste(namedups, collapse = ", "),". Germplasms were renamed with a suffix number."), type = "warning", duration = NULL)
          } else {
            data2TD[, genotype:=germplasmName]
          }
        }        
        
        genot_to_excl <- data2TD[!is.na(get(input$picker_trait)),.N,genotype][N<input$exclude_geno_nb_env]
        genot_to_excl <- rbind(genot_to_excl, rv_gxe$genot_manexcl)
        #browser()
        data2TD <- data2TD[!genotype%in%genot_to_excl$genotype]
        genot_incl <- data2TD[!is.na(get(input$picker_trait)),.N,genotype]
        output$TD_included_geno <- DT::renderDataTable(datatable(genot_incl,
                                                                  options = list(dom="if<t>lpr"), rownames= FALSE))
        output$copy_incgeno_table <- renderUI({
          rclipboard::rclipButton("clipbtnincg_table", "Copy table", paste(paste(colnames(genot_incl),collapse="\t"),
                                                                          paste(apply(genot_incl,1,paste,collapse="\t"),collapse = "\n"),
                                                                          sep="\n"))#, shiny::icon("clipboard"))
        })
        #if (exists("genot_to_excl")){
          if (nrow(genot_to_excl)>0){
            #showNotification(paste0("Excluding ", nrow(genot_to_excl)," genotypes"), type = "message")
            output$TD_excluded_geno <- DT::renderDataTable(datatable(genot_to_excl,
                                                                      options = list(dom="if<t>lpr"), rownames= FALSE))
            output$copy_excgeno_table <- renderUI({
              rclipboard::rclipButton("clipbtnincg_table", "Copy table", paste(paste(colnames(genot_to_excl),collapse="\t"),
                                                                                paste(apply(genot_to_excl,1,paste,collapse="\t"),collapse = "\n"),
                                                                                sep="\n"))#, shiny::icon("clipboard"))
            })

          } else {
            #showNotification(paste0("Using all genotypes"), type = "message")
            output$TD_excluded_geno <- DT::renderDataTable(data.table()[0L], rownames= FALSE)
            output$copy_excgeno_table <- renderUI({NULL})
          }
        rv_gxe$genot_incl <- genot_incl  
        rv_gxe$genot_to_excl <- genot_to_excl  
        
          
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
            if (input$transf_weights==TRUE){
              data2TD[,wt:=(1/.SD)^2, .SDcols=input$weight_var]
            } else {
              data2TD[,wt:=.SD, .SDcols=input$weight_var]
            }
          }
        }
        if (any(colnames(data2TD)%in%input$picker_germplasm_attr)){
          for (ga in input$picker_germplasm_attr){
            data2TD[[ga]][is.na(data2TD[[ga]])] <- replace_na_germplasm_attr_by
          }
        }
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
      })
      
      ## boxplot ####
      output$TD_boxplot <- renderPlot({
        validate(
          need(input$picker_trait, "You must select a trait"),
          need(input$picker_germplasm_level, "You must select a germplasm level"),
          need(input$picker_env_variable, "You must select the environment name")
        )
        req(rv$TD)
        if (!is.null(input$picker_scenario)){
          #if ("scenarioFull"%in%names(data2TD)){
          plot(rv$TD,
               plotType = "box",
               traits = input$picker_trait,
               colorTrialBy = "scenario",
               orderBy = "descending")
        } else {
          plot(rv$TD,
               plotType = "box",
               traits = input$picker_trait,
               orderBy = "descending") 
        }
      })
      
      ## MM model ####
      ### Run MM model ####
      observeEvent(input$mm_run_model,{
        if (any(c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level)))){
          misspicks <- c("'Trait'","'Variable to use as Environment'", "'Germplasm level'")[c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level))] 
          showNotification(stringmagic::string_magic("{enum ? misspicks}  should be selected first on Data preparation Tab"), type = "error", duration = notification_duration)
        } else {
          #rv$console <- NULL
          
          updatePickerInput(session, "MM_predict_level", choices = "genotype", selected = "genotype")
          
          withCallingHandlers({
          rv_gxe$TDVarComp <- switch(input$picker_gxe_mm_env_struct,
                                 `1`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})},
                                 `2`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, locationYear = TRUE), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})},
                                 `3`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "year"), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})},
                                 `4`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "loc"), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})},
                                 `5`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, regionLocationYear = TRUE), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})},
                                 `6`={tryCatch(gxeVarComp(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights, nestingFactor = "scenario"), error=function(e) {rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",e), sep="")
                                 return(NULL)})})
          accordion_panel_set(id="MM_accord1", values=TRUE)
          accordion_panel_set(id="MM_accord2", values=TRUE)
          }, message = function(m) rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",m), sep=""),
             warning = function(w) rv_gxe$console <- paste(rv_gxe$console, paste0("Mixed model run at ",Sys.time(), " : ",w), sep=""))
      }
})
      
      output$MM_text_output <- renderPrint({
        req(rv_gxe$TDVarComp)
        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          summary(rv_gxe$TDVarComp)
        } else {
          rv_gxe$TDVarComp
        }
      })
      output$MM_diagnostics <- renderPrint({
        req(rv_gxe$TDVarComp)
        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          diagnostics(rv_gxe$TDVarComp)
        } else {
          "Model failed"
        }
      })
      output$MM_vc <- renderPrint({
        req(rv_gxe$TDVarComp)
        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          list(`Variance components`=vc(rv_gxe$TDVarComp), heritability=herit(rv_gxe$TDVarComp))
        } else {
          "Model failed"
        }
      })
      output$MM_plot_output <- renderPlot({
        req(rv_gxe$TDVarComp)
        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          plot(rv_gxe$TDVarComp)
        } else {
          "Model failed"
        }
      })
      
      
      ### Rendering console ####
      observeEvent(rv_gxe$console, {
          output$console <- renderText(rv_gxe$console)
      })
      
      observeEvent(rv_gxe$TDVarComp, {
        req(rv_gxe$TDVarComp)
        if (rv_gxe$TDVarComp$useLocYear) {
          predict_levels <- c("genotype", "trial", "loc", "year")
        }
        else if (rv_gxe$TDVarComp$useRegionLocYear) {
          predict_levels <- c("genotype", "trial", "region", "loc", 
                        "year")
        }
        else {
          predict_levels <- c("genotype", "trial", rv_gxe$TDVarComp$nestingFactor)
        }
        
        #if (is.null(rv_gxe$TDVarComp$nestingFactor)){
        #  predict_levels <- "genotype"
        #  if (!is.null(rv_gxe$TDVarComp$useLocYear)){
        #    if (rv_gxe$TDVarComp$useLocYear){
        #      predict_levels <- c("genotype","trial","loc","year")
        #    }
        #  }
        #} else {
        #  predict_levels <- c("genotype",rv_gxe$TDVarComp$nestingFactor)
        #}
        updatePickerInput(session, "MM_predict_level", choices = predict_levels, selected = "genotype")

        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          rv_gxe$MM_predict_table <- tryCatch(predict(rv_gxe$TDVarComp, predictLevel=input$MM_predict_level), 
                                              error=function(e) {data.table()[]})
            
        } else {
          rv_gxe$MM_predict_table <- data.table()[]
        }
      })
      
      ### Change prediction level ####
      observeEvent(eventExpr = {input$MM_predict_level
                                rv_gxe$TDVarComp}, ignoreNULL = FALSE, {
        if ("varComp"%in%class(rv_gxe$TDVarComp)){
          rv_gxe$MM_predict_table <-tryCatch(predict(rv_gxe$TDVarComp, predictLevel=input$MM_predict_level), 
                                             error=function(e) {data.table()[]})
        } else {
          rv_gxe$MM_predict_table <- data.table()[]
        }
      })
      
      ### Predictions table ####
      output$MM_predictions <- DT::renderDataTable({
        req(rv_gxe$MM_predict_table)
        rv_gxe$MM_predict_table
      },rownames= FALSE, filter = "top")
      
      ### Enable/disable group creation button ####
      observeEvent(input$MM_predictions_rows_selected, {
        if (!is.null(input$MM_predictions_rows_selected)) {
          shinyjs::enable("MM_create_group")
          shinyjs::enable("MM_predict_unselect")
        } else {
          shinyjs::disable("MM_create_group")
          shinyjs::disable("MM_predict_unselect")
        }
      }, ignoreNULL = F)
      
      ### handle select all ####
      observeEvent(input$MM_predict_select_all, {
        filtered_rows <- input$MM_predictions_rows_all
        DT::selectRows(predictDTproxy, selected=filtered_rows)
      })
      
      ### handle unselect ####
      observeEvent(input$MM_predict_unselect, {
        DT::selectRows(predictDTproxy, selected=NULL)
      })
      
      ### handle download csv ####
      output$MM_predict_download <- downloadHandler(
        filename = function() {
          paste0("predicted_values.csv")
        },
        content = function(file) {
          write.csv(rv_gxe$MM_predict_table, file, row.names = F)
        }
      )
      
      ### Handle group creation ####
      observeEvent(input$MM_create_group,{
        req(length(input$MM_predictions_rows_selected) > 0)
        selected_genotypes = rv_gxe$MM_predict_table[input$MM_predictions_rows_selected,]$genotype
        rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                             data.table(Genotype=selected_genotypes)),
                                                y=unique(rbindlist(rv$TD)),
                                                by.x = "Genotype",
                                                by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
        showModal(groupModal(rv=rv, 
                             parent_session = parent_session, 
                             modal_title = "Create new group", 
                             group_description = paste0("Group manually created from selected genotypes in mixed model analysis of ", input$picker_trait, " variable"),
                             group_prefix =paste0("M_MM@",input$picker_trait,".")
        ))
      })
      
      ## FW ####
      ### Run FW ####
      observeEvent(input$FW_run,{
        if (any(c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level)))){
          misspicks <- c("'Trait'","'Variable to use as Environment'", "'Germplasm level'")[c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level))] 
          showNotification(stringmagic::string_magic("{enum ? misspicks}  should be selected first on Data preparation Tab"), type = "error", duration = notification_duration)
        } else {
          withCallingHandlers({
          rv_gxe$TDFW <- tryCatch(gxeFw(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
          },
          message = function(m) rv_gxe$console <- paste(rv_gxe$console, paste0("FW run at ",Sys.time(), " : ",m), sep=""),
          warning = function(w) rv_gxe$console <- paste(rv_gxe$console, paste0("FW run at ",Sys.time(), " : ",w), sep=""))
          rv_gxe$TDFWplot <- rv_gxe$TDFW
          sensclust <- data.table(rv_gxe$TDFW$estimates)
          sensclust[,sensitivity_cluster:=NA]
          rv_gxe$sensclust <- sensclust
          
          output$FW_text_output <- renderPrint({
            if ("FW"%in%class(rv_gxe$TDFW)){
              summary(rv_gxe$TDFW)
            } else {
              rv_gxe$TDFW
            }
          })
        }
      })
      ### FW plot ####
      #### renderPlot observer ####
      output$FW_plot <- renderPlot({
        #browser()
        req(rv_gxe$TDFWplot)
        validate(
          need({!(input$FW_picker_color_by=="sensitivity clusters" & !("sensitivity_cluster"%in%colnames(rbindlist(rv_gxe$TDFWplot$TD))))}, "You must make clusters first"),
        )
        
        TDFWplot <- rv_gxe$TDFWplot
        if (any(colnames(rbindlist(rv$TD))=="scenario")){
          ts <- rbindlist(rv$TD)[,.N,.(trial, scenario)]
          ts [,newtrial:=paste0(trial," (",scenario,")")]
          TDFWplot$fittedGeno$trial <- ts$newtrial[match(TDFWplot$fittedGeno$trial, ts$trial)]
          TDFWplot$envEffs$Trial <- ts$newtrial[match(TDFWplot$envEffs$Trial, ts$trial)]
          TDFWplot$TD <- rename_envs(TDFWplot$TD, ts$trial, ts$newtrial)
        }
        if (is.null(input$FW_picker_color_by)){
            p <- plot(TDFWplot, plotType = input$FW_picker_plot_type)
        } else {
          if (input$FW_picker_color_by=="sensitivity clusters"){
            shinyjs::show(id="FW_sens_clust_select_buttons")
            #browser()
            p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy="sensitivity_cluster")
            req(input$FW_sens_clust_select_buttons)
            if (input$FW_sens_clust_select_buttons!="none" & input$FW_picker_plot_type=="line"){
              #browser()
              stacolors <- getOption("statgen.genoColors")
              names(stacolors)<-1:length(stacolors)
              p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                ggnewscale::new_scale_color() + 
                ggplot2::geom_line(data=p$data[p$data$genotype%in%rv_gxe$sensclust[sensitivity_cluster==input$FW_sens_clust_select_buttons, Genotype],], aes(y = fitted, color=sensitivity_cluster), size=1) + scale_color_manual(values = stacolors)
              
            }
            
            if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
              rv_gxe$selected_genotypes <- rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
              #browser()
              p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                  ggnewscale::new_scale_color() + 
                  ggplot2::geom_line(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                  geom_point(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(color=genotype), size=3)
                  if (input$FW_display_raw_data){
                    p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv_gxe$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
                  }
            }
            if (input$FW_picker_plot_type=="trellis"){
              if (!is.null(input$FW_sens_clusters_DT_rows_selected)){
                rv$selected_genotypes <- rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, genotypes=rv_gxe$selected_genotypes) 
              } else {
                #p <- plot(TDFWplot, plotType = input$FW_picker_plot_type) 
                p <- ggplot() + geom_text(aes(x=1,y=1,label="Please select germplasms to display in the Germplasm list and clusters below"), size=5) + 
                  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                        axis.text.y=element_blank(),axis.ticks=element_blank(),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),legend.position="none",
                        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank(),plot.background=element_blank())
              }
            }

          } else {
            shinyjs::hide(id="FW_sens_clust_select_buttons")
            
            if (input$FW_picker_color_by=="Nothing"){
              if (input$FW_picker_plot_type=="trellis"){
                if (!is.null(input$FW_sens_clusters_DT_rows_selected)){
                  rv_gxe$selected_genotypes <- rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                  p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, genotypes=rv_gxe$selected_genotypes) 
                } else {
                  #p <- plot(TDFWplot, plotType = input$FW_picker_plot_type) 
                  p <- ggplot() + geom_text(aes(x=1,y=1,label="Please select germplasms to display in the Germplasm list and clusters below"), size=5) + 
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),legend.position="none",
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
                }
              } else {
                
                p <- plot(TDFWplot, plotType = input$FW_picker_plot_type)
              }
              if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                #browser()
                rv_gxe$selected_genotypes <- rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                    ggnewscale::new_scale_color() + 
                    ggplot2::geom_line(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                    geom_point(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(color=genotype), size=3) + theme(legend.position = "right")
                    if (input$FW_display_raw_data){
                      p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv_gxe$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
                    }
              }
              
            } else {
              if (!input$FW_picker_color_by%in%colnames(TDFWplot$TD)){
                TDFWplot$TD <- lapply(seq_along(TDFWplot$TD),function(a) data.table(TDFWplot$TD[[a]])[data.table(rv$TD[[a]])[,.SD, .SDcols=c("genotype",input$FW_picker_color_by)], on=.(genotype)])
              }
              
              colgenobys <- unique(rbindlist(TDFWplot$TD)[[input$FW_picker_color_by]])
              colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)
              
              p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, 
                        colorGenoBy=input$FW_picker_color_by, 
                        colGeno=colGeno)
              
              # In case there is only two classes in color geno by
              # rebuild the line plot so that the smallest class is on top
              if (length(unique(p$data[[input$FW_picker_color_by]]))==2){
                levs <- names(sort(table(p$data[[input$FW_picker_color_by]])))
                cols <- getOption("statgen.genoColors")[1:2]
                names(cols) <- levs
                # Remove existing geom_point and geom_line layers
                p$layers[[1]] <- NULL
                p$layers[[2]] <- NULL
                p <- p + 
                  ggplot2::geom_line(data=p$data[p$data[[input$FW_picker_color_by]]==levs[2],], aes(y = fitted, color=get(input$FW_picker_color_by)), size=0.5) +
                  ggplot2::geom_point(data=p$data[p$data[[input$FW_picker_color_by]]==levs[2],], aes(y = fitted, color=get(input$FW_picker_color_by)), size=0.5) +
                  ggplot2::geom_line(data=p$data[p$data[[input$FW_picker_color_by]]==levs[1],], aes(y = fitted, color=get(input$FW_picker_color_by)), size=1) +
                  ggplot2::geom_point(data=p$data[p$data[[input$FW_picker_color_by]]==levs[1],], aes(y = fitted, color=get(input$FW_picker_color_by)), size=1) +
                  scale_color_manual(values=cols)
              }
              if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                rv_gxe$selected_genotypes <- rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                    ggnewscale::new_scale_color() + 
                    ggplot2::geom_line(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                    geom_point(data=p$data[p$data$genotype%in%rv_gxe$selected_genotypes,], aes(color=genotype), size=3)
                    if (input$FW_display_raw_data){
                      p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv_gxe$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
                    }
                
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
        #if (!is.null(input$FWplot_click)){
        #  print("clickpasnull")
        #  click=input$FWplot_click
        #  #browser()
        #  dist=sqrt((click$x-p$data$EnvMean)^2+(click$y-p$data$fitted)^2)
        #  p + geom_text(data = p$data[which.min(dist),], aes(x=EnvMean,y=fitted,label = genotype, size = 12))
        #} else {
        #  print("clicknull")
        
        # Rotate legend title so that it doesnt't take too much space
        # in case of long group name
        
        p <- p + theme(legend.title = element_text(angle = 90),
                       axis.text.x = element_text(size=input$FW_axis.text.x.size),
                       axis.text.y = element_text(size=input$FW_axis.text.y.size))
        if (input$FW_picker_plot_type=="line" & !input$FW_coord_equal){
          p + coord_cartesian()
        } else {
          p
        }
        #}
      })
      
      #### Handle hover event ####
      
      # observeEvent(input$FWplot_hover, {
      #   browser()
      #   
      # })

      output$FWhover_vinfo <- renderText({
        req(rv_gxe$TDFWplot)
        if(!is.null(input$FWplot_hover)) {
          F <- as.data.table(rv_gxe$TDFWplot$fittedGeno)
          E <- as.data.table(rv_gxe$TDFWplot$envEffs)
          EF <- E[F, on=.(Trial=trial)]
          hover=input$FWplot_hover
          dist=sqrt((hover$x-EF$EnvMean)^2+(hover$y-EF$fittedValue)^2)
          prox <- max(c(EF$EnvMean,EF$fittedValue))/30
          genot <- as.character(EF$genotype)[which.min(dist)]
          if(min(dist) < prox & input$FW_picker_plot_type%in%c("line","scatterFit")) {genot}
        }
      })
      
      #### Handle click event ####
      
      observeEvent(input$FWplot_click,{
        req(rv_gxe$TDFWplot)
        if(!is.null(input$FWplot_hover)) {
          F <- as.data.table(rv_gxe$TDFWplot$fittedGeno)
          E <- as.data.table(rv_gxe$TDFWplot$envEffs)
          EF <- E[F, on=.(Trial=trial)]
          hover=input$FWplot_hover
          dist=sqrt((hover$x-EF$EnvMean)^2+(hover$y-EF$fittedValue)^2)
          prox <- max(c(EF$EnvMean,EF$fittedValue))/30
          rv_gxe$FWclicked_genotypes <- unique(c(rv_gxe$FWclicked_genotypes,as.character(EF$genotype)[which.min(dist)]))
          dtsc <- dcast(rbindlist(rv$TD)[,c("genotype","trial",input$picker_trait), with = FALSE],genotype~trial)[rv_gxe$sensclust, on=.(genotype=Genotype)][,-c("SE_GenMean","SE_Sens","MSdeviation")]
          genot_trial_counts <- rbindlist(rv$TD)[,.N,genotype]
          dtsc <- genot_trial_counts[dtsc,on=.(genotype)]
          rv_gxe$formatcols <- colnames(dtsc)[-which(colnames(dtsc)%in%c("genotype","N","sensitivity_cluster", "Rank"))]
          rv_gxe$dtsc <- dtsc
        }
      })
      
      output$FW_sens_clusters_DT <- DT::renderDataTable({
        req(rv_gxe$dtsc, rv_gxe$formatcols)
        if (is.null(rv_gxe$FWclicked_genotypes)) {
          dt <- DT::datatable(rv_gxe$dtsc, filter = "top", rownames = F)
        } else {
          dt <- DT::datatable(
            rv_gxe$dtsc, 
            filter = "top",
            selection = list(
              mode="multiple", 
              selected=which(rv_gxe$dtsc$genotype%in%rv_gxe$FWclicked_genotypes)
            ),
            rownames = F
          )
        }
        formatRound(
          dt,
          columns = rv_gxe$formatcols,
          digits = 2)
        },
        server=TRUE
      )
      
      #### Handle dbleclick event ####
      observeEvent(input$FWplot_dblclick,{
        rv_gxe$FWclicked_genotypes <- NULL
        DT::selectRows(dtproxy, selected=NULL)
      })
        # output$FWhover_info <- renderText({
        #   if(!is.null(input$FWplot_hover)) {
        #   #browser()
        #   F <- as.data.table(rv_gxe$TDFWplot$fittedGeno)
        #   E <- as.data.table(rv_gxe$TDFWplot$envEffs)
        #   EF <- E[F, on=.(Trial=trial)]
        #   hover=input$FWplot_hover
        #   dist=sqrt((hover$x-EF$EnvMean)^2+(hover$y-EF$fittedValue)^2)
        #   genot <- as.character(EF$genotype)[which.min(dist)]
        #   if(min(dist) < 3) {
        #     htmltools::renderTags(
        #       tags$div(
        #         genot,
        #         style = paste0(
        #           "position: absolute; ",
        #           "top: ", hover$coords_css$y, "px; ",
        #           "left: ", hover$coords_css$x, "px; ",
        #           "background: gray; ",
        #           "padding: 3px; ",
        #           "color: white; "
        #         )
        #       )
        #     )$html
        #     
        #   }
        #   }
        # })
      
      #### compute sensitivity_clusters when ####
      observeEvent(  eventExpr = {
        #input$FW_picker_color_by
        #input$FW_cluster_sensitivity_nb
        #input$FW_picker_cluster_on
        ##input$FW_picker_plot_type
        #input$FW_picker_cluster_meth
        #rv_gxe$TDFW
        input$FW_cluster
      }, handlerExpr = {
        req(rv_gxe$TDFW)
        req(input$FW_cluster_sensitivity_nb, input$FW_picker_cluster_on)
        #req(input$FW_picker_color_by=="sensitivity clusters")
        #browser()
        #if (input$FW_picker_plot_type=="trellis"){
        #  output$FW_trellis_genot_select_ui <- renderUI({
        #    genots <- as.character(unique(rbindlist(rv$TDFW$TD)$genotype))
        #    pickerInput(ns("FW_trellis_genot_select"), label = "Select genotypes", choices = genots, selected = genots[1:round(length(genots)*0.05,0)], multiple = T, options = pickerOptions(liveSearch = TRUE))
        #  })
        #} else {
        #  output$FW_trellis_genot_select_ui <- renderUI({NULL})
        #}
        #if (input$FW_picker_plot_type=="line" & input$FW_picker_color_by=="sensitivity clusters"){
          sensclust <- data.table(rv_gxe$TDFW$estimates)
          sensclust <- sensclust[!is.na(Sens)]
          if (input$FW_picker_cluster_meth=="Kmeans"){
            sensclust[,sensitivity_cluster:=kmeans(scale(.SD),centers = input$FW_cluster_sensitivity_nb)$cluster, .SDcols = input$FW_picker_cluster_on]
          } else {
            sensclust[,sensitivity_cluster:=cutree(hclust(dist(scale(.SD))),k = input$FW_cluster_sensitivity_nb), .SDcols = input$FW_picker_cluster_on]
          }
          renum <- sensclust[,lapply(.SD,mean),sensitivity_cluster, .SDcols = c("GenMean","Sens")][order(GenMean)]
          renum[,renum:=1:.N]
          sensclustren <- renum[,.(sensitivity_cluster,renum)][sensclust, on=.(sensitivity_cluster)][,sensitivity_cluster:=renum]
          sensclustren[,renum:=NULL]
          rv_gxe$sensclust <- sensclustren
          rv_gxe$TDFWplot <- rv_gxe$TDFW
          rv_gxe$TDFWplot$TD <- lapply(rv_gxe$TDFWplot$TD, function(a) data.table(a)[rv_gxe$sensclust, on=.(genotype=Genotype)][!is.na(trial)])
          output$FW_sens_clust_select <- renderUI(
            prettyRadioButtons( 
              inputId = ns("FW_sens_clust_select_buttons"),
              label = "Select a cluster to highlight",
              choices = c("none", sort(unique(rv_gxe$sensclust$sensitivity_cluster))),
              inline = TRUE,
              shape = "round",
              status = "primary",
              fill = TRUE,
              bigger = TRUE
            )
          )
          updatePickerInput(
            session, "FW_picker_color_by",
            selected = "sensitivity clusters"
          )          
          
        #} else {
        #  if (is.null(rv_gxe$sensclust)){
        #    sensclust <- data.table(rv_gxe$TDFW$estimates)
        #    sensclust[,sensitivity_cluster:=NA]
        #    rv_gxe$sensclust <- sensclust
        #  }
        #  output$FW_sens_clust_select <- renderUI(expr = NULL)
        #}
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
      

      #### Render FW sensclusters DT ####
      observeEvent(rv_gxe$sensclust, {
        dtsc <- dcast(rbindlist(rv$TD)[,c("genotype","trial",input$picker_trait), with = F],genotype~trial)[rv_gxe$sensclust, on=.(genotype=Genotype)][,-c("SE_GenMean","SE_Sens","MSdeviation")]
        genot_trial_counts <- rbindlist(rv$TD)[,.N,genotype]
        dtsc <- genot_trial_counts[dtsc,on=.(genotype)]
        rv_gxe$formatcols <- colnames(dtsc)[-which(colnames(dtsc)%in%c("genotype","N","sensitivity_cluster", "Rank"))]
        #if (any(colnames(dtsc)%in%"sensitivity_cluster")){
        #  dtsc[,sensitivity_cluster:=as.character(sensitivity_cluster)]
        #}
        if (any(colnames(rv_gxe$sensclust)=="sensitivity_cluster")){
          shinyjs::enable("create_groups_from_sensclusters")
          #shinyjs::addClass("create_groups_from_sensclusters", "active")
        } else {
          shinyjs::disable("create_groups_from_sensclusters")
        }
        rv_gxe$dtsc <- dtsc
      })
      
      observeEvent(input$sens_clusters_DT.clearsel,{
        rv_gxe$FWclicked_genotypes <- NULL
        DT::selectRows(dtproxy, selected=NULL)
      })
      
      #### Handle FW group creation ####
      observeEvent(input$create_groups_from_sensclusters,{
        shinyjs::disable("create_groups_from_sensclusters")
        shinyjs::addClass("create_groups_from_sensclusters", "active")
        if (nrow(rv$groups)==0){
          clustering_id <- 1
        } else {
          clustering_id <- ifelse(length(rv$groups[!is.na(clustering_id)]$clustering_id)==0, 1, max(rv$groups[!is.na(clustering_id)]$clustering_id) + 1)
        }
        
        clusters <- unique(rbindlist(rv$TD)[,.(genotype,germplasmDbId,germplasmName)])[rv_gxe$sensclust, on=.(genotype=Genotype)][order(sensitivity_cluster)][,.(
          group_name = paste0("cl",clustering_id,"_FW@",input$picker_trait,".", sensitivity_cluster),
          group_desc = paste0(
            "Clustering method: FW clusters on ",input$picker_trait, " variable, using ", input$FW_picker_cluster_meth, " on ", paste(input$FW_picker_cluster_on, collapse = ", "), "<br>",
            "Cluster: ", sensitivity_cluster,"/", input$FW_cluster_sensitivity_nb, "<br>",
            "Timestamp: ", Sys.time()
          ),
          germplasmDbIds = list(germplasmDbId),
          germplasmNames = list(germplasmName),
          .N),sensitivity_cluster]
        clusters[N>6 ,germplasmNames := paste(
          paste(unlist(germplasmNames)[1:5], collapse = ", "),
          paste("and", N - 5, "others")
        ), sensitivity_cluster]
        clusters[N <= 6, germplasmNames := paste(unlist(germplasmNames), collapse = ", ")]
        
        group_id_start <- ifelse(length(rv$groups$group_id)==0, 1, max(rv$groups$group_id) + 1)
        group_ids <- group_id_start:(group_id_start+clusters[,.N] -1)
        clusters[, group_id := group_ids]
        clusters[, clustering_id := clustering_id]
        clusters[, clustering_name := paste0("cl",clustering_id,"_FW@",input$picker_trait)]
        rv$groups <- rbindlist(list(
          rv$groups,
          clusters
        ), fill = T, use.names = T)
        
        # Add a column to 
        data_plot <- copy(rv$extradata) # to avoid reactivity issues related to assignment by reference
        data_plot <- data_plot[clusters[,.(germplasmDbId=unlist(germplasmDbIds)),sensitivity_cluster],on=.(germplasmDbId)]
        setnames(data_plot,old = "sensitivity_cluster",new = paste0("cl",clustering_id,"_FW@",input$picker_trait))
        rv$column_datasource <- rbindlist(
          list(
            rv$column_datasource,
            #data.table(cols = clusters[,unique(group_name)],  type = "Text", source = "group", visible = T)
            data.table(cols = paste0("cl",clustering_id,"_FW@",input$picker_trait) ,  type = "Text", source = "group", visible = T)
          )
        )
        #browser()
        rv$extradata <- data_plot
      })
      
      observeEvent(input$create_groups_from_selgeno,{
        if(length(rv_gxe$selected_genotypes)>0){
          #browser()
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               rv_gxe$sensclust[input$FW_sens_clusters_DT_rows_selected,]),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, 
                               parent_session = parent_session, 
                               modal_title = "Create new group", 
                               group_description = paste0("Group manually created from selected genotypes in Finlay Wilkinson analysis of ", input$picker_trait, " variable"),
                               group_prefix =paste0("M_FW@",input$picker_trait,".")
                               )
                    )
        }
      })
      
      #output$FW_selected_obs_DT <- renderTable({
      #  req(input$observ_fwplot_brush)
      #  browser()
      #  rv_gxe$obs_fwtable <- brushedPoints(rv_gxe$fwp$data,
      #                                 input$observ_fwplot_brush)
      #  rv_gxe$obs_fwtable
      #}, width='50%')
      
      ## GGE ####
      ### Run GGE ####
      observeEvent(input$GGE_run,{
        req(rv$TD)
        #browser()
        rv$TD.metangge <- rbindlist(rv$TD)[,.SD, .SDcols=c("trial","genotype",input$picker_trait)]
        
        withCallingHandlers({
        rv_gxe$TDGGEmetan <- tryCatch(metan::gge(rv$TD.metangge,
                                             env=trial,
                                             gen=genotype,
                                             resp = input$picker_trait,
                                             centering = input$GGE_advs_centering,
                                             scaling = input$GGE_advs_scaling,
                                             svp = input$GGE_advs_svp,), error=function(e) e)
        rv_gxe$TDGGE <- tryCatch(gxeGGE(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
        },
        message = function(m) rv_gxe$console <- paste(rv_gxe$console, paste0("GGE run at ",Sys.time(), " : ",m), sep=""),
        warning = function(w) rv_gxe$console <- paste(rv_gxe$console, paste0("GGE run at ",Sys.time(), " : ",w), sep=""))
        
        
        updatePickerInput(
          session, "GGE_picker_gen_select",
          choices = rv_gxe$TDGGEmetan[[1]]$labelgen
          #selected = character(0)
        )
        updatePickerInput(
          session, "GGE_picker_gen2_select",
          choices = rv_gxe$TDGGEmetan[[1]]$labelgen,
          selected = rv_gxe$TDGGEmetan[[1]]$labelgen[2]
        )
        updatePickerInput(
          session, "GGE_picker_env_select",
          choices = rv_gxe$TDGGEmetan[[1]]$labelenv,
          selected = rv_gxe$TDGGEmetan[[1]]$labelenv
        )
      })
      
      output$GGE_text_output <- renderPrint({
        req(rv_gxe$TDGGE)
        if ("AMMI"%in%class(rv_gxe$TDGGE)){
          summary(rv_gxe$TDGGE)
        } else {
          rv_gxe$TDGGE
        }
      })
      
      ### GGE plot ####

      output$GGE_plot <- renderPlot({
        req(rv_gxe$TDGGEmetan)
        TDGGEplot <- rv_gxe$TDGGEmetan
        gg <- NULL
        #browser()
        if (input$GGE_picker_plot_type==5){
          gg <- metan:::plot.gge(TDGGEplot,
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
          gg <- metan:::plot.gge(TDGGEplot,
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
          gg <- metan:::plot.gge(TDGGEplot,
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
          gg <- metan:::plot.gge(TDGGEplot,
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
          if (input$GGE_picker_plot_type == 3) {
            rv_gxe$gp_WwW <- gg$layers[[length(gg$layers)]]$data$label
          }
        }
        if ((input$GGE_colorGenoBy!="Nothing" || input$GGE_colorEnvBy!="Nothing") & input$GGE_picker_plot_type==1){
          #browser() 
          geompdat <- as.data.table(gg$data)
          
          if (input$GGE_colorGenoBy!="Nothing"){
            geompdat <- merge.data.table(x=geompdat, 
                                         y=setnames(unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$GGE_colorGenoBy)]),new = c("genotype","colorby1")),
                                         by.x = "label", by.y = "genotype", all = TRUE)
          } else {
            geompdat[, colorby1:=NA]
          }
          if (input$GGE_colorEnvBy!="Nothing"){
            geompdat <- merge.data.table(x=geompdat, 
                                         y=setnames(unique(rbindlist(rv$TD)[,.SD,.SDcols=c("trial",input$GGE_colorEnvBy)]),new = c("trial","colorby2")),
                                         by.x = "label", by.y = "trial", all = TRUE)
          } else {
            geompdat[, colorby2:=NA]
          }
          geompdat[, colorby:=""]
          geompdat[is.na(colorby1), colorby:=colorby2]
          geompdat[is.na(colorby2), colorby:=colorby1]

          gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL

          if (input$GGE_colorGenoBy!="Nothing"){
            colgenobys <- unique(geompdat$colorby)
            colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)
            
            gg <- gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat[type=="genotype"], 
                                  aes(d1, d2, #color=as.factor(.data[["colorby"]]), 
                                      fill = as.factor(.data[["colorby"]])),
                                  color= input$GGE_plot_col.stroke, 
                                  shape = 21, 
                                  size = input$GGE_plot_size.shape, 
                                  stroke = input$GGE_plot_size.stroke, 
                                  alpha = input$GGE_plot_col.alpha) + 
            scale_fill_manual(values=colGeno, guide= guide_legend(override.aes = list(shape=21, stroke = 0, size = 5))) #+ 
            #scale_color_manual(values=colGeno, guide= "none") 
          } else {
            gg <- gg + geom_point(data=geompdat[type=="genotype"], 
                                  aes(d1, d2, color=as.factor(.data[["colorby"]])), 
                                  shape = 21, color= input$GGE_plot_col.stroke, 
                                  fill= input$GGE_plot_col.gen, 
                                  size = input$GGE_plot_size.shape,
                                  stroke = input$GGE_plot_size.stroke, 
                                  alpha = input$GGE_plot_col.alpha) 
          }
          if (input$GGE_colorEnvBy!="Nothing"){
            gg <- gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat[type=="environment"], 
                                  aes(d1, d2, #color=as.factor(.data[["colorby"]]), 
                                      fill = as.factor(.data[["colorby"]])),
                                  color= input$GGE_plot_col.stroke,
                                  shape = 23, size = input$GGE_plot_size.text.env, 
                                  stroke = input$GGE_plot_size.stroke, 
                                  alpha = input$GGE_plot_col.alpha) + 
            scale_fill_manual(values=getOption("statgen.trialColors"), guide = guide_legend(override.aes = list(shape=23, stroke = 0, size = 5))) #+ 
            #scale_color_manual(values=getOption("statgen.trialColors"), guide= "none")
          } else {
            gg <- gg + geom_point(data=geompdat[type=="environment"], 
                                  aes(d1, d2, color=as.factor(.data[["colorby"]])), 
                                  shape = 23, color= input$GGE_plot_col.stroke, 
                                  fill= input$GGE_plot_col.env, 
                                  size = input$GGE_plot_size.text.env, 
                                  stroke = input$GGE_plot_size.stroke, 
                                  alpha = input$GGE_plot_col.alpha) 
          }
        }
        #browser()
        rv_gxe$GGEplotdat <- gg$data
        if(length(rv_gxe$GGEclicked_genotypes)>0){
          clickgeno <- gg$data[gg$data$type=="genotype" & gg$data$label%in%rv_gxe$GGEclicked_genotypes,]
          gg + ggnewscale::new_scale_fill()
          gg <- gg + geom_point(data = clickgeno, aes(x=d1, y = d2), shape = 21, size=input$GGE_plot_size.shape+2, stroke=input$GGE_plot_size.stroke, color="red") 
        }
        gg + theme(legend.position.inside=NULL, legend.position = "right") 

        
      })      
      
      #### Handle click event ####
      observeEvent(input$GGEplot_click,{
        if(!is.null(input$GGEplot_click)) {
          GG <- rv_gxe$GGEplotdat
          click=input$GGEplot_click
          dist=sqrt((click$x-GG[,1])^2+(click$y-GG[,2])^2)
          clickedgeno <- GG$label[which.min(dist)]
          if (clickedgeno%in%rv$GGEclicked_genotypes){
            rv_gxe$GGEclicked_genotypes <- rv_gxe$GGEclicked_genotypes[-which(rv_gxe$GGEclicked_genotypes==clickedgeno)]
          } else {
            rv_gxe$GGEclicked_genotypes <- unique(c(rv_gxe$GGEclicked_genotypes,clickedgeno))
          }
          #browser()
        }
      })
      
      #### Handle dbleclick event ####
      observeEvent(input$GGEplot_dblclick,{
        rv_gxe$GGEclicked_genotypes <- NULL
      })
      
      #observeEvent(input$create_group_from_wWw,{
      #  #browser()
      #  if(length(rv_gxe$gp_WwW)>0){
      #    #browser()
      #    rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
      #                                                         Genotype=rv_gxe$gp_WwW),
      #                                            y=unique(rbindlist(rv$TD)),
      #                                            by.x = "Genotype",
      #                                            by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
      #    showModal(groupModal(rv=rv, 
      #                         parent_session = parent_session, 
      #                         modal_title = "Create new group", 
      #                         group_description = paste0("Group of which won where genotypes in GGE analysis of ", input$picker_trait, " variable"),
      #                         group_prefix =paste0("www_GGE@",input$picker_trait,".")
      #    )
      #    )
      #  }
      #})
      ### Handle group creation in GGE plot ####
      observe({
        if(length(rv_gxe$GGEclicked_genotypes)<1){
          shinyjs::disable("create_groups_from_GGEsel")
        } else {
          shinyjs::enable("create_groups_from_GGEsel")
        }
      })
      observeEvent(input$create_groups_from_GGEsel,{
        if(length(rv_gxe$GGEclicked_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv_gxe$GGEclicked_genotypes)),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, 
                               parent_session = parent_session, 
                               modal_title = "Create new group", 
                               group_description = paste0("Group manually created from selected genotypes in GGE analysis of ", input$picker_trait, " variable"),
                               group_prefix =paste0("M_GGE@",input$picker_trait,".")
          )
          )
        }
      })
      
      
      ## AMMI ####
      ### Make sur year exists in TD if byYear is chosen ####
      #observeEvent(input$AMMI_byYear,{
      #  if(input$AMMI_byYear & !any(colnames(rbindlist(rv$TD))=="year"))
      #    isolate({
      #      showNotification("Select an env. detail to use as year in Data preparation Tab", type = "warning", duration = notification_duration)
      #      updatePickerInput(
      #        session, "AMMI_byYear",
      #        selected = FALSE
      #      )
      #    })
      #})
      
      ### Run AMMI ####
      observeEvent(input$AMMI_run,{
        req(rv$TD)
        withCallingHandlers({
        rv_gxe$TDAMMI <- tryCatch(gxeAmmi(TD = rv$TD,
                                      trait = input$picker_trait,
                                      nPC = switch((input$AMMI_nPC=="Auto")+1,  as.numeric(input$AMMI_nPC,NULL)),
                                      byYear = FALSE, #input$AMMI_byYear,
                                      center = input$AMMI_center,
                                      excludeGeno = input$AMMI_excludeGeno,
                                      useWt = input$use_weights), error=function(e) e)
        },
        message = function(m) rv_gxe$console <- paste(rv_gxe$console, paste0("AMMI run at ",Sys.time(), " : ",m), sep=""),
        warning = function(w) rv_gxe$console <- paste(rv_gxe$console, paste0("AMMI run at ",Sys.time(), " : ",w), sep=""))
        
        #browser()
        output$AMMI_text_output <- renderPrint({
          if ("AMMI"%in%class(rv_gxe$TDAMMI)){
            summary(rv_gxe$TDAMMI)
          } else {
            rv_gxe$TDAMMI
          }
        })
      })

      ### AMMI plot ####
      observeEvent(rv_gxe$TDAMMI, {
        req(rv_gxe$TDAMMI)
        #browser()
        #Update rotatePC AMMI picker
        updatePickerInput(
          session, "AMMI_primAxis",
          choices = colnames(rv_gxe$TDAMMI$envScores),
          selected = colnames(rv_gxe$TDAMMI$envScores)[1]
        )
        updatePickerInput(
          session, "AMMI_secAxis",
          choices = colnames(rv_gxe$TDAMMI$envScores),
          selected = colnames(rv_gxe$TDAMMI$envScores)[2]
        )
      })
      
      output$AMMI_plot <- renderPlot({
        req(input$AMMI_primAxis, input$AMMI_secAxis)
        # This is to update the dat component of TDAMMI whenever TD changes
        # this occurs for example at group creation, a column with group memberships
        # is added to the TD object and available in the colorGenoBy picker input
        rv_gxe$TDAMMI$dat <- rbindlist(rv$TD)
        colgenobys <- unique(rv_gxe$TDAMMI$dat[[input$AMMI_colorGenoBy]])
        colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)

        p <- statgenGxE:::plot.AMMI(rv_gxe$TDAMMI,
                                    plotType = "AMMI2", #input$AMMI_plotType,
                                    scale = input$AMMI_scale,
                                    plotGeno = TRUE,
                                    colorGenoBy = switch((input$AMMI_colorGenoBy=="Nothing")+1,  input$AMMI_colorGenoBy, NULL),
                                    colGeno = colGeno,
                                    plotConvHull = input$AMMI_plotConvHull,
                                    colorEnvBy = switch((input$AMMI_colorEnvBy=="Nothing")+1,  input$AMMI_colorEnvBy, NULL),
                                    rotatePC = input$AMMI_rotatePC,
                                    primAxis = input$AMMI_primAxis,
                                    secAxis = input$AMMI_secAxis,
                                    envFactor = input$AMMI_plot_envFactor,
                                    sizeGeno = input$AMMI_plot_sizeGeno + 1,
                                    sizeEnv = input$AMMI_plot_sizeEnv + 1,
                                    title = switch((input$AMMI_plot_title=="")+1,  input$AMMI_plot_title, NULL))
        # Following is to handle vizualization of clicked genotypes
        # data structure is different for AMMI1 and AMMI2 plots
        # hence the distinction between both cases
        #if (input$AMMI_plotType=="AMMI2"){
          p$layers[[1]] <- NULL
          if (input$AMMI_colorGenoBy!="Nothing"){
            p <- p + ggnewscale::new_scale_color()
            p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], color = .data[[".group"]]), size=input$AMMI_plot_sizePoint) +
              scale_color_manual(values=colGeno, name=input$AMMI_colorGenoBy) + 
              geom_text(data=p$data[p$data$type=="env",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="env",])), size=input$AMMI_plot_sizeEnv + 1)
            if (input$AMMI_plot_sizeGeno>0){
              p <- p + geom_text(data =p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="geno",]), color = .data[[".group"]]), size=input$AMMI_plot_sizeGeno + 1, position = position_nudge(y=input$AMMI_plot_envFactor*max(p$data[p$data$type=="geno",input$AMMI_secAxis])/8))
            }
          } else {
            p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]]), size=input$AMMI_plot_sizePoint) +
              geom_text(data=p$data[p$data$type=="env",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="env",])), size=input$AMMI_plot_sizeEnv + 1)
              if (input$AMMI_plot_sizeGeno>0){
                p <- p + geom_text(data =p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="geno",])), size=input$AMMI_plot_sizeGeno + 1, position = position_nudge(y=input$AMMI_plot_envFactor*max(p$data[p$data$type=="geno",input$AMMI_secAxis])/8))
              }
          }
          
          if (!is.null(rv_gxe$AMMIclicked_genotypes)){
            clickgeno <- p$data[p$data$type=="geno" & row.names(p$data)%in%rv_gxe$AMMIclicked_genotypes,]
            p <- p + geom_point(data = clickgeno, aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]]), shape = 21, color="red", size=input$AMMI_plot_sizePoint+1)
          }
        #} else {
        #  if (input$AMMI_colorGenoBy!="Nothing"){
        #    p <- p + ggnewscale::new_scale_color()
        #    p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=x, y = y, color = .data[[".group"]]), size=input$AMMI_plot_sizePoint) +
        #      scale_color_manual(values=colGeno, name=input$AMMI_colorGenoBy)
        #  } else {
        #    p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=x, y = y), size=input$AMMI_plot_sizePoint) #+ 
        #  }
        #  if (!is.null(rv_gxe$AMMIclicked_genotypes)){
        #    clickgeno <- p$data[p$data$type=="geno" & row.names(p$data)%in%rv_gxe$AMMIclicked_genotypes,]
        #    p <- p + ggnewscale::new_scale_color()
        #    p <- p + geom_point(data = clickgeno, aes(x=x, y = y), shape = 21, color="red", size=input$AMMI_plot_sizePoint+1)
        #  }
        #}
        # This reactive is useful to handle click event
        rv_gxe$AMMIplotdat <- p$data
        p + guides(color = guide_legend(override.aes = aes(label = "")))
      })
      
      #### Handle click event ####
      
      observeEvent(input$AMMIplot_click,{
        if(!is.null(input$AMMIplot_click)) {
          #if (input$AMMI_plotType=="AMMI2"){
            AG <- rv_gxe$AMMIplotdat[,c(input$AMMI_primAxis,input$AMMI_secAxis)]
            click=input$AMMIplot_click
            dist=sqrt((click$x-AG[,1])^2+(click$y-AG[,2])^2)
            clickedgeno <- as.character(row.names(AG))[which.min(dist)]
          #} else {
          #  AG <- rv_gxe$AMMIplotdat
          #  click=input$AMMIplot_click
          #  dist=sqrt((click$x-AG$x)^2+(click$y-AG$y)^2)
          #  clickedgeno <- as.character(row.names(AG))[which.min(dist)]
          #}
          if (clickedgeno%in%rv_gxe$AMMIclicked_genotypes){
            rv_gxe$AMMIclicked_genotypes <- rv_gxe$AMMIclicked_genotypes[-which(rv_gxe$AMMIclicked_genotypes==clickedgeno)]
          } else {
            rv_gxe$AMMIclicked_genotypes <- unique(c(rv_gxe$AMMIclicked_genotypes,clickedgeno))
          }
          #browser()
          #if (length(rv$AMMIclicked_genotypes)<1){
          #  shinyjs::disable("create_groups_from_AMMIsel")
          #} else {
          #  shinyjs::enable("create_groups_from_AMMIsel")
          #}
          
        }
      })
      
      #### Handle dbleclick event ####
      observeEvent(input$AMMIplot_dblclick,{
        rv_gxe$AMMIclicked_genotypes <- NULL
      })
      
      ### Handle group creation in AMMI plot ####
      observe({
        if(length(rv_gxe$AMMIclicked_genotypes)<1){
          shinyjs::disable("create_groups_from_AMMIsel")
        } else {
          shinyjs::enable("create_groups_from_AMMIsel")
        }
      })
      observeEvent(input$create_groups_from_AMMIsel,{
        if(length(rv_gxe$AMMIclicked_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv_gxe$AMMIclicked_genotypes)),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, 
                               parent_session = parent_session, 
                               modal_title = "Create new group", 
                               group_description = paste0("Group manually created from selected genotypes in AMMI analysis of ", input$picker_trait, " variable"),
                               group_prefix =paste0("M_AMMI@",input$picker_trait,".")
          )
          )
        }
      })

      
      ## Stability ####
      ### Run Stab ####
      observeEvent(c(rv$TD,
                     input$use_predict), {
        req(rv$TD)
        TDStab <- tryCatch(statgenGxE::gxeStability(TD = rv$TD,
                                                        trait = input$picker_trait), error=function(e) e)
        req(TDStab$superiority)
        dtsup <- as.data.table(TDStab$superiority)
        setnames(dtsup, old = "Superiority", new = "Sup")
        dtsta <- as.data.table(TDStab$static)
        setnames(dtsta, old = "Static", new = "S")
        dtsta[,sqrtS:= sqrt(S)]
        dtwri <- as.data.table(TDStab$wricke)
        setnames(dtwri, old = "Wricke", new = "W")
        nenv <- rbindlist(rv$TD)[,.N,.(genotype)]
        dtwri <- nenv[dtwri, on=.(genotype=Genotype)]
        dtwri[,sqrtWe := sqrt(W/N)]
        if (input$use_predict & !is.null(rv_gxe$TDVarComp)){
          tdvcp <- as.data.table(predict(rv_gxe$TDVarComp, predictLevel = "genotype"))
          dtsup <- tdvcp[dtsup, on=.(genotype=Genotype)][,.(Genotype=genotype, Mean=predictedValue, Sup)]
        }
        dtsupsta <- dtsup[dtsta[,.(Genotype,S,sqrtS)], on=.(Genotype)]
        dtsupstawri <- dtsupsta[dtwri[,.(genotype,W,sqrtWe)], on=.(Genotype=genotype)]
        TDStab$dtres <- setcolorder(as.data.frame(dtsupstawri), 
                                    neworder = c("Genotype","Mean","S","sqrtS","W","sqrtWe","Sup"))
        rv_gxe$TDStab <- TDStab

        if (nrow(rv_gxe$TDStab$superiority)>0) {
          output$copy_STABsup_table <- renderUI({
            rclipboard::rclipButton("clipbtnsup_table", "Copy table", paste(paste(colnames(rv_gxe$TDStab$dtres),collapse="\t"),
                                                                            paste(apply(rv_gxe$TDStab$dtres,1,paste,collapse="\t"),collapse = "\n"),
                                                                            sep="\n"))#, shiny::icon("clipboard"))
          })
        }
      })
      
      #### table ####
      output$STAB_sup <- renderDataTable({
        formatRound(datatable(rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),], rownames = FALSE,
        #formatRound(datatable(rv_gxe$TDStab$dtres, rownames = FALSE,
                              options = list(pageLength = 30),
                              selection = list(mode="multiple", 
                                               selected=which(rv_gxe$TDStab$dtres$Genotype[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes)]%in%rv_gxe$STSclicked_genotypes))),
                                               #selected=which(rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes))),
                    columns = c("Mean", "Sup", "S", "W", "sqrtS", "sqrtWe"), 
                    digits=3)
      })
      
      ### Enable/disable group creation button ####
      observeEvent(input$STAB_sup_rows_selected, {
        if (!is.null(input$STAB_sup_rows_selected)) {
          shinyjs::enable("STAB_unselect")
        } else {
          shinyjs::disable("STAB_unselect")
        }
      }, ignoreNULL = F)
      
      ### handle select all ####
      observeEvent(input$STAB_select_all, {
        filtered_rows <- input$STAB_sup_rows_all
        DT::selectRows(STAB_supproxy, selected=filtered_rows)
      })
      
      ### handle unselect ####
      observeEvent(input$STAB_unselect, {
        DT::selectRows(STAB_supproxy, selected=NULL)
        rv_gxe$STSclicked_genotypes <- NULL
      })
      

      #### Handle DT selection ####
      #observeEvent(input$STAB_sup_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
      #  req(abs(lastsel_stabtab - Sys.time()) >=0.3)
      #  if (!is.null(input$STAB_sup_rows_selected)){
      #    tab <- rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),]
      #    rv_gxe$STSclicked_genotypes <- as.character(tab[input$STAB_sup_rows_selected, "Genotype"])
      #    lastsel_stabtab <<- Sys.time()
      #  } else {
      #    rv_gxe$STSclicked_genotypes <- NULL
      #  }
      #})
      
      
      ### Static ####
      output$STAB_static_plot <- renderPlot({
        gg <- ggplot(rv_gxe$TDStab$dtres) + 
          geom_point(aes(x=Mean, y= sqrt(S))) +
          ylab("Square root of Static stability")
        rv_gxe$st_sta_plotdat <- gg$data
        if (input$STAB_plots_colorby!="Nothing"){
          #browser() 
          geompdat <- as.data.table(gg$data)
          geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
          colgenobys <- unique(geompdat[[input$STAB_plots_colorby]])
          colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)
          
          gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
          
          gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrtS, color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
            scale_fill_manual(values=colGeno, name = input$STAB_plots_colorby) +#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
            scale_color_manual(values=colGeno, name = input$STAB_plots_colorby)#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
        }
        #if(length(rv_gxe$STSclicked_genotypes)>0){
        if(length(input$STAB_sup_rows_selected)>0){
          clickgeno <- gg$data[gg$data$Genotype%in%rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"],]
          #browser()
          #gg + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrt(S)), shape = 21, size=3, color="red") + 
            geom_text(data = clickgeno, aes(x=Mean , y = sqrt(S), label=Genotype), size=3, color="red",
                      position = position_nudge(y=max(sqrt(gg$data[,"S"]))/50))
          
        }
        gg
        
      })
      
      #### Handle click event ####
      observeEvent(input$STAB_static_plot_click,{
        #req(abs(lastclick_stabsta - Sys.time()) >=0.8)
        if(!is.null(input$STAB_static_plot_click)) {
          #browser()
          clicked_genotypes <- as.character(rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"])
          sta <- rv_gxe$st_sta_plotdat
          click=input$STAB_static_plot_click
          dist=sqrt((click$x-sta[,2])^2+(click$y-sqrt(sta[,3]))^2)
          clickedgeno <- as.character(sta$Genotype[which.min(dist)])
          if (clickedgeno%in%clicked_genotypes){
            rv_gxe$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
          } else {
            rv_gxe$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
          }
          #lastclick_stabsta <<- Sys.time()
        }
      })
      #### Handle dbleclick event ####
      observeEvent(input$STAB_static_plot_dblclick,{
        #req(abs(lastclick_stabsta - Sys.time()) >=0.8)
        rv_gxe$STSclicked_genotypes <- NULL
      })
      
      
      ### Wricke ####
      
      #### plot
      output$STAB_wricke_plot <- renderPlot({
        gg <- ggplot(rv_gxe$TDStab$dtres) + 
          geom_point(aes(x=Mean, y= sqrtWe)) +
          ylab("Square root of Wricke ecovalence/Ne")
        rv_gxe$st_stw_plotdat <- gg$data
        if (input$STAB_plots_colorby!="Nothing"){
          #browser() 
          geompdat <- as.data.table(gg$data)
          geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
          colgenobys <- unique(geompdat[[input$STAB_plots_colorby]])
          colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)
          gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL

          
          gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrtWe, color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
            scale_fill_manual(values=colGeno, name = input$STAB_plots_colorby) +#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
            scale_color_manual(values=colGeno, name = input$STAB_plots_colorby)#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
        }
        if(length(input$STAB_sup_rows_selected)>0){
          clickgeno <- gg$data[gg$data$Genotype%in%rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"],]
          #browser()
          #gg + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrtWe), shape = 21, size=3, color="red") +
            geom_text(data = clickgeno, aes(x=Mean , y = sqrtWe, label=Genotype), size=3, color="red",
                      position = position_nudge(y=max(gg$data[,"sqrtWe"])/50))
          
        }
        gg
        
      })
      
      #### Handle click event ####
      observeEvent(input$STAB_wricke_plot_click,{
        #req(abs(lastclick_stabwri - Sys.time()) >=0.8)
        if(!is.null(input$STAB_wricke_plot_click)) {
          clicked_genotypes <- as.character(rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"])
          stw <- rv_gxe$st_stw_plotdat
          click=input$STAB_wricke_plot_click
          dist=sqrt((click$x-stw[,2])^2+(click$y-stw[,6])^2)
          clickedgeno <- as.character(stw$Genotype[which.min(dist)])
          if (clickedgeno%in%clicked_genotypes){
            rv_gxe$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
          } else {
            rv_gxe$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
          }
          #lastclick_stabwri <<- Sys.time()
        }
      })
      #### Handle dbleclick event ####
      observeEvent(input$STAB_wricke_plot_dblclick,{
        #req(abs(lastclick_stabwri - Sys.time()) >=0.8)
        rv_gxe$STSclicked_genotypes <- NULL
      })
      ### Superiority ####
      #### plot ####
      output$STAB_sup_plot <- renderPlot({
        gg <- ggplot(rv_gxe$TDStab$dtres) + 
          geom_point(aes(x=Mean, y= sqrt(Sup))) +
          ylab("Square root of superiority")
        rv_gxe$st_sup_plotdat <- gg$data
        if (input$STAB_plots_colorby!="Nothing"){
          #browser() 
          geompdat <- as.data.table(gg$data)
          geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
          colgenobys <- unique(geompdat[[input$STAB_plots_colorby]])
          colGeno <- colgeno(colgenobys, missing = replace_na_germplasm_attr_by)
          gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
          
          gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrt(Sup), color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
            scale_fill_manual(values=colGeno, name = input$STAB_plots_colorby) +#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
            scale_color_manual(values=colGeno, name = input$STAB_plots_colorby)#getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
        }
        if(length(input$STAB_sup_rows_selected)>0){
          clickgeno <- gg$data[gg$data$Genotype%in%rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"],]
          #browser()
          #gg + ggnewscale::new_scale_color()
          gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrt(Sup)), shape = 21, size=3, color="red") +
            geom_text(data = clickgeno, aes(x=Mean , y = sqrt(Sup), label=Genotype), size=3, color="red",
                      position = position_nudge(y=max(sqrt(gg$data[,"Sup"]))/50))
        }
        gg
      })
      
      #### Handle click event ####
      observeEvent(input$STAB_sup_plot_click,{
        #req(abs(lastclick_stabsup - Sys.time()) >=0.8)
        if(!is.null(input$STAB_sup_plot_click)) {
          clicked_genotypes <- as.character(rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"])
          sts <- rv_gxe$st_sup_plotdat
          click=input$STAB_sup_plot_click
          dist=sqrt((click$x-sts[,2])^2+(click$y-sqrt(sts[,7]))^2)
          clickedgeno <- as.character(sts$Genotype[which.min(dist)])
          if (clickedgeno%in%clicked_genotypes){
            rv_gxe$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
          } else {
            rv_gxe$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
          }
          #lastclick_stabsup <<- Sys.time()
        }
      })
      #### Handle dbleclick event ####
      observeEvent(input$STAB_sup_plot_dblclick,{
        #req(abs(lastclick_stabsup - Sys.time()) >=0.8)
        rv_gxe$STSclicked_genotypes <- NULL
      })
      
      ### Handle group creation in Stability selection ####
      observeEvent(input$STAB_sup_rows_selected, {
        if(length(input$STAB_sup_rows_selected)<1){
          shinyjs::disable("create_groups_from_STABsel")
        } else {
          shinyjs::enable("create_groups_from_STABsel")
        }
      })
      
      observeEvent(input$create_groups_from_STABsel,{
        if(length(input$STAB_sup_rows_selected)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv_gxe$TDStab$dtres[order(!rv_gxe$TDStab$dtres$Genotype%in%rv_gxe$STSclicked_genotypes),][input$STAB_sup_rows_selected,"Genotype"])),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, 
                               parent_session = parent_session, 
                               modal_title = "Create new group", 
                               group_description = paste0("Group manually created from selected genotypes in Stability analysis of ", input$picker_trait, " variable"),
                               group_prefix =paste0("M_STAB@",input$picker_trait,".")
          )
          )
        }
      })
      
      ## MM Report ####
      output$MM_report <- downloadHandler(
        filename = function() {
          username <- gsub("(^.*?)\\:.*","\\1",rv$con$token)
          trial <- unique(rv$study_metadata$trialName)
          paste0("GxE_MM-", username, "-",  trial, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
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
          username <- gsub("(^.*?)\\:.*","\\1",rv$con$token)
          trial <- unique(rv$study_metadata$trialName)
          paste0("GxE_FW-", username, "-",  trial, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
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
          username <- gsub("(^.*?)\\:.*","\\1",rv$con$token)
          trial <- unique(rv$study_metadata$trialName)
          paste0("GxE_GGE-", username, "-",  trial, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
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
          username <- gsub("(^.*?)\\:.*","\\1",rv$con$token)
          trial <- unique(rv$study_metadata$trialName)
          paste0("GxE_AMMI-", username, "-",  trial, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
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
      ## Stab Report ####
      output$Stab_report <- downloadHandler(
        filename = function() {
          username <- gsub("(^.*?)\\:.*","\\1",rv$con$token)
          trial <- unique(rv$study_metadata$trialName)
          paste0("GxE_Stab-", username, "-",  trial, "-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".html")
        },
        content = function(file) {
          #if (is.null(rv$TDAMMI)){
          #  showNotification("Please Run analysis once first", type = "warning", duration = notification_duration)
          #} else {
          rmarkdown::render(
            input="reports/GxE_Stab.Rmd", output_file = file
          )
          #}
        }
      )
      
    }
  )
}