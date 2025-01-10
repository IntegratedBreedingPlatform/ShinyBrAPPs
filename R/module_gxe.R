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
      }"))
    ),
    bslib::navset_tab(
      bslib::nav_panel(
        ## Data prep panel ####
        title = "Data preparation", 
        ### Sidebar ####
        bslib::layout_sidebar(
          width = 1/3,
          #height = 800,
          sidebar=bslib::sidebar(#bslib::card(
            # bslib::card_header(
            #   h4('Options ', icon('screwdriver-wrench'))
            # ),
            width = 350,
            pickerInput(ns("picker_trait"), label = tags$span(style="color: red;","Trait"), choices = c())|>
              tooltip("Select a single trait to work on", options = list(trigger="hover")),
            materialSwitch(ns("use_weights"),label = "Use weights", value = FALSE, inline = T, status = "info"),
            div(style="display: flex;gap: 10px;",
                pickerInput(ns("weight_var"), label = "Weight variable", choices = c()),
                materialSwitch(ns("transf_weights"),label = "Weight variable is seBLUE/P", value = TRUE, inline = T, status = "info"))|>
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
          #bslib::layout_column_wrap(
          #  width = 1/2,
          ### Plots & tables ####
            bslib::card(full_screen = TRUE,#height = "33%",
              plotOutput(ns("TD_boxplot"))
            ),
          bslib::layout_columns(
            bslib::card(full_screen = FALSE, height = "650px",
                        bslib::card_header("Included genotypes"),
                        DT::dataTableOutput(ns("TD_included_geno")),
                        bslib::card_footer(uiOutput(ns("copy_incgeno_table")))
                        #plotOutput(ns("TD_scatterplots"))
            ),
            bslib::card(full_screen = FALSE, height = "650px",
                        bslib::card_header("Excluded genotypes"),
                        DT::dataTableOutput(ns("TD_excluded_geno")),
                        bslib::card_footer(uiOutput(ns("copy_excgeno_table")))
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
            )#,
            #bslib::accordion_panel(title = "Console", 
            #                       div(class = "console",verbatimTextOutput(ns("MM_console")))
            #)
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
            pickerInput(ns("FW_picker_color_by"), label="Color genotypes by", multiple = F, choices = c("Nothing","sensitivity clusters"), selected = "Nothing"),
            #materialSwitch(ns("FW_cluster_sensitivity"), "Color by sensitivity clusters", value = FALSE, status = "info"),
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
              bigger = TRUE
            ),
            shiny::downloadButton(ns("FW_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
          ),
          #bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("FW_accord1"),
                             #actionButton(ns("expand_MM_accord1"),label = "Open all", class = "btn btn-info"),
                             open = c("FW plot","Germplasm list and clusters"),

                             bslib::accordion_panel(title = "FW plot",
                                                    bslib::layout_columns(col_widths = c(9,3),
                                                                          bslib::card(full_screen = T, height = "800",
                                                                                      bslib::card_body(
                                                                                        #uiOutput(ns("FW_trellis_genot_select_ui")),
                                                                                        plotOutput(ns("FW_plot"), hover = hoverOpts(id =ns("FWplot_hover"),delay = 50), click = clickOpts(id=ns("FWplot_click")), dblclick = dblclickOpts(id=ns("FWplot_dblclick"))),
                                                                                        #htmlOutput(ns("FWhover_info")),
                                                                                        uiOutput(ns("FW_sens_clust_select"))
                                                                                      ),
                                                                                      #div(style=".bslib-card .card-footer.font-size: 1.2rem;",
                                                                                          bslib::card_footer(
                                                                                            div(style = "display: flex; margin-top: 1rem;",
                                                                                                materialSwitch(ns("FW_coord_equal"), "Equal axes on line plot", value = TRUE, status = "info"),
                                                                                                materialSwitch(ns("FW_display_raw_data"), "Plot also non fitted values on selected genotypes", value = FALSE, status = "info")
                                                                                            )
                                                                                          )#)
                                                                                      ),
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
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("FW_text_output")) 
                             )
            )
          #)
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
                             bslib::accordion_panel(title = "Analysis settings", value = "advs",
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
            pickerInput(ns("GGE_colorGenoBy"), label="Color genotypes by", choices = "Nothing", selected = "Nothing"),
            shiny::downloadButton(ns("GGE_report"), "Download report", icon = icon(NULL), class = "btn-block btn-primary")
            
          ),
          bslib::layout_columns(
            #### Accordion results ####
            bslib::accordion(id = ns("GGE_accord1"),
                             bslib::accordion_panel(title = "GGE plot",
                                                    bslib::layout_sidebar(
                                                      bslib::card(full_screen = T,height = "800",max_height = "800",
                                                                  bslib::card_body(plotOutput(ns("GGE_plot"), click = clickOpts(id=ns("GGEplot_click")), dblclick = dblclickOpts(id=ns("GGEplot_dblclick")))),
                                                                  bslib::card_footer(div(style="display: flex;gap: 10px;",
                                                                    shiny::actionButton(ns("create_groups_from_GGEsel"), "Create group from selection", icon = icon(NULL), class = "btn btn-info"),
                                                                    shiny::actionButton(ns("create_group_from_wWw"), "Create groups from which won where", icon = icon(NULL), class = "btn btn-info")
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
                                                                                         sliderInput(ns("GGE_plot_size.stroke"), label="Points stroke width", value = 0.3, min=1, max=5, step=0.1),
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
            bslib::accordion(id = ns("AMMI_adv_settings_acc"),
                             bslib::accordion_panel(title = "Analysis settings", value = "advs",
                                                    pickerInput(ns("AMMI_nPC"),
                                                                label="Number of PC",
                                                                choices = c("Auto"), selected = "Auto"),
                                                    materialSwitch(ns("AMMI_center"), "center", value = TRUE, status = "info"),
                                                    pickerInput(ns("AMMI_excludeGeno"), label="Exclude genotypes", multiple = T, choices = c(), options = pickerOptions(liveSearch = TRUE))
                                                    )
            ),
            #materialSwitch(ns("AMMI_byYear"), "Run by year", value = FALSE, status = "info"),
            #hr(style = "border-top: 1px solid #000000;"),
            pickerInput(ns("AMMI_plotType"), label="Plot type", choices = c("AMMI1", "AMMI2"), selected = "AMMI2"),
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
          bslib::layout_columns(
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
                             bslib::accordion_panel(title = "Analysis summary", 
                                                    verbatimTextOutput(ns("AMMI_text_output")) 
                             )
            )
          )
        )
      ),
      bslib::nav_panel(
        ## Stability panel ####
        title = "Stability",
        bslib::layout_sidebar(
          ### Sidebar ####
          sidebar=bslib::sidebar(
            width = 350,
            pickerInput(ns("STAB_plots_colorby"),"Color Genotypes by", choices = c())),
          ### Results ####
          bslib::accordion(id = ns("STAB_accordsup"),
                         open = c("Superiority measure of Lin and Binns",
                                  "Shukla's stability variance",
                                  "Wricke's ecovalence"),
                         bslib::accordion_panel(title = "Superiority measure of Lin and Binns, Shukla's stability variance & Wricke's ecovalence",
                                                bslib::layout_columns(col_widths = 6,
                                                  bslib::card(
                                                    dataTableOutput(ns("STAB_sup")),
                                                    bslib::card_footer(
                                                      div(style="display: flex;gap: 10px;",
                                                          uiOutput(ns("copy_STABsup_table")),
                                                          shiny::actionButton(ns("create_groups_from_STABsel"), "Create group from selection", icon = icon(NULL), class = "btn btn-info")
                                                      )
                                                    )
                                                  ),
                                                  bslib::layout_columns(col_widths = 12,
                                                  bslib::card(full_screen = TRUE,
                                                    plotOutput(ns("STAB_sup_plot"),
                                                               #hover = hoverOpts(id =ns("STAB_sup_plot_hover"),delay = 50),
                                                               click = clickOpts(id=ns("STAB_sup_plot_click")),
                                                               #brush = brushOpts(id=ns("STAB_sup_plot_brush")),
                                                               dblclick = dblclickOpts(id=ns("STAB_sup_plot_dblclick"), delay = 1000))
                                                    ),
                        #                        )
                        #                        ),
                        # bslib::accordion_panel(title = "Shukla's stability variance",
                        #                        bslib::layout_columns(
                        #                          bslib::card(
                        #                            dataTableOutput(ns("STAB_static")),
                        #                            bslib::card_footer(
                        #                              uiOutput(ns("copy_STABstatic_table"))
                        #                            )
                        #                          ),
                                                  bslib::card(full_screen = TRUE,
                                                    plotOutput(ns("STAB_static_plot"),
                                                               #hover = hoverOpts(id =ns("STAB_static_plot_hover"),delay = 50),
                                                               click = clickOpts(id=ns("STAB_static_plot_click")),
                                                               #brush = brushOpts(id=ns("STAB_static_plot_brush")),
                                                               dblclick = dblclickOpts(id=ns("STAB_static_plot_dblclick"), delay = 1000))
                                                  ),
                         #                       )
                         #),
                         #bslib::accordion_panel(title = "Wricke's ecovalence",
                         #                       bslib::layout_columns(
                         #                         bslib::card(
                         #                           dataTableOutput(ns("STAB_wricke")),
                         #                           bslib::card_footer(
                         #                             uiOutput(ns("copy_STABwricke_table"))
                         #                           )
                         #                         ),
                                                  bslib::card(full_screen = TRUE,
                                                    plotOutput(ns("STAB_wricke_plot"),
                                                               #hover = hoverOpts(id =ns("STAB_wricke_plot_hover"),delay = 50),
                                                               click = clickOpts(id=ns("STAB_wricke_plot_click")),
                                                               #brush = brushOpts(id=ns("STAB_wricke_plot_brush")),
                                                               dblclick = dblclickOpts(id=ns("STAB_wricke_plot_dblclick"), delay = 1000))
                                                  )
                                                )
                         )
        )
        ))),
      bslib::nav_panel(
        ## Mega-env panel ####
        title = "Mega-envs",
        bslib::layout_sidebar(
        )
      )
      
    ),
    bslib::accordion(id = ns("console_accord"),
                     bslib::accordion_panel(title = "Console", 
                                            div(class = "console",verbatimTextOutput(ns("console")))
                     )
    )###
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
      bslib::accordion_panel_close("AMMI_adv_settings_acc", values="advs", session = session)
      ## observe data and update Trait picker ####
      rv$selected_genotypes <- NULL
      rv$FWclicked_genotypes <- NULL
      rv$console <- NULL
      
      lastclick_stabsup <- Sys.time()
      lastclick_stabsta <- Sys.time()
      lastclick_stabwri <- Sys.time()
      lastsel_stabtab <- Sys.time()
      
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        req(!isTRUE(input$picker_germplasm_attr_open))
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
        datagef <- lapply(rv$data_gxe[,.SD, .SDcols = c("studyDbId",rv$column_datasource[source=="environment", cols])], function(a) as.factor(a))
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
        #browser()
        #colorbychoices <- input$picker_germplasm_attr
        #colorbychoices <- c(colorbychoices,rv$column_datasource[source %in% "group"]$cols)
        colorbychoices <- list(Nothing=c("Nothing"))
        if (nrow(rv$column_datasource[source %in% "group"])>0){
          colorbychoices <- c(colorbychoices, list(`Groups and clusters`=as.list(rv$column_datasource[source %in% "group"]$cols)))
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
        colorbychoices <- c(colorbychoices, list(`Compute new clusters`=list("sensitivity clusters")))
        updatePickerInput(
          session, "FW_picker_color_by",
          choices = colorbychoices,
          #choices = c("Nothing",colorbychoices, "sensitivity clusters"),
          selected = "Nothing"
        )
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
          sliderInput(ns("exclude_geno_nb_env"),label = "Set the minimum number of environments in which each germplasm should be at least present", value = 1, min = 1, max = length(input$picker_env), step = 1 )
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
      ## main observer to build TD object and output basic plots ####
      observe({
        req(rv$data_gxe)
        req(rv$column_datasource)
        req(input$picker_trait)
        req(input$picker_env)
        req(!isTRUE(input$picker_germplasm_attr_open))
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
          genot_incl <- data2TD[!is.na(get(input$picker_trait)),.N,genotype]
          output$TD_included_geno <- DT::renderDataTable(datatable(genot_incl,
                                                                   options = list(dom="if<t>lpr")), rownames= FALSE)
          output$copy_incgeno_table <- renderUI({
            rclipboard::rclipButton("clipbtnincg_table", "Copy table", paste(paste(colnames(genot_incl),collapse="\t"),
                                                                            paste(apply(genot_incl,1,paste,collapse="\t"),collapse = "\n"),
                                                                            sep="\n"))#, shiny::icon("clipboard"))
          })
          
          #if (exists("genot_to_excl")){
            if (nrow(genot_to_excl)>1){
              #showNotification(paste0("Excluding ", nrow(genot_to_excl)," genotypes"), type = "message")
              output$TD_excluded_geno <- DT::renderDataTable(datatable(genot_to_excl,
                                                                       options = list(dom="if<t>lpr")), rownames= FALSE)
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
            if (input$transf_weights==TRUE){
              data2TD[,wt:=(1/.SD)^2, .SDcols=input$weight_var]
            } else {
              data2TD[,wt:=.SD, .SDcols=input$weight_var]
            }
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
      })
      
      ## MM model ####
      ### Run MM model ####
      observeEvent(input$mm_run_model,{
        #browser()
        if (any(c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level)))){
          misspicks <- c("'Trait'","'Variable to use as Environment'", "'Germplasm level'")[c(is.null(input$picker_trait),is.null(input$picker_env_variable), is.null(input$picker_germplasm_level))] 
          showNotification(stringmagic::string_magic("{enum ? misspicks}  should be selected first on Data preparation Tab"), type = "error", duration = notification_duration)
        } else {
          #rv$console <- NULL
          withCallingHandlers({
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
          }, message = function(m) rv$console <- paste(rv$console, paste0("Mixed model run at ",Sys.time(), " : ",m), sep=""),
             warning = function(w) rv$console <- paste(rv$console, paste0("Mixed model run at ",Sys.time(), " : ",w), sep=""))}
      })
      
      observe({
        if(!is.null(rv$console)){
          output$console <- renderText(rv$console)
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
          withCallingHandlers({
          rv$TDFW <- tryCatch(gxeFw(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
          },
          message = function(m) rv$console <- paste(rv$console, paste0("FW run at ",Sys.time(), " : ",m), sep=""),
          warning = function(w) rv$console <- paste(rv$console, paste0("FW run at ",Sys.time(), " : ",w), sep=""))
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
          output$FW_plot <- renderPlot({
            req(rv$TDFW)
            TDFWplot <- rv$TDFWplot
            if (is.null(input$FW_picker_color_by)){
                p <- plot(TDFWplot, plotType = input$FW_picker_plot_type)
            } else {
              if (input$FW_picker_color_by=="sensitivity clusters"){
                #browser()
                p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy="sensitivity_cluster")
                req(input$FW_sens_clust_select_buttons)
                if (input$FW_sens_clust_select_buttons!="none" & input$FW_picker_plot_type=="line"){
                  #browser()
                  stacolors <- getOption("statgen.genoColors")
                  names(stacolors)<-1:length(stacolors)
                  p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                    ggnewscale::new_scale_color() + 
                    ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$sensclust[sensitivity_cluster==input$FW_sens_clust_select_buttons, Genotype],], aes(y = fitted, color=sensitivity_cluster), size=1) + scale_color_manual(values = stacolors)
                  
                }
                
                if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                  rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                  #browser()
                  p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                      ggnewscale::new_scale_color() + 
                      ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                      geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3)
                      if (input$FW_display_raw_data){
                        p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
                      }
                }
                if (input$FW_picker_plot_type=="trellis"){
                  if (!is.null(input$FW_sens_clusters_DT_rows_selected)){
                    rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                    p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, genotypes=rv$selected_genotypes) 
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
                if (input$FW_picker_color_by=="Nothing"){
                  if (input$FW_picker_plot_type=="trellis"){
                    if (!is.null(input$FW_sens_clusters_DT_rows_selected)){
                      rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                      p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, genotypes=rv$selected_genotypes) 
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
                    rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                    p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                        ggnewscale::new_scale_color() + 
                        ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                        geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3) + theme(legend.position = "right")
                        if (input$FW_display_raw_data){
                          p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
                        }
                  }
                  
                } else {
                  if (!input$FW_picker_color_by%in%colnames(TDFWplot$TD)){
                    TDFWplot$TD <- rv$TD
                  }
                  p <- plot(TDFWplot, plotType = input$FW_picker_plot_type, colorGenoBy=input$FW_picker_color_by)
                  if (!is.null(input$FW_sens_clusters_DT_rows_selected) & input$FW_picker_plot_type=="line"){
                    rv$selected_genotypes <- rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]$Genotype
                    p <- p + scale_color_grey(start = 0.8, end = 0.8, guide = "none") +
                        ggnewscale::new_scale_color() + 
                        ggplot2::geom_line(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(y = fitted, color=genotype), size=2) + 
                        geom_point(data=p$data[p$data$genotype%in%rv$selected_genotypes,], aes(color=genotype), size=3)
                        if (input$FW_display_raw_data){
                          p <- p + geom_point(data=as.data.table(p$data)[rbindlist(rv$TD)[genotype%in%rv$selected_genotypes,c("genotype", "trial" ,input$picker_trait), with=FALSE], on=.(genotype, trial)], aes(y=get(input$picker_trait), x=EnvMean, color=genotype), size=4, shape=1, stroke=2)
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
            if (input$FW_picker_plot_type=="line" & !input$FW_coord_equal){
              p + coord_cartesian()
            } else {
              p
            }
            #}
          })

      #### Handle hover event ####
      observe({
        output$FWhover_vinfo <- renderText({
          if(!is.null(input$FWplot_hover)) {
            #browser()
            F <- as.data.table(rv$TDFWplot$fittedGeno)
            E <- as.data.table(rv$TDFWplot$envEffs)
            EF <- E[F, on=.(Trial=trial)]
            hover=input$FWplot_hover
            dist=sqrt((hover$x-EF$EnvMean)^2+(hover$y-EF$fittedValue)^2)
            prox <- max(c(EF$EnvMean,EF$fittedValue))/30
            genot <- as.character(EF$genotype)[which.min(dist)]
            if(min(dist) < prox & input$FW_picker_plot_type%in%c("line","scatterFit")) {genot}
          }
        })
        # output$FWhover_info <- renderText({
        #   if(!is.null(input$FWplot_hover)) {
        #   #browser()
        #   F <- as.data.table(rv$TDFWplot$fittedGeno)
        #   E <- as.data.table(rv$TDFWplot$envEffs)
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
      })
      #### Handle click event ####
      
      observeEvent(input$FWplot_click,{
        if(!is.null(input$FWplot_hover)) {
          #browser()
          F <- as.data.table(rv$TDFWplot$fittedGeno)
          E <- as.data.table(rv$TDFWplot$envEffs)
          EF <- E[F, on=.(Trial=trial)]
          hover=input$FWplot_hover
          dist=sqrt((hover$x-EF$EnvMean)^2+(hover$y-EF$fittedValue)^2)
          prox <- max(c(EF$EnvMean,EF$fittedValue))/30
          rv$FWclicked_genotypes <- unique(c(rv$FWclicked_genotypes,as.character(EF$genotype)[which.min(dist)]))
          dtsc <- dcast(rbindlist(rv$TD)[,c("genotype","trial",input$picker_trait), with = FALSE],genotype~trial)[rv$sensclust, on=.(genotype=Genotype)][,-c("SE_GenMean","SE_Sens","MSdeviation")]
          formatcols <- colnames(dtsc)[-which(colnames(dtsc)%in%c("genotype","sensitivity_cluster", "Rank"))]
          output$FW_sens_clusters_DT <- DT::renderDataTable(formatRound(DT::datatable(dtsc, filter = "top",
                                                                                      selection = list(mode="multiple", 
                                                                                                       selected=which(dtsc$genotype%in%rv$FWclicked_genotypes))),
                                                                        columns = formatcols,digits = 2),rownames= FALSE, server=TRUE)
        }
      })
      #### Handle dbleclick event ####
      
      observeEvent(input$FWplot_dblclick,{
        rv$FWclicked_genotypes <- NULL
        selectRows(dtproxy, selected=NULL)
        
      })
      
      
      #### compute sensitivity_clusters whenever a picker changes ####
      observeEvent(  eventExpr = {
        input$FW_picker_color_by
        input$FW_cluster_sensitivity_nb
        input$FW_picker_cluster_on
        input$FW_picker_plot_type
        input$FW_picker_cluster_meth
        rv$TDFW
      }, handlerExpr = {
        req(rv$TDFW)
        req(input$FW_cluster_sensitivity_nb, input$FW_picker_cluster_on)
        #browser()
        #if (input$FW_picker_plot_type=="trellis"){
        #  output$FW_trellis_genot_select_ui <- renderUI({
        #    genots <- as.character(unique(rbindlist(rv$TDFW$TD)$genotype))
        #    pickerInput(ns("FW_trellis_genot_select"), label = "Select genotypes", choices = genots, selected = genots[1:round(length(genots)*0.05,0)], multiple = T, options = pickerOptions(liveSearch = TRUE))
        #  })
        #} else {
        #  output$FW_trellis_genot_select_ui <- renderUI({NULL})
        #}
        if (input$FW_picker_plot_type=="line" & input$FW_picker_color_by=="sensitivity clusters"){
          sensclust <- data.table(rv$TDFW$estimates)
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
          rv$sensclust <- sensclustren
          rv$TDFWplot <- rv$TDFW
          rv$TDFWplot$TD <- lapply(rv$TDFWplot$TD, function(a) data.table(a)[rv$sensclust, on=.(genotype=Genotype)])
          output$FW_sens_clust_select <- renderUI(
            prettyRadioButtons( 
              inputId = ns("FW_sens_clust_select_buttons"),
              label = "Select a cluster to highlight",
              choices = c("none", sort(unique(rv$sensclust$sensitivity_cluster))),
              inline = TRUE,
              shape = "round",
              status = "primary",
              fill = TRUE,
              bigger = TRUE
            )
          )
        } else {
          sensclust <- data.table(rv$TDFW$estimates)
          rv$sensclust <- sensclust
          output$FW_sens_clust_select <- renderUI(expr = NULL)
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
      

      #### Render FW sensclusters DT ####
      observe({
        req(rv$sensclust)
        #browser()
        dtsc <- dcast(rbindlist(rv$TD)[,c("genotype","trial",input$picker_trait), with = F],genotype~trial)[rv$sensclust, on=.(genotype=Genotype)][,-c("SE_GenMean","SE_Sens","MSdeviation")]
        formatcols <- colnames(dtsc)[-which(colnames(dtsc)%in%c("genotype","sensitivity_cluster", "Rank"))]
        if (any(colnames(dtsc)%in%"sensitivity_cluster")){
          dtsc[,sensitivity_cluster:=as.character(sensitivity_cluster)]
        }
        output$FW_sens_clusters_DT <- DT::renderDataTable(formatRound(DT::datatable(dtsc, filter = "top"),
                                                                      columns = formatcols,
                                                                      digits = 2),
                                                          rownames= FALSE,
                                                          selection = 'multiple')
        dtproxy <<- dataTableProxy('FW_sens_clusters_DT')
        if (any(colnames(rv$sensclust)=="sensitivity_cluster")){
          shinyjs::enable("create_groups_from_sensclusters")
          #shinyjs::addClass("create_groups_from_sensclusters", "active")
        } else {
          shinyjs::disable("create_groups_from_sensclusters")
        }
        
      })
      
      observeEvent(input$sens_clusters_DT.clearsel,{
        rv$FWclicked_genotypes <- NULL
        selectRows(dtproxy, selected=NULL)
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
        
        clusters <- unique(rbindlist(rv$TD)[,.(genotype,germplasmDbId,germplasmName)])[rv$sensclust, on=.(genotype=Genotype)][order(sensitivity_cluster)][,.(
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
        rv$groups <- rbindlist(list(
          rv$groups,
          clusters
        ), fill = T, use.names = T)
        
        ## update selectors (shape, colour)
        data_plot <- copy(rv$data_plot) # to avoid reactivity issues related to assignment by reference
        ## Revoir ca pour ne créer qu'un seule varaible dans data.plot avec les numéros de clusters
        #browser()
        #for(id in clusters[,unique(group_id)]){
        #  group_name <- clusters[group_id == id,group_name]
        #  data_plot[germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)], eval(group_name) := paste0('In "', group_name,'"')]
        #  data_plot[!(germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)]), eval(group_name) := paste0('Not in "', group_name,'"')]
        #}
        data_plot <- setnames(data_plot[clusters[,.(germplasmDbId=unlist(germplasmDbIds)),sensitivity_cluster],on=.(germplasmDbId)],old = "sensitivity_cluster",new = paste0("cl",clustering_id,"_FW@",input$picker_trait))[]
        rv$column_datasource <- rbindlist(
          list(
            rv$column_datasource,
            #data.table(cols = clusters[,unique(group_name)],  type = "Text", source = "group", visible = T)
            data.table(cols = paste0("cl",clustering_id,"_FW@",input$picker_trait) ,  type = "Text", source = "group", visible = T)
          )
        )
        rv$data_plot <- data_plot

        
      })
      
      observeEvent(input$create_groups_from_selgeno,{
        #browser()
        if(length(rv$selected_genotypes)>0){
          #browser()
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               rv$sensclust[input$FW_sens_clusters_DT_rows_selected,]),
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
        withCallingHandlers({
        rv$TDGGEmetan <- tryCatch(metan::gge(rv$TD.metangge,
                                             env=trial,
                                             gen=genotype,
                                             resp = input$picker_trait,
                                             centering = input$GGE_advs_centering,
                                             scaling = input$GGE_advs_scaling,
                                             svp = input$GGE_advs_svp,), error=function(e) e)
        rv$TDGGE <- tryCatch(gxeGGE(TD = rv$TD, trait = input$picker_trait, useWt = input$use_weights), error=function(e) e)
        },
        message = function(m) rv$console <- paste(rv$console, paste0("GGE run at ",Sys.time(), " : ",m), sep=""),
        warning = function(w) rv$console <- paste(rv$console, paste0("GGE run at ",Sys.time(), " : ",w), sep=""))
        
        
        updatePickerInput(
          session, "GGE_picker_gen_select",
          choices = rv$TDGGEmetan[[1]]$labelgen
          #selected = character(0)
        )
        updatePickerInput(
          session, "GGE_picker_gen2_select",
          choices = rv$TDGGEmetan[[1]]$labelgen,
          selected = rv$TDGGEmetan[[1]]$labelgen[2]
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
        #browser()
        output$GGE_plot <- renderPlot({
          #output$FW_plot <- renderPlotly({
          #browser()
          TDGGEplot <- rv$TDGGEmetan
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
              rv$gp_WwW <- gg$layers[[length(gg$layers)]]$data$label
            }
          }
          if (input$GGE_colorGenoBy!="Nothing" & input$GGE_picker_plot_type!=2){
            #browser() 
            geompdat <- as.data.table(gg$data)
            geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$GGE_colorGenoBy)]), by.x = "label", by.y = "genotype", all = TRUE)
            
            gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
            gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat, aes(d1, d2, color=as.factor(.data[[input$GGE_colorGenoBy]]), fill = as.factor(.data[[input$GGE_colorGenoBy]]), shape = type), size = input$GGE_plot_size.shape, 
                                  stroke = input$GGE_plot_size.stroke, alpha = input$GGE_plot_col.alpha) + scale_fill_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + scale_color_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
          }
          #browser()
          rv$GGEplotdat <- gg$data
          if(length(rv$GGEclicked_genotypes)>0){
            clickgeno <- gg$data[gg$data$type=="genotype" & gg$data$label%in%rv$GGEclicked_genotypes,]
            gg + ggnewscale::new_scale_fill()
            gg <- gg + geom_point(data = clickgeno, aes(x=d1, y = d2), shape = 21, size=input$GGE_plot_size.shape+2, stroke=input$GGE_plot_size.stroke, color="red") 
          }
          gg
          
        })
      })
      #### Handle click event ####
      
      observeEvent(input$GGEplot_click,{
        if(!is.null(input$GGEplot_click)) {
          
          GG <- rv$GGEplotdat
          click=input$GGEplot_click
          dist=sqrt((click$x-GG[,1])^2+(click$y-GG[,2])^2)
          clickedgeno <- GG$label[which.min(dist)]
          if (clickedgeno%in%rv$GGEclicked_genotypes){
            rv$GGEclicked_genotypes <- rv$GGEclicked_genotypes[-which(rv$GGEclicked_genotypes==clickedgeno)]
          } else {
            rv$GGEclicked_genotypes <- unique(c(rv$GGEclicked_genotypes,clickedgeno))
          }
          #browser()
        }
      })
      #### Handle dbleclick event ####
      
      observeEvent(input$GGEplot_dblclick,{
        rv$GGEclicked_genotypes <- NULL
      })
      
      observeEvent(input$create_group_from_wWw,{
        #browser()
        if(length(rv$gp_WwW)>0){
          #browser()
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               Genotype=rv$gp_WwW),
                                                  y=unique(rbindlist(rv$TD)),
                                                  by.x = "Genotype",
                                                  by.y ="genotype", all.x = TRUE, all.y = FALSE)[,.(group_id, germplasmDbId, germplasmName, plot_param="None", Genotype)])[, .(.N, germplasmDbIds=list(germplasmDbId), germplasmNames=list(germplasmName),plot_params=list(plot_param), germplasmNames_label=paste(Genotype, collapse=", ")), group_id]
          showModal(groupModal(rv=rv, 
                               parent_session = parent_session, 
                               modal_title = "Create new group", 
                               group_description = paste0("Group of which won where genotypes in GGE analysis of ", input$picker_trait, " variable"),
                               group_prefix =paste0("www_GGE@",input$picker_trait,".")
          )
          )
        }
      })
      ### Handle group creation in GGE plot ####
      observe({
        if(length(rv$GGEclicked_genotypes)<1){
          shinyjs::disable("create_groups_from_GGEsel")
        } else {
          shinyjs::enable("create_groups_from_GGEsel")
        }
      })
      observeEvent(input$create_groups_from_GGEsel,{
        if(length(rv$GGEclicked_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv$GGEclicked_genotypes)),
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
        #browser()
        withCallingHandlers({
        rv$TDAMMI <- tryCatch(gxeAmmi(TD = rv$TD,
                                      trait = input$picker_trait,
                                      nPC = switch((input$AMMI_nPC=="Auto")+1,  as.numeric(input$AMMI_nPC,NULL)),
                                      byYear = FALSE, #input$AMMI_byYear,
                                      center = input$AMMI_center,
                                      excludeGeno = input$AMMI_excludeGeno,
                                      useWt = input$use_weights), error=function(e) e)
        },
        message = function(m) rv$console <- paste(rv$console, paste0("AMMI run at ",Sys.time(), " : ",m), sep=""),
        warning = function(w) rv$console <- paste(rv$console, paste0("AMMI run at ",Sys.time(), " : ",w), sep=""))
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
          req(input$AMMI_primAxis, input$AMMI_secAxis)
          # This is to update the dat component of TDAMMI whenever TD changes
          # this occurs for example at group creation, a column with group memberships
          # is added to the TD object and available in the colorGenoBy picker input
          rv$TDAMMI$dat <- rbindlist(rv$TD)
            p <- statgenGxE:::plot.AMMI(rv$TDAMMI,
                                        plotType = input$AMMI_plotType,
                                        scale = input$AMMI_scale,
                                        plotGeno = TRUE,
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
            # Following is to handle vizualization of clicked genotypes
            # data structure is different for AMMI1 and AMMI2 plots
            # hence the distinction between both cases
            if (input$AMMI_plotType=="AMMI2"){
              p$layers[[1]] <- NULL
              p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]]), size=input$AMMI_plot_sizePoint) +
              geom_text(data=p$data[p$data$type=="env",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="env",]))) +
              geom_text(data =p$data[p$data$type=="geno",], aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]], label=rownames(p$data[p$data$type=="geno",]), size=input$AMMI_plot_sizeGeno), position = position_nudge(y=input$AMMI_plot_envFactor*max(p$data[p$data$type=="geno",input$AMMI_secAxis])/8))
              
              if (!is.null(rv$AMMIclicked_genotypes)){
                clickgeno <- p$data[p$data$type=="geno" & row.names(p$data)%in%rv$AMMIclicked_genotypes,]
                p <- p + geom_point(data = clickgeno, aes(x=.data[[input$AMMI_primAxis]], y = .data[[input$AMMI_secAxis]]), shape = 21, color="red", size=input$AMMI_plot_sizePoint+1)
              }
            } else {
              p <- p + geom_point(data = p$data[p$data$type=="geno",], aes(x=x, y = y), size=input$AMMI_plot_sizePoint) #+ 
              if (!is.null(rv$AMMIclicked_genotypes)){
                clickgeno <- p$data[p$data$type=="geno" & row.names(p$data)%in%rv$AMMIclicked_genotypes,]
                p <- p + ggnewscale::new_scale_color()
                p <- p + geom_point(data = clickgeno, aes(x=x, y = y), shape = 21, color="red", size=input$AMMI_plot_sizePoint+1)
              }
            }
          # This reactive is useful to handle click event
          rv$AMMIplotdat <- p$data
          p
        })
      })
      
      #### Handle click event ####
      
      observeEvent(input$AMMIplot_click,{
        if(!is.null(input$AMMIplot_click)) {
          #browser()
          if (input$AMMI_plotType=="AMMI2"){
            AG <- rv$AMMIplotdat[,c(input$AMMI_primAxis,input$AMMI_secAxis)]
            click=input$AMMIplot_click
            dist=sqrt((click$x-AG[,1])^2+(click$y-AG[,2])^2)
            clickedgeno <- as.character(row.names(AG))[which.min(dist)]
          } else {
            AG <- rv$AMMIplotdat
            click=input$AMMIplot_click
            dist=sqrt((click$x-AG$x)^2+(click$y-AG$y)^2)
            clickedgeno <- as.character(row.names(AG))[which.min(dist)]
          }
          if (clickedgeno%in%rv$AMMIclicked_genotypes){
            rv$AMMIclicked_genotypes <- rv$AMMIclicked_genotypes[-which(rv$AMMIclicked_genotypes==clickedgeno)]
          } else {
            rv$AMMIclicked_genotypes <- unique(c(rv$AMMIclicked_genotypes,clickedgeno))
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
        rv$AMMIclicked_genotypes <- NULL
      })
      
      
      ### Handle group creation in AMMI plot ####
      observe({
        if(length(rv$AMMIclicked_genotypes)<1){
          shinyjs::disable("create_groups_from_AMMIsel")
        } else {
          shinyjs::enable("create_groups_from_AMMIsel")
        }
      })
      observeEvent(input$create_groups_from_AMMIsel,{
        if(length(rv$AMMIclicked_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv$AMMIclicked_genotypes)),
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
      observe({
        req(rv$TD)
        TDStab <- tryCatch(statgenGxE::gxeStability(TD = rv$TD,
                                                        trait = input$picker_trait), error=function(e) e)
        dtsup <- as.data.table(TDStab$superiority)
        dtsta <- as.data.table(TDStab$static)
        dtwri <- as.data.table(TDStab$wricke)
        TDStab$dtres <- as.data.frame(dtsup[dtsta[,.(Genotype,Static)], on=.(Genotype)][dtwri[,.(Genotype,Wricke)], on=.(Genotype)])
        rv$TDStab <- TDStab
      })
      #### Render DT ####
      output$STAB_sup <- renderDataTable({
        #browser()
        formatRound(datatable(rv$TDStab$dtres[order(!rv$TDStab$dtres$Genotype%in%rv$STSclicked_genotypes),], rownames = FALSE,
                              options = list(pageLength = 30),
                              selection = list(mode="multiple", 
                                               selected=which(rv$TDStab$dtres$Genotype[order(!rv$TDStab$dtres$Genotype%in%rv$STSclicked_genotypes)]%in%rv$STSclicked_genotypes))),
                    columns = c("Mean", "Superiority", "Static", "Wricke"), 
                    digits=3)
      })
      #### Handle DT selection ####
      observeEvent(input$STAB_sup_rows_selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
        #browser()
        req(abs(lastsel_stabtab - Sys.time()) >=0.3)
        if (!is.null(input$STAB_sup_rows_selected)){
          tab <- rv$TDStab$dtres[order(!rv$TDStab$dtres$Genotype%in%rv$STSclicked_genotypes),]
          rv$STSclicked_genotypes <- as.character(tab[input$STAB_sup_rows_selected, "Genotype"])
          lastsel_stabtab <<- Sys.time()
        } else {
          rv$STSclicked_genotypes <- NULL
        }
      })
      #### Handle DT Copy ####
      observe({
        req(nrow(rv$TDStab$superiority)>0)
        output$copy_STABsup_table <- renderUI({
          rclipboard::rclipButton("clipbtnsup_table", "Copy table", paste(paste(colnames(rv$TDStab$superiority),collapse="\t"),
                                                                          paste(apply(rv$TDStab$superiority,1,paste,collapse="\t"),collapse = "\n"),
                                                                          sep="\n"))#, shiny::icon("clipboard"))
        })
      })
      
      #### Superiority ####
      ##### Render Plot ####
      output$STAB_sup_plot <- renderPlot({
          gg <- ggplot(rv$TDStab$superiority) + 
                     geom_point(aes(x=Mean, y= sqrt(Superiority), text = Genotype)) +
                     ylab("Square root of superiority")
          rv$st_sup_plotdat <- gg$data
          if (input$STAB_plots_colorby!="Nothing"){
            #browser() 
            geompdat <- as.data.table(gg$data)
            geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
            gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
            
            gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrt(Superiority), color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
                                    scale_fill_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
                                    scale_color_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
          }
          if(length(rv$STSclicked_genotypes)>0){
            clickgeno <- gg$data[gg$data$Genotype%in%rv$STSclicked_genotypes,]
            gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrt(Superiority)), shape = 21, size=3, color="red") +
                       geom_text(data = clickgeno, aes(x=Mean , y = sqrt(Superiority), label=Genotype), size=2, color="red",
                                 position = position_nudge(y=max(gg$data[,"Superiority"])/150))
          }
          gg
        })
        
        ##### Handle click event ####
        observeEvent(input$STAB_sup_plot_click,{
          req(abs(lastclick_stabsup - Sys.time()) >=0.8)
          if(!is.null(input$STAB_sup_plot_click)) {
              clicked_genotypes <- rv$STSclicked_genotypes
              sts <- rv$st_sup_plotdat
              click=input$STAB_sup_plot_click
              dist=sqrt((click$x-sts[,2])^2+(click$y-sqrt(sts[,3]))^2)
              clickedgeno <- as.character(sts$Genotype[which.min(dist)])
              if (clickedgeno%in%clicked_genotypes){
                rv$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
              } else {
                rv$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
              }
              lastclick_stabsup <<- Sys.time()
          }
        })
        ##### Handle dbleclick event ####
        observeEvent(input$STAB_sup_plot_dblclick,{
          rv$STSclicked_genotypes <- NULL
        })
      
      
      #### Static ####
        
      ##### Render Plot ####
      
        output$STAB_static_plot <- renderPlot({
          gg <- ggplot(rv$TDStab$static) + 
                     geom_point(aes(x=Mean, y= sqrt(Static), text = Genotype)) +
                     ylab("Square root of Static stability")
          rv$st_sta_plotdat <- gg$data
          if (input$STAB_plots_colorby!="Nothing"){
            geompdat <- as.data.table(gg$data)
            geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
            gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
            
            gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrt(Static), color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
              scale_fill_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
              scale_color_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
          }
          if(length(rv$STSclicked_genotypes)>0){
            clickgeno <- gg$data[gg$data$Genotype%in%rv$STSclicked_genotypes,]
            gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrt(Static)), shape = 21, size=3, color="red") + 
              geom_text(data = clickgeno, aes(x=Mean , y = sqrt(Static), label=Genotype), size=2, color="red",
                        position = position_nudge(y=max(gg$data[,"Static"])/150))
            
          }
          gg
          
        })
        ##### Handle click event ####
        observeEvent(input$STAB_static_plot_click,{
          req(abs(lastclick_stabsta - Sys.time()) >=0.8)
          
          if(!is.null(input$STAB_static_plot_click)) {
            clicked_genotypes <- rv$STSclicked_genotypes
            sta <- rv$st_sta_plotdat
            click=input$STAB_static_plot_click
            dist=sqrt((click$x-sta[,2])^2+(click$y-sqrt(sta[,3]))^2)
            clickedgeno <- as.character(sta$Genotype[which.min(dist)])
            if (clickedgeno%in%clicked_genotypes){
              rv$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
            } else {
              rv$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
            }
            lastclick_stabsta <<- Sys.time()
            
          }
        })
        ##### Handle dbleclick event ####
        observeEvent(input$STAB_static_plot_dblclick,{
          rv$STSclicked_genotypes <- NULL
        })
        
      #### Wricke ####
        
      ##### Render Plot ####
      
        output$STAB_wricke_plot <- renderPlot({
          gg <- ggplot(rv$TDStab$wricke) + 
            geom_point(aes(x=Mean, y= sqrt(Wricke), text = Genotype)) +
            ylab("Square root of Wricke ecovalence")
          rv$st_stw_plotdat <- gg$data
          if (input$STAB_plots_colorby!="Nothing"){
            geompdat <- as.data.table(gg$data)
            geompdat <- merge.data.table(x=geompdat, y=unique(rbindlist(rv$TD)[,.SD,.SDcols=c("genotype",input$STAB_plots_colorby)]), by.x = "Genotype", by.y = "genotype", all = TRUE)
            gg$layers[[which(unlist(lapply(gg$layers, function(a) class(a$geom)[1]))=="GeomPoint")[1]]] <- NULL
            
            gg + ggnewscale::new_scale_fill() + ggnewscale::new_scale_color()
            gg <- gg + geom_point(data=geompdat, aes(x=Mean, y= sqrt(Wricke), color=as.factor(.data[[input$STAB_plots_colorby]]), fill = as.factor(.data[[input$STAB_plots_colorby]]))) + 
              scale_fill_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none") + 
              scale_color_manual(values=getOption("statgen.genoColors"), na.value = "forestgreen", guide="none")
          }
          if(length(rv$STSclicked_genotypes)>0){
            clickgeno <- gg$data[gg$data$Genotype%in%rv$STSclicked_genotypes,]
            gg <- gg + geom_point(data = clickgeno, aes(x=Mean , y = sqrt(Wricke)), shape = 21, size=3, color="red") +
                       geom_text(data = clickgeno, aes(x=Mean , y = sqrt(Wricke), label=Genotype), size=2, color="red",
                                 position = position_nudge(y=max(gg$data[,"Wricke"])/150))
            
          }
          gg
          
        })
        ##### Handle click event ####
        observeEvent(input$STAB_wricke_plot_click,{
          req(abs(lastclick_stabwri - Sys.time()) >=0.8)
          
          if(!is.null(input$STAB_wricke_plot_click)) {
            clicked_genotypes <- rv$STSclicked_genotypes
            stw <- rv$st_stw_plotdat
            click=input$STAB_wricke_plot_click
            dist=sqrt((click$x-stw[,2])^2+(click$y-sqrt(stw[,3]))^2)
            clickedgeno <- as.character(stw$Genotype[which.min(dist)])
            if (clickedgeno%in%clicked_genotypes){
              rv$STSclicked_genotypes <- clicked_genotypes[-which(clicked_genotypes==clickedgeno)]
            } else {
              rv$STSclicked_genotypes <- unique(c(clicked_genotypes,clickedgeno))
            }
            lastclick_stabwri <<- Sys.time()
            
          }
        })
        ##### Handle dbleclick event ####
        observeEvent(input$STAB_wricke_plot_dblclick,{
          rv$STSclicked_genotypes <- NULL
        })
        
      #### Handle group creation in Stability selection ####
      observe({
        if(length(rv$STSclicked_genotypes)<1){
          shinyjs::disable("create_groups_from_STABsel")
        } else {
          shinyjs::enable("create_groups_from_STABsel")
        }
      })
      observeEvent(input$create_groups_from_STABsel,{
        if(length(rv$STSclicked_genotypes)>0){
          rv$selection <- unique(merge.data.table(x=data.table(group_id=ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1),
                                                               data.table(Genotype=rv$STSclicked_genotypes)),
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