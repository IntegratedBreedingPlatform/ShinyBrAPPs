#' @export
mod_scatterplot_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$style(
      HTML(
        ".nav .nav-item .nav-link { font-size: 20px; }
        .center-checkbox {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100%; /* Optionnel, pour centrer verticalement dans une colonne pleine hauteur */
        }
        ",
      )
    ),
    
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
         pickerInput(
           inputId = ns("env"),
           label = "Environments",
           choices = NULL,
           width = "100%",
           options = list(container = "body"), 
           multiple = T
         ),
         #tags$h4("Aggregate observations", style = "display:inline", class = "space-right"),
         fluidRow(
           materialSwitch(inputId = ns("switch_aggregate"), label = "Aggregate observations", value = F, inline = T, status = "info"),
           pickerInput(ns("aggregate_by"), "Aggregate by",  options = list(container = "body"), choices =  c("germplasm and environment", "germplasm"), selected = "germplasm", multiple = F)
         ),
         fluidRow(
           column(6, pickerInput(ns("picker_X"), "X",  options = list(container = "body"), choices = NULL)),
           column(6, hidden(pickerInput(ns("aggreg_fun_X"), "Aggregate", choices = c("mean", "max", "min", "sum"))))
         ),
         fluidRow(
           column(6, awesomeCheckbox(ns("express_X_as_ranks"), label = "As ranks")),
           column(6, hidden(switchInput(ns("ranking_order_X"), onLabel = "asc", offLabel = "desc", size = "mini")))
         ),
        fluidRow(
          column(6, awesomeCheckbox(ns("express_X_relative"), label = "relatively to genotype")),
          column(6, hidden(pickerInput(ns("ref_genotype_X"), "Reference", choices = NULL, inline = T)))
        ),
         fluidRow(
           column(6, pickerInput(ns("picker_Y"), "Y",  options = list(container = "body"), choices = NULL)),
           column(6, hidden(pickerInput(ns("aggreg_fun_Y"), "Aggregate", choices = c("mean", "max", "min", "sum"))))
         ),
        fluidRow(
          column(6, awesomeCheckbox(ns("express_Y_as_ranks"), label = "As ranks"), inline = T),
          column(6, hidden(switchInput(ns("ranking_order_Y"), onLabel = "asc", offLabel = "desc", size = "mini")))
        ),
        fluidRow(
          column(6, awesomeCheckbox(ns("express_Y_relative"), label = "relatively to genotype"), inline = T),
          column(6, hidden(pickerInput(ns("ref_genotype_Y"), "Reference", choices = NULL, inline = T)))
        ),
         div(
           bslib::card(
             bslib::card_header(
               h4('Options ', icon('screwdriver-wrench'))
             ),
             bslib::card_body(
               fluidRow(
                 column(2),
                 column(5, strong("Variables")),
                 column(5, hidden(strong("Aggregate", id = ns("aggregate_text"))))
               ),
               
               fluidRow(
                 column(2, strong("Shape")),
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",pickerInput(ns("picker_SHAPE"), options = list(container = "body"), choices = c("---" = ""), selected = ""))
                 ),
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",pickerInput(ns("aggreg_fun_SHAPE"),  options = list(container = "body"), choices = c("concatenate unique values"="unique_values")))
                 )
               ),
               fluidRow(
                 column(2, strong("Colour")),
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",
                            pickerInput(ns("picker_COLOUR"),  options = list(container = "body"), choices = c("---" = ""), selected = ""))
                 ), 
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",
                            pickerInput(ns("aggreg_fun_COLOUR"),  options = list(container = "body"), choices = c("concatenate unique values"="unique_values")))
                 )
               ),
               fluidRow(
                 column(2, strong("Size")),
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",
                            pickerInput(ns("picker_SIZE"),  options = list(container = "body"), choices = c("---"= ""), selected = ""))
                 ),
                 column(5, 
                        div(style = "display:inline-block; width: 100%;",
                            pickerInput(ns("aggreg_fun_SIZE"),  options = list(container = "body"), choices = c("concatenate unique values"="unique_values")))
                 )
               )
             )
           )
         )
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::layout_sidebar(
          
         plotlyOutput(
           ns("scatterplot"), width = "100%", height = "600px"
           # click = ns("scatterplot_click"),
           # hover = ns("scatterplot_hover"),
           # brush = ns("scatterplot_brush")
         ),
         bsTooltip(ns("scatterplot"), title = "Tip: press the shift key for multiple mouse selection", placement = "left"),
         fluidRow(
           column(
             12,
             hidden(actionButton(ns("go_regression"), "Draw Regression", css.class = "btn btn-info", `aria-pressed`="true", icon = "box")),
             uiOutput(ns("ui_clusters"), inline = T),
             span(class = ns("ui_create_group"), style = "display: none;",
                  actionButton(ns("go_create_group"), "Create Group", css.class = "btn btn-info")
             ),
             bsModal(ns("modal_create_group"), "Create Group", NULL, size = "small",
                     uiOutput(ns("modal_create_group_ui")))
           )
         ),
         fluidRow(
           column(
             12,
             uiOutput(ns("regression_output"))
           )
         ),
         sidebar = bslib::sidebar(
             position = "right",
             width = 300,
             fluidRow(
               column(12,uiOutput(ns("ui_groups")))
             ),
             fluidRow(
               column(12,
                  bslib::card(
                    class = ns("at_least_one_group_selected"),
                    bslib::card_header(
                      h4('Actions ', icon('screwdriver-wrench'))
                    ),
                    bslib::card_body(
                      #title = span('Options ', icon('screwdriver-wrench')),
                      #width = 12,
                      #h4('Actions ', icon('screwdriver-wrench')),
                      actionButton(ns("action__seplot_creation_params"),label = "Visualize like at group creation", block = T, css.class = paste("btn btn-info", ns("one_group_selected"))),
                      actionButton(ns("action_groups_union"),label = "Union", block = T, css.class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                      actionButton(ns("action_groups_intersect"),label = "Intersect", block = T, css.class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                      actionButton(ns("action_groups_complement"),label = "Complement", block = T, css.class = paste("btn btn-info", ns("at_least_one_group_selected"))),
                      actionButton(ns("action_groups_delete"),label = "Delete", block = T, css.class =paste("btn btn-info", ns("at_least_one_group_selected")))
                    )
                  ),
                  bslib::card(
                    id = ns("export_box"),
                    bslib::card_header(
                      h4('Export ')
                    ),
                    bslib::card_body(
                      downloadButton(ns("action_groups_export_group_details"),label = "Export Group Details", class = "btn-block btn-primary"),
                      actionButton(ns("action_groups_export_as_list"),label = "Export as List", block = T, css.class = "btn btn-primary", icon = "cloud", icon.library = "font awesome"),
                      actionButton(ns("action_groups_mark_as_selection"),label = "Mark as Selection", block = T, css.class = "btn btn-primary", icon = "cloud", icon.library = "font awesome")
                    )
                  )
               )
             )
           )
         )
      )
    ),
    # bsModal(ns("modal_export_group_as_list"), "Export Group as List", NULL, size = "l",
    #         uiOutput(ns("modal_export_group_as_list_ui"))),
    # bsModal(ns("modal_export_group_mark_as_selection"), "Mark as Selection", NULL, size = "l",
    #         uiOutput(ns("modal_export_group_mark_as_selection_ui")))
  )
}

#' @export
mod_scatterplot_server <- function(id, rv){
  no_selection <- list("-----------" = "")
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv_plot <- reactiveValues()
      
      rv_plot$groups <- data.table()
      rv_plot$draw_regression <- F
      rv_plot$draw_clusters <- F
      rv_plot$counter_hca <- 0
      rv_plot$counter_kmeans <- 0
      rv_plot$selected_express_X_as <- NULL
      rv_plot$selected_express_Y_as <- NULL
      
      ## function for data aggregation
      aggreg_functions <- data.table(
        fun = c("mean", "max", "min", "sum", "unique_values"),
        label = c("mean", "max", "min", "sum", "concatenate unique values"),
        for_num = c(T,T,T,T,F)
      )
      unique_values <- function(x, ...){
        xx <- unique(x)
        if(any(!is.na(xx))){
          xx <- xx[!is.na(xx)]
        }
        paste(unique(xx), collapse = ", ")
      }
      ## util for the scatterplot
      scale_color_custom <- function(is_num, name, ...){
        if(is_num==T){
          scale_color_continuous(name = name)
        }else{
          scale_color_discrete(name = name)
        }
      }
      
      
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        ## turn numeric variables that should not be numeric into text
        for(col in names(rv$data_plot)[
          rv$data_plot[, lapply(.SD, is.numeric)]==T &
          names(rv$data_plot) %in% rv$column_datasource[type != "Numerical", cols]
        ]){
          rv$data_plot[, c(col) := list(as.character(eval(as.name(col))))]
        }
        
        ## update environments
        envs <- unique(rv$data_plot[,.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]
        
        # to avoid selected env reinitialized after creating a group
        if (is.null(input$env)) {
          env_selected <- env_choices
        } else {
          env_selected <- input$env
        }
        updatePickerInput(
          session, "env",
          choices = env_choices, selected = env_selected
        )
      })
      
      observe({
        req(rv$data_plot)
        ## update group of germplasms
        germplasmNames <- rv$data_plot[,.(germplasmNames = list(unique(germplasmName))), .(entryType)][order(entryType)]
        updatePickerInput(
          session = session, inputId = "ref_genotype_X",
          choices = setNames(germplasmNames[,germplasmNames], germplasmNames[,entryType]),
          options = list(
            title = "Select reference germplasm",
            `live-search` = TRUE
          )
        )
        updatePickerInput(
          session = session, inputId = "ref_genotype_Y",
          choices = setNames(germplasmNames[,germplasmNames], germplasmNames[,entryType]),
          options = list(
            title = "Select reference germplasm",
            `live-search` = TRUE
          )
        )
      })
      
      observeEvent(input$left_side,{
        updateControlbar(id = "sidebar", session = session)
      })
      
      observeEvent(input$right_side,{
        updateControlbar(id = "controlbar", session = session)
      })
      
      ## update variable selectors
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        rv$column_datasource[,.N]
        
        print("update variable selectors: debut")
        
        num_var_choices <- rv$column_datasource[type == "Numerical",.(cols = list(cols)), source]
        non_num_var_choices <- rv$column_datasource[type != "Numerical",.(cols = list(cols)), source]
        var_choices_all <- rv$column_datasource[,.(cols = list(cols)), source]
        
        if(input$switch_aggregate == T){
          shinyjs::show("aggregate_by")
          shinyjs::show("aggreg_fun_X")
          shinyjs::show("aggreg_fun_Y")
          shinyjs::show("aggregate_text")
          shinyjs::show("aggreg_fun_SHAPE")
          shinyjs::show("aggreg_fun_COLOUR")
          shinyjs::show("aggreg_fun_SIZE")
          shinyjs::show("express_X_relative")
          shinyjs::show("express_Y_relative")
          if(input$aggregate_by == "germplasm and environment"){
            var_choices_SHAPE <- rv$column_datasource[type != "Numerical" & !(source %in% "plot"),.(cols = list(cols)), source]
            var_choices_COLOUR <- rv$column_datasource[type == "Numerical" | !(source %in% c("plot")),.(cols = list(cols)), source]
          }else if(input$aggregate_by == "germplasm"){
            var_choices_SHAPE <- rv$column_datasource[type != "Numerical" & !(source %in% c("plot", "environment")),.(cols = list(cols)), source]
            var_choices_COLOUR <- rv$column_datasource[type == "Numerical" | !(source %in% c("plot", "environment")),.(cols = list(cols)), source]
          }
        }else{
          shinyjs::hide("aggregate_by")
          shinyjs::hide("aggreg_fun_X")
          shinyjs::hide("aggreg_fun_Y")
          shinyjs::hide("aggregate_text")
          shinyjs::hide("aggreg_fun_SHAPE")
          shinyjs::hide("aggreg_fun_COLOUR")
          shinyjs::hide("aggreg_fun_SIZE")
          shinyjs::hide("express_X_relative")
          shinyjs::hide("express_Y_relative")
          var_choices_SHAPE <- non_num_var_choices
          var_choices_COLOUR <- var_choices_all
        }
        
        
        # shinyjs::toggle("ui_SHAPE", condition = input$switch_aggregate==T)
        # shinyjs::toggle("ui_COLOUR", condition = input$switch_aggregate==T)
        # shinyjs::toggle("ui_SIZE", condition = input$switch_aggregate==T)
        
        # Work around for pickerInputs with option groups that have only one option.
        # The default behaviour is to display only the group name.
        # Work around: naming the group by the singleton element
        for(k in 1:num_var_choices[,.N]){
          cols <- num_var_choices[k,cols][[1]]
          if(length(cols)==1){num_var_choices[k, source := cols]}
        }
        for(k in 1:var_choices_SHAPE[,.N]){
          cols <- var_choices_SHAPE[k,cols][[1]]
          if(length(cols)==1){var_choices_SHAPE[k, source := cols]}
        }
        for(k in 1:var_choices_COLOUR[,.N]){
          cols <- var_choices_COLOUR[k,cols][[1]]
          if(length(cols)==1){var_choices_COLOUR[k, source := cols]}
        }
        
        # default values or reset previously selected values
        if(isTruthy(input$picker_X)){
          val_picker_X <- input$picker_X
        }else{
          val_picker_X <- rv$column_datasource[type == "Numerical" & source == "GxE"][1, cols]
        }
        if(isTruthy(input$picker_Y)){
          val_picker_Y <- input$picker_Y
        }else{
          if(rv$column_datasource[type == "Numerical" & source == "GxE"][,.N]>1){
            val_picker_Y <- rv$column_datasource[type == "Numerical" & source == "GxE"][2, cols]
          }else{
            val_picker_Y <- val_picker_X
          }
        }
        val_picker_SHAPE <- ""
        val_picker_COLOUR <- ""
        val_picker_SIZE <- ""
        if(isTruthy(input$picker_SHAPE)){val_picker_SHAPE <- input$picker_SHAPE}
        if(isTruthy(input$picker_COLOUR)){val_picker_COLOUR <- input$picker_COLOUR}
        if(isTruthy(input$picker_SIZE)){val_picker_SIZE <- input$picker_SIZE}
        picker_section_names <- data.frame(source=c("GxE",
                                                    "plot",
                                                    "germplasm",
                                                    "environment"),
                                           rename=c("Measured variable",
                                                    "Observation units attributes",
                                                    "Germplasm attributes",
                                                    "Environment details"))
        updatePickerInput(
          session = session, inputId = "picker_X",
          choices = setNames(num_var_choices[,cols], picker_section_names$rename[match(num_var_choices[,source], picker_section_names$source)]),
          selected = val_picker_X
        )
        updatePickerInput(
          session = session, inputId = "picker_Y",
          choices = setNames(num_var_choices[,cols], picker_section_names$rename[match(num_var_choices[,source], picker_section_names$source)]),
          selected = val_picker_Y
        )
        
        updatePickerInput(
          session = session, inputId = "picker_SIZE",
          choices = c(no_selection, setNames(num_var_choices[,cols], picker_section_names$rename[match(num_var_choices[,source], picker_section_names$source)])),
          selected = val_picker_SIZE
        )
        updatePickerInput(
          session = session, inputId = "picker_SHAPE",
          choices = c(no_selection, setNames(var_choices_SHAPE[,cols], picker_section_names$rename[match(var_choices_SHAPE[,source], picker_section_names$source)])),
          selected = val_picker_SHAPE
        )
        updatePickerInput(
          session = session, inputId = "picker_COLOUR",
          choices = c(no_selection, setNames(var_choices_COLOUR[,cols], picker_section_names$rename[match(var_choices_COLOUR[,source], picker_section_names$source)])),
          selected = val_picker_COLOUR
        )
      })
      
      ## update colour aggreg functions (colour can be num or categorical)
      observeEvent(input$picker_COLOUR, {
        req(input$picker_COLOUR)
        COLOUR_is_num <- rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"]
        choices_fun <- aggreg_functions[for_num == COLOUR_is_num, fun]
        names(choices_fun) <- aggreg_functions[for_num == COLOUR_is_num, label]
        updatePickerInput(
          session = session, inputId = "aggreg_fun_COLOUR",
          choices = choices_fun
        )
      })
      
      ## toggle draw regression button
      observeEvent(c(input$express_X_as_ranks, input$express_Y_as_ranks),{
        if(!is.null(input$express_X_as_ranks) && input$express_X_as_ranks
           && !is.null(input$express_Y_as_ranks) && input$express_Y_as_ranks){
          shinyjs::hide("go_regression")
          if(rv_plot$draw_regression == T){
            rv_plot$draw_regression <- F
          }
        }else{
          shinyjs::show("go_regression")
        }
      })
      
      observeEvent({
        list(input$express_X_as_ranks, input$express_X_relative)
        }, {
          if (is.null(rv_plot$selected_express_X)) {
            if (input$express_X_as_ranks) {
              rv_plot$selected_express_X = "ranks"
            } else if (input$express_X_relative) {
              rv_plot$selected_express_X = "relative"
            }
          } else {
            if (rv_plot$selected_express_X == "ranks" && input$express_X_relative) {
              updateCheckboxInput(session, "express_X_as_ranks", value = F)
              rv_plot$selected_express_X = "relative"
            } else if (rv_plot$selected_express_X == "relative" && input$express_X_as_ranks) {
              updateCheckboxInput(session, "express_X_as_relative", value = F)
              rv_plot$selected_express_X = "ranks"
            }
          }
          if (!input$express_X_as_ranks && !input$express_X_relative) {
            rv_plot$selected_express_X = NULL
          }
          shinyjs::toggle("ranking_order_X", condition = !is.null(rv_plot$selected_express_X) && rv_plot$selected_express_X == "ranks")
          shinyjs::toggle("ref_genotype_X", condition = !is.null(rv_plot$selected_express_X) && rv_plot$selected_express_X == "relative")
        })
      
      observeEvent({
        list(input$express_Y_as_ranks, input$express_Y_relative)
      }, {
        if (is.null(rv_plot$selected_express_Y)) {
          if (input$express_Y_as_ranks) {
            rv_plot$selected_express_Y = "ranks"
          } else if (input$express_Y_relative) {
            rv_plot$selected_express_Y = "relative"
          }
        } else {
          if (rv_plot$selected_express_Y == "ranks" && input$express_Y_relative) {
            updateCheckboxInput(session, "express_Y_as_ranks", value = F)
            rv_plot$selected_express_Y = "relative"
          } else if (rv_plot$selected_express_Y == "relative" && input$express_Y_as_ranks) {
            updateCheckboxInput(session, "express_Y_as_relative", value = F)
            rv_plot$selected_express_Y = "ranks"
          }
        }
        if (!input$express_Y_as_ranks && !input$express_Y_relative) {
          rv_plot$selected_express_Y = NULL
        }
        shinyjs::toggle("ranking_order_Y", condition = !is.null(rv_plot$selected_express_Y) && rv_plot$selected_express_Y == "ranks")
        shinyjs::toggle("ref_genotype_Y", condition = !is.null(rv_plot$selected_express_Y) && rv_plot$selected_express_Y == "relative")
      })
      
      ## aggreg dataset
      observe({
        #browser()
        req(rv$data_plot)
        req((is.null(input$express_X_as) ||  length(input$express_X_as) == 1 )
            && (is.null(input$express_Y_as) ||  length(input$express_Y_as) == 1))
        if(input$switch_aggregate== T){
          if(input$aggregate_by == "germplasm and environment"){
            group_by_cols <- c("studyName", "germplasmName", "germplasmDbId")
          }else if(input$aggregate_by == c("germplasm")){
            group_by_cols <- c("germplasmName", "germplasmDbId")
          }
        }else{
          group_by_cols <- names(rv$data_plot)[grep("Id$|germplasmName",names(rv$data_plot))]
        }
        req(input$picker_X %in% names(rv$data_plot))
        req(input$picker_Y %in% names(rv$data_plot))
        #req(input$picker_COLOUR %in% names(rv$data_plot))
        #req(input$picker_SIZE %in% names(rv$data_plot))
        #req(input$picker_SHAPE %in% names(rv$data_plot))
        #req(input$aggreg_fun_COLOUR)
        #req(aggreg_functions[fun == input$aggreg_fun_COLOUR, for_num] == rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"])
        
        data_plot_aggr <- rv$data_plot[
          studyDbId %in% input$env,
          .(
            N_obs = .N,
            # observationDbIds = list(), ## XXX
            # germplasm = paste(germplasmName, collapse = ", "),
            VAR_X = do.call(what = input$aggreg_fun_X, args = list(
              x = if(isTruthy(input$picker_X)) eval(as.name(input$picker_X)) else NA,
              na.rm = T)),
            VAR_Y = do.call(what = input$aggreg_fun_Y, args = list(
              x = if(isTruthy(input$picker_Y)) eval(as.name(input$picker_Y)) else NA,
              na.rm = T)),
            VAR_SHAPE = do.call(what = input$aggreg_fun_SHAPE, args = list(
              x = if(isTruthy(input$picker_SHAPE)) eval(as.name(input$picker_SHAPE)) else NA,
              na.rm = T)),
            VAR_COLOUR = do.call(what = input$aggreg_fun_COLOUR, args = list(
              x = if(isTruthy(input$picker_COLOUR)) eval(as.name(input$picker_COLOUR)) else NA,
              na.rm = T)),
            VAR_SIZE = do.call(what = input$aggreg_fun_SIZE, args = list(
              x = if(isTruthy(input$picker_SIZE)) eval(as.name(input$picker_SIZE)) else NA,
              na.rm = T))
          ),
          by = group_by_cols
        ]
        
        ## transform X variable
        # - no transformation (default)
        data_plot_aggr[,VAR_X_PLOT:=VAR_X]
        if (!is.null(input$express_X_as) && input$express_X_as=="as ranks"){
          # - ranking
          if(input$ranking_order_X == "descending"){
            data_plot_aggr[, VAR_X_PLOT := base::rank(x = -VAR_X, na.last = T, ties.method = "min")]
          }else{
            data_plot_aggr[, VAR_X_PLOT := base::rank(x = VAR_X, na.last = T, ties.method = "min")]
          }
        } else if(input$switch_aggregate==T && !is.null(input$express_X_as) && input$express_X_as=="relatively to genotype" && !(input$aggregate_by %in% c("plot"))){
          # - variation to genotype
          req(input$ref_genotype_X)
          if(input$aggregate_by=="germplasm"){
            group_by_cols <- c("germplasmName")
            ref_val <- data_plot_aggr[germplasmName == input$ref_genotype_X, VAR_X]
            data_plot_aggr[,reference_value:=ref_val]
          }else if(input$aggregate_by=="germplasm and environment"){
            group_by_cols <- c("studyName", "germplasmName")
            ref_val <- data_plot_aggr[germplasmName == input$ref_genotype_X, .(reference_value = VAR_X), by = group_by_cols]
            setkeyv(ref_val, group_by_cols)
            setkeyv(data_plot_aggr, group_by_cols)
            data_plot_aggr <- ref_val[,-c("germplasmName"), with = F][data_plot_aggr]
          }
          data_plot_aggr[,VAR_X_PLOT := 1 + (reference_value - VAR_X)/reference_value]
        }
        
        ## transform Y variable
        # - no transformation (default)
        data_plot_aggr[,VAR_Y_PLOT:=VAR_Y]
        if(!is.null(input$express_Y_as) && input$express_Y_as=="as ranks"){
          # - ranking
          if(input$ranking_order_Y == "descending"){
            data_plot_aggr[, VAR_Y_PLOT := base::rank(x = -VAR_Y, na.last = T, ties.method = "min")]
          }else{
            data_plot_aggr[, VAR_Y_PLOT := base::rank(x = VAR_Y, na.last = T, ties.method = "min")]
          }
        }else if(input$switch_aggregate==T && !is.null(input$express_Y_as) && input$express_Y_as=="relatively to genotype" && !(input$aggregate_by %in% c("plot"))){
          # - variation to genotype
          req(input$ref_genotype_Y)
          if(input$aggregate_by=="germplasm"){
            group_by_cols <- c("germplasmName")
            ref_val <- data_plot_aggr[germplasmName == input$ref_genotype_Y, VAR_Y]
            data_plot_aggr[,reference_value:=ref_val]
          }else if(input$aggregate_by=="germplasm and environment"){
            group_by_cols <- c("studyName", "germplasmName")
            ref_val <- data_plot_aggr[germplasmName == input$ref_genotype_Y, .(reference_value = VAR_Y), by = group_by_cols]
            setkeyv(ref_val, group_by_cols)
            setkeyv(data_plot_aggr, group_by_cols)
            data_plot_aggr <- ref_val[,-c("germplasmName"), with = F][data_plot_aggr]
          }
          data_plot_aggr[,VAR_Y_PLOT := 1 + (reference_value - VAR_Y)/reference_value]
        }
        
        
        ## if many SHAPE or COLOR categorical values, make a "other" category
        if(data_plot_aggr[,.N,VAR_SHAPE][,.N]>6){
          other_values <- data_plot_aggr[,.N,VAR_SHAPE][order(N, decreasing = T)][6:.N, VAR_SHAPE]
          data_plot_aggr[VAR_SHAPE%in%other_values, VAR_SHAPE := "other"]
        }
        if(!data_plot_aggr[,is.numeric(VAR_COLOUR)]){
          if(data_plot_aggr[,.N,VAR_COLOUR][,.N]>12){
            other_values <- data_plot_aggr[,.N,VAR_COLOUR][order(N, decreasing = T)][6:.N, VAR_COLOUR]
            data_plot_aggr[VAR_COLOUR%in%other_values, VAR_COLOUR := "other"]
          }
        }
        
        rv$data_plot_aggr <- data_plot_aggr
      })
      
      observeEvent(input$go_regression, {
        rv_plot$draw_regression <- !rv_plot$draw_regression
      })
      
      output$scatterplot <- renderPlotly({
        
        #browser()
        req(rv$data_plot_aggr)
        req((is.null(input$express_X_as) ||  length(input$express_X_as) == 1 )
            && (is.null(input$express_Y_as) ||  length(input$express_Y_as) == 1))
        #req(input$picker_COLOUR)
        #req(rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"] == is.numeric(rv$data_plot_aggr[,VAR_COLOUR]))
        
        rv_plot$draw_regression
        rv_plot$draw_clusters
        
        d <- rv$data_plot_aggr
        
        if(rv_plot$draw_clusters==T){
          if("cluster" %in% names(d)){d[,cluster := NULL]}
          d <- merge.data.table(x = d, y = rv_plot$clusters[,.(germplasmName, cluster)], by = "germplasmName")
          d[,VAR_COLOUR:=as.factor(cluster)]
        }
        
        reg <- NULL
        if(rv_plot$draw_regression==T){
          try({
            reg <- lm(formula = VAR_Y_PLOT ~ VAR_X_PLOT, data = d, na.action = na.exclude)
          })
          #shinyjs::addClass(id = "go_regression", class = "active")
          #updateActionButton(session, "go_regression", icon = icon("check"))
          output$regression_output <- renderUI({
            tags$p(
              tags$b("Y"),"=",signif(reg$coefficients[2], digits = 2),tags$b("X"),
              ifelse(sign(reg$coefficients[1])<0,"","+"), signif(reg$coefficients[1],digits = 2),
              tags$br(),
              tags$b("P-value"), "=", signif(summary(reg)$coefficients[,"Pr(>|t|)"][2], digits = 2),
              tags$br(),
              tags$b("R-squared"), "=", signif(summary(reg)$r.squared, digits = 1)
            )
          })
        }else{
          updateActionButton(session, "go_regression", icon = icon(NULL))
          shinyjs::removeClass(id = "go_regression", class = "active")
          output$regression_output <- renderUI("")
        }
        
        
        d[, "Germplasm Name" := germplasmName] # workaround for the plotly tooltip
        d[, "X value" := VAR_X_PLOT] # workaround for the plotly tooltip
        d[, "Y value" := VAR_Y_PLOT] # workaround for the plotly tooltip
        d[, "Shape scale" := if(isTruthy(input$picker_SHAPE)) VAR_SHAPE else NA] # workaround for the plotly tooltip
        d[, "Colour scale" := if(isTruthy(input$picker_COLOUR)) VAR_COLOUR else NA] # workaround for the plotly tooltip
        d[, "Size scale" := if(isTruthy(input$picker_SIZE)) VAR_COLOUR else NA] # workaround for the plotly tooltip
        d[, Cluster := if(rv_plot$draw_clusters == T) VAR_COLOUR else NA] # workaround for the plotly tooltip
        
        d <- highlight_key(d)
        p <- ggplot(d, aes(
          x = VAR_X_PLOT, y = VAR_Y_PLOT,
          colour = if(isTruthy(input$picker_COLOUR) | rv_plot$draw_clusters == T) VAR_COLOUR else NULL,
          shape = if(isTruthy(input$picker_SHAPE)) VAR_SHAPE else NULL,
          size = if(isTruthy(input$picker_SIZE)) VAR_SIZE else NULL,
          key = germplasmDbId,
          germplasmName = `Germplasm Name`,
          x_val = `X value`, # workaround for the plotly tooltip
          y_val = `Y value`, # workaround for the plotly tooltip
          Shape = `Shape scale`, # workaround for the plotly tooltip
          Size = `Size scale`, # workaround for the plotly tooltip
          Colour = `Colour scale`, # workaround for the plotly tooltip
          Cluster = Cluster # workaround for the plotly tooltip
        ))
        
        if(isTruthy(reg)){
          p <- p + geom_abline(
            slope = reg$coefficients[2],
            intercept = reg$coefficients[1],
            colour = "#269abc",
            alpha = 0.8,
            size = 1
          )
        }
        
        p <- p + geom_point(alpha = 0.5) +
          scale_x_continuous(
            labels = if(!is.null(input$express_X_as) && input$express_X_as=="relatively to genotype" && isTruthy(input$ref_genotype_X)){scales::percent}else{waiver()},
            # trans = if(input$express_X_as=="as ranks"){"reverse"}else{"identity"}, # disabled to make regression computation and drawing more simple
            breaks = if(!is.null(input$express_X_as) && input$express_X_as=="as ranks"){as.numeric(floor(quantile(rv$data_plot_aggr$VAR_X_PLOT, na.rm = T, probs = seq(1,0,-0.2))))}else{waiver()},
            name = if(!is.null(input$express_X_as) && input$express_X_as=="as ranks"){
              paste(input$picker_X, "(ranks)")
            }else if(!is.null(input$express_X_as) && input$express_X_as=="relatively to genotype" & isTruthy(input$ref_genotype_X)){
              paste0(input$picker_X, " (relative to ", input$ref_genotype_X, ")")
            }else{
              input$picker_X
            }
          ) +
          scale_y_continuous(
            labels = if(!is.null(input$express_Y_as) && input$express_Y_as=="relatively to genotype" && isTruthy(input$ref_genotype_Y)){scales::percent}else{waiver()},
            # trans = if(input$express_Y_as=="as ranks"){"reverse"}else{"identity"}, # disabled to make regression computation and drawing more simple
            breaks = if(!is.null(input$express_Y_as) && input$express_Y_as=="as ranks"){as.numeric(floor(quantile(rv$data_plot_aggr$VAR_Y_PLOT, na.rm = T, probs = seq(1,0,-0.2))))}else{waiver()},
            name = if(!is.null(input$express_Y_as) && input$express_Y_as=="as ranks"){
              paste(input$picker_Y, "(ranks)")
            }else if(!is.null(input$express_Y_as) && input$express_Y_as=="relatively to genotype" && isTruthy(input$ref_genotype_Y)){
              paste0(input$picker_Y, " (relative to ", input$ref_genotype_Y, ")")
            }else{
              input$picker_Y
            }
          ) +
          scale_shape(name = input$picker_SHAPE) +
          scale_size(name = input$picker_SIZE) +
          if (isTruthy(input$picker_COLOUR)) scale_color_custom(
            is_num = if (rv_plot$draw_clusters == TRUE) { FALSE } else { rv$column_datasource[cols == isolate(input$picker_COLOUR), type == "Numerical"] },
            name = if (rv_plot$draw_clusters == TRUE) { "cluster" } else { isolate(input$picker_COLOUR) }
          ) else NULL +
          theme_minimal() #+
        # theme(legend.position = "bottom") # uneffective with plotyly
        
        # pp <- ggMarginal(p, type = "density", fill =  "black", alpha = 0.05)
        # pp
        
        tooltip_var <- c("germplasmName", "x_val", "y_val", "Shape", "Colour", "Size", "Cluster")
        ggplotly(#height=length(input$studies)*400,
          p,
          dynamicTicks = "TRUE", source = "A", originalData = T,
          tooltip = tooltip_var[c(T,T,T,isTruthy(input$picker_SHAPE), isTruthy(input$picker_COLOUR), 
                                  isTruthy(input$picker_SIZE), rv_plot$draw_clusters)]
        ) %>%
          style(hoverlabel = list(bgcolor = grey(0.3))) %>%
          layout(dragmode = "lasso") %>%
          highlight(
            on = "plotly_selected",
            off = "plotly_deselect",
            opacityDim = T,
            persistent = F,
            selected = attrs_selected(
              mode = "markers",
              opacity = 1,
              marker = list(
                alpha = 1,
                line = list(
                  color = "red",
                  width = 5
                )
              )
            )
          )
      })
      
      observeEvent(event_data("plotly_click", source = "A"),{
        rv_plot$plot_selection <- as.data.table(event_data("plotly_click", source = "A"))
      })
      observeEvent(event_data("plotly_selected", source = "A"),{
        rv_plot$plot_selection <- as.data.table(event_data("plotly_selected", source = "A"))
      })
      observeEvent(rv_plot$plot_selection[,.N],{
        shinyjs::toggle(selector = paste0(".",ns("ui_create_group")), condition = rv_plot$plot_selection[,.N]>0)
        req(dim(rv_plot$plot_selection)[1]>0)
        germplasms <- unique(rv$data_plot[germplasmDbId %in% rv_plot$plot_selection[,unique(key)], .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv_plot$groups$group_id), 1, max(rv_plot$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName),
          plot_params = list(
            list(
              switch_aggregate = input$switch_aggregate,
              aggregate_by = input$aggregate_by,
              picker_X = input$picker_X,
              aggreg_fun_X = input$aggreg_fun_X,
              express_X_as = input$express_X_as,
              ref_genotype_X = input$ref_genotype_X,
              ranking_order_X = input$ranking_order_X,
              picker_Y = input$picker_Y,
              aggreg_fun_ = input$aggreg_fun_Y,
              express_Y_as = input$express_Y_as,
              ref_genotype_Y = input$ref_genotype_Y,
              ranking_order_Y = input$ranking_order_Y,
              switch_SHAPE = input$switch_SHAPE,
              picker_SHAPE = input$picker_SHAPE,
              aggreg_fun_SHAPE = input$aggreg_fun_SHAPE,
              switch_COLOUR = input$switch_COLOUR,
              picker_COLOUR = input$picker_COLOUR,
              aggreg_fun_COLOUR = input$aggreg_fun_COLOUR,
              switch_SIZE = input$switch_SIZE,
              picker_SIZE = input$picker_SIZE,
              aggreg_fun_SIZE = input$aggreg_fun_SIZE
            )
          )
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv_plot$selection <- selection_data
      })
      
      observeEvent(input$go_create_group,{
        if(rv_plot$selection[,.N]>0){
          showModal(groupModal("Create new group", paste0(
            "Visualization at group creation:\nX=",input$picker_X,", \nY=",input$picker_Y,
            if(isTruthy(input$picker_COLOUR)) paste(", \nColour=", input$picker_COLOUR),
            if(isTruthy(input$picker_SHAPE)) paste(", \nShape=", input$picker_SHAPE),
            if(isTruthy(input$picker_SIZE)) paste(", \nSize=", input$picker_SIZE)
          )))
        }
      })
      
      groupModal <- function(modal_title, group_description) {
        req(rv_plot$selection[,.N]>0)
        modalDialog(
          title = modal_title,
          fade = F,
          size = "l",
          tagList(
            tags$label(paste(rv_plot$selection[,N]," selected germplasms")),
            tags$p(rv_plot$selection[,germplasmNames_label]),
            textInput(ns("modal_create_group_text_input_label"), label = "Group Name", value = paste("Group", rv_plot$selection[,group_id]), placeholder = "Group Label"),
            textAreaInput(
              ns("modal_create_group_text_input_descr"), 
              label = "Group Description", 
              placeholder = "Group Description", 
              resize = "vertical",
              value = group_description
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("modal_create_group_go"), label = "Create", css.class = "btn btn-info")
          )
        )
      }
      
      observeEvent(input$modal_create_group_go, {
        rv_plot$selection[, group_name := input$modal_create_group_text_input_label]
        rv_plot$selection[, group_desc := input$modal_create_group_text_input_descr]
        rv_plot$groups <- rbindlist(list(
          rv_plot$groups,
          rv_plot$selection
        ), fill = T, use.names = T)
        
        ## update selectors (shape, colour)
        data_plot <- copy(rv$data_plot) # to avoid issues related to assignment by reference
        data_plot[germplasmDbId %in% rv_plot$selection[,unlist(germplasmDbIds)], eval(input$modal_create_group_text_input_label) := paste0('In "', input$modal_create_group_text_input_label,'"')]
        data_plot[!(germplasmDbId %in% rv_plot$selection[,unlist(germplasmDbIds)]), eval(input$modal_create_group_text_input_label) := paste0('Not in "', input$modal_create_group_text_input_label,'"')]
        rv$column_datasource <- rbindlist(
          list(
            rv$column_datasource,
            data.table(cols = input$modal_create_group_text_input_label, source = "group", type = "Text")
          )
        )
        rv$data_plot <- data_plot
        
        rv_plot$selection <- data.table()
        rv_plot$plot_selection <- data.table()
        
        removeModal()
        #toggleModal(session, "modal_create_group", toggle = "close")
      })
      observeEvent(rv_plot$groups$group_id,{
        req(rv_plot$groups)
        output$ui_groups <- renderUI({
          group_selector(input_id = ns("group_sel_input"), group_table = rv_plot$groups, column_datasource = rv$column_datasource, data_plot = rv$data_plot, panel_style = "info")
        })
      })
      
      observe({
        shinyjs::toggle(selector = paste0(".",ns("at_least_one_group_selected")), condition = length(input$group_sel_input)>0)
        shinyjs::toggle(selector = paste0(".",ns("create_new_groups_from_groups")), condition = length(input$group_sel_input)>1)
        shinyjs::toggle(selector = paste0(".",ns("one_group_selected")), condition = length(input$group_sel_input)==1)
        shinyjs::toggle(id = "export_box", condition = length(input$group_sel_input)==1)
      })
      
      ## Create new groups
      observeEvent(input$action_groups_union,{
        union_germplasms_id <- rv_plot$groups[group_id %in% input$group_sel_input, unlist(germplasmDbIds)]
        germplasms <- unique(rv$data[germplasmDbId %in% union_germplasms_id, .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv_plot$groups$group_id), 1, max(rv_plot$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv_plot$selection <- selection_data
        
        showModal(groupModal("Create new group from Union", 
                             paste(rv_plot$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∪ ")))
      })
      observeEvent(input$action_groups_intersect,{
        toggleModal(session, "modal_create_group")
        intersect_germplasms_id <- Reduce(intersect, rv_plot$groups[group_id %in% input$group_sel_input, germplasmDbIds])
        germplasms <- unique(rv$data[germplasmDbId %in% intersect_germplasms_id, .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv_plot$groups$group_id), 1, max(rv_plot$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv_plot$selection <- selection_data
        
        if(rv_plot$selection[1,N>0]){
          showModal(
            groupModal(
              "Create new group from Complement",
              paste(rv_plot$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∩ ")
            )
          )
        } else {
          showModal(modalDialog(tags$label("This intersection results in an empty group.")))
        }
       
      })
      observeEvent(input$action_groups_complement,{
        toggleModal(session, "modal_create_group")
        union_germplasms_id <- rv_plot$groups[group_id %in% input$group_sel_input, unlist(germplasmDbIds)]
        germplasms <- unique(rv$data[!(germplasmDbId %in% union_germplasms_id), .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv_plot$groups$group_id), 1, max(rv_plot$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv_plot$selection <- selection_data
        
        showModal(
          groupModal(
            "Create new group from Complement",
            paste("Complement of (", paste(rv_plot$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∪ "), ")")
          )
        )
      })
      
      observeEvent(input$action_groups_plot_creation_params,{
        plot_params <- rv_plot$groups[group_id == input$group_sel_input,plot_params][[1]]
        
        updateMaterialSwitch(session, "switch_aggregate", value = plot_params$switch_aggregate)
        updatePickerInput(session, "aggregate_by", selected = plot_params$aggregate_by)
        
        updatePickerInput(session, "picker_X", selected = plot_params$picker_X)
        updatePickerInput(session, "aggreg_fun_X", selected = plot_params$aggreg_fun_X)
        updateRadioButtons(session, "express_X_as", selected = plot_params$express_X_as)
        updatePickerInput(session, "ref_genotype_X", selected = plot_params$ref_genotype_X)
        updateCheckboxInput(session, "ranking_order_X", value = plot_params$ranking_order_X)
        
        updatePickerInput(session, "picker_Y", selected = plot_params$picker_Y)
        updatePickerInput(session, "aggreg_fun_Y", selected = plot_params$aggreg_fun_Y)
        updateRadioButtons(session, "express_Y_as", selected = plot_params$express_Y_as)
        updatePickerInput(session, "ref_genotype_Y", selected = plot_params$ref_genotype_Y)
        updateCheckboxInput(session, "ranking_order_Y", value = plot_params$ranking_order_Y)
        
        updateMaterialSwitch(session, "switch_SHAPE", value = plot_params$switch_SHAPE)
        updatePickerInput(session, "picker_SHAPE", selected = plot_params$picker_SHAPE)
        updatePickerInput(session, "aggreg_fun_SHAPE", selected = plot_params$aggreg_fun_SHAPE)
        
        updateMaterialSwitch(session, "switch_COLOUR", value = plot_params$switch_COLOUR)
        updatePickerInput(session, "picker_COLOUR", selected = plot_params$picker_COLOUR)
        updatePickerInput(session, "aggreg_fun_COLOUR", selected = plot_params$aggreg_fun_COLOUR)
        
        updateMaterialSwitch(session, "switch_SIZE", value = plot_params$switch_SIZE)
        updatePickerInput(session, "picker_SIZE", selected = plot_params$picker_SIZE)
        updatePickerInput(session, "aggreg_fun_SIZE", selected = plot_params$aggreg_fun_SIZE)
      })
      
      observeEvent(input$action_groups_delete,{
        
        ## update selectors (shape, colour)
        for(k in input$group_sel_input){
          rv$data_plot[,eval(rv_plot$groups[group_id == k, group_name]) := NULL]
        }
        rv$column_datasource <- rv$column_datasource[!(cols %in% rv_plot$groups[group_id %in% input$group_sel_input, group_name])]
        
        ## delete groups
        rv_plot$groups <- rv_plot$groups[!(group_id %in% input$group_sel_input)]
      })      
      
      observeEvent(input$action_groups_export_as_list,{
        showModal(modalDialog(
          fade = F,
          title = "Export Group as List",
          tagList(
            textInput(
              ns("listName"),
              label = "Group Name",
              value = rv_plot$groups[group_id == input$group_sel_input, group_name],
              placeholder = "Human readable name of a List",
              width = "100%"
            ),
            textAreaInput(
              ns("listDescription"),
              label = "List description",
              value = rv_plot$groups[group_id == input$group_sel_input, group_desc],
              placeholder = "Description of a List",
              width = "100%"
            )
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("go_create_list"), "Create list", css.class = "btn btn-info")
          )
        ))
      })
      
      # observeEvent(c(input$listDescription, input$listName), {
      #   if(input$listName != "" & input$listDescription != ""){
      #     output$go_create_list_ui <- renderUI({actionButton(ns("go_create_list"), "Create list", css.class = "btn btn-primary")})
      #   }else{
      #     output$go_create_list_ui <- renderUI({actionButton(ns("go_create_list"), "Create list", css.class = "btn btn-primary", disabled = "")})
      #   }
      # })
      
      observeEvent(input$go_create_list,{
        removeModal()
        req(length(input$group_sel_input)==1)
        tryCatch({
          brapirv2::brapi_post_lists(
            con = rv$con,
            data = rv_plot$groups[group_id == input$group_sel_input, germplasmDbIds][[1]],
            listSize = rv_plot$groups[group_id == input$group_sel_input, N],
            dateCreated = as.character(Sys.Date()), # XXX
            dateModified = as.character(Sys.Date()), # XXX
            listName = input$listName,
            listDescription= input$listDescription,
            listOwnerName = "Admin Admin", # XXX
            listOwnerPersonDbId = "1", # XXX
            listSource = "test", # XXX
            listType = "germplasm"
          )
          showNotification("List posted", type = "message", duration = notification_duration)
        }, error = function(e)({
          showNotification("Could not post list", type = "error", duration = notification_duration)
        }))
      })
      
      
      observeEvent(input$action_groups_mark_as_selection,{
        req(length(input$group_sel_input)==1)
        envs <- unique(rv$data_plot[,.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]
        
        #TODO propose only variables that are selection type
        #trait_classes <- rv$ontology_variables[,unique(trait.traitClass)]
        
        showModal(modalDialog(
          fade = F,
          title = "Export Group as List",
          tagList(
            awesomeCheckboxGroup(
              inputId = ns("mark_as_sel_envs"),
              label = "Environments",
              choices = env_choices,
              selected = env_choices,
              width = "100%"
            ),
            pickerInput(
              inputId = ns("mark_as_sel_var_to_use"),
              label = "Variable to use",
              choices = NULL,
              inline = T
            ),
            # pickerInput(
            #   inputId = ns("mark_as_sel_trait_classes"),
            #   label = "Filter Variable by Trait Class",
            #   choices = trait_classes,
            #   inline = T
            # ),
            awesomeRadio(
              ns("mark_as_sel_all_plots_radio"),
              label = "",
              choices = list(all = "Mark all plots", rep1 = "Mark the first replicate of each plot"),
              selected = "rep1",
              status = "primary"
            ),
            #uiOutput(ns("mark_as_sel_info")),
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("action_groups_mark_as_selection_go"),
                         "OK", style = "primary")
          )
        ))
      })
      
      
      
      output$action_groups_export_group_details <- downloadHandler(
        filename = function() {
          paste0("group_", input$group_sel_input, ".csv")
        },
        content = function(file) {
          group_detail <- unique(rv$data_plot[
            germplasmDbId %in% unlist(rv_plot$groups[group_id%in%input$group_sel_input, germplasmDbIds]),
            .SD, .SD = rv$column_datasource[source == "germplasm", cols]
          ])
          write.csv(group_detail, file, row.names = F)
        }
      )
      
      observeEvent(input$mark_as_sel_trait_classes,{
        req(input$mark_as_sel_trait_classes)
        vars <- unique(rv$ontology_variables[trait.traitClass == input$mark_as_sel_trait_classes,.(observationVariableDbId, observationVariableName)])
        
        choices_vars <- vars[,observationVariableDbId]
        names(choices_vars) <- vars[,observationVariableName]
        
        updatePickerInput(
          session, "mark_as_sel_var_to_use",
          choices = choices_vars
        )
      })
      
      observeEvent(c(input$mark_as_sel_var_to_use, input$mark_as_sel_all_plots_radio, input$mark_as_sel_envs), {
        rv_plot$as_sel_data <- NULL
        output$mark_as_sel_info <- renderUI("")
        
        req(input$mark_as_sel_var_to_use)
        req(input$mark_as_sel_envs)
        
        as_sel_data <- rv$data[
          observationLevel == "PLOT" & studyDbId %in% input$mark_as_sel_envs &
            germplasmDbId %in% rv_plot$groups[group_id == input$group_sel_input, germplasmDbIds][[1]]
        ]
        if(input$mark_as_sel_all_plots_radio=="rep1" & as_sel_data[replicate=="1",.N]>0){
          as_sel_data <- as_sel_data[replicate == "1"]
        }
        as_sel_data <- unique(as_sel_data[,.(germplasmDbId, observationUnitDbId, studyDbId)])
        
        as_sel_data[,observationVariableDbId := input$mark_as_sel_var_to_use]
        as_sel_data[,observationValue := "1"]
        
        output$mark_as_sel_info <- renderUI({
          tagList(
            div(
              class = "panel panel-info", style = "width: 50%",
              div(
                class = "panel-heading",
                icon("info", style="font-size: 2em; float:left; margin-right:15px;"),
                p("You've selected", tags$b(as_sel_data[,.N,germplasmDbId][,.N]) ,"germplasms.", tags$br(),
                  "The selection will impact", tags$b(as_sel_data[,.N,observationUnitDbId][,.N]), "plots.")
              )
            )
          )
        })
        
        rv_plot$as_sel_data <- as_sel_data
      })
      
      observeEvent(input$action_groups_mark_as_selection_go,{
        req(rv_plot$as_sel_data[,.N]>0)
        withProgress(message = "POST brapi/v2/observations", value = 0, {
          lapply(rownames(rv_plot$as_sel_data), function(x){
            row_id <- as.numeric(x)
            incProgress(1/length(rownames(rv_plot$as_sel_data)))
            a <- tryCatch({
              brapirv2::brapi_post_observations(
                con = rv$con,
                studyDbId = as.character(rv_plot$as_sel_data[row_id, studyDbId]),
                germplasmDbId = as.character(rv_plot$as_sel_data[row_id, germplasmDbId]),
                observationUnitDbId = as.character(rv_plot$as_sel_data[row_id, observationUnitDbId]),
                observationVariableDbId = as.character(rv_plot$as_sel_data[row_id, observationVariableDbId]),
                value = as.character(rv_plot$as_sel_data[row_id, observationValue]),
                additionalInfo = list() # otherwise error message: "Argument: \"additionalInfo\" should be provided as a list, see the help page on how the list should be constructed."
              )
            }, error = function(e)({e})
            )
            mess <- a$message
            if(!is.null(mess)){
              showNotification(
                ui =
                  tagList(
                    tags$p("Could not post observation"),
                    tags$code(paste0(
                      '{ "studyDbId":"', as.character(rv_plot$as_sel_data[row_id, studyDbId]),'",',
                      '"germplasmDbId":"', as.character(rv_plot$as_sel_data[row_id, germplasmDbId]),'",',
                      '"observationUnitDbId":"', as.character(rv_plot$as_sel_data[row_id, observationUnitDbId]),'",',
                      '"observationVariableDbId":"', as.character(rv_plot$as_sel_data[row_id, observationVariableDbId]),'",',
                      '"value":"', as.character(rv_plot$as_sel_data[row_id, observationValue]),'"'
                    )),
                    tags$code(mess)
                  ),
                type = "error", duration = notification_duration
              )
            }
          })
        })
        toggleModal(session, "modal_export_group_mark_as_selection", toggle = "open")
        rv_plot$as_sel_data <- NULL
      })
      
      output$ui_clusters <- renderUI({
        if(input$aggregate_by == "germplasm" & input$switch_aggregate == T){
          req(rv$data_plot_aggr)
          max_k <- rv$data_plot_aggr[,.N,germplasmName][,.N]
          dropdownButton(
            label = "Cluster germplasms", status = "info", circle = F, inline = T, up = F, width = "500px",
            radioGroupButtons(ns("cluster_algo"), "Alogrithm", choiceNames = c("K-means", "HCA with Ward distance"), choiceValues = c("kmeans", "hca")),
            numericInput(ns("n_clusters"), label = "Number of clusters", min = 1, max = max_k, step = 1, value = min(3, max_k)),
            actionButton(ns("go_clustering"), "Cluster", css.class = "btn btn-primary"),
            uiOutput(ns("cluster_results"))
          )
        }else{
          span()
        }
      })
      
      observeEvent(input$go_clustering,{
        req(rv$data_plot_aggr)
        req(input$n_clusters)
        max_k <- rv$data_plot_aggr[,.N,germplasmName][,.N]
        n_clusters <- input$n_clusters
        if(input$n_clusters > max_k){
          updateNumericInput(session, "n_clusters", value = max_k)
          n_clusters <- max_k
        }else if(input$n_clusters <= 0){
          updateNumericInput(session, "n_clusters", value = 1)
          n_clusters <- 1
        }
        d <- rv$data_plot_aggr[, .(scale(VAR_X, center = T, scale = T), scale(VAR_Y, center = T, scale = T))]
        if(input$cluster_algo=="hca"){
          mat_dist <- dist(d, method = "euclidean")
          dend <- hclust(d = mat_dist, method = "ward.D2")
          clus <- cutree(dend, k = n_clusters)
          desc_clust <- tagList(
            tags$ul(
              tags$li("Clustering method:", tags$b("HCA")),
              tags$li("Dissimilarity structure measure:", tags$b("euclidean"), "distance."),
              tags$li("Agglomeration method:", tags$b("Ward"), tags$small(tags$em("Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification, 31, 274–295. doi: 10.1007/s00357-014-9161-z."))),
              tags$li("Number of clusters: ", tags$b(n_clusters))
            )
          )
          rv_plot$counter_hca <- rv_plot$counter_hca + 1
          counter <- rv_plot$counter_hca
        }else{
          kmeans_res <- kmeans(x = d, algorithm = "Forgy", centers = n_clusters)
          clus <- kmeans_res$cluster
          desc_clust <- tagList(
            tags$ul(
              tags$li("Clustering method:", tags$b("K-means")),
              tags$li("Algorithm:", tags$b("Forgy"), tags$small(tags$em("Forgy, E. W. (1965). Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics, 21, 768–769."))),
              tags$li("Number of iterations:", kmeans_res$iter),
              tags$li("Number of clusters: ", tags$b(n_clusters))
            )
          )
          rv_plot$counter_kmeans <- rv_plot$counter_kmeans + 1
          counter <- rv_plot$counter_kmeans
        }
        clusters <- data.table(rv$data_plot_aggr[,.(germplasmDbId, germplasmName)], cluster = clus, method = input$cluster_algo, counter = counter)
        rv_plot$clusters <- clusters
        rv_plot$draw_clusters <- T
        output$cluster_results <- renderUI({
          tagList(
            tags$hr(),
            tags$h3("Results", style = "text-align: center;"),
            tags$label("Clusters"),
            renderDT(datatable(
              rv_plot$clusters[,.(germplasmName, cluster)],
              rownames = F,
              options = list(
                paging = F,
                scrollX = T,
                scrollY = "250px",
                scrollCollapse = T,
                dom = 't'
              ))),
            downloadButton(ns("download_clusters"), "Download", class = "btn btn-info"),
            if(rv_plot$draw_clusters == T){
              shiny::actionButton(ns("draw_clusters"), "Draw", icon = icon("check"), class = "btn btn-info active")
            }else{
              shiny::actionButton(ns("draw_clusters"), "Draw", icon = icon(NULL), class = "btn btn-info")
            },
            shiny::actionButton(ns("create_groups_from_clusters"), "Create groups", icon = icon(NULL), class = "btn btn-info"),
            tags$br(),
            tags$label("Number of germplasms per cluster"),
            renderDT(datatable(
              t(rv_plot$clusters[,.N,cluster]),
              rownames = T, colnames = NULL,
              options = list(
                ordering = F,
                paging = F,
                scrollX = T,
                scrollY = "250px",
                scrollCollapse = T,
                dom = 't'
              ))),
            tags$label("Clustering info"),
            desc_clust
          )
        })
      })
      
      output$download_clusters <- downloadHandler(
        filename = function() {
          paste0("clusters_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(rv_plot$clusters, file, row.names = F)
        }
      )
      
      observeEvent(input$draw_clusters, {
        rv_plot$draw_clusters <- !rv_plot$draw_clusters
      })
      
      observeEvent(rv_plot$draw_clusters, {
        if(rv_plot$draw_clusters == T){
          updateActionButton(session, "draw_clusters", icon = icon("check"))
          shinyjs::addClass(id = "draw_clusters", class = "active")
          updateSwitchInput(session, "switch_COLOUR", value = F)
        }else{
          updateActionButton(session, "draw_clusters", icon = icon(NULL))
          shinyjs::removeClass(id = "draw_clusters", class = "active")
        }
      })
      observeEvent(input$switch_COLOUR, {
        if(input$switch_COLOUR == T){
          rv_plot$draw_clusters <- F
        }
      })
      observeEvent(input$create_groups_from_clusters,{
        shinyjs::disable("create_groups_from_clusters")
        shinyjs::addClass("create_groups_from_clusters", "active")
        clusters <- rv_plot$clusters[order(cluster)][,.(
          group_name = paste0(unique(method), unique(counter), ".", cluster),
          group_desc = paste0(
            "Clustering method: ", tags$b(input$cluster_algo), tags$br(),
            "Cluster: ", tags$b(cluster,"/", input$n_clusters), tags$br(),
            "Timestamp: ", tags$b(Sys.time())
          ),
          germplasmDbIds = list(germplasmDbId),
          germplasmNames = list(germplasmName),
          .N,
          plot_params = list(
            list(
              switch_aggregate = input$switch_aggregate,
              aggregate_by = input$aggregate_by,
              picker_X = input$picker_X,
              aggreg_fun_X = input$aggreg_fun_X,
              express_X_as = input$express_X_as,
              ref_genotype_X = input$ref_genotype_X,
              ranking_order_X = input$ranking_order_X,
              picker_Y = input$picker_Y,
              aggreg_fun_ = input$aggreg_fun_Y,
              express_Y_as = input$express_Y_as,
              ref_genotype_Y = input$ref_genotype_Y,
              ranking_order_Y = input$ranking_order_Y,
              switch_SHAPE = input$switch_SHAPE,
              picker_SHAPE = input$picker_SHAPE,
              aggreg_fun_SHAPE = input$aggreg_fun_SHAPE,
              switch_COLOUR = input$switch_COLOUR,
              picker_COLOUR = input$picker_COLOUR,
              aggreg_fun_COLOUR = input$aggreg_fun_COLOUR,
              switch_SIZE = input$switch_SIZE,
              picker_SIZE = input$picker_SIZE,
              aggreg_fun_SIZE = input$aggreg_fun_SIZE
            )
          )
        ),cluster]
        clusters[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        group_id_start <- ifelse(is.null(rv_plot$groups$group_id), 1, max(rv_plot$groups$group_id) + 1)
        group_ids <- group_id_start:(group_id_start+clusters[,.N] -1)
        clusters[, group_id := group_ids]
        rv_plot$groups <- rbindlist(list(
          rv_plot$groups,
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
            data.table(cols = clusters[,unique(group_name)], source = "group", type = "Text")
          )
        )
        rv$data_plot <- data_plot
        
      })
      
      # observeEvent(input$toggleButton, {
      #   browser()
      #   session$sendCustomMessage(type = "toggle", message = list(id = "collapseCardBody"))
      # })
    }
  )
}
