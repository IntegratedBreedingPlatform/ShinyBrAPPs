#' @import shinyWidgets
#' @import shinyjs
#' @import bslib
#' @export
# UI ####
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
    
    layout_sidebar(
      ## Param sidebar ####
      sidebar = sidebar(
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
        ### Options card ####
         div(
           card(
             card_header(
               h4('Options ', icon('screwdriver-wrench')),
               class = "d-flex justify-content-between",
               actionBttn(
                 inputId = ns("reset"),
                 label = NULL,
                 style = "border", 
                 color = "primary",
                 icon = icon("refresh"),
                 size = "xs"
              )
             ),
             card_body(
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
                            pickerInput(ns("aggreg_fun_COLOUR"),  options = list(container = "body"), choices = c("-----------" = "")))
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
                            pickerInput(ns("aggreg_fun_SIZE"),  options = list(container = "body"), choices = c("mean", "max", "min", "sum")))
                 )
               )
             )
           )
         )
      ),
      ## Plot card ####
      card(height = "800px",
        full_screen = TRUE,
         plotlyOutput(
           ns("scatterplot"), width = "100%", height = "600px"
           # click = ns("scatterplot_click"),
           # hover = ns("scatterplot_hover"),
           # brush = ns("scatterplot_brush")
         ),
         #bsTooltip(ns("scatterplot"), title = "Tip: press the shift key for multiple mouse selection", placement = "left"),
         fluidRow(
           column(
             12,
             hidden(actionButton(ns("go_regression"), "Draw Regression", class = "btn btn-info", `aria-pressed`="true", icon = icon("box"))),
             uiOutput(ns("ui_clusters"), inline = T),
             span(class = ns("ui_create_group"), style = "display: none;",
                  actionButton(ns("go_create_group"), "Create Group", class = "btn btn-info")
             ),
             # bsModal(ns("modal_create_group"), "Create Group", NULL, size = "small",
             #         uiOutput(ns("modal_create_group_ui")))
           )
         ),
         fluidRow(
           column(
             12,
             uiOutput(ns("regression_output"))
           )
         )
      )
    )
  )
}

#' @import data.table
#' @import ggplot2
#' @export
# SERVER ####
mod_scatterplot_server <- function(id, rv, parent_session){
  no_selection <- list("-----------" = "")
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv_plot <- reactiveValues(
        draw_regression = F,
        draw_clusters = F,
        counter_hca = 0,
        counter_kmeans = 0,
        selected_express_X_as = NULL,
        selected_express_Y_as = NULL,
        selection_variables = NULL
      )
      
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
     
      ## initialize selectinputs ####
      observeEvent(rv$extradata, {
        req(rv$column_datasource)

        ## turn numeric variables that should not be numeric into text
        for(col in names(rv$extradata)[
          rv$extradata[, lapply(.SD, is.numeric)]==T &
          names(rv$extradata) %in% rv$column_datasource[type != "Numerical", cols]
        ]){
          rv$extradata[, c(col) := list(as.character(eval(as.name(col))))]
        }
        
        ## Set environments selector ####
        # don't change environment selection after creating a group
        if (!rv$new_group_created) {
          envs <- unique(rv$extradata[,.(studyDbId, study_name_app)])
          env_choices <- envs[,studyDbId]
          names(env_choices) <- envs[,study_name_app]
          updatePickerInput(
            session, "env",
            choices = env_choices,
            selected = env_choices
          )
        }
        
        ## Set germplasm refs ####
        germplasmNames <- rv$extradata[,.(germplasmNames = list(unique(germplasmName))), .(entryType)][order(entryType)]
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
        
        ## Set Variables selectors ####
        picker_section_names <- data.table(source=c("GxE",
                                                    "plot",
                                                    "germplasm",
                                                    "environment",
                                                    "Means",
                                                    "group"),
                                           rename=c("Measured variable",
                                                    "Observation units attributes",
                                                    "Germplasm attributes",
                                                    "Environment details",
                                                    "Means",
                                                    "Groups and clusters"))
        column_datasource <- merge(rv$column_datasource, picker_section_names, by = c("source"), all.x = T)[!is.na(rename), source := rename]
        
        num_var_choices <- column_datasource[type == "Numerical" & visible == T,.(cols = list(cols)), source]
        non_num_var_choices <- column_datasource[type != "Numerical" & visible == T,.(cols = list(cols)), source]
        var_choices_all <- column_datasource[visible == T,.(cols = list(cols)), source]
        
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
            var_choices_SHAPE <- column_datasource[visible == T & type != "Numerical" & !(source %in% "Observation units attributes"),.(cols = list(cols)), source]
            var_choices_COLOUR <- column_datasource[visible == T & (type == "Numerical" | type == "Text") & !(source %in% c("Observation units attributes")),.(cols = list(cols)), source]
          }else if(input$aggregate_by == "germplasm"){
            var_choices_SHAPE <- column_datasource[visible == T & type != "Numerical" & !(source %in% c("Observation Unit attributes", "Environment details")),.(cols = list(cols)), source]
            var_choices_COLOUR <- column_datasource[visible == T & (type == "Numerical" | type == "Text") & !(source %in% c("Observation Unit attributes", "Environment details")),.(cols = list(cols)), source]
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
        
        # Work around for pickerInputs with option groups that have only one option.
        # The default behaviour is to display only the group name.
        # Work around: naming the group by the singleton element
        # for(k in 1:num_var_choices[,.N]){
        #   cols <- num_var_choices[k,cols][[1]]
        #   if(length(cols)==1){num_var_choices[k, source := cols]}
        # }
        # for(k in 1:var_choices_SHAPE[,.N]){
        #   cols <- var_choices_SHAPE[k,cols][[1]]
        #   if(length(cols)==1){var_choices_SHAPE[k, source := cols]}
        # }
        # for(k in 1:var_choices_COLOUR[,.N]){
        #   cols <- var_choices_COLOUR[k,cols][[1]]
        #   if(length(cols)==1){var_choices_COLOUR[k, source := cols]}
        # }
        
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

        num_var_choices <- setNames(lapply(num_var_choices$cols, function(x) as.list(x)), num_var_choices$source)
        var_choices_SHAPE <- setNames(lapply(var_choices_SHAPE$cols, function(x) as.list(x)), var_choices_SHAPE$source)
        var_choices_COLOUR <- setNames(lapply(var_choices_COLOUR$cols, function(x) as.list(x)), var_choices_COLOUR$source)
        
        updatePickerInput(
          session = session, inputId = "picker_X",
          choices = num_var_choices,
          selected = val_picker_X
        )
        updatePickerInput(
          session = session, inputId = "picker_Y",
          choices = num_var_choices,
          selected = val_picker_Y
        )
        updatePickerInput(
          session = session, inputId = "picker_SIZE",
          choices = c(no_selection, num_var_choices),
          selected = val_picker_SIZE
        )
        updatePickerInput(
          session = session, inputId = "picker_SHAPE",
          choices = c(no_selection, var_choices_SHAPE),
          selected = val_picker_SHAPE
        )
        updatePickerInput(
          session = session, inputId = "picker_COLOUR",
          choices = c(no_selection, var_choices_COLOUR),
          selected = val_picker_COLOUR
        )
        
        rv_plot$data <- rv$extradata

      })
      
      ## update colour aggreg functions ####
      # (colour can be num or categorical)
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
      
      ## toggle draw regression button ####
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
      
      ## update as ranks ####
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
              updateCheckboxInput(session, "express_X_relative", value = F)
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
            updateCheckboxInput(session, "express_Y_relative", value = F)
            rv_plot$selected_express_Y = "ranks"
          }
        }
        if (!input$express_Y_as_ranks && !input$express_Y_relative) {
          rv_plot$selected_express_Y = NULL
        }
        shinyjs::toggle("ranking_order_Y", condition = !is.null(rv_plot$selected_express_Y) && rv_plot$selected_express_Y == "ranks")
        shinyjs::toggle("ref_genotype_Y", condition = !is.null(rv_plot$selected_express_Y) && rv_plot$selected_express_Y == "relative")
      })
      
      ## reset options ####
      observeEvent(input$reset, {
        updatePickerInput(
          session = session, inputId = "picker_SIZE",
          #choices = setNames(num_var_choices[,cols], num_var_choices[,source]),
          selected = ""
        )
        updatePickerInput(
          session = session, inputId = "picker_SHAPE",
          #choices = setNames(var_choices_SHAPE[,cols], var_choices_SHAPE[,source]),
          selected = ""
        )
        updatePickerInput(
          session = session, inputId = "picker_COLOUR",
          #choices = setNames(var_choices_COLOUR[,cols], var_choices_COLOUR[,source]),
          selected = ""
        )
      })
      
      ## aggreg dataset ####
      observe({
        req(rv_plot$data)
        if(input$switch_aggregate== T){
          if(input$aggregate_by == "germplasm and environment"){
            group_by_cols <- c("studyName", "germplasmName", "germplasmDbId")
          }else if(input$aggregate_by == c("germplasm")){
            group_by_cols <- c("germplasmName", "germplasmDbId")
          }
        }else{
          group_by_cols <- names(rv_plot$data)[grep("Id$|germplasmName",names(rv_plot$data))]
        }
        req(input$picker_X %in% names(rv_plot$data))
        req(input$picker_Y %in% names(rv_plot$data))
        #req(input$picker_COLOUR %in% names(rv_plot$data))
        #req(input$picker_SIZE %in% names(rv_plot$data))
        #req(input$picker_SHAPE %in% names(rv_plot$data))
        #req(input$aggreg_fun_COLOUR)
        #req(aggreg_functions[fun == input$aggreg_fun_COLOUR, for_num] == rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"])

        data_plot_aggr <- rv_plot$data[
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
            VAR_SHAPE = ifelse(isTruthy(input$picker_SHAPE) && input$picker_SHAPE != "",
              do.call(what = input$aggreg_fun_SHAPE, args = list(x = eval(as.name(input$picker_SHAPE)),na.rm = T)),
              NA),
            VAR_COLOUR = ifelse(
              isTruthy(input$picker_COLOUR) && input$picker_COLOUR != "" && input$aggreg_fun_COLOUR != "",
              do.call(what = input$aggreg_fun_COLOUR, args = list(x = eval(as.name(input$picker_COLOUR)),na.rm = T)),
              NA),
            VAR_SIZE = ifelse(
              isTruthy(input$picker_SIZE) && input$picker_SIZE != "",
              do.call(what = input$aggreg_fun_SIZE, args = list(x = eval(as.name(input$picker_SIZE)), na.rm = T)),
              NA)
          ),
          by = group_by_cols
        ]
        
        ## transform X variable
        # - no transformation (default)
        data_plot_aggr[,VAR_X_PLOT:=VAR_X]
        if (isTruthy(input$express_X_as_ranks)){
          # - ranking
          if(input$ranking_order_X == F){ #descending
            data_plot_aggr[, VAR_X_PLOT := base::rank(x = -VAR_X, na.last = T, ties.method = "min")]
          }else{
            data_plot_aggr[, VAR_X_PLOT := base::rank(x = VAR_X, na.last = T, ties.method = "min")]
          }
        } else if(input$switch_aggregate==T && isTruthy(input$express_X_relative) && !(input$aggregate_by %in% c("plot"))){
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
        if(isTruthy(input$express_Y_as_ranks)){
          # - ranking
          if(input$ranking_order_Y == F){ #descending
            data_plot_aggr[, VAR_Y_PLOT := base::rank(x = -VAR_Y, na.last = T, ties.method = "min")]
          }else{
            data_plot_aggr[, VAR_Y_PLOT := base::rank(x = VAR_Y, na.last = T, ties.method = "min")]
          }
        }else if(input$switch_aggregate==T && isTruthy(input$express_Y_relative) && !(input$aggregate_by %in% c("plot"))){
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
        
        rv_plot$data_aggr <- data_plot_aggr
      })
      
      ## go regression ####
      observeEvent(input$go_regression, {
        rv_plot$draw_regression <- !rv_plot$draw_regression
      })
      
      ## Plot ####
      output$scatterplot <- renderPlotly({
        req(rv_plot$data_aggr)
        #req(input$picker_COLOUR)
        #req(rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"] == is.numeric(rv_plot$data_aggr[,VAR_COLOUR]))
        
        rv_plot$draw_regression
        rv_plot$draw_clusters
        
        d <- rv_plot$data_aggr
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
        d[, "Size scale" := if(isTruthy(input$picker_SIZE)) VAR_SIZE else NA] # workaround for the plotly tooltip
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
            labels = if(isTruthy(input$express_X_relative) && isTruthy(input$ref_genotype_X)){scales::percent}else{waiver()},
            # trans = if(input$express_X_as=="as ranks"){"reverse"}else{"identity"}, # disabled to make regression computation and drawing more simple
            breaks = if(isTruthy(input$express_X_as_ranks)){as.numeric(floor(quantile(rv_plot$data_aggr$VAR_X_PLOT, na.rm = T, probs = seq(1,0,-0.2))))}else{waiver()},
            name = if(isTruthy(input$express_X_as_ranks)){
              paste(input$picker_X, "(ranks)")
            }else if(isTruthy(input$express_X_relative) & isTruthy(input$ref_genotype_X)){
              paste0(input$picker_X, " (relative to ", input$ref_genotype_X, ")")
            }else{
              input$picker_X
            }
          ) +
          scale_y_continuous(
            labels = if(isTruthy(input$express_Y_relative) && isTruthy(input$ref_genotype_Y)){scales::percent}else{waiver()},
            # trans = if(input$express_Y_as=="as ranks"){"reverse"}else{"identity"}, # disabled to make regression computation and drawing more simple
            breaks = if(isTruthy(input$express_Y_as_ranks)){as.numeric(floor(quantile(rv_plot$data_aggr$VAR_Y_PLOT, na.rm = T, probs = seq(1,0,-0.2))))}else{waiver()},
            name = if(isTruthy(input$express_Y_as_ranks)){
              paste(input$picker_Y, "(ranks)")
            }else if(isTruthy(input$express_Y_relative) && isTruthy(input$ref_genotype_Y)){
              paste0(input$picker_Y, " (relative to ", input$ref_genotype_Y, ")")
            }else{
              input$picker_Y
            }
          ) +
          scale_shape(name = input$picker_SHAPE) +
          scale_size(name = input$picker_SIZE) +
          if (isTruthy(input$picker_COLOUR) | rv_plot$draw_clusters == T) scale_color_custom(
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
      
      ## Selection ####
      observeEvent(event_data("plotly_click", source = "A"),{
        rv_plot$plot_selection <- as.data.table(event_data("plotly_click", source = "A"))
      })
      observeEvent(event_data("plotly_selected", source = "A"),{
        rv_plot$plot_selection <- as.data.table(event_data("plotly_selected", source = "A"))
      })
      observeEvent(rv_plot$plot_selection[,.N],{
        shinyjs::toggle(selector = paste0(".",ns("ui_create_group")), condition = rv_plot$plot_selection[,.N]>0)
        req(dim(rv_plot$plot_selection)[1]>0)
        germplasms <- unique(rv_plot$data[germplasmDbId %in% rv_plot$plot_selection[,unique(key)], .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv$groups$group_id) || length(rv$groups$group_id) == 0, 1, max(rv$groups$group_id) + 1)
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
              express_X_as_ranks = input$express_X_as_ranks,
              express_X_relative = input$express_X_relative,
              ref_genotype_X = input$ref_genotype_X,
              ranking_order_X = input$ranking_order_X,
              picker_Y = input$picker_Y,
              aggreg_fun_ = input$aggreg_fun_Y,
              express_Y_as_ranks = input$express_Y_as_ranks,
              express_Y_relative = input$express_Y_relative,
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
        rv$selection <- selection_data
      })
      
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
            germplasmDbId %in% rv$groups[group_id == input$group_sel_input, germplasmDbIds][[1]]
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
      
      output$ui_clusters <- renderUI({
        if(input$aggregate_by == "germplasm" & input$switch_aggregate == T){
          req(rv_plot$data_aggr)
          max_k <- rv_plot$data_aggr[,.N,germplasmName][,.N]
          dropdownButton(
            label = "Cluster germplasms", status = "info", circle = F, inline = T, up = F, width = "500px",
            radioGroupButtons(ns("cluster_algo"), "Alogrithm", choiceNames = c("K-means", "HCA with Ward distance"), choiceValues = c("kmeans", "hca")),
            numericInput(ns("n_clusters"), label = "Number of clusters", min = 1, max = max_k, step = 1, value = min(3, max_k)),
            actionButton(ns("go_clustering"), "Cluster", class = "btn btn-primary"),
            uiOutput(ns("cluster_results"))
          )
        }else{
          span()
        }
      })
      
      ## go clustering ####
      observeEvent(input$go_clustering,{
        req(rv_plot$data_aggr)
        req(input$n_clusters)
        max_k <- rv_plot$data_aggr[,.N,germplasmName][,.N]
        n_clusters <- input$n_clusters
        if(input$n_clusters > max_k){
          updateNumericInput(session, "n_clusters", value = max_k)
          n_clusters <- max_k
        }else if(input$n_clusters <= 0){
          updateNumericInput(session, "n_clusters", value = 1)
          n_clusters <- 1
        }
        d <- rv_plot$data_aggr[, .(scale(VAR_X, center = T, scale = T), scale(VAR_Y, center = T, scale = T))]
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
        clusters <- data.table(rv_plot$data_aggr[,.(germplasmDbId, germplasmName)], cluster = clus, method = input$cluster_algo, counter = counter)
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
      
      ## download cluster ####
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
      
      ## group from cluster ####
      observeEvent(input$create_groups_from_clusters,{
        shinyjs::disable("create_groups_from_clusters")
        shinyjs::addClass("create_groups_from_clusters", "active")
        if (nrow(rv$groups)==0){
          clustering_id <- 1
        } else {
          clustering_id <- ifelse(length(rv$groups[!is.na(clustering_id)]$clustering_id)==0, 1, max(rv$groups[!is.na(clustering_id)]$clustering_id) + 1)
        }
        
        clusters <- rv_plot$clusters[order(cluster)][,.(
          group_name = paste0("cl",clustering_id,"_",unique(method), ".", cluster),
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
              express_X_as_ranks = input$express_X_as_ranks,
              express_X_relative = input$express_X_relative,
              ref_genotype_X = input$ref_genotype_X,
              ranking_order_X = input$ranking_order_X,
              picker_Y = input$picker_Y,
              aggreg_fun_ = input$aggreg_fun_Y,
              express_Y_as_ranks = input$express_Y_as_ranks,
              express_Y_relative = input$express_Y_relative,
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
        clusters[N>6 ,germplasmNames_label := paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
        )]
        clusters[N <= 6, germplasmNames_label := paste(unlist(germplasmNames), collapse = ", ")]
        
        group_id_start <- ifelse(length(rv$groups$group_id)==0, 1, max(rv$groups$group_id) + 1)
        group_ids <- group_id_start:(group_id_start+clusters[,.N] -1)
        clusters[, group_id := group_ids]
        clusters[, clustering_id := clustering_id]
        rv$groups <- rbindlist(list(
          rv$groups,
          clusters
        ), fill = T, use.names = T)
        
        ## update selectors (shape, colour)
        data_plot <- copy(rv_plot$data) # to avoid reactivity issues related to assignment by reference
        #for(id in clusters[,unique(group_id)]){
        #  group_name <- clusters[group_id == id,group_name]
        #  data_plot[germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)], eval(group_name) := paste0('In "', group_name,'"')]
        #  data_plot[!(germplasmDbId %in% clusters[group_id == id,unlist(germplasmDbIds)]), eval(group_name) := paste0('Not in "', group_name,'"')]
        #}
        
        data_plot <- setnames(data_plot[clusters[,.(germplasmDbId=unlist(germplasmDbIds)),cluster],on=.(germplasmDbId)],old = "cluster",new = paste0("cl",clustering_id,"_",unique(rv_plot$clusters$method)))[]
        
        rv$column_datasource <- rbindlist(
          list(
            rv$column_datasource,
            #data.table(cols = clusters[,unique(group_name)], source = "group", type = "Text", visible = T)
            data.table(cols = paste0("cl",clustering_id,"_",unique(rv_plot$clusters$method)) ,  type = "Text", source = "group", visible = T)
            
          ), 
          use.names = T
        )
        rv_plot$data <- data_plot
      })
      
      ## create group ####
      observeEvent(input$go_create_group,{
        if(rv$selection[,.N]>0){
          showModal(groupModal(rv, parent_session, "Create new group", paste0(
            "Visualization at group creation:\nX=",input$picker_X,", \nY=",input$picker_Y,
            if(isTruthy(input$picker_COLOUR)) paste(", \nColour=", input$picker_COLOUR),
            if(isTruthy(input$picker_SHAPE)) paste(", \nShape=", input$picker_SHAPE),
            if(isTruthy(input$picker_SIZE)) paste(", \nSize=", input$picker_SIZE)
          )))
        }
      })
      
      ## update params from group ####
      observeEvent(rv$visu_as_group,{
        plot_params <- rv$groups[group_id == rv$visu_as_group, plot_params][[1]]
        
        updateMaterialSwitch(session, "switch_aggregate", value = plot_params$switch_aggregate)
        updatePickerInput(session, "aggregate_by", selected = plot_params$aggregate_by)
        
        updatePickerInput(session, "picker_X", selected = plot_params$picker_X)
        updatePickerInput(session, "aggreg_fun_X", selected = plot_params$aggreg_fun_X)
        updateCheckboxInput(session, "express_X_as_ranks", value = plot_params$express_X_as_ranks)
        updateCheckboxInput(session, "express_X_relative", value = plot_params$express_X_relative)
        updatePickerInput(session, "ref_genotype_X", selected = plot_params$ref_genotype_X)
        updateCheckboxInput(session, "ranking_order_X", value = plot_params$ranking_order_X)
        
        updatePickerInput(session, "picker_Y", selected = plot_params$picker_Y)
        updatePickerInput(session, "aggreg_fun_Y", selected = plot_params$aggreg_fun_Y)
        updateCheckboxInput(session, "express_Y_as_ranks", value = plot_params$express_Y_as_ranks)
        updateCheckboxInput(session, "express_Y_relative", value = plot_params$express_Y_relative)
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
        
        rv$visu_as_group <- NULL
      })
    }
  )
}
