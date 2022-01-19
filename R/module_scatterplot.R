#' @export
mod_scatterplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        pickerInput(
          inputId = ns("env"),
          label = "Environments",
          choices = NULL,
          width = "100%",
          multiple = T,
          options = list(
            `actions-box` = TRUE,
            title = 'Load Environments First',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        materialSwitch(inputId = ns("switch_aggregate"), label = "Aggregate observations", value = F, inline = T),
        div(class = ns("ui_aggregate"),
            pickerInput(ns("aggregate_by"), "Aggregate by", choices =  c("germplasm and environment", "germplasm"), selected = "germplasm", multiple = F)
        ),
        tags$label("X", style = "display:block"),
        pickerInput(ns("picker_X"), "Variable", choices = NULL, inline = T),
        div(
          class = ns("ui_aggregate"), style = "display:inline-block",
          pickerInput(ns("aggreg_fun_X"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T)
        ),
        div(radioButtons(ns("express_X_as"), "Show values", choices = "", inline = T), style = "display:inline-block"),
        div(id = ns("div_ref_genotype_X"), pickerInput(ns("ref_genotype_X"), "", choices = NULL, inline = T), style = "display:inline-block"),
        tags$label("Y", style = "display:block"),
        pickerInput(ns("picker_Y"), "Variable", choices = NULL, inline = T),
        div(
          class = ns("ui_aggregate"), style = "display:inline-block",
          pickerInput(ns("aggreg_fun_Y"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T)
        ),
        div(radioButtons(ns("express_Y_as"), "Show values", choices = "", inline = T), style = "display:inline-block"),
        div(id = ns("div_ref_genotype_Y"), pickerInput(ns("ref_genotype_Y"), "", choices = NULL, inline = T), style = "display:inline-block"),
        materialSwitch(inputId = ns("switch_SHAPE"), label = "Shape", value = F),
        div(
          id = ns("ui_SHAPE"),
          pickerInput(ns("picker_SHAPE"), "Variable", choices = NULL, inline = T),
          div(
            class = ns("ui_aggregate"), style = "display:inline-block",
            pickerInput(ns("aggreg_fun_SHAPE"), "Aggregation function", choices = c("concatenate unique values"="unique_values"), inline = T)
          )
        ),
        materialSwitch(inputId = ns("switch_COLOUR"), label = "Colour", value = F),
        div(
          id = ns("ui_COLOUR"),
          pickerInput(ns("picker_COLOUR"), "Variable", choices = NULL, inline = T),
          div(
            class = ns("ui_aggregate"), style = "display:inline-block",
            pickerInput(ns("aggreg_fun_COLOUR"), "Aggregation function", choices = NULL, inline = T)
          )
        ),
        materialSwitch(inputId = ns("switch_SIZE"), label = "Size", value = F),
        div(
          id = ns("ui_SIZE"),
          pickerInput(ns("picker_SIZE"), "Variable", choices = NULL, inline = T),
          div(
            class = ns("ui_aggregate"), style = "display:inline-block",
            pickerInput(ns("aggreg_fun_SIZE"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T)
          )
        )
      )
    ),
    fluidRow(
      column(12,
             plotOutput(
               ns("scatterplot"), width = "100%", height = "600px",
               click = ns("scatterplot_click"),
               hover = ns("scatterplot_hover"),
               brush = ns("scatterplot_brush")
             ),
             verbatimTextOutput(ns("debug"))
      )
    )
  )
}

#' @export
mod_scatterplot_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns
      rv_plot <- reactiveValues()

      ## function for data aggregation
      aggreg_functions <- data.table(
        fun = c("mean", "max", "min", "sum", "unique_values"),
        label = c("average", "max", "min", "sum", "concatenate unique values"),
        for_num = c(T,T,T,T,F)
      )
      unique_values <- function(x, ...){
        paste(unique(x), collapse = ", ")
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

        column_datasource <- rv$column_datasource

        ## update environments
        envs <- unique(rv$data_plot[,.(studyDbId, studyName)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,studyName]
        updatePickerInput(
          session, "env",
          choices = env_choices, selected = env_choices
        )

        ## update variable selectors
        num_var_choices <- rv$column_datasource[type == "Numerical",.(cols = list(cols)), source]
        non_num_var_choices <- rv$column_datasource[type != "Numerical",.(cols = list(cols)), source]
        var_choices_all <- rv$column_datasource[,.(cols = list(cols)), source]
        default_X <- rv$column_datasource[type == "Numerical" & source == "GxE"][1, cols]
        default_Y <- rv$column_datasource[type == "Numerical" & source == "GxE"][2, cols]
        updatePickerInput(
          session = session, inputId = "picker_X",
          choices = setNames(num_var_choices[,cols], num_var_choices[,source]),
          selected = default_X
        )
        updatePickerInput(
          session = session, inputId = "picker_Y",
          choices = setNames(num_var_choices[,cols], num_var_choices[,source]),
          selected = default_Y
        )
        updatePickerInput(
          session = session, inputId = "picker_SIZE",
          choices = setNames(num_var_choices[,cols], num_var_choices[,source])
        )
        updatePickerInput(
          session = session, inputId = "picker_COLOUR",
          choices = setNames(var_choices_all[,cols], var_choices_all[,source])
        )
        updatePickerInput(
          session = session, inputId = "picker_SHAPE",
          choices = setNames(non_num_var_choices[,cols], non_num_var_choices[,source])
        )

        ## update list of germplasms
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

      ## disable "express relatively to genotype" if aggregation by plot
      observeEvent(input$switch_aggregate, {
        # XXX
          # non_num_var_choices <- rv$column_datasource[type != "Numerical",.(cols = list(cols)), source]
          # print(non_num_var_choices)
        if(input$switch_aggregate == T){
          updateRadioButtons(session, "express_X_as", choices = c("as they are", "as ranks", "relatively to genotype"), inline = T)
          updateRadioButtons(session, "express_Y_as", choices = c("as they are", "as ranks", "relatively to genotype"), inline = T)
        }else{
          updateRadioButtons(session, "express_X_as", choices = c("as they are", "as ranks"), inline = T)
          updateRadioButtons(session, "express_Y_as", choices = c("as they are", "as ranks"), inline = T)
          # updatePickerInput(
          #   session = session, inputId = "picker_SHAPE",
          #   choices = setNames(non_num_var_choices[,cols], non_num_var_choices[,source])
          # )
        }
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

      ## toggle genotype reference UI
      observe({shinyjs::toggle("div_ref_genotype_X", condition = input$express_X_as=="relatively to genotype")})
      observe({shinyjs::toggle("div_ref_genotype_Y", condition = input$express_Y_as=="relatively to genotype")})
      ## toggle aes UI
      observe({shinyjs::toggle(selector = paste0(".",ns("ui_aggregate")), condition = input$switch_aggregate==T)})
      observe({shinyjs::toggle("ui_SHAPE", condition = input$switch_SHAPE==T)})
      observe({shinyjs::toggle("ui_COLOUR", condition = input$switch_COLOUR==T)})
      observe({shinyjs::toggle("ui_SIZE", condition = input$switch_SIZE==T)})


      ## aggreg dataset
      observe({
        rv$data_plot_aggr <- NULL
        req(rv$data_plot)
        if(input$switch_aggregate== T){
          if(input$aggregate_by == "germplasm and environment"){
            group_by_cols <- c("studyName", "germplasmName")
          }else if(input$aggregate_by == "germplasm"){
            group_by_cols <- c("germplasmName")
          }
        }else{
          group_by_cols <- names(rv$data_plot)[grep("Id$",names(rv$data_plot))]
        }
        req(input$picker_X %in% names(rv$data_plot))
        req(input$picker_Y %in% names(rv$data_plot))
        req(input$picker_COLOUR %in% names(rv$data_plot))
        req(input$picker_SIZE %in% names(rv$data_plot))
        req(input$picker_SHAPE %in% names(rv$data_plot))
        req(input$aggreg_fun_COLOUR)
        req(aggreg_functions[fun == input$aggreg_fun_COLOUR, for_num] == rv$column_datasource[cols == input$picker_COLOUR, type == "Numerical"])
        data_plot_aggr <- rv$data_plot[
          studyDbId %in% input$env,
          .(
            N_obs = .N,
            VAR_X = do.call(what = input$aggreg_fun_X, args = list(
              x = if(isTruthy(input$picker_X)) eval(as.name(input$picker_X)) else NA,
              na.rm = T)),
            VAR_Y = do.call(what = input$aggreg_fun_Y, args = list(
              x = if(isTruthy(input$picker_Y)) eval(as.name(input$picker_Y)) else NA,
              na.rm = T)),
            VAR_SHAPE = do.call(what = input$aggreg_fun_SHAPE, args = list(
              x = if(input$switch_SHAPE) eval(as.name(input$picker_SHAPE)) else NA,
              na.rm = T)),
            VAR_COLOUR = do.call(what = input$aggreg_fun_COLOUR, args = list(
              x = if(input$switch_COLOUR) eval(as.name(input$picker_COLOUR)) else NA,
              na.rm = T)),
            VAR_SIZE = do.call(what = input$aggreg_fun_SIZE, args = list(
              x = if(input$switch_SIZE) eval(as.name(input$picker_SIZE)) else NA,
              na.rm = T))
          ),
          by = group_by_cols
        ]

        ## transform X variable
        # - no transformation (default)
        data_plot_aggr[,VAR_X_PLOT:=VAR_X]
        if(input$express_X_as=="as ranks"){
          # - ranking
          data_plot_aggr[, VAR_X_PLOT := base::rank(x = VAR_X, na.last = T, ties.method = "min")]
        }else if(input$express_X_as=="relatively to genotype" & !(input$aggregate_by %in% c("plot"))){
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
        if(input$express_Y_as=="as ranks"){
          # - ranking
          data_plot_aggr[, VAR_Y_PLOT := base::rank(x = VAR_Y, na.last = T, ties.method = "min")]
        }else if(input$express_Y_as=="relatively to genotype" & !(input$aggregate_by %in% c("plot"))){
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


      output$scatterplot <- renderPlot({
        req(rv$data_plot_aggr)
        p <- ggplot(rv$data_plot_aggr, aes(
          x = VAR_X_PLOT, y = VAR_Y_PLOT,
          colour = if(input$switch_COLOUR == T) VAR_COLOUR else NULL,
          shape = if(input$switch_SHAPE == T) VAR_SHAPE else NULL,
          size = if(input$switch_SIZE == T) VAR_SIZE else NULL
        )) +
          geom_point(alpha = 0.5) +
          scale_x_continuous(
            labels = if(input$express_X_as=="relatively to genotype" & isTruthy(input$ref_genotype_X)){scales::percent}else{waiver()},
            trans = if(input$express_X_as=="as ranks"){"reverse"}else{"identity"},
            name = if(input$express_X_as=="as ranks"){
              paste(input$picker_X, "(ranks)")
            }else if(input$express_X_as=="relatively to genotype" & isTruthy(input$ref_genotype_X)){
              paste0(input$picker_X, " (relative to ", input$ref_genotype_X, ")")
            }else{
              input$picker_X
            }
          ) +
          scale_y_continuous(
            labels = if(input$express_Y_as=="relatively to genotype" & isTruthy(input$ref_genotype_Y)){scales::percent}else{waiver()},
            trans = if(input$express_Y_as=="as ranks"){"reverse"}else{"identity"},
            name = if(input$express_Y_as=="as ranks"){
              paste(input$picker_Y, "(ranks)")
            }else if(input$express_Y_as=="relatively to genotype" & isTruthy(input$ref_genotype_Y)){
              paste0(input$picker_Y, " (relative to ", input$ref_genotype_Y, ")")
            }else{
              input$picker_Y
            }
          ) +
          scale_shape(name = input$picker_SHAPE) +
          scale_size(name = input$picker_SIZE) +
          scale_color_custom(
            is_num = rv$column_datasource[cols == isolate(input$picker_COLOUR), type == "Numerical"],
            name = isolate(input$picker_COLOUR)
          ) +
          theme_minimal() +
          theme(legend.position = "bottom")

        pp <- ggMarginal(p, type = "density", fill =  "black", alpha = 0.05)
        pp
      })

      output$debug <- renderPrint({
        rv$data_plot_aggr$VAR_Y_PLOT
        rv$data_plot_aggr$VAR_X_PLOT
        list(
          # rv$column_datasource,
          # input$scatterplot_click,
          # nearPoints(rv$data_plot_aggr, input$scatterplot_click, xvar = "VAR_X_PLOT", yvar = "VAR_Y_PLOT"),
          # nearPoints(rv$data_plot_aggr, input$scatterplot_hover, xvar = "VAR_X_PLOT", yvar = "VAR_Y_PLOT"),
          # nearPoints(rv$data_plot_aggr, input$scatterplot_brush, xvar = "VAR_X_PLOT", yvar = "VAR_Y_PLOT"),
          # rv$data_plot_aggr[,.N,germplasmName][N>1],
          rv$data_plot_aggr
        )
      })
    }
  )
}
