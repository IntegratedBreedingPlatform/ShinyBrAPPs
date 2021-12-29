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
        pickerInput(ns("aggregate_by"), "Aggregate by", choices = c("environment", "plot"), multiple = F),
        tags$label("X", style = "display:block"),
        pickerInput(ns("picker_X"), "Variable", choices = NULL, inline = T),
        pickerInput(ns("aggreg_fun_X"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T),
        div(radioButtons(ns("express_X_as"), "Show values", choices = c("as they are", "as ranks", "relatively to genotype"), inline = T), style = "display:inline-block"),
        div(id = ns("div_ref_genotype_X"), pickerInput(ns("ref_genotype_X"), "", choices = NULL, inline = T), style = "display:inline-block"),
        tags$label("Y", style = "display:block"),
        pickerInput(ns("picker_Y"), "Variable", choices = NULL, inline = T),
        pickerInput(ns("aggreg_fun_Y"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T),
        div(radioButtons(ns("express_Y_as"), "Show values", choices = c("as they are", "as ranks", "relatively to genotype"), inline = T), style = "display:inline-block"),
        div(id = ns("div_ref_genotype_Y"), pickerInput(ns("ref_genotype_Y"), "", choices = NULL, inline = T), style = "display:inline-block"),
        materialSwitch(inputId = ns("switch_SHAPE"), label = "Shape", value = F),
        div(
          id = ns("ui_SHAPE"),
          pickerInput(ns("picker_SHAPE"), "Variable", choices = NULL, inline = T),
          pickerInput(ns("aggreg_fun_SHAPE"), "Aggregation function", choices = c("concatenate unique values"="unique_values"), inline = T)
        ),
        materialSwitch(inputId = ns("switch_COLOUR"), label = "Colour", value = F),
        div(
          id = ns("ui_COLOUR"),
          pickerInput(ns("picker_COLOUR"), "Variable", choices = NULL, inline = T),
          pickerInput(ns("aggreg_fun_COLOUR"), "Aggregation function", choices = NULL, inline = T)
        ),
        materialSwitch(inputId = ns("switch_SIZE"), label = "Size", value = F),
        div(
          id = ns("ui_SIZE"),
          pickerInput(ns("picker_SIZE"), "Variable", choices = NULL, inline = T),
          pickerInput(ns("aggreg_fun_SIZE"), "Aggregation function", choices = c("mean", "max", "min", "sum"), inline = T)
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
        column_datasource <- rv$column_datasource
        ## update environments
        envs <- unique(rv$data_plot[,.(studyDbId, studyName)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,studyName]
        updatePickerInput(
          session, "env",
          choices = env_choices, selected = env_choices
        )

        ## which variables can be numerical
        # XXX
        are_num <- rv$data_plot[,lapply(.SD, is.numeric)]
        numeric_variables <- names(are_num)[are_num==T]
        non_numeric_variables <- names(are_num)[are_num==F]
        column_datasource[,is_num:=F]
        column_datasource[cols %in% numeric_variables,is_num:=T]

        ## update variable selectors
        num_var_choices <- column_datasource[is_num==T,.(cols = list(cols)), source]
        non_num_var_choices <- column_datasource[is_num==F,.(cols = list(cols)), source]
        var_choices_all <- column_datasource[,.(cols = list(cols)), source]
        default_X <- column_datasource[is_num == T & source == "GxE"][1, cols]
        default_Y <- column_datasource[is_num == T & source == "GxE"][2, cols]
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
      })

      ## update colour aggreg functions (colour can be num or categorical)
      observeEvent(input$picker_COLOUR, {
        req(input$picker_COLOUR)
        COLOUR_is_num <- rv$column_datasource[cols == input$picker_COLOUR, is_num]
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
      observe({shinyjs::toggle("ui_SHAPE", condition = input$switch_SHAPE==T)})
      observe({shinyjs::toggle("ui_COLOUR", condition = input$switch_COLOUR==T)})
      observe({shinyjs::toggle("ui_SIZE", condition = input$switch_SIZE==T)})


      ## aggreg dataset
      observe({
        req(input$env)
        if(input$aggregate_by=="plot"){
          group_by_cols <- c("studyName", "germplasmName","observationUnitName")
        }else if(input$aggregate_by=="environment"){
          group_by_cols <- c("studyName", "germplasmName")
        }

        req(input$aggreg_fun_COLOUR)
        req(aggreg_functions[fun == input$aggreg_fun_COLOUR, for_num] == rv$column_datasource[cols == input$picker_COLOUR, is_num])
        rv$data_plot_aggr <- rv$data_plot[
          studyDbId %in% input$env,
          .(
            N_obs = .N,
            VAR_X = do.call(what = input$aggreg_fun_X, args = list(x = eval(as.name(input$picker_X)), na.rm = T)),
            VAR_Y = do.call(what = input$aggreg_fun_Y, args = list(x = eval(as.name(input$picker_Y)), na.rm = T)),
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
      })

      output$scatterplot <- renderPlot({
        req(rv$data_plot_aggr)

        p <- ggplot(rv$data_plot_aggr, aes(
          x = VAR_X, y = VAR_Y,
          colour = if(input$switch_COLOUR == T) VAR_COLOUR else NULL,
          shape = if(input$switch_SHAPE == T) VAR_SHAPE else NULL,
          size = if(input$switch_SIZE == T) VAR_SIZE else NULL
        )) +
          xlab(input$picker_X) +
          ylab(input$picker_Y) +
          geom_point(alpha = 0.5) +
          scale_shape(name = input$picker_SHAPE) +
          scale_size(name = input$picker_SIZE) +
          scale_color_custom(
            is_num = rv$column_datasource[cols == isolate(input$picker_COLOUR), is_num],
            name = isolate(input$picker_COLOUR)
          ) +
          theme_minimal() +
          theme(legend.position = "bottom")

        pp <- ggMarginal(p, type = "density", fill =  "black", alpha = 0.05)
        pp
      })

      output$debug <- renderPrint({
        list(
          # input$picker_Y,
          # rv$data_plot_filt,
          # rv$column_datasource
          # input$scatterplot_hover,
          # input$scatterplot_click,
          # input$scatterplot_brush,
          rv$data_plot_aggr
        )
      })
    }
  )
}
