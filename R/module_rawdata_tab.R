#' @export
mod_rawdata_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("QA"),
    fluidRow(
      column(
        7,
        tabsetPanel(
          selected = "Histogram",
          tabPanel(
            "Histogram",
            h2("Histogram"),
            sliderInput(ns("histogram_binwidth"), label = "Bin Width", min = 0.5, max = 100, value = 10),
            plotlyOutput(ns("histogram_plot")),# width = 800, height = 600),
          ),
          tabPanel(
            "Boxplot",
            h2("Boxplot"),
            plotlyOutput(ns("boxplot_plot")),# width = 800, height = 600),
          ),
          tabPanel(
            "Layout",
            h2("Layout"),
            column(
              8,
              checkboxInput(ns("show_obs_label"), "Show observationDbId", value = F),
              plotOutput(ns("layout_plot"), click = ns("layout_plot_click"), hover = ns("layout_plot_hover"), brush = ns("layout_plot_brush"))# width = 800, height = 600),
            ),
            column(
              4,
              uiOutput(ns("layout_hover_ui"))
            )
          ),
          tabPanel(
            "Summary Statistics",
            h2("Summary Statistics"),
            uiOutput(ns("sum_stats"))
          )
        )
      ),
      column(
        5,
        h2("Selected data"),
        dataTableOutput(ns("selected_obs_table")),
        actionButton(ns("set_excluded_obs"), "Set selected row(s) as excluded observation(s)"),
        actionButton(ns("reset_selected_obs"), "Clear selection"),
        tags$hr(),
        h2("Excluded data"),
        dataTableOutput(ns("excluded_obs_table")),
        actionButton(ns("set_non_excluded_obs"), "Set selected row(s) as non-excluded observation(s)"),
      )
    )
  )
}

#' @export
mod_rawdata_server <- function(id, d){
  req(d)
  moduleServer(
    id,
    function(input, output, session){

      rv_mod <- reactiveValues(d = d)

      observeEvent(rv_mod$d()[,.(observations.value)],{
        range_obs <- range(rv_mod$d()[,.(observations.value)], na.rm = T)
        updateSliderInput(session, "histogram_binwidth", value = diff(range_obs)/10,
                          min = diff(range_obs)/100, max = diff(range_obs)/2, step = diff(range_obs) /100)
      })

      output$histogram_plot <- renderPlotly({
        g <- ggplot(rv_mod$d()[is.excluded==F], aes(x = observations.value)) +
          geom_histogram(aes(y=..density..), binwidth = input$histogram_binwidth, position = "identity") +
          geom_density() +
          theme_minimal()
        ggplotly(g, dynamicTicks = "y", source = "A") %>%
          style(hoveron = "points", hoverinfo = "text", hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "lasso")
      })

      observeEvent(event_data("plotly_selected", source = "A"),{
        rv_mod$selected_obs_boxplot <- NULL
        rv_mod$selected_obs_layout <- NULL
        rv_mod$selected_obs_hist <- unlist(lapply(event_data("plotly_selected", source = "A")$x, function(val){
          rv_mod$d()[is.excluded==F][observations.value >= val - input$histogram_binwidth/2 & observations.value <= val + input$histogram_binwidth/2, observations.observationDbId]
        }))
      })

      # boxplot
      output$boxplot_plot <- renderPlotly({
        g <- ggplot(rv_mod$d()[is.excluded==F], aes(x = observations.value)) +
          geom_point(color = "green", y = 0, alpha = 0) +
          geom_boxplot(notch = T, fill = "red", stat = "boxplot") +
          theme_minimal()
        ggplotly(g, dynamicTicks = T, source = "B") %>%
          style(hoveron = "fill", hoverinfo = "text", hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "lasso")
      })

      observeEvent(event_data("plotly_selected", source = "B"),{
        rv_mod$selected_obs_hist <- NULL
        rv_mod$selected_obs_layout <- NULL
        rv_mod$selected_obs_boxplot <- unlist(lapply(event_data("plotly_selected", source = "B")$x, function(val){
          rv_mod$d()[is.excluded==F][observations.value == val, observations.observationDbId]
        }))
      })

      # layout
      output$layout_plot <- renderPlot({
        myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
        rv_mod$d()[,replicate:=as.factor(replicate)]

        g <- ggplot(rv_mod$d()[is.excluded==F], aes(x = positionCoordinateX, y = positionCoordinateY, fill = observations.value, linetype = replicate)) +
          geom_tile(size = 0.7, color = "grey") +
          coord_equal() +
          scale_fill_gradientn(colours = myPalette(100)) +
          scale_linetype(guide = F) +
          theme_minimal()+
          theme(legend.position="top", panel.grid = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank())
        if(input$show_obs_label){
          g <- g + geom_text(aes(label = observations.observationDbId))
        }
        g
      })

      output$layout_hover_ui <- renderUI({
        req(input$layout_plot_hover)
        obs <- select_from_layout(rv_mod$d()[is.excluded==F], input_click = input$layout_plot_hover)
        tagList(
          tags$table(
            tags$tr(
              tags$td(tags$label("observationDbId")),
              tags$td(tags$data(obs))
            ),
            tags$tr(
              tags$td(tags$label("replicate")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, replicate]))
            ),
            tags$tr(
              tags$td(tags$label("observationUnitName")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, observationUnitName]))
            ),
            tags$tr(
              tags$td(tags$label("plot no.")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, plotNumber]))
            ),
            tags$tr(
              tags$td(tags$label("germplasmDbId")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, germplasmDbId]))
            ),
            tags$tr(
              tags$td(tags$label("germplasmName")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, germplasmName]))
            ),
            tags$tr(
              tags$td(tags$label("value")),
              tags$td(tags$data(rv_mod$d()[is.excluded==F][observations.observationDbId %in% obs, observations.value]))
            )
          )
        )
      })

      observeEvent(input$layout_plot_brush,{
        rv_mod$selected_obs_hist <- NULL
        rv_mod$selected_obs_boxplot <- NULL
        rv_mod$selected_obs_layout <- select_from_layout(rv_mod$d()[is.excluded==F], input_brush = input$layout_plot_brush)
      })
      observeEvent(input$layout_plot_click,{
        rv_mod$selected_obs_hist <- NULL
        rv_mod$selected_obs_boxplot <- NULL
        rv_mod$selected_obs_layout <- select_from_layout(rv_mod$d()[is.excluded==F], input_click = input$layout_plot_click)
      })

      # summary statistics
      output$sum_stats <- renderUI({

        q <- rv_mod$d()[is.excluded==F, quantile(observations.value, probs = c(0,0.25,0.5,0.75,1))]
        n_obs <- rv_mod$d()[is.excluded==F,.N,observations.observationDbId][,.N]
        d_mean <- rv_mod$d()[is.excluded==F,mean(observations.value, na.rm = T)]
        d_sd <- rv_mod$d()[is.excluded==F,sd(observations.value, na.rm = T)]
        d_var <- rv_mod$d()[is.excluded==F,var(observations.value, na.rm = T)]
        d_skewness <- e1071::skewness(rv_mod$d()[is.excluded==F,observations.value])
        d_kurtosis <- e1071::kurtosis(rv_mod$d()[is.excluded==F,observations.value])

        tags$table(
          tags$tr(
            tags$td(tags$label("No. of values")),
            tags$td(tags$data(rv_mod$d()[,.N]))
          ),
          tags$tr(
            tags$td(tags$label("No. of observations")),
            tags$td(tags$data(n_obs))
          ),
          tags$tr(
            tags$td(tags$label("No. of excluded values")),
            tags$td(tags$data(rv_mod$d()[is.excluded==T,.N,observations.observationDbId][,.N]))
          ),
          tags$tr(
            tags$td(tags$label("Mean")),
            tags$td(tags$data(d_mean))
          ),
          tags$tr(
            tags$td(tags$label("Median")),
            tags$td(tags$data(q[3]))
          ),
          tags$tr(
            tags$td(tags$label("Min")),
            tags$td(tags$data(q[1]))
          ),
          tags$tr(
            tags$td(tags$label("Max")),
            tags$td(tags$data(q[5]))
          ),
          tags$tr(
            tags$td(tags$label("Range")),
            tags$td(tags$data(q[5]-q[1]))
          ),
          tags$tr(
            tags$td(tags$label("Lower quantile")),
            tags$td(tags$data(q[2]))
          ),
          tags$tr(
            tags$td(tags$label("Upper quantile")),
            tags$td(tags$data(q[4]))
          ),
          tags$tr(
            tags$td(tags$label("Standard deviation")),
            tags$td(tags$data(d_sd))
          ),
          tags$tr(
            tags$td(tags$label("Standard error of mean")),
            tags$td(tags$data(d_sd/sqrt(n_obs)))
          ),
          tags$tr(
            tags$td(tags$label("Variance")),
            tags$td(tags$data(d_var))
          ),
          tags$tr(
            tags$td(tags$label("Standard error of variance")),
            tags$td(tags$data(d_var/sqrt(n_obs)))
          ),
          tags$tr(
            tags$td(tags$label("%cov")),
            tags$td(tags$data(d_var/d_mean))
          ),
          tags$tr(
            tags$td(tags$label("Sum of values")),
            tags$td(tags$data(rv_mod$d()[is.excluded==F,sum(observations.value, na.rm = T)]))
          ),
          tags$tr(
            tags$td(tags$label("Sum of squares")),
            tags$td(tags$data(sum(rv_mod$d()[is.excluded==F,(observations.value-d_mean)^2], na.rm = T)))
          ),
          tags$tr(
            tags$td(tags$label("Uncorrected sum of squares")),
            tags$td(tags$data(sum(rv_mod$d()[is.excluded==F,observations.value^2], na.rm = T)))
          ),
          tags$tr(
            tags$td(tags$label("Skewness")),
            tags$td(tags$data(d_skewness))
          ),
          tags$tr(
            tags$td(tags$label("Standard error of skewness")),
            tags$td(tags$data(d_skewness/sqrt(n_obs)))
          ),
          tags$tr(
            tags$td(tags$label("Kurtosis")),
            tags$td(tags$data(d_kurtosis))
          ),
          tags$tr(
            tags$td(tags$label("Standard error of kurtosis")),
            tags$td(tags$data(d_kurtosis/sqrt(n_obs)))
          )
        )
      })

      # selected and excluded obs (right column)

      observe({
        rv_mod$d_excluded <- rv_mod$d()[
          observations.observationDbId %in% c(rv_mod$selected_obs_hist, rv_mod$selected_obs_boxplot, rv_mod$selected_obs_layout)
        ]
      })
      observeEvent(input$reset_selected_obs,{
        rv_mod$selected_obs_hist <- NULL
        rv_mod$selected_obs_boxplot <- NULL
        rv_mod$selected_obs_layout <- NULL
      })

      output$selected_obs_table <- renderDT(
        datatable(
          rv_mod$d_excluded,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
              visible=FALSE,
              targets=match(hidden_columns_observationunits, names(rv_mod$d_excluded))
              )
            ),
            paging = F,
            scrollX = T,
            scrollY = "300px",
            scrollCollapse = T,
            dom = 'Bt',
            buttons = I('colvis')
          )
        )
      )

      observeEvent(input$set_excluded_obs,{
        d <- rv_mod$d()
        excluded_obs <- rv_mod$d()[
          observations.observationDbId %in% c(rv_mod$selected_obs_hist, rv_mod$selected_obs_boxplot, rv_mod$selected_obs_layout)
        ][
          input$selected_obs_table_rows_selected, observations.observationDbId]
        d[observations.observationDbId %in% excluded_obs, is.excluded:=T]
        rv_mod$d <- reactive(d)
      })

      output$excluded_obs_table <- renderDT(
        datatable(
          rv_mod$d()[is.excluded==T],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv_mod$d_excluded))
              )
            ),
            paging = F,
            scrollX = T,
            scrollY = "300px",
            scrollCollapse = T,
            dom = 'Bt',
            buttons = I('colvis')
          )
        )
      )

      observeEvent(input$set_non_excluded_obs,{
        d <- rv_mod$d()
        non_excluded_obs <- rv_mod$d()[is.excluded==T][
          input$excluded_obs_table_rows_selected, observations.observationDbId]
        d[observations.observationDbId %in% non_excluded_obs, is.excluded:=F]
        rv_mod$d <- reactive(d)
      })
    }
  )
}
