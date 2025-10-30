#' @import plotly
#' @import bslib
#' @export
mod_dataquality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$style(
    #   HTML(
    #     ".nav .nav-item .nav-link { font-size: 20px; }
    #     ",
    #   )
    # ),
    ## Select inputs ####
    layout_columns(
      col_widths = c(8, 3, 1),
      pickerInput(
        inputId = ns("studies"),
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
      pickerInput(
        ns("trait"),
        label = "Trait",
        choices = NULL,
        width = "100%",
        options = list(
          title = 'Load Environments First',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      shiny::actionLink(
        ns("envXtrait"),
        label = "Env x Trait",
        width = "100%",
        icon = icon("info"),
        class = "btn btn-info"
      ),
      # bsModal(
      #   ns("envXtraitModal"),
      #   title = "Environment x Trait",
      #   trigger = ns("envXtrait"),
      #   size = "large",
      #   plotOutput(ns("envXtraitViz"))
      # )
    ),
    navset_tab(
      selected = "Distributions",
      
      ## Distributions panel ####
      nav_panel(
        title = "Distributions",
        layout_sidebar(
          ### sidebar observations ####
          sidebar = sidebar(
            id = ns("sel_obs_sidebar"),
            position = "right",
            width = "30%",
            open = F,
            h3("Selected observations"),
            dataTableOutput(ns("selected_obs_table")),
            layout_columns(
              col_widths = c(6, 6),
              shiny::actionButton(
                ns("set_excluded_obs"),
                "Exclude selected rows",
                class = "btn btn-info",
                #style = "width: auto;"
                disabled = T
              ),
              shiny::actionButton(
                ns("unselect_obs"),
                "Reset selection",
                class = "btn btn-info",
                #style = "width: auto;"
                disabled = T
              )
            ),
            #h3("Excluded observations", class = "display_if_selection", style = "display: none"),
            #dataTableOutput(ns("excluded_obs_table")),
            # shiny::actionButton(
            #   ns("set_non_excluded_obs"),
            #   "Set selected row(s) as non-excluded observation(s)",
            #   class = "btn btn-info display_if_exclusion",
            #   style = "width: auto; display: none"
            # ),
          ),
          ### select obs input ####
          div(
            id = "container",
            #add a div to avoid having a big space between this the pickerInputs and the plots when excluding some observations
            layout_columns(
              col_widths = c(6, 6),
              pickerInput(
                ns("select_variable"),
                label = "Select observations by variable value",
                choices = NULL,
                multiple = F,
                width = "100%",
                options = list(
                  title = 'Select a variable to filter observations',
                  `live-search` = TRUE,
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              pickerInput(
                ns("select_variable_value"),
                label = HTML("<br/>"),
                choices = NULL,
                multiple = T,
                width = "100%",
                options = list(
                  title = 'Pick a value',
                  `live-search` = TRUE,
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            )
          ),
          tags$label("Or select observations directly on the plots"),
          ### plots ####
          uiOutput(ns("cols"))
        )
      ),
      ## Correlations panel ####
      nav_panel(
        title = "Correlations", 
        plotlyOutput(ns("correlationPlot")), 
        # width = 800, height = 600)),
      ),
      ## Summary Statistics panel ####
      nav_panel(
        title = "Summary Statistics",
        shinyjs::disabled(downloadButton(ns("summary_stats_export"), "CSV export", class = "btn btn-primary", style = "margin: 15px;")),
        dataTableOutput(ns("sumstats_table"))
      )
    )
  )
}

#' @import ggplot2
#' @importFrom ggpubr get_legend
#' @import data.table
#' @importFrom statgenSTA createTD
#' @importFrom DT datatable formatStyle
#' @export
mod_dataquality_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv_dq <- reactiveValues(
      width = 12,
      sel_observationDbIds = character(0),
      data_viz = NULL,
      data = NULL
    )
    
    ## observe rv$data ####
    observeEvent(rv$data, {
      validate(
        need(
          "observationVariableName" %in% names(rv$data),
          "There is no trait data for this study"
        )
      )
      
      newdt <- rv$data
      newdt[, status := "default"]
      newdt[, status := factor(status, levels = c("default", "selected", "excluded"))]
      
      rv_dq$data <- newdt
      
      env_choices <- rv$data[!is.na(observationValue)][, unique(studyDbId)]
      names(env_choices) <- rv$data[!is.na(observationValue)][, unique(study_name_app)]
      #names(env_choices) <- rv$study_metadata[loaded==T,unique(study_name_app)]
      updatePickerInput(
        inputId = "studies",
        session = session,
        choices = env_choices,
        selected = env_choices,
        options = list(
          title = 'Select 1 or more environments',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
      
      are_num <- rv$data[, lapply(.SD, is.numeric)]
      non_numeric_variables <- names(are_num)[are_num == F]
      non_numeric_variables <- non_numeric_variables[!non_numeric_variables %in%
                                                       hidden_col_names]
      updatePickerInput(
        session = session,
        inputId = "select_variable",
        choices = non_numeric_variables,
        options = list(
          title = 'Select a variable',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
      updatePickerInput(
        session = session,
        inputId = "select_variable_value",
        options = list(
          title = 'Select a value',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    
    ## output cols ####
    output$cols <- renderUI({
      ns <- NS(id)
      tagList(
        bslib::layout_columns(
          col_widths = c(rv_dq$width, 5, 1),
          plotlyOutput(ns("distribution_viz")),
          #,height = "400px")
          plotlyOutput(ns("layout_viz")),
          plotOutput(ns("layout_legend"))
        )
      )
    })
    
    ## observe input$studies ####
    observeEvent(input$studies, {
      req(rv$data)
      
      choices_traits <- unique(rv$data[studyDbId %in% input$studies]$observationVariableName)
      if (input$trait %in% choices_traits) {
        selected_trait <- input$trait
      } else {
        selected_trait <- choices_traits[1]
      }
      
      updatePickerInput(
        session,
        "trait",
        choices = choices_traits,
        selected = selected_trait,
        options = list(
          placeholder = 'Select 1 or more traits',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    
    ## update rv_dq$data_viz ####
    #Filter data on selected trait and studies and convert values in numeric or dates
    observeEvent(c(input$trait, input$studies, rv_dq$data), {
      req(rv$data)
      req(input$trait != "")
      req(input$studies != "")
      rv_dq$data_viz <- rv_dq$data[observationVariableName == input$trait &
                                  studyDbId %in% input$studies]
      
      rv_dq$data_viz[observationDbId %in% rv_dq$sel_observationDbIds &
                       !(observationDbId %in% rv$excluded_obs$observationDbId)]
      if (length(unique(rv_dq$data_viz$scale.dataType)) &&
          unique(rv_dq$data_viz$scale.dataType) == "Numerical") {
        rv_dq$data_viz[, observationValue := as.numeric(observationValue)]
      } else if (length(unique(rv_dq$data_viz$scale.dataType)) &&
                 unique(rv_dq$data_viz$scale.dataType) == "Date") {
        rv_dq$data_viz[, observationValue := as.Date(observationValue)]
      }
      
      rv_dq$data_viz[, study_name_abbrev_app := factor(study_name_abbrev_app, levels = rev(levels(factor(
        study_name_abbrev_app
      ))))]

    })
    
    ## observe input$select_variable ####
    observeEvent(input$select_variable, {
      req(input$select_variable)
      req(rv_dq$data_viz)
      
      if (input$select_variable == "") {
        shinyjs::hide("select_variable_value")
      } else{
        shinyjs::show("select_variable_value")
      }
      
      values <- unique(rv_dq$data_viz[, input$select_variable, with = F])
      
      if (values[, .N] == 1) {
        values <- unname(values)
      }
      updatePickerInput(
        session = session,
        inputId = "select_variable_value",
        # label = paste(input$select_variable, "values"),
        choices = values,
        options = list(
          title = 'Select a value',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    })
    
    ## observe input$select_variable_value ####
    observeEvent(input$select_variable_value, {
      req(rv_dq$data_viz)
      sel_observationDbIds <- rv_dq$data_viz[eval(as.name(input$select_variable)) %in% input$select_variable_value, observationDbId]
      rv_dq$sel_observationDbIds <- sel_observationDbIds
      bslib::toggle_sidebar(id = "sel_obs_sidebar", open = T)
    })
    
    ## output distribution plot ####
    output$distribution_viz <- renderPlotly({
      req(rv_dq$data_viz[, .N] > 0)
      req(input$trait)
      req(input$studies)
      
      if (rv_dq$data_viz[, .N, .(positionCoordinateX, positionCoordinateY)][, .N] >
          1) {
        rv_dq$width <- 6
      } else {
        rv_dq$width <- 12
      }

      data_dq <- rv_dq$data_viz
      
      g1 <- ggplot(as.data.frame(data_dq),
                   aes(y = observationValue, x = study_name_abbrev_app)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(fill = grey(0.8), alpha = 0.2, ) +
        # geom_point(
        geom_jitter(
          width = 0.05,
          height = 0,
          shape = 21,
          alpha = 0.5,
          fill = grey(0.9),
          aes(
            #fill = observationValue,
            plotNumber = plotNumber,
            blockNumber = blockNumber,
            replicate = replicate,
            positionCoordinateX = positionCoordinateX,
            positionCoordinateY = positionCoordinateY,
            entryType = entryType,
            germplasmName = germplasmName,
            stroke = ifelse(status == "default", 0.1, 1),
            color = status,
            #fill = is.excluded,
            key = observationDbId
          ),
          size = 3
        ) +
        scale_color_manual(
          values = c("default" = "black", "selected" = "red", "excluded" = "purple"),
          name = NULL,
          breaks = c("excluded"),
          labels = c("excluded" = "Excluded")
        ) +
        scale_alpha(guide = "none") + coord_flip() +
        theme_minimal() +
        ylab(input$trait) +
        theme(
          axis.text.y = if (all(data_dq[, .(is.na(positionCoordinateX) |
                                            is.na(positionCoordinateY))]))
            element_text(angle = 90)
          else
            element_blank(),
          axis.title.y = element_blank()
        )
      p <- ggplotly(
        height = length(unique(data_dq$studyName)) * 400,
        g1,
        dynamicTicks = "TRUE",
        source = "A",
        originalData = T,
        tooltip = c(
          "germplasmName",
          "observationValue",
          "key",
          "plotNumber",
          "blockNumber",
          "replicate",
          "entryType"
        )
      ) %>%
        style(hoverlabel = list(bgcolor = "white")) %>%
        layout(dragmode = "lasso", legend = list(orientation = 'h', x = 0.9, y = 0.99, title = list(text = '')))
      
      # only show legend for excluded points
      for (i in seq_along(p$x$data)) {
        if (!is.null(p$x$data[[i]]$name) && p$x$data[[i]]$name != "excluded") {
          p$x$data[[i]]$showlegend <- FALSE
        }
      }
      p
    })
    
    
    ## output layout plot ####
    output$layout_viz <- renderPlotly({
      req(rv_dq$data_viz[, .N > 0])
      req(rv_dq$data_viz[, .N, .(positionCoordinateX, positionCoordinateY)][, .N] > 1)
      req(input$studies)
      #req(all(input$studies%in%rv_dq$data_viz[,unique(studyDbId)]))
      req(input$trait)

      data_dq <- rv_dq$data_viz
  
      data_dq[, positionCoordinateX := as.numeric(positionCoordinateX)]
      data_dq[, positionCoordinateY := as.numeric(positionCoordinateY)]
      
      #plot_text <- data_dq[,.(N=.N,x=min(positionCoordinateX)+(max(positionCoordinateX)-min(positionCoordinateX))/2,y=min(positionCoordinateY)+(max(positionCoordinateY)-min(positionCoordinateY))/2),.(study_name_BMS)]
      #plot_text[,x:=1]
      #plot_text[,y:=1]
      #plot_text[N<=1,label:="No layout"]
      
      g2 <- ggplot(data_dq[!(observationDbId %in% rv$excluded_obs$observationDbId)],
                   aes(x = positionCoordinateX, y = positionCoordinateY)) +
        geom_point(# fixes shaky tile selection via plotly
          aes(fill = observationValue, key = observationDbId),
          alpha = 0) +
        geom_tile(
          aes(
            fill = observationValue,
            observationDbId = observationDbId,
            germplasmName = germplasmName,
            replicate = replicate,
            plotNumber = plotNumber,
            blockNumber = blockNumber,
            entryType = entryType
          )
        ) +
        #geom_text(data = plot_text, aes(x = x, y = y, label = label), hjust = 1) +
        #coord_equal() +
        facet_wrap(study_name_BMS ~ ., ncol = 1, scales = "free") +
        scale_fill_gradientn(name = input$trait, colours = topo.colors(100)) +
        scale_color_discrete(guide = "none") +
        scale_alpha(guide = "none") +
        # scale_linetype(guide = "none") +
        theme_minimal() +
        theme(
          panel.spacing = unit(0, "lines"),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
        )
      
      ## drawing a vertical and horizontal lines for replicates
      if (data_dq[!is.na(replicate), .N] > 0) {
        req(!all(data_dq[, .(is.na(positionCoordinateX) |
                               is.na(positionCoordinateY))]))
        repBords <- NULL
        try({
          repBords <- rbindlist(lapply(unique(data_dq$studyDbId), function(tr) {
            if (rv_dq$data_viz[studyDbId == tr, any(!is.na(positionCoordinateY))]) {
              repBord <- calcPlotBorders(as.data.frame(data_dq[studyDbId == tr, .(
                rowCoord = as.numeric(positionCoordinateY),
                colCoord = as.numeric(positionCoordinateX),
                repId = as.numeric(replicate)
              )]), bordVar = "repId")
              repBord$horW$W <- "horW"
              repBord$vertW$W <- "vertW"
              repBordBind <- rbindlist(repBord, use.names = T, fill = T)
              repBordBind[, study_name_BMS := data_dq[studyDbId == tr, unique(study_name_BMS)]]
              repBordBind
            }
          }),
          use.names = T,
          fill = T)
        })
        if (!is.null(repBords)) {
          g2 <- g2 +
            ggplot2::geom_segment(
              ggplot2::aes(
                x = x - 0.5,
                xend = x - 0.5,
                y = y - 0.5,
                yend = y + 0.5
              ),
              data = repBords[W == "vertW"],
              size = 1,
              linetype = "dashed",
              colour = grey(0.5)
            ) +
            ggplot2::geom_segment(
              ggplot2::aes(
                x = x - 0.5,
                xend = x + 0.5,
                y = y - 0.5,
                yend = y - 0.5
              ),
              data = repBords[W == "horW"],
              size = 1,
              linetype = "dashed",
              colour = grey(0.5)
            ) +
            scale_linetype(guide = "none")
        }
      }
      
      ## drawing a border (4 segments) for each tile that is selected
      if (data_dq[status == "selected", .N] > 0) {
        g2 <- g2 +
          geom_rect(
            data = data_dq[status == "selected", ],
            aes(
              xmin = positionCoordinateX - 0.5,
              xmax = positionCoordinateX + 0.5,
              ymin = positionCoordinateY - 0.5,
              ymax = positionCoordinateY + 0.5
            ),
            fill = NA,
            color = "red",
            size = 1,
            alpha = 1
          )
      }
      
      ## drawing a border (4 segments) for each tile that is excluded
      if (data_dq[status == "excluded", .N] > 0) {
        g2 <- g2 +
          geom_rect(
            data = data_dq[status == "excluded", ],
            aes(
              xmin = positionCoordinateX - 0.5,
              xmax = positionCoordinateX + 0.5,
              ymin = positionCoordinateY - 0.5,
              ymax = positionCoordinateY + 0.5
            ),
            fill = NA,
            color = "purple",
            size = 1,
            alpha = 1
          )
      }
      
      ## extract legend
      rv_dq$layout_legend <- get_legend(g2)
      g2 <- g2 + theme(legend.position = "none")
      
      ggplotly(
        height = length(unique(data_dq$studyName)) * 400,
        g2,
        dynamicTicks = "TRUE",
        source = "A",
        originalData = T,
        tooltip = c(
          "germplasmName",
          "observationValue",
          "key",
          "plotNumber",
          "blockNumber",
          "replicate",
          "positionCoordinateX",
          "positionCoordinateY",
          "entryType"
        )
      ) %>%
        style(hoverlabel = list(bgcolor = "white")) %>%
        layout(dragmode = "lasso")
    })
    
    ## output layout_legend ####
    output$layout_legend <- renderPlot({
      req(rv_dq$data_viz[, .N] > 0)
      req(rv_dq$data_viz[, .N, .(positionCoordinateX, positionCoordinateY)][, .N] >
            1)
      req(input$studies)
      req(input$trait)
      req(rv_dq$layout_legend)
      ggpubr::as_ggplot(rv_dq$layout_legend)
    })
    
    ## observe plotly_click ####
    observeEvent(c(
      event_data("plotly_click", source = "A"),
      event_data("plotly_selected", source = "A")
    ), {
      selection <- rbindlist(list(
        event_data("plotly_click", source = "A"),
        event_data("plotly_selected", source = "A")
      ),
      use.names = T,
      fill = T)
      
      if ("key" %in% names(selection)) {
        sel_observationDbIds <- unique(selection[!is.na(key), key])
        if (is.list(sel_observationDbIds)) {
          # in case an aggregated plot shape is selected like a boxplot
          sel_observationDbIds <- c()
        }
      } else{
        sel_observationDbIds <- c()
      }
      
      output$debug <- renderPrint({
        list(selection,
             sel_observationDbIds,
             length(sel_observationDbIds))
      })
      if (length(sel_observationDbIds) > 0) {
        updatePickerInput(
          session = session,
          inputId = "select_variable_value",
          options = list(
            title = 'Select a value',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        bslib::toggle_sidebar(id = "sel_obs_sidebar", open = T)
      }
      rv_dq$sel_observationDbIds <- sel_observationDbIds
    })
    
    ## observe rv_dq$sel_observationDbIds ####
    observeEvent(rv_dq$sel_observationDbIds, {
      req(rv_dq$data_viz)
      newdt <- copy(rv_dq$data_viz)
      newdt[!observationDbId %in% rv$excluded_obs$observationDbId, status := "default"]
      if (length(rv_dq$sel_observationDbIds) > 0) {
        
        newdt[
          observationDbId %in% rv_dq$sel_observationDbIds & 
            !(observationDbId %in% rv$excluded_obs$observationDbId), 
          status := "selected"
        ]
        
        updateActionButton(
          inputId = "unselect_obs",
          disabled = F
        )
        
      } else {
        bslib::toggle_sidebar(id = "sel_obs_sidebar", open = F)
      }
      rv_dq$data_viz <- newdt
    }, ignoreNULL = F)
    
    ## observe rv$excluded_obs ####
    observeEvent(rv$excluded_obs, {
      req(rv_dq$data)
      newdt <- copy(rv_dq$data)
      newdt[status == "excluded", status := "default"]
      newdt[observationDbId %in% rv_dq$sel_observationDbIds, status := "selected"]
      if (nrow(rv$excluded_obs) > 0) {
        newdt[observationDbId %in% rv$excluded_obs$observationDbId, status := "excluded"]
      }
      rv_dq$data <- newdt
      save_user_data(rv)
    }, ignoreNULL = F)
    
    ## output correlation plot ####
    output$correlationPlot <- renderPlotly({
      data_dq_casted <- dcast(
        rv_dq$data_viz[!(observationDbId %in% rv$excluded_obs$observationDbId) &
                         studyDbId %in% input$studies],
        germplasmDbId + studyDbId + locationDbId + study_name_app + germplasmName +
          replicate + observationUnitDbId + positionCoordinateY +
          positionCoordinateX ~ observationVariableName,
        value.var = "observationValue"
      )
      req(data_dq_casted)
      
      TD <- createTD(
        data = data_dq_casted,
        genotype = "germplasmName",
        trial = "study_name_app",
        loc = "locationDbId",
        repId = "replicate",
        subBlock = "observationUnitDbId",
        rowCoord = "positionCoordinateY",
        colCoord = "positionCoordinateX"
      )
      p <- plot(
        TD,
        plotType = "cor",
        traits = input$trait,
        output = F,
        trials = unique(data_dq_casted$study_name_app)
      )
      ggplotly(p[[input$trait]], source = "B")
    })
    
    observeEvent(rv_dq$data_viz, {
      data_dq <- rv_dq$data_viz
      data_dq_notexcl <- rv_dq$data_viz[!(observationDbId %in% rv$excluded_obs$observationDbId)]
      
      sumtable_all <- data_dq[, .("No. of values" = .N), study_name_app]
      
      dataType = unique(data_dq_notexcl$scale.dataType)
      
      if (dataType == "Date") {
        sumtable_notexcl <- data_dq_notexcl[, .(
          "Environment" = study_name_app,
          "No. of observations" = .N,
          "Mean" = mean(observationValue, na.rm = T),
          "Minimum" = min(observationValue, na.rm = T),
          "Quantile 0.25" = quantile(
            observationValue,
            probs = c(0.25),
            type = 1
          ),
          "Median" = quantile(
            observationValue,
            probs = c(0.5),
            type = 1
          ),
          "Quantile 0.75" = quantile(
            observationValue,
            probs = c(0.75),
            type = 1
          ),
          "Maximum" = max(observationValue, na.rm = T)
        ), study_name_app]
        
        columns <- c(
          "Environment",
          "No. of values",
          "No. of observations",
          "No. of excluded values",
          "Mean",
          "Minimum",
          "Quantile 0.25",
          "Median",
          "Quantile 0.75",
          "Maximum"
        )
      } else {
        sumtable_notexcl <- data_dq_notexcl[, .(
          "Environment" = study_name_app,
          "No. of observations" = .N,
          "Mean" = mean(observationValue, na.rm = T),
          "Minimum" = min(observationValue, na.rm = T),
          "Quantile 0.25" = ifelse(
            dataType == "Date",
            quantile(
              observationValue,
              probs = c(0.25),
              type = 1
            ),
            quantile(observationValue, probs = c(0.25))
          ),
          "Median" = ifelse(
            dataType == "Date",
            quantile(
              observationValue,
              probs = c(0.5),
              type = 1
            ),
            quantile(observationValue, probs = c(0.5))
          ),
          "Quantile 0.75" = ifelse(
            dataType == "Date",
            quantile(
              observationValue,
              probs = c(0.75),
              type = 1
            ),
            quantile(observationValue, probs = c(0.75))
          ),
          "Maximum" = max(observationValue, na.rm = T),
          "Standard deviation" = sd(observationValue, na.rm = T),
          "Variance" = var(observationValue, na.rm = T),
          "Sum of values" = sum(observationValue, na.rm = T),
          "Sum of squares" = sum((
            observationValue - mean(observationValue, na.rm = T)
          ) ^ 2),
          "Uncorrected sum of squares" = sum(observationValue ^ 2, na.rm = T),
          "Skewness" = e1071::skewness(observationValue),
          "Kurtosis" = e1071::kurtosis(observationValue)
        ), study_name_app]
        
        sumtable_notexcl[, "Standard error of mean" := `Standard deviation` /
                           sqrt(`No. of observations`)]
        sumtable_notexcl[, "Standard error of variance" := `Variance` /
                           sqrt(`No. of observations`)]
        sumtable_notexcl[, "%cov" := `Variance` / `Mean`]
        sumtable_notexcl[, "%Standard error of skewness" := `Skewness` /
                           sqrt(`No. of observations`)]
        sumtable_notexcl[, "%Standard error of kurtosis" := `Kurtosis` /
                           sqrt(`No. of observations`)]
        sumtable_notexcl[, "Range" := Maximum - Minimum]
        
        columns <- c(
          "Environment",
          "No. of values",
          "No. of observations",
          "No. of excluded values",
          "Mean",
          "Minimum",
          "Quantile 0.25",
          "Median",
          "Quantile 0.75",
          "Maximum",
          "Range",
          "Standard deviation",
          "Standard error of mean",
          "Variance",
          "Standard error of variance",
          "%cov",
          "Sum of values",
          "Sum of squares",
          "Uncorrected sum of squares",
          "Skewness",
          "%Standard error of skewness",
          "Kurtosis",
          "%Standard error of kurtosis"
        )
      }
      
      setkey(sumtable_all, study_name_app)
      setkey(sumtable_notexcl, study_name_app)
      sumtable <- sumtable_all[sumtable_notexcl]
      sumtable[, "No. of excluded values" := `No. of values` - `No. of observations`]
      rv_dq$sumtable <- sumtable[, columns, with = F]
      
      shinyjs::enable(id = "summary_stats_export")
    })
    
    ## output summary stats table ####
    output$sumstats_table <- renderDataTable({
      req(rv_dq$sumtable)
      
      decimals_cols <- colnames(rv_dq$sumtable)[sapply(rv_dq$sumtable,
        function(c) {
          is.numeric(c) && any(c != as.integer(c))
        }
      )]
      
      dt <- datatable(
        rv_dq$sumtable,
        rownames = FALSE,
        extensions = "FixedColumns",
        options = list(
          paging = F,
          scrollX = T,
          scrollY = "600px",
          scrollCollapse = T,
          dom = 't',
          #autoWidth = FALSE,
          fixedColumns = list(leftColumns = 1)
        )
      )
      formatRound(
        dt,
        columns = decimals_cols,
        digits = 3, 
        mark = ""
      )
    })
    
    ### handle download summary stats ####
    output$summary_stats_export <- downloadHandler(
      filename = function() {
        paste0(input$trait,"_summary_statistics.csv")
      },
      content = function(file) {
        write.csv(rv_dq$sumtable, file, row.names = F)
      }
    )
    
    ## output selected_obs ####
    output$selected_obs_table <- renderDT({
      validate(
        need(rv_dq$sel_observationDbIds, "Click and drag to select observations on graphs")
      )
      selected_obs <- rv_dq$data_viz[observationDbId %in% rv_dq$sel_observationDbIds &
                                       !(observationDbId %in% rv$excluded_obs$observationDbId)]
      setcolorder(selected_obs, visible_columns_selected_obs)
      
      datatable(
        selected_obs,
        extensions = c('Select', 'Buttons'),
        colnames = c("study" = "study_name_abbrev_app"),
        options = list(
          columnDefs = list(list(
            visible = FALSE,
            targets = match(names(selected_obs)[!(names(selected_obs) %in%
                                                    visible_columns_selected_obs)], names(selected_obs))
          )),
          paging = F,
          scrollX = T,
          scrollY = "300px",
          scrollCollapse = T,
          dom = 'Bt',
          buttons = c('selectAll', 'selectNone', I('colvis')),
          select = list(style = 'os', items = 'row')
        ),
        selection = 'none'
      ) %>%
        formatStyle(0, target = 'row', lineHeight = '90%')
    }, server = F)
    
    ## observe input$unselect_obs ####
    observeEvent(input$unselect_obs, {
      rv_dq$sel_observationDbIds <- c()
    })
    
    ## observe input$set_excluded_obs ####
    observeEvent(input$set_excluded_obs, {
      req(input$selected_obs_table_rows_selected)
      new_excluded_obs <- rv_dq$data_viz[observationDbId %in% rv_dq$sel_observationDbIds][input$selected_obs_table_rows_selected, .(observationDbId)]
      new_excluded_obs <- new_excluded_obs[, reason := "user choice"]
      rv_dq$sel_observationDbIds <- setdiff(rv_dq$sel_observationDbIds, new_excluded_obs) # remove the "selected" status from the new excluded observations
      #rv$excluded_obs <- union(rv$excluded_obs, new_excluded_obs)
      rv$excluded_obs <- unique(rbind(rv$excluded_obs, new_excluded_obs))
    })
    
    observeEvent(input$selected_obs_table_rows_selected, {
      if (is.null(input$selected_obs_table_rows_selected)) {
        updateActionButton(
          inputId = "set_excluded_obs",
          disabled = T
        )
      } else {
        updateActionButton(
          inputId = "set_excluded_obs",
          disabled = F
        )
      }
    }, ignoreNULL = F)
    
    ## output excluded_obs_table ####
    output$excluded_obs_table <- renderDT({
      req(rv_dq$data_viz)
      
      validate(
        need(nrow(rv$excluded_obs) > 0, "No excluded observations")
      )

      selected_excl_obs <- rv_dq$data_viz[observationDbId %in% rv$excluded_obs$observationDbId]
      setcolorder(selected_excl_obs, visible_columns_selected_obs)
      datatable(
        selected_excl_obs,
        colnames = c("study" = "study_name_abbrev_app"),
        extensions = c('Select', 'Buttons'),
        options = list(
          columnDefs = list(list(
            visible = FALSE,
            targets = match(names(selected_excl_obs)[!(names(selected_excl_obs) %in%
                                                         visible_columns_selected_obs)], names(selected_excl_obs))
          )),
          paging = F,
          scrollX = T,
          scrollY = "300px",
          scrollCollapse = T,
          dom = 'Bt',
          buttons = c('selectAll', 'selectNone', I('colvis')),
          select = list(style = 'os', items = 'row')
        )
      ) %>%
        formatStyle(0, target = 'row', lineHeight = '90%')
    }, server = F)
    
    observeEvent(input$envXtrait, {
      showModal(
        modalDialog(
          title = "Visualisation",
          easyClose = TRUE,
          footer = NULL,
          fade = F,
          size = "l",
          plotOutput(ns("envXtraitViz"), width = "700px", height = "500px")
        )
      )
    })
    
    ## envXtrait plot ####
    output$envXtraitViz <- renderPlot({
      req(rv$data)
      ggplot(rv$data[, .N, .(study_name_app,
                             observationVariableName,
                             observationLevel)],
             aes(
               y = study_name_app,
               x = observationVariableName,
               fill = N
             )) +
        geom_tile() +
        facet_wrap(vars(observationLevel),
                   nrow = 1,
                   drop = T,
        ) +
        # facet_grid(cols = vars(observationLevel), drop = T) +
        scale_fill_continuous(name = "Number of\nobservations") +
        coord_equal() +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
          )
        )
    })
  })
}
