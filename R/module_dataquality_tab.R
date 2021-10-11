#' @export
mod_dataquality_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        pickerInput(
          ns("trait"), label = "Trait", choices = NULL, width = "100%",
          options = list(
            title = 'Load Environments First',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        9,
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
        )
      )
    ),
    fluidRow(
      column(
        8,
        tabsetPanel(
          selected = "Distributions",
          # selected = "Summary Statistics",
          tabPanel(
            "Distributions",
            fluidRow(
              column(4,
                     pickerInput(
                       ns("select_variable"), label = "Select observations by variable value",
                       choices = NULL, multiple = F, width = "100%",
                       options = list(
                         title = 'Load Environments First',
                         onInitialize = I('function() { this.setValue(""); }')
                       )
                     )
              ),
              column(
                8,
                pickerInput(
                  ns("select_variable_value"),label = HTML("<br/>"), choices = NULL, multiple = T, width = "100%",
                  options = list(
                    title = 'Load Environments First',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                )
              )
            ),
            fluidRow(
              column(
                12,
                tags$label("Or select observations directly on the plots")
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotlyOutput(ns("distribution_viz"),height = "400px")),
              column(width = 6, plotlyOutput(ns("layout_viz")))
            )
          ),
          tabPanel(
            "Correlations",
            plotlyOutput(ns("correlationPlot")),# width = 800, height = 600),
          ),
          tabPanel(
            "Summary Statistics",
            h2("Summary Statistics"),
            dataTableOutput(ns("sumstats_table"))
          )
        )
      ),
      column(
        4,
        # verbatimTextOutput(ns("debug")),
        h2("Selected observations", class = "display_if_selection", style = "display: none"),
        dataTableOutput(ns("selected_obs_table")),
        shiny::actionButton(ns("set_excluded_obs"), "Set selected row(s) as excluded observation(s)", class = "display_if_selection", style = "display: none"),
        h2("Excluded observations", class = "display_if_exclusion", style = "display: none"),
        dataTableOutput(ns("excluded_obs_table")),
        shiny::actionButton(ns("set_non_excluded_obs"), "Set selected row(s) as non-excluded observation(s)", class = "display_if_exclusion", style = "display: none"),
      )
    )
  )
}

#' @export
mod_dataquality_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      observe({

        req(rv$data)

        updatePickerInput(
          inputId = "trait", session = session,
          choices = rv$data[,unique(observations.observationVariableName)],
          selected = rv$data[,unique(observations.observationVariableName)][1],
          options = list(
            title = 'Select a trait'
            # onInitialize = I('function() { this.setValue(""); }')
          )
        )

        env_choices <- rv$study_names[loaded==T,study_id]
        names(env_choices) <- rv$study_names[loaded==T,study_name_app]
        updatePickerInput(
          inputId = "studies", session = session,
          choices = env_choices, selected = env_choices,
          options = list(
            title = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        are_num <- rv$data[,lapply(.SD, is.numeric)]
        non_numeric_variables <- names(are_num)[are_num==F]
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

      observe({
        req(input$trait)
        req(input$studies)
        rv$data_dq <- rv$data[observations.observationVariableName == input$trait & studyDbId %in% input$studies]

      })

      observeEvent(input$select_variable,{
        req(input$select_variable)
        values <- unique(rv$data[,input$select_variable, with = F])
        if(values[,.N]==1){
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

      observeEvent(input$select_variable_value,{
        sel_observationDbIds <- rv$data[
          eval(as.name(input$select_variable)) %in% input$select_variable_value,
          observations.observationDbId
        ]
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$distribution_viz <- renderPlotly({

        req(rv$data_dq)
        req(input$studies)

        input$set_excluded_obs
        input$set_non_excluded_obs

        data_dq <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs)]

        data_dq[, is.selected:=F]
        data_dq[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]

        data_dq[,study_name_abbrev_app:=factor(study_name_abbrev_app, levels = rev(levels(factor(study_name_abbrev_app))))]

        g1 <- ggplot(data_dq, aes(
          y = observations.value,
          x = study_name_abbrev_app
        )) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(
            fill = grey(0.8), alpha = 0.2,
          ) +
          # geom_point(
          geom_jitter(
            width = 0.1,
            height = 0,
            shape = 21,
            alpha = 0.5,
            fill = grey(0.9),
            aes(
              # fill = observations.value,
              germplasmName = germplasmName,
              stroke = ifelse(is.selected,1,0.1),
              key = observations.observationDbId
            ),
            colour = "black",
            size = 4
          ) +
          scale_alpha(guide = "none") + coord_flip() +
          theme_minimal() +
          xlab(input$trait) +
          theme(#axis.text.y = element_text(angle = 60),
            axis.text.y = element_blank(),
            axis.title.y = element_blank())
        ggplotly(height=length(input$studies)*400,
                 g1,
                 dynamicTicks = "TRUE", source = "A", originalData = T,
                 tooltip = c("germplasmName", "observations.value", "key")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "select")
      })


      output$layout_viz <- renderPlotly({

        req(rv$data_dq)
        req(input$studies)

        input$set_excluded_obs
        input$set_non_excluded_obs

        data_dq <- rv$data_dq

        data_dq[,is.selected:=F]
        data_dq[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]
        data_dq[, x:=as.numeric(x)]
        data_dq[, y:=as.numeric(y)]

        g2 <- ggplot(
          data_dq[!(observations.observationDbId %in% rv$excluded_obs)],
          aes(x = x, y = y)
        ) +
          geom_point( # fixes shaky tile selection via plotly
            aes(
              fill = observations.value,
              key = observations.observationDbId
            ),
            alpha = 0
          )+
          geom_tile(
            aes(
              fill = observations.value,
              observations.observationDbId = observations.observationDbId,
              germplasmName = germplasmName,
              replicate = replicate
            )
          ) +
          #coord_equal() +
          facet_wrap(study_name_abbrev_app~., ncol = 1) +
          scale_fill_gradientn(colours = topo.colors(100)) +
          scale_color_discrete(guide = "none") +
          scale_alpha(guide = "none") +
          # scale_linetype(guide = "none") +
          theme_minimal() +
          theme(legend.position="none",panel.margin = unit(0, "lines"), panel.grid = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank())

        ## drawing a vertical and horizontal lines for replicates
        repBords <- rbindlist(lapply(input$studies, function(tr){
          repBord <- calcPlotBorders(as.data.frame(data_dq[studyDbId==tr, .(
            rowCoord = as.numeric(positionCoordinateY),
            colCoord = as.numeric(positionCoordinateX),
            repId = as.numeric(replicate))]), bordVar = "repId")
          repBord$horW$W <- "horW"
          repBord$vertW$W <- "vertW"
          repBordBind <- rbindlist(repBord, use.names = T, fill = T)
          repBordBind[,study_name_abbrev_app := data_dq[studyDbId==tr, unique(study_name_abbrev_app)]]
          repBordBind
        }), use.names = T, fill = T)
        g2 <- g2 +
          ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                    xend = "x - 0.5",
                                                    y = "y - 0.5",
                                                    yend = "y + 0.5"),
                                data = repBords[W == "vertW"], size = 1, linetype = "dashed", colour = grey(0.5)) +
          ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                    xend = "x + 0.5",
                                                    y = "y - 0.5",
                                                    yend = "y - 0.5"),
                                data = repBords[W == "horW"], size = 1, linetype = "dashed", colour = grey(0.5)) +
          scale_linetype(guide = "none")

        ## drawing a border (4 segments) for each tile that is selected
        if(data_dq[is.selected==T,.N]>0){
          g2 <- g2 +
            ggplot2::geom_segment(data = data_dq[is.selected==T],
                                  ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x - 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y + 0.5"),
                                  size = 1, alpha = 1) +
            ggplot2::geom_segment(data = data_dq[is.selected==T],
                                  ggplot2::aes_string(x = "x + 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y + 0.5"),
                                  size = 1, alpha = 1) +
            ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y - 0.5"),
                                  data = data_dq[is.selected==T], size = 1, alpha = 1) +
            ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y + 0.5",
                                                      yend = "y + 0.5"),
                                  data = data_dq[is.selected==T], size = 1, alpha = 1)
        }

        ggplotly(height=length(input$studies)*400,
                 g2,
                 dynamicTicks = "TRUE", source = "A", originalData = T,
                 tooltip = c("germplasmName", "observations.value", "replicate", "observations.observationDbId")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "select")
      })

      observeEvent(c(event_data("plotly_click", source = "A"),event_data("plotly_selected", source = "A")),{
        selection <- rbindlist(list(
          event_data("plotly_click", source = "A"),
          event_data("plotly_selected", source = "A")
        ), use.names = T, fill = T)

        if("key"%in%names(selection)){
          sel_observationDbIds <- unique(selection[!is.na(key),key])
          if(is.list(sel_observationDbIds)){ # in case an aggregated plot shape is selected like a boxplot
            sel_observationDbIds <- NULL
          }
        }else{
          sel_observationDbIds <- NULL
        }

        output$debug <- renderPrint({
          list(
            selection,
            sel_observationDbIds,
            length(sel_observationDbIds)
          )
        })
        if(length(sel_observationDbIds)>0){
          updatePickerInput(
            session = session,
            inputId = "select_variable_value",
            options = list(
              title = 'Select a value',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$correlationPlot <- renderPlotly({
        data_dq_casted <- dcast(
          rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs) & studyDbId %in% input$studies],
          germplasmDbId + studyDbId + studyLocationDbId + study_name_app + germplasmName +
            replicate + observationUnitDbId + positionCoordinateY +
            positionCoordinateX ~ observations.observationVariableName,
          value.var = "observations.value"
        )
        req(data_dq_casted)

        TD <- createTD(
          data = data_dq_casted,
          genotype = "germplasmName",
          trial = "study_name_app",
          loc = "studyLocationDbId",
          repId = "replicate",
          subBlock = "observationUnitDbId",
          rowCoord = "positionCoordinateY",
          colCoord = "positionCoordinateX"
        )
        p <- plot(TD, plotType="cor", traits = input$trait, output = F, trials = rv$study_names[study_id %in% input$studies, study_name_app])
        ggplotly(p[[input$trait]], source = "B")
      })

      ## summary statistics
      output$sumstats_table <- renderDataTable({
        req(rv$data_dq)
        data_dq <- rv$data_dq
        data_dq_notexcl <- rv$data_dq[!(observations.observationDbId %in% rv$excluded_obs)]

        sumtable_all <- data_dq[
          ,
          .(
            "No. of values" = .N
          ),
          study_name_app
        ]

        sumtable_notexcl <- data_dq_notexcl[
          ,
          .(
            "No. of observations" = .N,
            "Mean"=mean(observations.value, na.rm = T),
            "Minimum"=min(observations.value, na.rm = T),
            "Quantile 0.25"=quantile(observations.value, probs = c(0.25)),
            "Median"=quantile(observations.value, probs = c(0.5)),
            "Quantile 0.75"=quantile(observations.value, probs = c(0.75)),
            "Maximum"=max(observations.value, na.rm = T),
            "Standard deviation"=sd(observations.value, na.rm = T),
            "Variance"=var(observations.value, na.rm = T),
            "Sum of values"=sum(observations.value, na.rm = T),
            "Sum of squares"=sum((observations.value-mean(observations.value, na.rm = T))^2),
            "Uncorrected sum of squares"=sum(observations.value^2, na.rm = T),
            "Skewness"=e1071::skewness(observations.value),
            "Kurtosis"=e1071::kurtosis(observations.value)
          ),
          study_name_app
        ]

        setkey(sumtable_all,study_name_app)
        setkey(sumtable_notexcl,study_name_app)
        sumtable <- sumtable_all[sumtable_notexcl]

        sumtable[,"No. of excluded values":= `No. of values` - `No. of observations`]
        sumtable[,"Range":=Maximum - Minimum]
        sumtable[,"Standard error of mean":=`Standard deviation`/sqrt(`No. of observations`)]
        sumtable[,"Standard error of variance":=`Variance`/sqrt(`No. of observations`)]
        sumtable[,"%cov":=`Variance`/`Mean`]
        sumtable[,"%Standard error of skewness":=`Skewness`/sqrt(`No. of observations`)]
        sumtable[,"%Standard error of kurtosis":=`Kurtosis`/sqrt(`No. of observations`)]

        rownames(sumtable) <- sumtable[,study_name_app]
        sumtable[,study_name_app:=NULL]
        columns <- c(
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
        # sumtable <- t(sumtable)
        datatable(
          sumtable[,columns, with = F],
          options = list(
            paging = F,
            scrollX = T,
            scrollY = "600px",
            scrollCollapse = T,
            dom = 't'
          ))
      })


      ## selected and excluded obs (right column)

      output$selected_obs_table <- renderDT({

        shinyjs::hide(selector = ".display_if_selection")

        req(rv$sel_observationDbIds)
        req(input$studies)

        shinyjs::show(selector = ".display_if_selection")

        input$set_excluded_obs
        input$set_non_excluded_obs
        datatable(
          rv$data_dq[observations.observationDbId %in% rv$sel_observationDbIds & !(observations.observationDbId %in% rv$excluded_obs)],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$data_dq))
              )
            ),
            paging = F,
            scrollX = T,
            scrollY = "300px",
            scrollCollapse = T,
            dom = 'Bt',
            buttons = I('colvis')
          )
        ) %>%
          formatStyle(0, target= 'row', lineHeight='50%')
      })

      observeEvent(input$set_excluded_obs,{

        new_excluded_obs <- rv$data_dq[
          observations.observationDbId %in% rv$sel_observationDbIds
        ][
          input$selected_obs_table_rows_selected, observations.observationDbId
        ]

        rv$excluded_obs <- union(rv$excluded_obs, new_excluded_obs)
      })

      output$excluded_obs_table <- renderDT({
        shinyjs::hide(selector = ".display_if_exclusion")
        req(rv$data_dq)
        rv$data_dq
        input$set_excluded_obs
        input$set_non_excluded_obs
        req(rv$data_dq[observations.observationDbId %in% rv$excluded_obs,.N]>0)
        shinyjs::show(selector = ".display_if_exclusion")
        datatable(
          rv$data_dq[observations.observationDbId %in% rv$excluded_obs],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$data_dq))
              )
            ),
            paging = F,
            scrollX = T,
            scrollY = "300px",
            scrollCollapse = T,
            dom = 'Bt',
            buttons = I('colvis')
          )
        ) %>%
          formatStyle(0, target= 'row', lineHeight='50%')
      })

      observeEvent(input$set_non_excluded_obs,{
        non_excluded_obs <- rv$data_dq[observations.observationDbId %in% rv$excluded_obs][
          input$excluded_obs_table_rows_selected, observations.observationDbId]

        rv$excluded_obs <- setdiff(rv$excluded_obs, non_excluded_obs)
      })

      return(rv)
    }
  )
}
