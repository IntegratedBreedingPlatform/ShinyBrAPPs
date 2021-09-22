#' @export
mod_dataquality_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        selectizeInput(
          ns("trait"), label = "Trait", choices = NULL, width = "100%",
          options = list(
            placeholder = 'Select a dataset first',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        9,
        selectizeInput(
          ns("studies"), label = "Environments", choices = NULL, width = "100%",
          multiple = T,
          options = list(
            placeholder = 'Select a dataset first',
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
                     selectizeInput(ns("select_variable"), label = "Select observations by variable value",
                                    choices = NULL, multiple = F, width = "100%",
                                    options = list(
                                      placeholder = 'Select a dataset first',
                                      onInitialize = I('function() { this.setValue(""); }')
                                    ))
              ),
              column(8,
                     selectizeInput(ns("select_variable_value"),label = HTML("<br/>"), choices = NULL, multiple = T, width = "100%", options = list(
                       placeholder = 'Select a dataset first',
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
              )
            ),
            fluidRow(
              column(12,
                     tags$label("Or select observations directly on the plots")
              )
            ),
            fluidRow(
              column(width = 6,
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
        actionButton(ns("set_excluded_obs"), "Set selected row(s) as excluded observation(s)", class = "display_if_selection", style = "display: none"),
        h2("Excluded observations", class = "display_if_exclusion", style = "display: none"),
        dataTableOutput(ns("excluded_obs_table")),
        actionButton(ns("set_non_excluded_obs"), "Set selected row(s) as non-excluded observation(s)", class = "display_if_exclusion", style = "display: none"),
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

        req(rv$TD)

        updateSelectizeInput(
          inputId = "trait", session = session,
          choices = rv$TD$all[,unique(observations.observationVariableName)],
          options = list(
            placeholder = 'Select a trait'
            # onInitialize = I('function() { this.setValue(""); }')
          )
        )

        studyDbIds <- names(rv$TD)[names(rv$TD) != "all"]
        studyNames <- unlist(lapply(studyDbIds, function(x){
          rv$TD[[x]][,unique(study_name_app)]
        }))
        names(studyDbIds) <- studyNames
        updateSelectizeInput(
          inputId = "studies", session = session, choices = studyDbIds,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        are_num <- rv$TD$all[,lapply(.SD, is.numeric)]
        non_numeric_variables <- names(are_num)[are_num==F]
        updateSelectizeInput(
          session = session,
          inputId = "select_variable",
          choices = non_numeric_variables,
          options = list(
            placeholder = 'Select a variable',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        updateSelectizeInput(
          session = session,
          inputId = "select_variable_value",
          options = list(
            placeholder = 'Select a value',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })

      observe({
        req(input$trait)
        req(input$studies)
        TD <- rv$TD[c(input$studies, "all")]
        TD$all <- TD$all[trials%in%input$studies]
        for(i in c(input$studies, "all")){
          TD[[i]] <- TD[[i]][observations.observationVariableName == input$trait]
        }
        rv$TD_dq <- TD
        
      })

      observeEvent(input$select_variable,{
        req(input$select_variable)
        values <- unique(rv$TD$all[,input$select_variable, with = F])
        if(values[,.N]==1){
          values <- unname(values)
        }
        updateSelectizeInput(
          session = session,
          inputId = "select_variable_value",
          # label = paste(input$select_variable, "values"),
          choices = values,
          options = list(
            placeholder = 'Select a value',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })

      observeEvent(input$select_variable_value,{
        sel_observationDbIds <- rv$TD$all[
          eval(as.name(input$select_variable)) %in% input$select_variable_value,
          observations.observationDbId
        ]
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$distribution_viz <- renderPlotly({

        req(rv$TD_dq)
        req(input$studies)

        input$set_excluded_obs
        input$set_non_excluded_obs

        td <- rv$TD_dq$all[!(observations.observationDbId %in% rv$excluded_obs)]

        td[, is.selected:=F]
        td[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]

        td[,study_name_abbrev_app:=factor(study_name_abbrev_app, levels = rev(levels(factor(study_name_abbrev_app))))]

        g1 <- ggplot(td, aes(
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
          theme(#axis.text.y = element_text(angle = 60),
            axis.text.y = element_blank(),
            axis.title = element_blank())
        ggplotly(height=length(input$studies)*400,
          g1,
          dynamicTicks = "TRUE", source = "A", originalData = T,
          tooltip = c("germplasmName", "observations.value", "key")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "select")
      })


      output$layout_viz <- renderPlotly({

        req(rv$TD_dq)
        req(input$studies)

        input$set_excluded_obs
        input$set_non_excluded_obs

        td <- rv$TD_dq$all[!(observations.observationDbId %in% rv$excluded_obs)]

        td[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]
        td[, x:=as.numeric(x)]
        td[, y:=as.numeric(y)]

        g2 <- ggplot(
          td[!(observations.observationDbId %in% rv$excluded_obs)],
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
              replicate = repId
            )
          ) +
          #coord_equal() +
          facet_wrap(study_name_abbrev_app~., ncol = 1) +
          scale_fill_gradientn(colours = myPalette(100)) +
          scale_color_discrete(guide = "none") +
          scale_alpha(guide = "none") +
          # scale_linetype(guide = "none") +
          theme_minimal() +
          theme(legend.position="none",panel.margin = unit(0, "lines"), panel.grid = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank())

        ## drawing a vertical and horizontal lines for replicates
        repBords <- rbindlist(lapply(names(rv$TD)[names(rv$TD)%in%input$studies], function(tr){
          repBord <- calcPlotBorders(as.data.frame(rv$TD[[tr]][!(observations.observationDbId %in% rv$excluded_obs)]), bordVar = "repId")
          repBord$horW$W <- "horW"
          repBord$vertW$W <- "vertW"
          repBordBind <- rbindlist(repBord, use.names = T, fill = T)
          repBordBind[,study_name_abbrev_app := unique(rv$TD[[tr]][,study_name_abbrev_app])]
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
        if(td[is.selected==T,.N]>0){
          g2 <- g2 +
            ggplot2::geom_segment(data = td[is.selected==T],
                                  ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x - 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y + 0.5"),
                                  size = 1, alpha = 1) +
            ggplot2::geom_segment(data = td[is.selected==T],
                                  ggplot2::aes_string(x = "x + 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y + 0.5"),
                                  size = 1, alpha = 1) +
            ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y - 0.5",
                                                      yend = "y - 0.5"),
                                  data = td[is.selected==T], size = 1, alpha = 1) +
            ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                      xend = "x + 0.5",
                                                      y = "y + 0.5",
                                                      yend = "y + 0.5"),
                                  data = td[is.selected==T], size = 1, alpha = 1)
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
          updateSelectizeInput(
            session = session,
            inputId = "select_variable_value",
            options = list(
              placeholder = 'Select a value',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$correlationPlot <- renderPlotly({
        p <- plot(rv$TD, plotType="cor", traits = "observations.value", output = F, trials = names(rv$TD)[names(rv$TD)!="all"])
        # p <- plot(rv$TD, plotType="cor", traits = "observations.value", output = F, trials = names(rv$TD)[names(rv$TD)!="all"])
        ggplotly(p[['observations.value']], source = "C")
      })

      # summary statistics
      output$sumstats_table <- renderDataTable({
        sumtable <- data.table(Environment=unlist(lapply(rv$TD, function(x){
          if(unique(x$trial) == "all"){"all"}else{unique(x$studyName)}
        })))
        sumtable[, "No. of values":=unlist(lapply(rv$TD, function(x){x[,.N]}))]
        sumtable[, "No. of observations":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),.N]}))]
        sumtable[, "No. of excluded values":= `No. of values` - `No. of observations`]
        sumtable[, "Mean":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),mean(observations.value, na.rm = T)]}))]
        sumtable[, "Minimum":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),min(observations.value, na.rm = T)]}))]
        sumtable[, "Quantile 0.25":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),quantile(observations.value, probs = c(0.25))]}))]
        sumtable[, "Median":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),quantile(observations.value, probs = c(0.5))]}))]
        sumtable[, "Quantile 0.75":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),quantile(observations.value, probs = c(0.75))]}))]
        sumtable[, "Maximum":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),max(observations.value, na.rm = T)]}))]
        sumtable[, "Range":=Maximum - Minimum]
        sumtable[, "Standard deviation":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),sd(observations.value, na.rm = T)]}))]
        sumtable[, "Standard error of mean":=`Standard deviation`/sqrt(`No. of observations`)]
        sumtable[, "Variance":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),var(observations.value, na.rm = T)]}))]
        sumtable[, "Standard error of variance":=`Variance`/sqrt(`No. of observations`)]
        sumtable[, "%cov":=`Variance`/`Mean`]
        sumtable[, "Sum of values":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),sum(observations.value, na.rm = T)]}))]
        sumtable[, "Sum of squares":=unlist(lapply(rv$TD, function(x){
          m <- x[!(observations.observationDbId %in% rv$excluded_obs),mean(observations.value, na.rm = T)]
          sum(x[!(observations.observationDbId %in% rv$excluded_obs),(observations.value-m)^2])
        }))]
        sumtable[, "Uncorrected sum of squares":=unlist(lapply(rv$TD, function(x){x[!(observations.observationDbId %in% rv$excluded_obs),sum(observations.value^2, na.rm = T)]}))]
        sumtable[, "Skewness":=unlist(lapply(rv$TD, function(x){e1071::skewness(x[!(observations.observationDbId %in% rv$excluded_obs),observations.value])}))]
        sumtable[, "%Standard error of skewness":=`Skewness`/sqrt(`No. of observations`)]
        sumtable[, "Kurtosis":=unlist(lapply(rv$TD, function(x){e1071::kurtosis(x[!(observations.observationDbId %in% rv$excluded_obs),observations.value])}))]
        sumtable[, "%Standard error of kurtosis":=`Kurtosis`/sqrt(`No. of observations`)]
        rownames(sumtable) <- sumtable[,Environment]
        sumtable[,Environment:=NULL]
        # sumtable <- t(sumtable)
        datatable(
          sumtable,
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
          rv$TD_dq$all[observations.observationDbId %in% rv$sel_observationDbIds & !(observations.observationDbId %in% rv$excluded_obs)],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$TD_dq$all))
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

        new_excluded_obs <- rv$TD_dq$all[
          observations.observationDbId %in% rv$sel_observationDbIds
        ][
          input$selected_obs_table_rows_selected, observations.observationDbId
        ]

        rv$excluded_obs <- union(rv$excluded_obs, new_excluded_obs)
      })

      output$excluded_obs_table <- renderDT({
        shinyjs::hide(selector = ".display_if_exclusion")
        req(rv$TD_dq)
        rv$TD_dq
        input$set_excluded_obs
        input$set_non_excluded_obs
        req(rv$TD_dq$all[observations.observationDbId %in% rv$excluded_obs,.N]>0)
        shinyjs::show(selector = ".display_if_exclusion")
        datatable(
          rv$TD_dq$all[observations.observationDbId %in% rv$excluded_obs],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$TD_dq$all))
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
        non_excluded_obs <- rv$TD_dq$all[observations.observationDbId %in% rv$excluded_obs][
          input$excluded_obs_table_rows_selected, observations.observationDbId]

        rv$excluded_obs <- setdiff(rv$excluded_obs, non_excluded_obs)
      })

     return(rv)
    }
  )
}
