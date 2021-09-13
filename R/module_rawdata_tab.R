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
            placeholder = '',
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
            placeholder = '',
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
              column(width = 6,
                     plotlyOutput(ns("distribution_viz"))),
              column(width = 6, plotlyOutput(ns("layout_viz")))
            )
          ),
          tabPanel(
            "Correlations",
            plotlyOutput(ns("rawdata_cor")),# width = 800, height = 600),
          ),
          tabPanel(
            "Summary Statistics",
            h2("Summary Statistics"),
            dataTableOutput(ns("rawdata_sumstats"))
          )
        )
      ),
      column(
        4,
        # verbatimTextOutput(ns("debug")),
        h2("Select observations"),
        selectizeInput(ns("select_variable"), label = "Select observations by",
                       choices = NULL, multiple = F, width = "50%",
                       options = list(
                         placeholder = 'Select a dataset first',
                         onInitialize = I('function() { this.setValue(""); }')
                       )),
        selectizeInput(ns("select_variable_value"),label = "", choices = NULL, multiple = T, width = "100%", options = list(
          placeholder = 'Select a variable first',
          onInitialize = I('function() { this.setValue(""); }')
        )),
        dataTableOutput(ns("selected_obs_table")),
        actionButton(ns("set_excluded_obs"), "Set selected row(s) as excluded observation(s)"),
        tags$hr(),
        h2("Exclude observations"),
        dataTableOutput(ns("excluded_obs_table")),
        actionButton(ns("set_non_excluded_obs"), "Set selected row(s) as non-excluded observation(s)"),
      )
    )
  )
}

#' @export
mod_dataquality_server <- function(id, d){
  moduleServer(
    id,
    function(input, output, session){

      rv <- reactiveValues()
      observe({

        d()

        updateSelectizeInput(
          inputId = "trait", session = session,
          choices = d()$all[,unique(observations.observationVariableName)],
          options = list(
            placeholder = 'Select a trait'
            # onInitialize = I('function() { this.setValue(""); }')
          )
        )

        studyDbIds <- names(d())[names(d()) != "all"]
        studyNames <- unlist(lapply(studyDbIds, function(x){
          d()[[x]][,unique(studyName)]
        }))
        names(studyDbIds) <- studyNames
        updateSelectizeInput(
          inputId = "studies", session = session, choices = studyDbIds,
          options = list(
            placeholder = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )

        are_num <- d()$all[,lapply(.SD, is.numeric)]
        non_numeric_variables <- names(are_num)[are_num==F]
        updateSelectizeInput(
          session = session,
          inputId = "select_variable",
          choices = non_numeric_variables,
          options = list(
            placeholder = 'Select a variable below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      })

      observe({
        req(input$trait)
        req(input$studies)
        TD <- d()[c(input$studies, "all")]
        TD$all <- TD$all[trials%in%input$studies]
        for(i in c(input$studies, "all")){
          TD[[i]] <- TD[[i]][observations.observationVariableName == input$trait]
        }
        rv$TD <- TD
      })

      observeEvent(input$select_variable,{
        req(input$select_variable)
        values <- unique(d()$all[,input$select_variable, with = F])
        if(values[,.N]==1){
          values <- unname(values)
        }
        updateSelectizeInput(
          session = session,
          inputId = "select_variable_value",
          label = paste(input$select_variable, "values"),
          choices = values
        )
      })

      observeEvent(input$select_variable_value,{
        sel_observationDbIds <- d()$all[
          eval(as.name(input$select_variable)) %in% input$select_variable_value,
          observations.observationDbId
        ]
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$distribution_viz <- renderPlotly({

        req(rv$TD)

        input$set_excluded_obs
        input$set_non_excluded_obs

        td <- rv$TD$all[is.excluded==F]

        td[, is.selected:=F]
        td[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]


        g1 <- ggplot(td, aes(
          y = observations.value,
          x = trials
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
          theme_minimal()
        ggplotly(
          g1,
          dynamicTicks = "TRUE", source = "A", originalData = T,
          tooltip = c("germplasmName", "observations.value", "key")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "select")
      })


      output$layout_viz <- renderPlotly({

        req(rv$TD)

        input$set_excluded_obs
        input$set_non_excluded_obs

        td <- rv$TD$all[is.excluded==F]

        td[observations.observationDbId %in% rv$sel_observationDbIds, is.selected:=T]
        td[, x:=as.numeric(x)]
        td[, y:=as.numeric(y)]

        myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

        g2 <- ggplot(
          td[is.excluded==F],
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
          coord_equal() +
          facet_wrap(trials~., ncol = 1) +
          scale_fill_gradientn(colours = myPalette(100)) +
          scale_color_discrete(guide = "none") +
          scale_alpha(guide = "none") +
          # scale_linetype(guide = "none") +
          theme_minimal() +
          theme(legend.position="top", panel.grid = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank())

        ## drawing a vertical and horizontal lines for replicates
        repBords <- rbindlist(lapply(names(d())[names(d())!="all"], function(tr){
          repBord <- calcPlotBorders(as.data.frame(d()[[tr]][observations.observationDbId %in% td[is.excluded == F & trials == tr, observations.observationDbId]]), bordVar = "repId")
          repBord$horW$W <- "horW"
          repBord$vertW$W <- "vertW"
          repBordBind <- rbindlist(repBord, use.names = T, fill = T)
          repBordBind[,trials := tr]
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

        ggplotly(
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
              placeholder = 'Select values below',
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        }
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$rawdata_cor <- renderPlotly({
        p <- plot(d(), plotType="cor", traits = "observations.value", output = F, trials = names(d())[names(d())!="all"])
        # p <- plot(d(), plotType="cor", traits = "observations.value", output = F, trials = names(d())[names(d())!="all"])
        ggplotly(p[['observations.value']], source = "C")
      })

      # summary statistics
      output$rawdata_sumstats <- renderDataTable({
        sumtable <- data.table(Environment=unlist(lapply(d(), function(x){
          if(unique(x$trial) == "all"){"all"}else{unique(x$studyName)}
        })))
        sumtable[, "No. of values":=unlist(lapply(d(), function(x){x[,.N]}))]
        sumtable[, "No. of observations":=unlist(lapply(d(), function(x){x[is.excluded==F,.N]}))]
        sumtable[, "No. of excluded values":= `No. of values` - `No. of observations`]
        sumtable[, "Mean":=unlist(lapply(d(), function(x){x[is.excluded==F,mean(observations.value, na.rm = T)]}))]
        sumtable[, "Minimum":=unlist(lapply(d(), function(x){x[is.excluded==F,min(observations.value, na.rm = T)]}))]
        sumtable[, "Quantile 0.25":=unlist(lapply(d(), function(x){x[is.excluded==F,quantile(observations.value, probs = c(0.25))]}))]
        sumtable[, "Median":=unlist(lapply(d(), function(x){x[is.excluded==F,quantile(observations.value, probs = c(0.5))]}))]
        sumtable[, "Quantile 0.75":=unlist(lapply(d(), function(x){x[is.excluded==F,quantile(observations.value, probs = c(0.75))]}))]
        sumtable[, "Maximum":=unlist(lapply(d(), function(x){x[is.excluded==F,max(observations.value, na.rm = T)]}))]
        sumtable[, "Range":=Maximum - Minimum]
        sumtable[, "Standard deviation":=unlist(lapply(d(), function(x){x[is.excluded==F,sd(observations.value, na.rm = T)]}))]
        sumtable[, "Standard error of mean":=`Standard deviation`/sqrt(`No. of observations`)]
        sumtable[, "Variance":=unlist(lapply(d(), function(x){x[is.excluded==F,var(observations.value, na.rm = T)]}))]
        sumtable[, "Standard error of variance":=`Variance`/sqrt(`No. of observations`)]
        sumtable[, "%cov":=`Variance`/`Mean`]
        sumtable[, "Sum of values":=unlist(lapply(d(), function(x){x[is.excluded==F,sum(observations.value, na.rm = T)]}))]
        sumtable[, "Sum of squares":=unlist(lapply(d(), function(x){
          m <- x[is.excluded==F,mean(observations.value, na.rm = T)]
          sum(x[is.excluded==F,(observations.value-m)^2])
        }))]
        sumtable[, "Uncorrected sum of squares":=unlist(lapply(d(), function(x){x[is.excluded==F,sum(observations.value^2, na.rm = T)]}))]
        sumtable[, "Skewness":=unlist(lapply(d(), function(x){e1071::skewness(x[is.excluded==F,observations.value])}))]
        sumtable[, "%Standard error of skewness":=`Skewness`/sqrt(`No. of observations`)]
        sumtable[, "Kurtosis":=unlist(lapply(d(), function(x){e1071::kurtosis(x[is.excluded==F,observations.value])}))]
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

        req(rv$sel_observationDbIds)

        input$set_excluded_obs
        input$set_non_excluded_obs
        datatable(
          rv$TD$all[observations.observationDbId %in% rv$sel_observationDbIds & is.excluded ==F],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$TD$all))
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
      })

      observeEvent(input$set_excluded_obs,{
        td <- rv$TD$all
        excluded_obs <- td[
          observations.observationDbId %in% rv$sel_observationDbIds
        ][
          input$selected_obs_table_rows_selected, observations.observationDbId
        ]
        td[observations.observationDbId %in% excluded_obs, is.excluded:=T]
        rv$TD$all <- td
      })

      output$excluded_obs_table <- renderDT({
        req(rv$TD)
        rv$TD
        input$set_excluded_obs
        input$set_non_excluded_obs
        datatable(
          rv$TD$all[is.excluded==T],
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(hidden_columns_observationunits, names(rv$TD$all))
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
      })

      observeEvent(input$set_non_excluded_obs,{
        td <- rv$TD$all
        non_excluded_obs <- td[is.excluded==T][
          input$excluded_obs_table_rows_selected, observations.observationDbId]
        td[observations.observationDbId %in% non_excluded_obs, is.excluded:=F]
        rv$TD$all <- td
      })

      return(rv)
    }
  )
}
