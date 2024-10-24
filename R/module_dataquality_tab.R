#' @export
mod_dataquality_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(
        ".nav .nav-item .nav-link { font-size: 20px; }
        ",
      )
    ),
    bslib::layout_columns(
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
        ns("trait"), label = "Trait", choices = NULL, width = "100%",
        options = list(
          title = 'Load Environments First',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      shiny::actionLink(ns("envXtrait"), label = "Env x Trait", width = "100%", icon = icon("info"), class = "btn btn-info"),
      bsModal(ns("envXtraitModal"), title = "Environment x Trait", trigger = ns("envXtrait"), size = "large", plotOutput(ns("envXtraitViz")))
    ),
    bslib::navset_tab(
      selected = "Distributions",
      bslib::nav_panel(
        title = "Distributions",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = ns("sel_obs_sidebar"),
            position = "right",
            width = "30%",
            open = F,
            h3("Selected observations"),
            dataTableOutput(ns("selected_obs_table")),
            bslib::layout_columns(
              col_widths = c(8,4),
              shiny::actionButton(ns("set_excluded_obs"), "Set selected row(s) as excluded observation(s)", class = "btn btn-info display_if_selection", style = "width: auto; display: none"),
              shiny::actionButton(ns("unselect_obs"), "Reset selection", class = "btn btn-info display_if_selection", style = "width: auto; display: none ")
            ),
            h3("Excluded observations"),
            dataTableOutput(ns("excluded_obs_table")),
            shiny::actionButton(ns("set_non_excluded_obs"), "Set selected row(s) as non-excluded observation(s)", class = "btn btn-info display_if_exclusion", style = "width: auto; display: none"),
          ),
          bslib::layout_columns(
            col_widths = c(6,6),
            pickerInput(
              ns("select_variable"), label = "Select observations by variable value",
              choices = NULL, multiple = F, width = "100%",
              options = list(
                title = 'Load Environments First',
                `live-search` = TRUE,
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            pickerInput(
              ns("select_variable_value"),label = HTML("<br/>"), choices = NULL, multiple = T, width = "100%",
              options = list(
                title = 'Load Environments First',
                `live-search` = TRUE,
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          tags$label("Or select observations directly on the plots"),
          uiOutput(ns("cols"))
        )
      ),
      bslib::nav_panel(
        title = "Correlations",
        plotlyOutput(ns("correlationPlot")),# width = 800, height = 600)
      ),
      bslib::nav_panel(
        title = "Summary Statistics",
        h2("Summary Statistics"),
        dataTableOutput(ns("sumstats_table"))
      )
    )
  )
}

#' @export
mod_dataquality_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv_dq <- reactiveValues()
      rv_width <- reactiveVal(12)

      observe({
        req(rv$data)

        # if(rv$data[observationLevel!="PLOT", .N]>0){
        #   showNotification(
        #   paste0("Taking away the level(s) of observation: ",
        #         rv$data[observationLevel!="PLOT", paste(unique(observationLevel), collapse = ", ")],
        #         "\n(",rv$data[observationLevel!="PLOT", .N], " values)",
        #   "\n\n(Only the PLOT observation level is considered for STA)"
        #   ), type = "default", duration = notification_duration)
        # }

        rv$data_dq <- rv$data
        if(!("observationVariableName"%in%names(rv$data_dq))){
          showNotification("No trait data", type = "error", duration = notification_duration)
        }
        req("observationVariableName"%in%names(rv$data_dq))
       
        env_choices <- rv$data_dq[!is.na(observationValue)][,unique(studyDbId)]
        names(env_choices) <- rv$data_dq[!is.na(observationValue)][,unique(study_name_app)]
        #names(env_choices) <- rv$study_metadata[loaded==T,unique(study_name_app)]
        updatePickerInput(
          inputId = "studies", session = session,
          choices = env_choices, selected = env_choices,
          options = list(
            title = 'Select 1 or more environments',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
        # trait_choices <- unique(rv$data_dq$observationVariableName)
        # updatePickerInput(
        #   inputId = "trait", session = session,
        #   choices = trait_choices,
        #   selected = trait_choices[1],
        #   options = list(
        #     title = 'Select a trait'
        #     # onInitialize = I('function() { this.setValue(""); }')
        #   )
        # )

        are_num <- rv$data_dq[,lapply(.SD, is.numeric)]
        non_numeric_variables <- names(are_num)[are_num==F]
        non_numeric_variables <- non_numeric_variables[!non_numeric_variables%in%hidden_col_names]
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
      
      output$cols <- renderUI({
        ns <- NS(id)
        tagList(
          bslib::layout_columns(
            col_widths = c(rv_width(), 5, 1),
            plotlyOutput(ns("distribution_viz")),#,height = "400px")
            plotlyOutput(ns("layout_viz")),
            plotOutput(ns("layout_legend"))
          )
        )
      })

      observeEvent(input$studies,{
        req(rv$data)

        choices_traits <- unique(rv$data[studyDbId %in% input$studies]$observationVariableName)
        if (input$trait%in%choices_traits) {
          selected_trait <- input$trait
          loadData()
        } else {
          selected_trait <- choices_traits[1]
        }
        
        updatePickerInput(
          session, "trait",
          choices = choices_traits,
          selected = selected_trait,
          options = list(
            placeholder = 'Select 1 or more traits',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )        
      })
      
      observeEvent(input$trait,{
        req(rv$data)
        req(input$trait != "")
        loadData()
      })
      
      #'Filter data on selected trait and studies and convert values in numeric or dates
      loadData <- function() { 
        rv$data_dq_viz <- rv$data[observationVariableName == input$trait & studyDbId %in% input$studies]
        if (length(unique(rv$data_dq_viz$scale.dataType)) && unique(rv$data_dq_viz$scale.dataType) == "Numerical") {
          rv$data_dq_viz[, observationValue:=as.numeric(observationValue)]
        } else if (length(unique(rv$data_dq_viz$scale.dataType)) && unique(rv$data_dq_viz$scale.dataType) == "Date") {
          rv$data_dq_viz[, observationValue:=as.Date(observationValue)]
        }
      }

      observeEvent(input$select_variable,{
        req(input$select_variable)
        values <- unique(rv$data_dq_viz[,input$select_variable, with = F])
        
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
        sel_observationDbIds <- rv$data_dq_viz[
          eval(as.name(input$select_variable)) %in% input$select_variable_value,
          observationDbId
        ]
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$distribution_viz <- renderPlotly({
        req(rv$data_dq_viz[,.N]>0)
        req(input$trait)
        req(input$studies)
        
        if (rv$data_dq_viz[,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1) {
          rv_width(6)
        } else {
          rv_width(12)
        }

        input$set_excluded_obs
        input$set_non_excluded_obs

        data_dq <- rv$data_dq_viz[!(observationDbId %in% rv$excluded_obs)]

        data_dq[, is.selected:=F]
        data_dq[observationDbId %in% rv$sel_observationDbIds, is.selected:=T]

        data_dq[,study_name_abbrev_app:=factor(study_name_abbrev_app, levels = rev(levels(factor(study_name_abbrev_app))))]

        g1 <- ggplot(data_dq, aes(
          y = observationValue,
          x = study_name_abbrev_app
        )) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(
            fill = grey(0.8), alpha = 0.2,
          ) +
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
              stroke = ifelse(is.selected,1,0.1),
              color = is.selected,
              key = observationDbId
            ),
            size = 3
          ) +
          scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
          scale_alpha(guide = "none") + coord_flip() +
          theme_minimal() +
          ylab(input$trait) +
          theme(
            legend.position = "none",
            axis.text.y = if(all(data_dq[,.(is.na(positionCoordinateX) | is.na(positionCoordinateY))])) element_text(angle = 90) else element_blank(),
            axis.title.y = element_blank()
          )
        ggplotly(height=length(unique(data_dq$studyName))*400,
                 g1,
                 dynamicTicks = "TRUE", source = "A", originalData = T,
                 tooltip = c("germplasmName", "observationValue", "key", "plotNumber", "blockNumber", "replicate", "entryType")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "lasso")
      })


      output$layout_viz <- renderPlotly({
        req(rv$data_dq_viz[,.N>0])
        req(rv$data_dq_viz[,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1)
        req(input$studies)
        #req(all(input$studies%in%rv$data_dq_viz[,unique(studyDbId)]))
        req(input$trait)

        input$set_excluded_obs
        input$set_non_excluded_obs

        data_dq <- rv$data_dq_viz

        data_dq[,is.selected:=F]
        data_dq[observationDbId %in% rv$sel_observationDbIds, is.selected:=T]
        data_dq[, positionCoordinateX:=as.numeric(positionCoordinateX)]
        data_dq[, positionCoordinateY:=as.numeric(positionCoordinateY)]
        
        #plot_text <- data_dq[,.(N=.N,x=min(positionCoordinateX)+(max(positionCoordinateX)-min(positionCoordinateX))/2,y=min(positionCoordinateY)+(max(positionCoordinateY)-min(positionCoordinateY))/2),.(study_name_BMS)]
        #plot_text[,x:=1]
        #plot_text[,y:=1]
        #plot_text[N<=1,label:="No layout"]

        g2 <- ggplot(
          data_dq[!(observationDbId %in% rv$excluded_obs)],
          aes(x = positionCoordinateX, y = positionCoordinateY)
        ) +
          geom_point( # fixes shaky tile selection via plotly
            aes(
              fill = observationValue,
              key = observationDbId
            ),
            alpha = 0
          )+
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
          facet_wrap(study_name_BMS~., ncol = 1, scales="free") +
          scale_fill_gradientn(
            name=input$trait,
            colours = topo.colors(100)
          ) +
          scale_color_discrete(guide = "none") +
          scale_alpha(guide = "none") +
          # scale_linetype(guide = "none") +
          theme_minimal() +
          theme(panel.spacing = unit(0, "lines"), panel.grid = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank())

        ## drawing a vertical and horizontal lines for replicates
        if(data_dq[!is.na(replicate), .N]>0){
          req(!all(data_dq[,.(is.na(positionCoordinateX) | is.na(positionCoordinateY))]))
          repBords <- NULL
          try({
            repBords <- rbindlist(lapply(unique(data_dq$studyDbId), function(tr){
              if(rv$data_dq_viz[studyDbId==tr, any(!is.na(positionCoordinateY))]){
                repBord <- calcPlotBorders(as.data.frame(data_dq[studyDbId==tr, .(
                  rowCoord = as.numeric(positionCoordinateY),
                  colCoord = as.numeric(positionCoordinateX),
                  repId = as.numeric(replicate))]), bordVar = "repId")
                repBord$horW$W <- "horW"
                repBord$vertW$W <- "vertW"
                repBordBind <- rbindlist(repBord, use.names = T, fill = T)
                repBordBind[,study_name_BMS := data_dq[studyDbId==tr, unique(study_name_BMS)]]
                repBordBind
              }
            }), use.names = T, fill = T)
          })
          if(!is.null(repBords)){
            g2 <- g2 +
              ggplot2::geom_segment(ggplot2::aes(x = x - 0.5,
                                                 xend = x - 0.5,
                                                 y = y - 0.5,
                                                 yend = y + 0.5),
                                    data = repBords[W == "vertW"], size = 1, linetype = "dashed", colour = grey(0.5)) +
              ggplot2::geom_segment(ggplot2::aes(x = x - 0.5,
                                                 xend = x + 0.5,
                                                 y = y - 0.5,
                                                 yend = y - 0.5),
                                    data = repBords[W == "horW"], size = 1, linetype = "dashed", colour = grey(0.5)) +
              scale_linetype(guide = "none")
          }
        }

        ## drawing a border (4 segments) for each tile that is selected
        if(data_dq[is.selected==T,.N]>0){
          g2 <- g2 +
            geom_rect(data = data_dq[data_dq$is.selected == TRUE, ],
                      aes(xmin = positionCoordinateX - 0.5, xmax = positionCoordinateX + 0.5,
                          ymin = positionCoordinateY - 0.5, ymax = positionCoordinateY + 0.5),
                      fill = NA, color = "red", size = 1, alpha = 1)
        }

        ## extract legend
        rv_dq$layout_legend <- get_legend(g2)
        g2 <- g2 + theme(legend.position="none")

        ggplotly(height=length(unique(data_dq$studyName))*400,
                 g2,
                 dynamicTicks = "TRUE", source = "A", originalData = T,
                 tooltip = c("germplasmName", "observationValue", "key", "plotNumber", "blockNumber", "replicate", "positionCoordinateX", "positionCoordinateY", "entryType")) %>%
          style(hoverlabel = list(bgcolor = "white")) %>%
          layout(dragmode = "lasso")
      })

      output$layout_legend <- renderPlot({
        req(rv$data_dq_viz[,.N]>0)
        req(rv$data_dq_viz[,.N,.(positionCoordinateX, positionCoordinateY)][,.N]>1)
        req(input$studies)
        req(input$trait)
        req(rv_dq$layout_legend)
        as_ggplot(rv_dq$layout_legend)
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
          bslib::toggle_sidebar(
            id = "sel_obs_sidebar",
            open = T
          )
        }
        rv$sel_observationDbIds <- sel_observationDbIds
      })

      output$correlationPlot <- renderPlotly({
        data_dq_casted <- dcast(
          rv$data_dq_viz[!(observationDbId %in% rv$excluded_obs) & studyDbId %in% input$studies],
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
        p <- plot(TD, plotType="cor", traits = input$trait, output = F, trials = unique(data_dq_casted$study_name_app))
        ggplotly(p[[input$trait]], source = "B")
      })

      ## summary statistics
      output$sumstats_table <- renderDataTable({
        req(rv$data_dq_viz)
        data_dq <- rv$data_dq_viz
        data_dq_notexcl <- rv$data_dq_viz[!(observationDbId %in% rv$excluded_obs)]

        sumtable_all <- data_dq[
          ,
          .(
            "No. of values" = .N
          ),
          study_name_app
        ]

        dataType = unique(data_dq_notexcl$scale.dataType)
        
        if (dataType == "Date") {
          sumtable_notexcl <- data_dq_notexcl[
            ,
            .(
              "No. of observations" = .N,
              "Mean"=mean(observationValue, na.rm = T),
              "Minimum"=min(observationValue, na.rm = T),
              "Quantile 0.25"=quantile(observationValue, probs = c(0.25), type = 1),
              "Median"=quantile(observationValue, probs = c(0.5), type = 1),
              "Quantile 0.75"=quantile(observationValue, probs = c(0.75), type = 1),
              "Maximum"=max(observationValue, na.rm = T)
            ),
            study_name_app
          ]
          
          columns <- c(
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
          sumtable_notexcl <- data_dq_notexcl[
            ,
            .(
              "No. of observations" = .N,
              "Mean"=mean(observationValue, na.rm = T),
              "Minimum"=min(observationValue, na.rm = T),
              "Quantile 0.25"=ifelse(dataType == "Date",quantile(observationValue, probs = c(0.25), type = 1),quantile(observationValue, probs = c(0.25))),
              "Median"=ifelse(dataType == "Date",quantile(observationValue, probs = c(0.5), type = 1),quantile(observationValue, probs = c(0.5))),
              "Quantile 0.75"=ifelse(dataType == "Date",quantile(observationValue, probs = c(0.75), type = 1),quantile(observationValue, probs = c(0.75))),
              "Maximum"=max(observationValue, na.rm = T),
              "Standard deviation"=sd(observationValue, na.rm = T),
              "Variance"=var(observationValue, na.rm = T),
              "Sum of values"=sum(observationValue, na.rm = T),
              "Sum of squares"=sum((observationValue-mean(observationValue, na.rm = T))^2),
              "Uncorrected sum of squares"=sum(observationValue^2, na.rm = T),
              "Skewness"=e1071::skewness(observationValue),
              "Kurtosis"=e1071::kurtosis(observationValue)
            ),
            study_name_app
          ]
          
          sumtable_notexcl[,"Standard error of mean":=`Standard deviation`/sqrt(`No. of observations`)]
          sumtable_notexcl[,"Standard error of variance":=`Variance`/sqrt(`No. of observations`)]
          sumtable_notexcl[,"%cov":=`Variance`/`Mean`]
          sumtable_notexcl[,"%Standard error of skewness":=`Skewness`/sqrt(`No. of observations`)]
          sumtable_notexcl[,"%Standard error of kurtosis":=`Kurtosis`/sqrt(`No. of observations`)]
          sumtable_notexcl[,"Range":=Maximum - Minimum]
          
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
        }
        
        setkey(sumtable_all,study_name_app)
        setkey(sumtable_notexcl,study_name_app)
        sumtable <- sumtable_all[sumtable_notexcl]
        sumtable[,"No. of excluded values":= `No. of values` - `No. of observations`]

        rownames_sumtable <- sumtable[,study_name_app]

        sumtable <- sumtable[,columns, with = F]
        rownames(sumtable) <- rownames_sumtable
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
        selected_obs <- rv$data_dq_viz[observationDbId %in% rv$sel_observationDbIds & !(observationDbId %in% rv$excluded_obs)]
        setcolorder(selected_obs, visible_columns_selected_obs)
        datatable(
          selected_obs,
          extensions = 'Buttons',
          colnames = c("study"="study_name_abbrev_app"),
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(names(selected_obs)[!(names(selected_obs)%in%visible_columns_selected_obs)], names(selected_obs))
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
          formatStyle(0, target= 'row', lineHeight='90%')
      })

      observeEvent(input$unselect_obs, {
        rv$sel_observationDbIds <- NULL
      })

      observeEvent(input$set_excluded_obs,{

        new_excluded_obs <- rv$data_dq_viz[
          observationDbId %in% rv$sel_observationDbIds
        ][
          input$selected_obs_table_rows_selected, observationDbId
        ]

        rv$sel_observationDbIds <- setdiff(rv$sel_observationDbIds, new_excluded_obs) # remove the "selected" status from the new excluded observations
        rv$excluded_obs <- union(rv$excluded_obs, new_excluded_obs)
      })

      output$excluded_obs_table <- renderDT({
        shinyjs::hide(selector = ".display_if_exclusion")
        req(rv$data_dq_viz)
        rv$data_dq_viz
        input$set_excluded_obs
        input$set_non_excluded_obs
        req(rv$data_dq_viz[observationDbId %in% rv$excluded_obs,.N]>0)
        shinyjs::show(selector = ".display_if_exclusion")

        selected_excl_obs <- rv$data_dq_viz[observationDbId %in% rv$excluded_obs]
        setcolorder(selected_excl_obs, visible_columns_selected_obs)
        datatable(
          selected_excl_obs,
          colnames = c("study"="study_name_abbrev_app"),
          extensions = 'Buttons',
          options = list(
            columnDefs = list(
              list(
                visible=FALSE,
                targets=match(names(selected_excl_obs)[!(names(selected_excl_obs)%in%visible_columns_selected_obs)], names(selected_excl_obs))
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
          formatStyle(0, target= 'row', lineHeight='90%')
      })

      observeEvent(input$set_non_excluded_obs,{
        non_excluded_obs <- rv$data_dq_viz[observationDbId %in% rv$excluded_obs][
          input$excluded_obs_table_rows_selected, observationDbId]

        rv$excluded_obs <- setdiff(rv$excluded_obs, non_excluded_obs)
      })

      observeEvent(input$select_variable,{
        if(input$select_variable==""){
          shinyjs::hide("select_variable_value")
        }else{
          shinyjs::show("select_variable_value")
        }
      })

      observeEvent(input$envXtrait,{
        output$envXtraitViz <- renderPlot({
          req(rv$data)
          ggplot(rv$data[,.N,.(study_name_app,observationVariableName, observationLevel)],
                 aes(y = study_name_app, x=observationVariableName, fill=N))+
            geom_tile() +
            facet_wrap(vars(observationLevel), nrow = 1, drop = T, ) +
            # facet_grid(cols = vars(observationLevel), drop = T) +
            scale_fill_continuous(name = "Number of\nobservations") +
            coord_equal() +
            theme_minimal() +
            theme(
              legend.position = "bottom",
              panel.grid = element_blank(),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
              )
        })
      })

      return(rv)
    }
  )
}
