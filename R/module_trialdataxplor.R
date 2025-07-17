#' @export
mod_trialdataxplor_ui <- function(id){
  ns <- NS(id)
  div(class = "container-fluid",
      # Application title
      #titlePanel(title=div(img(src="ibp.png", width="34px", border.radius="6px"),"BMS trial data explorer"),windowTitle="BMS trial data explorer"),
      #shinysky::busyIndicator(wait = 1000, text = NULL),
      #use_waiter(),
      #waiter_on_busy(),
      # Sidebar
      tags$style(
        HTML(
          ".nav .nav-item .nav-link { font-size: 20px; }",
        )
      ),
      bslib::page_navbar(title = "", id = ns("tabsetId"), 
                             #tabsetPanel(#title = "", id = "tabsetId",
                         bslib::nav_panel("Data counts",value="counts",
                                      div(tableOutput(ns("counts_table")), style = "font-size: 75%;")),
                         bslib::nav_panel("Distributions",value="distrib",
                                      uiOutput(ns("spinning_boxplot"))),
                         bslib::nav_panel("Observations",value="observ",
                                          div(style="display: flex;",
                                      div(style="display: inline-block;vertical-align:middle;",selectInput(ns("obs_trait"), label="Variable", choices=NULL)),
                                      #div(style="display: inline-block;vertical-align:middle; width: 10px;",HTML("<br>")),
                                      div(style="display: inline-block;vertical-align:middle;",selectizeInput(ns("obs_study"), label="Single study", choices=NULL, multiple=FALSE)),
                                      div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                                      div(style="display: inline-block;vertical-align:middle;",uiOutput(ns("copy_obs_table")))),
                                      fluidRow(
                                        #column(
                                        #  5,
                                        shinycssloaders::withSpinner(
                                          #plotlyOutput("observ_boxplot", height=500, width = "50%"), type = 1,color.background = "white"
                                          plotOutput(ns("observ_boxplot"), height=200, width = "100%", brush = ns("observ_boxplot_brush")), type = 1,color.background = "white",
                                        ),
                                        div(style="display: flex;",
                                        div(style="display: inline-block;vertical-align:middle;",materialSwitch(inputId = ns("observ_boxplot_splitreps"), label = "One boxplot per rep", value = FALSE, status = "info")),
                                        div(style="display: inline-block;vertical-align:middle;",materialSwitch(inputId = ns("selected_obs_otherreps"), label = "Display all reps in observations table", value = FALSE, status = "info"))),
                                        div(DT::dataTableOutput(ns("selected_obs")), style = "font-size: 75%;"))
                                      #)
                             ),
                         bslib::nav_panel("Data check report",value="check",
                                          div(style="display: flex;",
                                      downloadButton(ns("download_check"), label = "Download report", icon = icon(NULL))),
                                      h3("Studies with no data:"),
                                      div(tableOutput(ns("study_no_dat")), style = "font-size: 75%;"),
                                      h3("Missing variables per study:"),
                                      div(tableOutput(ns("var_no_dat")), style = "font-size: 75%;"),
                                      h3("Candidate outliers:"),
                                      sliderInput(ns("outslid"), "coef", min = 1.5, max=5, value = 1.5),
                                      div(DT::dataTableOutput(ns("candidat_out")), style = "font-size: 75%;")
                                      
                             ),
                         bslib::nav_panel("Locations map",value="map",
                                      leaflet::leafletOutput(outputId = ns("locationmap"), height = 600)),
                         bslib::nav_spacer(),
                         bslib::nav_panel(
                           title = "About",
                           h1("Trial Data Explorer"),
                           img(src='img/sticker.png', height="178px", width="154px",  align = "right"),
                           h2("Contributors"),
                           p("Jean-François Rami (Maintainer) - rami 'at' cirad.fr"),
                           p("Alice Boizet (Author) - alice.boizet 'at' cirad.fr"),
                           hr(),hr(),
                           img(src='img/ibpcirad.png', height="61px", width="231px",  align = "left"),
                           br(),hr(),
                           h2(a("github",href="https://github.com/IntegratedBreedingPlatform/ShinyBrAPPs", target="_blank", icon("github")), align="right"),
                           hr(),hr(),
                           h2("Funded by"),
                           p("Trial Data Explorer development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en"), ", under the DESIRA initiative of the European Union"),
                           img(src='img/ABEE_logo_trspbckgd.png', height="57px", width="84px",  align = "right"),
                           hr(),hr(),
                           img(src='img/desira.png', height="56px", width="252px",  align = "right"),
                           hr(),hr(),
                           h2("Session info"),
                           verbatimTextOutput("Rsi")
                         )
                             
                  )
  )
  
}

#' @import leaflet
#' @export
mod_trialdataxplor_server <- function(id, rv){
  
  moduleServer(
    
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      rv_tdx <- reactiveValues(
        #tr = NULL,
        st = NULL,
        #stdatadt = NULL,
        variables = NULL,
        data_dq = NULL,
        locs = NULL,
        study_no_dat = NULL,
        var_no_dat = NULL,
        candidat_out = NULL
      )

      find_outlier <- function(x,c=1.5) {
        return(x < quantile(x, .25, na.rm = TRUE) - c*IQR(x, na.rm = TRUE) | x > quantile(x, .75, na.rm = TRUE) + c*IQR(x, na.rm = TRUE))
      }

      ## observe rv$data ####
      observeEvent(c(rv$data), {
        req(rv$data)
        if (nrow(rv$data)==0){
          showModal(modalDialog(paste0("No data in the selected studies"), fade = FALSE))
        } else {
          if(rv$data[observationLevel!="PLOT", .N]>0){
            showNotification(
              paste0("Taking away the level(s) of observation: ",
                     rv$data[observationLevel!="PLOT", paste(unique(observationLevel), collapse = ", ")],
                     "\n(",rv$data[observationLevel!="PLOT", .N], " values)",
                     "\n\n(Only the PLOT observation level is considered for STA)"
              ), type = "default", duration = notification_duration)
          }
          
          data_dq <- rv$data
          req("observationVariableName"%in%names(data_dq))        
          if(!("observationVariableName"%in%names(data_dq))){
            showNotification("No trait data", type = "error", duration = notification_duration)
          }
          
          env_choices <- rv$study_metadata[loaded==T,unique(studyDbId)]
          names(env_choices) <- rv$study_metadata[loaded==T,unique(study_name_app)]
          data_dq <- data_dq[!is.na(observationVariableDbId)]
          scrid <- brapir::phenotyping_variables_post_search(rv$con, observationVariableDbIds = as.character(unique(data_dq$observationVariableDbId)))$data$searchResultsDbId
          variables <- brapir::phenotyping_variables_get_search_searchResultsDbId(rv$con, searchResultsDbId = scrid)$data
          setDT(variables)
          variables[,observationVariableDbId:=as.numeric(observationVariableDbId)]
          rv_tdx$variables <- variables
          
          locs <- rbindlist(lapply(unique(rv$study_metadata$locationDbId), function(l){
             as.data.table(brapir::core_locations_get(rv$con, locationDbId = l)$data)
           }), use.names = T, fill = T)
          st <- locs[,.(locationDbId,countryName)][rv$study_metadata, on=.(locationDbId)]
          st <- unique(st[,.(studyDbId,locationDbId,countryName,studyName,locationName)])
          st[, studyDbId:=as.numeric(studyDbId)]
          st[, study_label:=paste0(locationName," (",countryName,")")]
          data_dq <- st[,.(studyDbId,countryName )][data_dq, on=.(studyDbId)]
          data_dq <- variables[,.(observationVariableDbId, trait.name, method.methodName, scale.scaleName)][data_dq, on=.(observationVariableDbId=observationVariableDbId)]
          data_dq[, study_label:=paste0(locationName," (",countryName,")")]
          data_dq[, observationValue:=as.numeric(observationValue)]
          data_dq[, replicate:=as.factor(replicate)]
          #browser()
          if (any(!st$studyDbId%in%data_dq$studyDbId)){
            missingst <- st[!studyDbId%in%data_dq$studyDbId]
            missingmsg <- paste(paste0(missingst$study_label,"(",missingst$studyDbId,")"),collapse=", ")
            showModal(modalDialog(paste0("The following studies had no observation data: ", missingmsg), fade = FALSE))
            rv_tdx$study_no_dat <- missingst
            output$study_no_dat <- renderTable(missingst,digits=0)
          }
  
          output$spinning_boxplot <- renderUI({
            shinycssloaders::withSpinner(
              plotOutput(ns("boxplots"), height=500), type = 1,color.background = "white"
            )
          })
          
          #rv_tdx$st <- data_dq[,.N,studyDbId][st, on=.(studyDbId)]
          rv_tdx$data_dq <- data_dq
          rv_tdx$locs <- locs
  
          updateSelectInput(session, inputId = "obs_trait",choices = unique(data_dq$observationVariableName))
          
          ct <- dcast(isolate(data_dq)[observationLevel=="PLOT", .N, .(study=paste0(studyDbId,"-",locationName),Variable=observationVariableName)],
                      Variable~study, fill = 0)
          rv_tdx$counts <- ct
          vnd <- melt(ct, variable.name = "StudyLocation")[value==0,.(StudyLocation, Variable)]
          rv_tdx$var_no_dat <- vnd
        }
      })
      observeEvent(input$outslid,{
        req(rv_tdx$data_dq, input$outslid)
        data_dq <- rv_tdx$data_dq
        cdout0 <- data_dq[observationValue==0, .(reason="value=0",studyDbId, study_label, observationVariableDbId, observationVariableName, observationValue, germplasmName, observationDbId, replicate, blockNumber, plotNumber, entryNumber)]
        #norm_var <- data_dq[scale.dataType=="Numerical" & observationValue!=0][data_dq[!is.na(observationValue),.(sd=sd(observationValue)),.(studyDbId,observationVariableDbId)][sd!=0],on=.(studyDbId,observationVariableDbId)][!is.na(observationValue)][,.(shapiro.test(observationValue)$`p.value`),.(studyDbId,observationVariableDbId, observationVariableName)][V1>=0.05]
        #cdoutbp <-data_dq[data_dq[norm_var, on=.(studyDbId,observationVariableDbId)][,.(observationValue=boxplot.stats(observationValue, coef = input$outslid)$out),.(studyDbId,observationVariableDbId)],on=.(studyDbId,observationVariableDbId, observationValue)][, .(reason="boxplot-outliers",studyDbId, study_label, observationVariableDbId,observationVariableName, observationValue, germplasmName, replicate, blockNumber, plotNumber, entryNumber)]
        cdoutbp <- data_dq[data_dq[,find_outlier(observationValue,input$outslid),.(studyDbId,observationVariableDbId)]$V1==TRUE][, .(reason="boxplot-outliers",studyDbId, study_label, observationVariableDbId,observationVariableName, observationValue, germplasmName, observationDbId, replicate, blockNumber, plotNumber, entryNumber)]
        cdout <- rbind(cdout0,cdoutbp)
        rv_tdx$cdout <- cdout
      })
      
      output$counts_table <- renderTable({
        req(rv_tdx$counts)
        rv_tdx$counts
      })
      
      output$var_no_dat <- renderTable({
        req(rv_tdx$var_no_dat)
        rv_tdx$var_no_dat
      })
      
      output$candidat_out <- DT::renderDataTable({
        req(rv_tdx$cdout)
        datatable(rv_tdx$cdout, options = list(paging = FALSE,searching = FALSE), selection = "none") |>
          formatRound(columns = "observationValue", digits = 2)
      })
      #output$candidat_out <-renderTable(cdout, digits=0)
      
      observeEvent(input$obs_trait, {
        req(rv_tdx$data_dq)
        obs_study_data <- rv_tdx$data_dq[observationVariableName==input$obs_trait,.N,.(studyDbId, locationName, studyName,countryName)]
        updateSelectizeInput(session,
                              inputId = "obs_study",
                              selected = input$obs_study,
                              server=TRUE,
                              choices = obs_study_data,
                              options = list(valueField='studyDbId',
                                            labelField='locationName',
                                            searchField=c('studyName',"locationName",'countryName'),
                                            render = I("{option: function(item, escape) {
                                                    return '<div><strong>'+ escape(item.studyDbId) +'-'+ escape(item.locationName) + '</strong> (' + escape(item.countryName) + ') ('+ escape(item.N)+ ')</div>';
                                          }}"))
        )
        rv_tdx$obs_btable <- data.table()[0L]
      }, ignoreInit = T)
     
      observeEvent(rv_tdx$locs, {
        locs <- rv_tdx$locs
        if ("coordinates.geometry.coordinates"%in%colnames(locs)){
          locs[coordinates.geometry.type=="Point",lat:=unlist(lapply(coordinates.geometry.coordinates, function(a) a[2]))]
          locs[coordinates.geometry.type=="Point",lon:=unlist(lapply(coordinates.geometry.coordinates, function(a) a[1]))]
          output$locationmap <- renderLeaflet(leaflet(data = locs) %>%  
                                              addCircleMarkers(popup = ~as.character(locationName),
                                                                label=~as.character(locationName),
                                                                labelOptions = labelOptions(noHide = T))%>%
                                              addTiles(group = "OSM") %>%
                                              addLayersControl(baseGroups = c("Positron", "Gray","Lite", "OSM", "Terrain", "Satellite"))%>%
                                              #addProviderTiles(providers$Stadia.StamenTerrainBackground, group = "Terrain") %>%
                                              #addProviderTiles(providers$Stadia.StamenTonerLite, group = "Lite") %>%
                                              addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
                                              addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray") %>%
                                              addProviderTiles(providers$Esri.WorldImagery, group = "Satellite"))
        
        }
      })
      
      dynamicHeight <- reactive({
        req(nrow(rv_tdx$variables)>0)
        return(nrow(rv_tdx$variables) * 150)
      })
      
      output$boxplots<-renderPlot({
        req(rv_tdx$data_dq)
        req(rv_tdx$data_dq[,.N]>0)
        data_dq <- rv_tdx$data_dq
        data_dq[, facetrows := paste0("V: ",observationVariableName,"\n",
                                          "T: ",trait.name, "\n",
                                          "M: ",method.methodName,"\n",
                                          "S: ",scale.scaleName)]
        data_dq[, facetcols := paste0(studyDbId, "-", locationName,"\n",countryName)]
        data_dq[,is_out:=FALSE]
        data_dq[observationDbId%in%rv_tdx$cdout$observationDbId,is_out:=TRUE]
        loclabels <- unique(data_dq[,.(facetcols,locationName)])
        g<-ggplot(data_dq, aes(y=observationValue, x=replicate)) +
          geom_boxplot(aes(fill=as.factor(replicate)), outlier.shape = NA) +#, coef = input$outslid) +
          facet_grid(rows=vars(facetrows), cols=vars(facetcols), scales = "free") +
          ggnewscale::new_scale_fill() +
          geom_jitter(aes(fill=is_out, size=is_out),
                      width = 0,
                      height = 0,
                      shape = 21,
                      alpha = 0.5,
                      #fill = grey(0.9),
                      #size = 3
          ) +
          scale_fill_manual(values=c(`TRUE`="red",`FALSE`="#FFFFFF01")) +
          scale_size_manual(values=c(`TRUE`=3,`FALSE`=0)) +
          ggtitle(input$trial) +
          theme(strip.text.y.right = element_text(angle = 0, vjust = 1, hjust=0, size = 10),
                strip.text.x = element_text(size = 10),
                strip.background.y = element_rect(fill = "white", colour = "black"),
                legend.position = "none") #+ coord_flip()
        g <- g + geom_text(
          data    = loclabels,
          size=3,
          mapping = aes(x = 0.25, y = 0, label = locationName),
          hjust   = 0,
          vjust   = 1, angle=90
        )
        g
        # g<-ggplot(toplot, aes(y=observationValue, fill=replicate, x=locationName)) +
        #   geom_boxplot() +
        #   facet_wrap(~paste0(observationVariableName,"\n",trait.name) , ncol= 1, scales = "free", strip.position="top") +
        #   ggtitle(t) +
        #   theme(strip.text.y.right = element_text(angle = 0), axis.text.x =  element_text(angle = 90)) #+ coord_flip()
        #browser()
      }, height = function() {
        dynamicHeight()
      })
      
      observeEvent(input$obs_study,{
        req(rv_tdx$data_dq$studyDbId)
        rv_tdx$observations <- rv_tdx$data_dq[studyDbId==input$obs_study & observationVariableName==input$obs_trait]
        rv_tdx$obs_btable <- data.table()[0L]
        
      })
      
      output$observ_boxplot <- renderPlot({
        req(rv_tdx$observations[,.N]>0)
        data_dq <- rv_tdx$observations
        #data_dq[, is.selected:=F]
        #data_dq[observationDbId %in% rv$sel_observationDbIds, is.selected:=T]
        #browser()
        data_dq[,is_out:=FALSE]
        data_dq[observationDbId%in%rv_tdx$cdout$observationDbId,is_out:=TRUE]
        if (!input$observ_boxplot_splitreps){
          g1 <- ggplot(data_dq, aes(
            y = observationValue,
            x = study_label
          )) +
            geom_boxplot(
              fill =  grey(0.8), outlier.shape = NA #coef = input$outslid, outlier.colour = "red",  outlier.size = 5
            ) +
            geom_jitter(aes(fill=is_out, size=is_out),
              width = 0.05,
              height = 0,
              shape = 21,
              alpha = 0.5,
              #fill = grey(0.9),
              #size = 3
            ) + 
            scale_fill_manual(values=c(`TRUE`="red",`FALSE`=grey(0.9))) +
            scale_size_manual(values=c(`TRUE`=5,`FALSE`=3)) +
            scale_alpha(guide = "none") + coord_flip() +
            theme_minimal() +
            theme(legend.position = "none") +
            xlab(label = element_blank())
        } else {
          g1 <- ggplot(data_dq, aes(
            y = observationValue,
            x = replicate
          )) +
            geom_boxplot(
              aes(fill=as.factor(replicate)), outlier.shape = NA #coef = input$outslid, outlier.colour = "red",  outlier.size = 5
            ) +
            ggnewscale::new_scale_fill() +
            geom_jitter(aes(fill=is_out, size=is_out),
                        width = 0.05,
                        height = 0,
                        shape = 21,
                        alpha = 0.5,
                        #fill = grey(0.9),
                        #size = 3
            ) +
            scale_fill_manual(values=c(`TRUE`="red",`FALSE`=grey(0.9))) +
            scale_size_manual(values=c(`TRUE`=5,`FALSE`=3)) +
            scale_alpha(guide = "none") + coord_flip() +
            theme_minimal() +
            theme(legend.position = "none") +
            xlab(label = element_text("Replicate"))
        }

        g1
      })
      
      observeEvent(input$observ_boxplot_brush, {
        req(rv_tdx$data_dq[,.N]>0)
        rv_tdx$obs_btable <- brushedPoints(rv_tdx$data_dq[studyDbId==input$obs_study & observationVariableName==input$obs_trait,.(trait.name,
                                                                                                                          VariableName=observationVariableName,
                                                                                                                          observationValue,
                                                                                                                          plotNumber,
                                                                                                                          germplasmName,
                                                                                                                          entryNumber,
                                                                                                                          blockNumber,
                                                                                                                          replicate,
                                                                                                                          positionCoordinateX,
                                                                                                                          positionCoordinateY,
                                                                                                                          TimeStamp=observationTimeStamp,
                                                                                                                          study_label,
                                                                                                                          observationUnitDbId,
                                                                                                                          germplasmDbId)],
                                          input$observ_boxplot_brush)

        output$copy_obs_table <- renderUI({
          rclipboard::rclipButton("clipbtnobs_table", "Copy observations table", paste(paste(colnames(rv_tdx$obs_btable),collapse="\t"),
                                                                                      paste(apply(rv_tdx$obs_btable,1,paste,collapse="\t"),collapse = "\n"),
                                                                                      sep="\n"))#, shiny::icon("clipboard"))
        })
      })
      
      output$selected_obs <- DT::renderDataTable({
        req(nrow(rv_tdx$obs_btable)>0)
        if (input$selected_obs_otherreps){
          obstable <- rv_tdx$data_dq[germplasmName%in%rv_tdx$obs_btable$germplasmName & studyDbId==input$obs_study & observationVariableName==input$obs_trait,.(trait.name,
                                                                                                 VariableName=observationVariableName,
                                                                                                 observationValue,
                                                                                                 plotNumber,
                                                                                                 germplasmName,
                                                                                                 entryNumber,
                                                                                                 blockNumber,
                                                                                                 replicate,
                                                                                                 positionCoordinateX,
                                                                                                 positionCoordinateY,
                                                                                                 TimeStamp=observationTimeStamp,
                                                                                                 study_label,
                                                                                                 observationUnitDbId,
                                                                                                 germplasmDbId)][order(germplasmName)]
          obstable[, is_bold:="normal"]
          obstable[observationUnitDbId %in% rv_tdx$obs_btable$observationUnitDbId, is_bold:="bold"]
        } else {
          obstable <- copy(rv_tdx$obs_btable)
          obstable[, is_bold:="normal"]
        }
        datatable(obstable, options = list(paging = FALSE,
                                           searching = FALSE,
                                           columnDefs = list(
                                             list(visible = FALSE, targets = which(colnames(obstable) == "is_bold"))
                                           )), selection = "none") |>
          formatRound(columns = "observationValue", digits = 2) |>
          formatStyle(
            columns = names(obstable)[-ncol(obstable)],
            fontWeight = styleEqual("bold", "bold"),
            target = "row",
            valueColumns = "is_bold"
          )
      })

      output$download_check <- downloadHandler(
        filename = function() {
          # Use the selected dataset as the suggested file name
          paste0("data_check", ".xlsx")
        },
        content = function(file) {
          # Write the dataset to the `file` that will be downloaded
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Studies with no data")
          openxlsx::addWorksheet(wb, "Missing variables per study")
          openxlsx::addWorksheet(wb, "Candidate outliers")
          openxlsx::writeData(wb, 1, rv_tdx$study_no_dat)
          openxlsx::writeData(wb, 2, rv_tdx$var_no_dat)
          openxlsx::writeData(wb, 3, rv_tdx$cdout)
          openxlsx::setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
          openxlsx::setColWidths(wb, sheet = 2, cols = 1:2, widths = "auto")
          openxlsx::setColWidths(wb, sheet = 3, cols = 1:8, widths = "auto")
          openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
        }
      )
    }
  )
  
}
