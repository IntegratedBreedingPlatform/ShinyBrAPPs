#' @export
mod_trialdataxplor_ui <- function(id){
  ns <- NS(id)

  #div(class = "page_fillable",
      # Application title
      #titlePanel(title=div(img(src="ibp.png", width="34px", border.radius="6px"),"BMS trial data explorer"),windowTitle="BMS trial data explorer"),
      #shinysky::busyIndicator(wait = 1000, text = NULL),
      #use_waiter(),
      #waiter_on_busy(),
      # Sidebar
  tagList(
      tags$style(
        HTML(
          ".nav .nav-item .nav-link { font-size: 20px; }
          .bslib-card .card-body {
              overflow: visible !important;
          }"
        ),
        HTML("
      /* Style the container */
      #my-rank-list {
        border: 1px solid #269100;
        background: #c4ee9a;
        padding: 2px;
      }
    ")
      ),
      bslib::navset_card_tab( id = ns("tabset"),
                              sidebar = sidebar(title = "Settings",width = 300,
                                open="closed",
                                accordion(open = FALSE,
                                  accordion_panel("Compose study name", 
                                                  uiOutput(ns("sortable_ui"))
                                                  ),
                                  accordion_panel("Parameters",
                                                  numericInput(ns("boxplot_basewidth"),label = "Distribution plot base width", value = 150),
                                                  numericInput(ns("boxplot_baseheight"),label = "Distribution plot base height", value = 150),
                                                  sliderInput(ns("outslid"), label = "Outliers detection coefficient", min = 1.5, max=5, value = 1.5)
                                  )
                                )
                              ),
                             #tabsetPanel(#title = "", id = "tabsetId",
                         bslib::nav_panel("Data counts",value="counts",
                                      div(tableOutput(ns("counts_table")), style = "font-size: 75%;")),
                         bslib::nav_panel("Distributions",value="distrib",
                                          div(style="display: flex;
                                                     gap: 10px;
                                                     align-items: center;",
                                              pickerInput(ns("dis_trait"), label="Variables",
                                                          multiple = TRUE, 
                                                          choices = NULL, 
                                                          options = list(`actions-box` = TRUE,
                                                                         size = 15,
                                                                         `live-search` = TRUE)),
                                              pickerInput(ns("dis_study"), label="Studies",
                                                          multiple = TRUE, 
                                                          choices = NULL, 
                                                          options = list(`actions-box` = TRUE,
                                                                         size = 15,
                                                                         `live-search` = TRUE)),
                                              actionButton(ns("refresh_dist"),label = "Plot distributions")),
                                            shinycssloaders::withSpinner(
                                              plotOutput(ns("boxplots")), type = 1,color.background = "white"
                                            )
                         ),
                                          #uiOutput(ns("spinning_boxplot"))),
                         bslib::nav_panel("Observations",value="observ",
                                          #div(style="display: flex;",
                                          div(style="display: flex;
                                                     gap: 10px;
                                                     align-items: center;",
                                              selectInput(ns("obs_trait"), label="Variable", choices=NULL),
                                              selectizeInput(ns("obs_study"), label="Single study", choices=NULL, multiple=FALSE)
                                              ),
                                      #div(style="display: inline-block;vertical-align:middle; width: 50px;",HTML("<br>")),
                                      #fluidRow(
                                        #column(
                                        #  5,
                                        shinycssloaders::withSpinner(
                                          #plotlyOutput("observ_boxplot", height=500, width = "50%"), type = 1,color.background = "white"
                                          plotOutput(ns("observ_boxplot"), height=200, width = "100%", brush = ns("observ_boxplot_brush")), type = 1,color.background = "white",
                                        ),
                                      div(style="display: flex;
                                                     gap: 10px;
                                                     align-items: center;",
                                          materialSwitch(inputId = ns("observ_boxplot_splitreps"), label = "One boxplot per rep", value = FALSE, status = "info"),
                                          materialSwitch(inputId = ns("selected_obs_otherreps"), label = "Display all reps in observations table", value = FALSE, status = "info"),
                                          uiOutput(ns("copy_obs_table"))),
                                        div(DT::dataTableOutput(ns("selected_obs")), style = "font-size: 75%;")
                                      #)
                             ),
                         bslib::nav_panel("Data check report",value="check",
                                          div(style="display: flex;",
                                      downloadButton(ns("download_check"), label = "Download report", icon = icon(NULL))),
                                      accordion(open = "Studies with no data",
                                                accordion_panel("Studies with no data", 
                                                                h3("Studies with no data:"),
                                                                div(tableOutput(ns("study_no_dat")), style = "font-size: 75%;")
                                                ),
                                                accordion_panel("Missing variables per study", 
                                                                h3("Missing variables per study:"),
                                                                div(tableOutput(ns("var_no_dat")), style = "font-size: 75%;")
                                                ),
                                                accordion_panel("Candidate outliers",
                                                                h3("Candidate outliers:"),
                                                                div(DT::dataTableOutput(ns("candidat_out")), style = "font-size: 75%;")
                                                )
                                      )
                                      
                             ),
                         bslib::nav_panel("Locations map",value="map",
                                      leaflet::leafletOutput(outputId = ns("locationmap"), height = 600)),
                         bslib::nav_spacer(),
                         bslib::nav_panel(
                           title = "About",
                           div(style="display: flex;
                                      justify-content: space-between;
                                      align-items: flex-start;",
                               div(class="flex-item-group",
                                 h1("Trial Data Explorer"),
                                 h2("Contributors"),
                                 p("Jean-François Rami (Maintainer) - rami 'at' cirad.fr"),
                                 p("Alice Boizet (Author) - alice.boizet 'at' cirad.fr")
                               ),
                               img(src='img/sticker.png', height="178px", width="154px",  align = "right")
                           ),
                           div(style="display: flex;
                                      justify-content: space-between;",
                               img(src='img/ibpcirad.png', height="61px", width="231px",  align = "left"),
                               h2(a("github",href="https://github.com/IntegratedBreedingPlatform/ShinyBrAPPs", target="_blank", icon("github")), align="right")
                           ),
                           div(style="display: flex;
                                      justify-content: space-between;",
                               div(class="flex-item-group",
                                   h2("Funded by"),
                                   p("Trial Data Explorer development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en"), ", under the DESIRA initiative of the European Union")
                               ),
                               div(class="flex-item-group",
                                   style="display: flex;
                                          gap: 10px;
                                          flex-direction: column;
                                          align-items: flex-end;",
                                   img(src='img/ABEE_logo_trspbckgd.png', height="57px", width="84px", align="right"),
                                   img(src='img/desira.png', height="56px", width="252px", align="right")
                               )
                           ),
                           h2("Session info"),
                           verbatimTextOutput("Rsi")
                         )
                             
                  )
  )
  
}

#' @import leaflet
#' @import sortable
#' @export
mod_trialdataxplor_server <- function(id, rv){
  
  moduleServer(
    
    id,
    function(input, output, session){
      
      ns <- session$ns
      
      js <- JS(
        "table.on('dblclick', 'td', function() {",
        "  var row = table.cell(this).index().row;",
        "  var col = table.cell(this).index().column;",
        "  Shiny.setInputValue('xplor-dt_dblclick', {row: row + 1, col: col + 1}, {priority: 'event'});",
        "});"
      )
      
      
      rv_tdx <- reactiveValues(
        #tr = NULL,
        st = NULL,
        #stdatadt = NULL,
        variables = NULL,
        data_dq = NULL,
        locs = NULL,
        study_no_dat = NULL,
        var_no_dat = NULL,
        candidat_out = NULL,
        obs_study_to_sel = NULL
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
          data_dq <- rv$environmentParameters[data_dq, on=.(studyDbId)]
          
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
          rv_tdx$variables <- variables
          
          locs <- rbindlist(lapply(unique(rv$study_metadata$locationDbId), function(l){
             as.data.table(brapir::core_locations_get(rv$con, locationDbId = l)$data)
           }), use.names = T, fill = T)
          st <- locs[,.(locationDbId,countryName)][rv$study_metadata, on=.(locationDbId)]
          st <- unique(st[,.(studyDbId,locationDbId,countryName,studyName,locationName)])
          st <- rv$environmentParameters[st, on=.(studyDbId)]
          #st[, study_label:=paste0(locationName," (",countryName,")")]
          st[, label_study:=paste0(locationName,"-",studyDbId)]
          data_dq <- st[,.(studyDbId,countryName )][data_dq, on=.(studyDbId)]
          data_dq <- variables[,.(observationVariableDbId, trait.name, method.methodName, scale.scaleName)][data_dq, on=.(observationVariableDbId=observationVariableDbId)]
          data_dq[, study_label:=paste0(locationName," (",countryName,")")]
          data_dq[, label_study:=paste0(locationName,"-",studyDbId)]
          data_dq[, observationValue:=as.numeric(observationValue)]
          data_dq[, replicate:=as.factor(replicate)]
          data_dq[, facetcols := paste0(studyDbId, "-", locationName,"\n",countryName)]
          
          #browser()
          if (any(!st$studyDbId%in%data_dq$studyDbId)){
            missingst <- st[!studyDbId%in%data_dq$studyDbId, .(studyDbId,locationDbId,countryName,studyName,locationName, label_study)]
            missingmsg <- paste(paste0(missingst$label_study,"(",missingst$studyDbId,")"),collapse=", ")
            showModal(modalDialog(paste0("The following studies had no observation data: ", missingmsg), fade = FALSE))
            rv_tdx$study_no_dat <- missingst
            
          }
  
          
          #rv_tdx$st <- data_dq[,.N,studyDbId][st, on=.(studyDbId)]
          rv_tdx$data_dq <- data_dq
          rv_tdx$locs <- locs
          rv_tdx$st <- st
        }
        })
      
      output$study_no_dat <- renderTable(rv_tdx$study_no_dat ,digits=0)
      
      observeEvent(c(rv$environmentParameters),{
        fromlabels <- c(#grep("location",colnames(rv_tdx$data_dq), value = TRUE),
                        #grep("study_*[n,N]ame",colnames(rv_tdx$data_dq), value = TRUE),
                        colnames(rv_tdx$data_dq)[colnames(rv_tdx$data_dq)%in%colnames(rv$environmentParameters)]
                        )
        tolabels <- "locationName"
        fromlabels <- setdiff(fromlabels,  c("locationName","studyDbId"))
        output$sortable_ui <- renderUI({
        sortable::bucket_list(
          header = "Drag elements to the green area to compose study name",
          group_name = "bucket_list_group",
          orientation = "vertical",
          sortable::add_rank_list(
            text = "",
            labels = fromlabels,
            input_id = ns("rank_list_1")
          ),
          sortable::add_rank_list(
            text = "",
            labels = tolabels,
            input_id = ns("rank_list_2"),
            css_id = "my-rank-list"
          )
        )
        })
      })
      observeEvent(input$rank_list_2,{
        req(rv_tdx$data_dq)
        examp <- paste(rv_tdx$data_dq[1, input$rank_list_2, with=FALSE],collapse="-")
        output$examp_study <- renderText(examp)
      })
      observeEvent(c(rv_tdx$data_dq,
                     rv_tdx$locs),{

          updateSelectInput(session, inputId = "obs_trait",choices = sort(unique(rv_tdx$data_dq$observationVariableName)))
          updatePickerInput(session, inputId = "dis_trait",
                            choices = sort(unique(rv_tdx$data_dq$observationVariableName)),
                            selected = sort(unique(rv_tdx$data_dq$observationVariableName)),
                            options = list(`actions-box` = TRUE,
                                            size = 15,
                                           `live-search` = TRUE))
          updatePickerInput(session, inputId = "dis_study",
                            choices = sort(unique(rv_tdx$data_dq$label_study)),
                            selected = sort(unique(rv_tdx$data_dq$label_study)),
                            options = list(`actions-box` = TRUE,
                                           size = 15,
                                           `live-search` = TRUE))
          
          #ct <- dcast(isolate(rv_tdx$data_dq)[observationLevel=="PLOT", .N, .(study=paste0(studyDbId,"-",locationName),Variable=observationVariableName)],
          ct <- dcast(isolate(rv_tdx$data_dq)[observationLevel=="PLOT", .N, .(study=label_study,Variable=observationVariableName)],
                      Variable~study, fill = 0)
          rv_tdx$counts <- ct
          vnd <- melt(ct, variable.name = "StudyLocation")[value==0,.(StudyLocation, Variable)]
          rv_tdx$var_no_dat <- vnd
      })
      
      observeEvent(input$rank_list_2, {
        x <- copy(rv_tdx$data_dq)
        st <- copy(rv_tdx$st)
        if (!any(input$rank_list_2=="studyDbId")){
          envtnames <- c(input$rank_list_2,"studyDbId")
        } else {
          envtnames <- input$rank_list_2
        }
        x[, label_study:=do.call(paste, c(.SD, sep="-")), .SDcols=envtnames]
        st[, label_study:=do.call(paste, c(.SD, sep="-")), .SDcols=envtnames]
        rv_tdx$data_dq <- x
        rv_tdx$st <- st
        rv_tdx$study_no_dat <- st[!studyDbId%in%x$studyDbId, .(studyDbId,locationDbId,countryName,studyName,locationName, label_study)]
      })
      
      observeEvent(c(input$outslid,rv_tdx$data_dq),{
        req(rv_tdx$data_dq, input$outslid)
        data_dq <- rv_tdx$data_dq
        cdout0 <- data_dq[observationValue==0, .(reason="value=0",studyDbId, label_study, observationVariableDbId, observationVariableName, observationValue, germplasmName, observationDbId, replicate, blockNumber, plotNumber, entryNumber)]
        #norm_var <- data_dq[scale.dataType=="Numerical" & observationValue!=0][data_dq[!is.na(observationValue),.(sd=sd(observationValue)),.(studyDbId,observationVariableDbId)][sd!=0],on=.(studyDbId,observationVariableDbId)][!is.na(observationValue)][,.(shapiro.test(observationValue)$`p.value`),.(studyDbId,observationVariableDbId, observationVariableName)][V1>=0.05]
        #cdoutbp <-data_dq[data_dq[norm_var, on=.(studyDbId,observationVariableDbId)][,.(observationValue=boxplot.stats(observationValue, coef = input$outslid)$out),.(studyDbId,observationVariableDbId)],on=.(studyDbId,observationVariableDbId, observationValue)][, .(reason="boxplot-outliers",studyDbId, study_label, observationVariableDbId,observationVariableName, observationValue, germplasmName, replicate, blockNumber, plotNumber, entryNumber)]
        cdoutbp <- data_dq[data_dq[,find_outlier(observationValue,input$outslid),.(studyDbId,observationVariableDbId)]$V1==TRUE][, .(reason="boxplot-outliers",studyDbId, label_study, observationVariableDbId,observationVariableName, observationValue, germplasmName, observationDbId, replicate, blockNumber, plotNumber, entryNumber)]
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
        datatable(rv_tdx$cdout, filter = "top", options = list(paging = FALSE,searching = TRUE), selection = "single", callback = js) |>
          formatRound(columns = "observationValue", digits = 2)
      })
      observeEvent(input$dt_dblclick, {
        dt_row <- input$dt_dblclick$row             # 1-based Position on current display
        all_rows <- input$candidat_out_rows_all     # Current mapping to original data.frame rows (1-based)
        if(!is.null(dt_row) && !is.null(all_rows)) {
          df_row <- all_rows[dt_row] 
        #updateSelectizeInput(session, inputId = "obs_study",selected = rv_tdx$cdout[df_row,label_study])
        rv_tdx$obs_study_to_sel <- rv_tdx$cdout[df_row,studyDbId]
        updateSelectInput(session, inputId = "obs_trait",selected = rv_tdx$cdout[df_row,observationVariableName])
        nav_select(id = "tabset", selected = "observ", session = session)
        }
      })
      
      
      observeEvent(c(input$obs_trait, rv_tdx$data_dq), {
        req(rv_tdx$data_dq)
        obs_study_data <- rv_tdx$data_dq[observationVariableName==input$obs_trait,.N,.(studyDbId, locationName, studyName,countryName, label_study)]
        updateSelectizeInput(session,
                              inputId = "obs_study",
                              selected = rv_tdx$obs_study_to_sel,
                              #selected = input$obs_study,
                              server=TRUE,
                              choices = obs_study_data,
                              options = list(valueField='studyDbId',
                                            labelField='label_study',
                                            searchField=c('studyName',"locationName",'countryName'),
                                            render="label_study"
                                            #render = I("{option: function(item, escape) {
                                            #        return '<div><strong>'+ escape(item.studyDbId) +'-'+ escape(item.locationName) + '</strong> (' + escape(item.countryName) + ') ('+ escape(item.N)+ ')</div>';
                                            #}}")
                                          )
        )
        rv_tdx$obs_btable <- data.table()[0L]
        rv_tdx$obs_study_to_sel <- NULL
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
        return(50+length(unique(rv_tdx$data_dq[label_study%in%input$dis_study & observationVariableName%in%input$dis_trait, observationVariableName])) * input$boxplot_baseheight)
      })
      dynamicWidth <- reactive({
        req(length(input$dis_study)>0)
        return(250+length(input$dis_study) * input$boxplot_basewidth)
      })
      
      output$boxplots<-bindEvent(renderPlot({
            req(rv_tdx$data_dq)
            req(rv_tdx$data_dq[observationVariableName%in%input$dis_trait & label_study%in%input$dis_study,.N]>0)
            data_dq <- rv_tdx$data_dq[observationVariableName%in%input$dis_trait & label_study%in%input$dis_study]
            data_dq[, facetcols := gsub("\\-","\n",label_study)]
            data_dq[, facetrows := paste0("V: ",observationVariableName,"\n",
                                          "T: ",trait.name, "\n",
                                          "M: ",method.methodName,"\n",
                                          "S: ",scale.scaleName)]
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
        }, width = function() {
          dynamicWidth()
        }), input$refresh_dist)          



      
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
            x = label_study
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
        rv_tdx$obs_btable <- brushedPoints(rv_tdx$data_dq[studyDbId==input$obs_study & observationVariableName==input$obs_trait],
                                          input$observ_boxplot_brush)

        output$copy_obs_table <- renderUI({
          req(nrow(rv_tdx$obs_btable)>0)
          rclipboard::rclipButton("clipbtnobs_table", "Copy observations table", paste(paste(colnames(rv_tdx$obs_btable),collapse="\t"),
                                                                                      paste(apply(rv_tdx$obs_btable,1,paste,collapse="\t"),collapse = "\n"),
                                                                                      sep="\n"))#, shiny::icon("clipboard"))
        })
      })
      
      output$selected_obs <- DT::renderDataTable({
        visibcols <- c("studyName",
                       "label_study",
                       "trait.name",
                       "observationVariableName",
                       "observationValue",
                       "plotNumber",
                       "germplasmName",
                       "entryNumber",
                       "blockNumber",
                       "replicate",
                       "positionCoordinateX",
                       "positionCoordinateY",
                       "observationTimeStamp",
                       "observationDbId",
                       "observationUnitDbId",
                       "germplasmDbId")
        req(nrow(rv_tdx$obs_btable)>0)
        if (input$selected_obs_otherreps){
          obstable <- rv_tdx$data_dq[germplasmName%in%rv_tdx$obs_btable$germplasmName & studyDbId==input$obs_study & observationVariableName==input$obs_trait,][order(germplasmName)]
          obstable[, is_bold:="normal"]
          obstable[observationUnitDbId %in% rv_tdx$obs_btable$observationUnitDbId, is_bold:="bold"]
        } else {
          obstable <- copy(rv_tdx$obs_btable)
          obstable[, is_bold:="normal"]
        }
        setcolorder(obstable, visibcols)
        #browser()
        datatable(obstable,
                  extensions = 'Buttons',
                  options = list(paging = FALSE,
                                 dom = 'Bt',
                                 buttons = c('colvis'),
                                 searching = FALSE,
                                 columnDefs = list(list(visible = FALSE, 
                                                        targets = which(!names(obstable)%in%visibcols))
                                                   )
                                 ),
                  selection = "none") |>
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
