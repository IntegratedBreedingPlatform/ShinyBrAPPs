#' @import tableHTML
#' @import DT
#' @export
mod_samplr_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
  div(class = "container-fluid",
      # Sidebar
      tags$style(
        HTML(
          ".nav .nav-item .nav-link { font-size: 15px; }",
        ),
        HTML("
        .navbar-nav > li{
          margin-left:20px;
          margin-right:20px;
        }"),
        HTML("
      .card-body select {
        z-index: 1000; /* Assurez-vous que la liste déroulante est au-dessus de tout autre élément */
      }
      .dropdown-menu {
        position: absolute !important;
        top: auto !important;
        left: auto !important;
        right: auto !important;
        bottom: auto !important;
        z-index: 1050; /* Assurez-vous que la liste déroulante est au-dessus de tout autre élément */
      }
         .bslib-card, .tab-content, .tab-pane, .card-body {
      overflow: visible !important;
    }
    "),
        HTML("
      .flex-container {
        display: flex;
        gap: 50px;
        overflow-x: visible;
        align-items: flex-end;
        margin-left: 50px;
        }
      .flex-item {
        flex: 1; /* Permet aux éléments de prendre l'espace disponible */
      }
    ")
    ),
      bslib::page_navbar(title = "", id = ns("tabsetId"), 
                         bslib::nav_panel("Search samples by trial/study",value="bystudy",
                                          layout_sidebar(
                                                         sidebar=bslib::sidebar(width = 350,
                                                         selectInput(ns("program"),label = "Program", choices = NULL, selected = NULL),
                                                         selectInput(ns("trial"),label = "Study name", choices = NULL),
                                                         selectizeInput(ns("studies"),label = "Environments", choices = NULL, multiple=TRUE),
                                                         div(style="display:flex;gap: 10px;",
                                                             actionButton(ns("get_samples"),"Get samples"),
                                                             uiOutput(ns("dbutton_container"))),
                                                         tags$style(make_css(list('.container-fluid', 'padding-left', '0px'))),
                                                         tags$style(make_css(list('.container-fluid', 'padding-right', '0px')))
                                                         ),
                                                        card(full_screen = TRUE, height = "100%",
                                                             card_body(min_height = "600px",
                                                             div(DTOutput(ns("sample_table")), style = "font-size: 75%;")
                                                             )
                                                             )
                                          )
                                          ),
                         bslib::nav_panel("Search samples by germplasm name",value="bygermplasm",
                                          layout_sidebar(width = 1/3,
                                                         sidebar=bslib::sidebar(width = 350,
                                                                                textInput(ns("germp_search"), "Search Germplasm by name"),
                                                                                radioGroupButtons(ns("germp_s_how"), status= "custom-class", choices = c("Starts with","Ends with","Exact Match","Contains")),
                                                                                div(style="display:flex;gap: 10px;",
                                                                                    actionButton(ns("get_samples_byg"),"Get samples"),
                                                                                    uiOutput(ns("dbutton_byg_container"))),
                                                                                tags$style(make_css(list('.container-fluid', 'padding-left', '0px'))),
                                                                                tags$style(make_css(list('.container-fluid', 'padding-right', '0px')))
                                                         ),
                                                         card(full_screen = TRUE,
                                                              card_body(min_height = "600px",
                                                              div(DTOutput(ns("sample_table_byg")), style = "font-size: 75%;")
                                                             ))
                                                        )
                                          ),
                         bslib::nav_panel("Get sample details",value="sampledetails",
                                          layout_sidebar(width = 1/3,
                                                         sidebar=bslib::sidebar(width = 350,
                                                                                textInput(ns("sample_UID"), "SampleDbId")
                                                                                ),
                                                         card(full_screen = TRUE,
                                                              card_body(min_height = "600px",
                                                                        
                                                              div(DTOutput(ns("one_sample_table")), style = "font-size: 75%;")
                                                              ))
                                          )
                         ),
                         bslib::nav_panel("Print labels",value="printlabels",
                                          layout_sidebar(width = 1/3,
                                                         sidebar=bslib::sidebar(width = 350,
                                                                                selectInput(ns("label_layout"), "Label layout", choices =  names(label_layouts)),
                                                                                selectInput(ns("label_page"), "Page settings", choices = label_sizes$Template),
                                                                                radioGroupButtons(
                                                                                  inputId = ns("barcode_type"),
                                                                                  label = "Barcode type",
                                                                                  choices = c(QRcode="qr", Barcode="128"),
                                                                                  status = "info",
                                                                                  checkIcon = list(
                                                                                    yes = icon("ok", lib = "glyphicon"),
                                                                                    no = icon("xmark", lib = "glyphicon"))
                                                                                ),
                                                                                div(class = "p-3 flex-container",
                                                                                    div(class = "flex-item", numericInput(ns("fontsize"), label = "Font size", value = 6, min = 4, max = 12, step = 1)),
                                                                                    div(class = "flex-item", checkboxInput(ns("labelbold"), label = "Bold"))
                                                                                ),
                                                                                div(class = "p-3 flex-container",
                                                                                    div(class = "flex-item", checkboxInput(ns("inborder"), label = "Inner border", value = TRUE)),
                                                                                    div(class = "flex-item", checkboxInput(ns("outborder"), label = "Outer border", value = TRUE))
                                                                                ),
                                                                                downloadButton(ns("print_labels"),"Print labels")

                                                         ),
                                                         card(full_screen = FALSE,
                                                              card_body(min_height = "400px",
                                                              uiOutput(ns("label_inputs"))
                                                              )
                                                              ),
                                                         card(full_screen = FALSE,
                                                              card_body(min_height = "400px",
                                                              plotOutput(ns("label_preview"), width = "300px", height = "200px")
                                                              )
                                                         )
                                                         

                                          )
                         ),
                         bslib::nav_spacer(),
                         bslib::nav_panel(
                           title = "About",
                           h1("SampleR"),
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
                           p("SamplR development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en", target="_blank"), ", under the DESIRA initiative of the European Union"),
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

#' @export
mod_samplr_server <- function(id, rv){
  
  moduleServer(
    
    id,
    function(input, output, session){
      rv_samp <- reactiveValues(
        tr=NULL,
        stdatadt=NULL,
        stdatadt_one=NULL,
        stdatadt_byg=NULL
      )
      
      ns <- session$ns
      observeEvent(rv$con,{
        withProgress(message = "Getting programs", value = 0, {
          incProgress(1)
          tryCatch({            
            pr <- handle_api_response(brapir::core_programs_get(rv$con))$data
            updateSelectizeInput(session,
                                inputId = "program",
                                selected = NULL,
                                server=TRUE,
                                choices = pr,
                                options = list(valueField='programDbId',
                                                labelField='programName',
                                                render = I("{option: function(item, escape) {
                                                    return '<div><strong>'+ escape(item.programName) + '</strong></div>';
                                          }}"))
            )
            if (rv$connect_mode=="UI"){
              showNotification("Connection successful", type = "message", duration = notification_duration)
              shinyjs::runjs('
                var accordionBody = $("#connect-connectAccPanel");
                var accordionPanel = accordionBody.parent();
                accordionPanel.collapse("hide");'
              )
            }
          }, error=function(e){
            showNotification(paste0(e$message, "- Check url, token and/or cropDb"), type = "error", duration = notification_duration)
          })
        })
      })
      
      observeEvent(input$program,{
        if (input$program!=""){
          tryCatch({
            tr <- handle_api_response(brapir::core_trials_get(rv$con, programDbId = input$program))$data
            setDT(tr)
            updateSelectInput(session = session, inputId = "trial", choices = tr$trialName, selected = NULL)
            rv_samp$tr <- tr
          }, error=function(e){
            showNotification(paste0(e$message, "- Check url, token and/or cropDb"), type = "error", duration = notification_duration)
          })
        }
      })
          
      observeEvent(input$trial,{
        if (input$trial!=""){
          tryCatch({
            selectedtr <- as.character(rv_samp$tr[trialName==input$trial, trialDbId])
            st <- handle_api_response(brapir::core_studies_get(rv$con, trialDbId = selectedtr))$data
            setDT(st)
            st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
            #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
            #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
            locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
              handle_api_response(brapir::core_locations_get(rv$con, locationDbId = l))$data
            }))
            setDT(locs)
            if (!any(colnames(locs)=="countryName")) locs[,countryName:=NA]
            rv_samp$st <- locs[,.(locationDbId,countryName)][st, on=.(locationDbId)]
            updateSelectizeInput(session,
                                  inputId = "studies",
                                  selected = NULL,
                                  server=TRUE,
                                  choices = rv_samp$st,
                                  options = list(valueField='studyDbId',
                                                labelField='locationName',
                                                searchField=c('studyName',"locationName",'countryName'),
                                                render = I("{option: function(item, escape) {
                                                  return '<div><strong>'+ escape(item.locationName) + '</strong> (' + escape(item.countryName) + ')</div>';
                                      }}"))
            )
          }, error=function(e){
            showNotification(paste0(e$message), type = "error", duration = notification_duration)
          })
        }
      })


      observeEvent(input$get_samples,{
        if (is.null(input$studies)){
          selectedst <- unique(rv_samp$st$studyDbId)
        } else {
          selectedst <- input$studies
        }
        tryCatch({
          stdata <- lapply(selectedst, function(s){
            samp_srDbId <- handle_api_response(brapir::genotyping_samples_post_search(rv$con, studyDbIds = s))$data$searchResultsDbId
            resp <- handle_api_response(brapir::genotyping_samples_get_search_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId))
            samples <- setDT(resp$data)
            samples.pag <- resp$metadata$pagination
            if (samples.pag$pageSize < samples.pag$totalCount){
              for (p in (1:(samples.pag$totalPages-1))){
                nextp <- setDT(handle_api_response(brapir::genotyping_samples_get_search_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId, page = p))$data)
                samples <- rbind(samples, nextp, fill=T)
              }
            }
            if (nrow(samples>0)){
              srid <- handle_api_response(brapir::phenotyping_observationunits_post_search(rv$con, observationUnitDbIds = samples$observationUnitDbId))$data$searchResultsDbId
              os <- setDT(handle_api_response(brapir::phenotyping_observationunits_get_search_searchResultsDbId(rv$con, searchResultsDbId = srid))$data)
              os <- setDT(tidyr::unnest(os, cols = "observationUnitPosition.observationLevelRelationships", names_sep = ".", keep_empty = T))
              os <- os[,.(observationUnitDbId,trialName,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
              srid2 <- handle_api_response(brapir::germplasm_germplasm_post_search(rv$con, germplasmDbIds = samples$germplasmDbId))$data$searchResultsDbId
              gp <- setDT(handle_api_response(brapir::germplasm_germplasm_get_search_searchResultsDbId(rv$con, searchResultsDbId = srid2))$data)
              gp <- gp[,.(germplasmDbId, germplasmName, pedigree)]
              bc1 <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][samples, on=.(observationUnitDbId)]
              bc2 <- os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT",.(observationUnitDbId,PLOT=observationUnitPosition.observationLevelRelationships.levelCode, trialName)][bc1, on=.(observationUnitDbId)]
              bc3 <- gp[bc2, on=.(germplasmDbId)]
              bc <- rv_samp$st[,.(studyDbId,studyName, locationName)][bc3, on=.(studyDbId)]
              #bc <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT"][samples, on=.(observationUnitDbId)], on=.(observationUnitDbId)]
              colorder <- c("sampleDbId",
                            "sampleName",
                            "observationUnitDbId",
                            "programDbId",
                            "trialDbId",
                            "trialName",
                            "studyDbId",
                            "studyName",
                            "locationName",
                            "PLOT",
                            "REP",
                            "germplasmDbId",
                            "germplasmName",
                            "pedigree",
                            "sampleTimestamp",
                            "takenBy")
              colorder <- colorder[colorder%in%colnames(bc)]
              setcolorder(bc, colorder)
              return(unique(bc))
            } else {
              return(samples)
            }
          })
          stdata <- stdata[unlist(lapply(stdata, nrow))>0]
          if (length(stdata)>0){
            stdatadt <- data.table(crop=rv$con$commoncropname,rbindlist(stdata, fill = T))
            rv_samp$stdatadt <- stdatadt
          } else {
            showModal(modalDialog("There is no sample associated to this study", fade = F))
            rv_samp$stdatadt <- data.table()
          }
        }, error=function(e){
            showNotification(paste0(e$message), type = "error", duration = notification_duration)
        })
      })

      observeEvent(input$sample_UID,{
        req(input$sample_UID)
        samp_srDbId <- brapir::genotyping_samples_post_search(rv$con, sampleDbIds = input$sample_UID)$data$searchResultsDbId
        samples <- setDT(brapir::genotyping_samples_get_search_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId)$data)
        if (nrow(samples>0)){
          os <- setDT(brapir::phenotyping_observationunits_get_search_searchResultsDbId(rv$con, searchResultsDbId = brapir::phenotyping_observationunits_post_search(rv$con, observationUnitDbIds = samples$observationUnitDbId)$data$searchResultsDbId)$data)
          os <- setDT(tidyr::unnest(os, cols = "observationUnitPosition.observationLevelRelationships", names_sep = ".", keep_empty = T))
          os <- os[,.(observationUnitDbId,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
          gp <- setDT(brapir::germplasm_germplasm_get_search_searchResultsDbId(rv$con, searchResultsDbId = brapir::germplasm_germplasm_post_search(rv$con, germplasmDbIds = unique(samples$germplasmDbId))$data$searchResultsDbId)$data)
          gp <- unique(gp[,.(germplasmDbId, germplasmName, pedigree)])
          bc1 <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][samples, on=.(observationUnitDbId)]
          bc2 <- os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT",.(observationUnitDbId,PLOT=observationUnitPosition.observationLevelRelationships.levelCode)][bc1, on=.(observationUnitDbId)]
          bc3 <- gp[bc2, on=.(germplasmDbId)]
          #bc4 <- gs[,.(germplasmUUID,names)][bc3, on=.(germplasmUUID=germplasmDbId)]
          
          st <- as.data.table(do.call(gtools::smartbind,lapply(unique(bc3$studyDbId), function(s) handle_api_response(brapir::core_studies_get(rv$con, studyDbId = s))$data)))
          st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
          #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
          #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
          locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
            handle_api_response(brapir::core_locations_get(rv$con, locationDbId = l))$data
          }))
          setDT(locs)
          if (!any(colnames(locs)=="countryName")) locs[,countryName:=NA]
          rv_samp$st <- locs[,.(locationDbId,countryName)][st, on=.(locationDbId)]
          
          
          bc <- rv_samp$st[,.(studyDbId,studyName, locationName)][bc3, on=.(studyDbId)]
          #bc <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT"][samples, on=.(observationUnitDbId)], on=.(observationUnitDbId)]
          colorder <- c("sampleDbId",
                        "sampleName",
                        "observationUnitDbId",
                        "programDbId",
                        "trialDbId",
                        "studyDbId",
                        "studyName",
                        "locationName",
                        "PLOT",
                        "REP",
                        "germplasmDbId",
                        "germplasmName",
                        "names",
                        "pedigree",
                        "sampleTimestamp",
                        "takenBy")
          colorder <- colorder[colorder%in%colnames(bc)]
          setcolorder(bc, colorder)
          rv_samp$stdatadt_one <- data.table(crop=rv$con$commoncropname,bc)
        } else {
          showModal(modalDialog("There is no sample with this DbId", fade = F))
          rv_samp$stdatadt_one <- data.table()
        }
      })
          
      observeEvent(input$get_samples_byg,{
        if (!is.null(input$germp_search)){
          tryCatch({
            nf <- switch(input$germp_s_how,
                          `Starts with`="STARTSWITH",
                          `Ends with`="ENDSWITH",
                          `Exact Match`="EXACTMATCH",
                          `Contains`="CONTAINS")
            gs_srdbid <- bmsapi_post_germplasm_search(rv$con, nameFilter = list(type=nf,value=input$germp_search))$searchResultDbId
            gs <- bmsapi_get_germplasm_search_searchResultsDbId(rv$con, gs_srdbid)
            if (nrow(gs)>0){
              gs <- gs[,-which(names(gs)%in%c("attributeTypesValueMap","nameTypesValueMap"))]
              setDT(gs)
              guids <- gs$germplasmUUID
              samp_srDbId <- handle_api_response(brapir::genotyping_samples_post_search(rv$con, germplasmDbIds = guids))$data$searchResultsDbId
              resp <- handle_api_response(brapir::genotyping_samples_get_search_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId))
              samples <- setDT(resp$data)
              samples.pag <- resp$metadata$pagination
              if (samples.pag$pageSize < samples.pag$totalCount){
                for (p in (1:(samples.pag$totalPages-1))){
                  nextp <- setDT(handle_api_response(brapir::genotyping_samples_get_search_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId, page = p)))
                  samples <- rbind(samples, nextp, fill=T)
                }
              }
              if (nrow(samples>0)){
                srid <- handle_api_response(brapir::phenotyping_observationunits_post_search(rv$con, observationUnitDbIds = samples$observationUnitDbId))$data$searchResultsDbId
                os <- setDT(handle_api_response(brapir::phenotyping_observationunits_get_search_searchResultsDbId(rv$con, searchResultsDbId = srid))$data)
                os <- setDT(tidyr::unnest(os, cols = "observationUnitPosition.observationLevelRelationships", names_sep = ".", keep_empty = T))
                os <- os[,.(observationUnitDbId,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
                srid2 <- handle_api_response(brapir::germplasm_germplasm_post_search(rv$con, germplasmDbIds = unique(samples$germplasmDbId)))$data$searchResultsDbId
                gp <- setDT(handle_api_response(brapir::germplasm_germplasm_get_search_searchResultsDbId(rv$con, searchResultsDbId = srid2))$data)
                gp <- unique(gp[,.(germplasmDbId, germplasmName, pedigree)])
                bc1 <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][samples, on=.(observationUnitDbId)]
                bc2 <- os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT",.(observationUnitDbId,PLOT=observationUnitPosition.observationLevelRelationships.levelCode)][bc1, on=.(observationUnitDbId)]
                bc3 <- gp[bc2, on=.(germplasmDbId)]
                bc4 <- gs[,.(germplasmUUID,names)][bc3, on=.(germplasmUUID=germplasmDbId)]
                
                st <- as.data.table(do.call(gtools::smartbind,lapply(unique(bc3$studyDbId), function(s) brapir::core_studies_get(rv$con, studyDbId = s)$data)))
                st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
                #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
                #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
                locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
                  brapir::core_locations_get(rv$con, locationDbId = l)$data
                }))
                setDT(locs)
                if (!any(colnames(locs)=="countryName")) locs[,countryName:=NA]
                rv_samp$st <- locs[,.(locationDbId,countryName)][st, on=.(locationDbId)]
                
                
                bc <- rv_samp$st[,.(studyDbId,studyName, locationName)][bc4, on=.(studyDbId)]
                #bc <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT"][samples, on=.(observationUnitDbId)], on=.(observationUnitDbId)]
                colorder <- c("sampleDbId",
                              "sampleName",
                              "observationUnitDbId",
                              "programDbId",
                              "trialDbId",
                              "studyDbId",
                              "studyName",
                              "locationName",
                              "PLOT",
                              "REP",
                              "germplasmDbId",
                              "germplasmName",
                              "names",
                              "pedigree",
                              "sampleTimestamp",
                              "takenBy")
                colorder <- colorder[colorder%in%colnames(bc)]
                setcolorder(bc, colorder)
                rv_samp$stdatadt_byg <- data.table(crop=rv$con$commoncropname,bc)
              } else {
                showModal(modalDialog("There is no sample associated to this germplasm name", fade = F))
                rv_samp$stdatadt_byg <- data.table()
              }
            } else {
                showModal(modalDialog("No germplasm found", fade = F))
                rv_samp$stdatadt_byg <- data.table()                
            }
          }, error=function(e){
            showNotification(paste0(e$message), type = "error", duration = notification_duration)
          })
        }
      })
      
          observe({
            req(rv_samp$stdatadt)
            if (rv_samp$stdatadt[,.N]>0){
              output$sample_table <- DT::renderDT(rv_samp$stdatadt)
              output$dbutton_container<- renderUI(downloadButton(ns("download_data"),"Download samples info"))
            } else {
              output$sample_table <- DT::renderDT(NULL)
              output$dbutton_container<- renderUI(NULL)
            }
            
          })
          observe({
            req(rv_samp$stdatadt_byg)
            if (rv_samp$stdatadt_byg[,.N]>0){
              output$sample_table_byg <- DT::renderDT(rv_samp$stdatadt_byg)
              output$dbutton_byg_container<- renderUI(downloadButton(ns("download_data_byg"),"Download samples info"))
            } else {
              output$sample_table_byg <- DT::renderDT(NULL)
              output$dbutton_byg_container<- renderUI(NULL)
            }
            
          })
          observe({
            req(rv_samp$stdatadt_one)
            if (rv_samp$stdatadt_one[,.N]>0){
              output$one_sample_table <- DT::renderDT(rv_samp$stdatadt_one)
              #output$dbutton_byg_container<- renderUI(downloadButton("download_data_byg","Download samples info"))
            } else {
              output$one_sample_table <- DT::renderDT(NULL)
              #output$dbutton_byg_container<- renderUI(NULL)
            }
            
          })
          
          output$label_inputs <- renderUI({
            req(rv_samp$stdatadt)
            #browser()
            layout <- label_layouts[[input$label_layout]]
            choices <- colnames(rv_samp$stdatadt)
            num_rows <- length(unique(unlist(lapply(layout,function(a) a[[1]][2]))))
            num_cols <- length(unique(unlist(lapply(layout,function(a) a[[1]][1]))))
            
            generate_ui_with_grid(num_rows, num_cols, choices=choices, ns=ns)
          })
          
          #observe({
          #  req(rv_samp$stdatadt)
          #  updateSelectInput(session = session, inputId = "bc_field", choices = colnames(rv_samp$stdatadt))
          #})
          
          observe({
            output$label_preview <- renderPlot({
              layout <- label_layouts[[input$label_layout]]
              dat <- t(rv_samp$stdatadt[1,])[,1]
              cols <- sapply(1:length(layout), function(a) input[[paste0("select_",a)]])
              texts <- dat[match(cols,names(rv_samp$stdatadt))]
              bc <- dat[match(barcode_field,names(rv_samp$stdatadt))]
              if (input$labelbold){
                ff <- "bold"
              } else {
                ff <- "plain"
              }
              bct <-  input$barcode_type
              #cols <- cols[length(cols):1]
              #texts <- texts[length(texts):1]
              #browser()
              make_single_label(layout, texts = texts, labels = cols, bctype = bct, bc = bc, fontsize = rep(input$fontsize,length(layout)), fontface=rep(ff,length(layout)), inner.border = input$inborder)
            }, height = 100*label_sizes[Template==input$label_page,Lab.H.in], width = 100*label_sizes[Template==input$label_page,Lab.W.in])
          })

          output$print_labels <- downloadHandler(
            filename = function() {
              paste("labels-", Sys.Date(), ".pdf", sep="")
            },
            content = function(file) {
              layout <- label_layouts[[input$label_layout]]
              bct <- input$barcode_type
              if (input$labelbold){
                ff <- "bold"
              } else {
                ff <- "plain"
              }
              
              #browser()
              print_labels(filename = file,
                           data = rv_samp$stdatadt,
                           layout = layout,
                           columns = sapply(1:length(layout), function(a) input[[paste0("select_",a)]]),
                           bccol = barcode_field,
                           bctype = bct,
                           label_sizes = label_sizes[Template==input$label_page],
                           fontsize = input$fontsize, 
                           fontface = ff,
                           inner.border = input$inborder,
                           outer.border = input$outborder)
            }
          )
          
          output$download_data <- downloadHandler(
            filename = function() {
              paste("data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(rv_samp$stdatadt, file, row.names = F)
            }
          )
          
          output$download_data_byg <- downloadHandler(
            filename = function() {
              paste("data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(rv_samp$stdatadt_byg, file, row.names = F)
            }
          )
          
      
})
}
