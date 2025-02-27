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
        }")
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
                         bslib::nav_panel("Print labels",value="printlabels"
                           
                         ),
                         bslib::nav_spacer(),
                         bslib::nav_panel(
                           title = "About",
                           h1("SamplR"),
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
                           p("SamplR development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en"), ", under the DESIRA initiative of the European Union"),
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

#' @import brapirv2
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
            
          pr <- brapi_get_programs(rv$con)
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
          showNotification("Connection succesful", type = "message", duration = notification_duration)
          shinyjs::runjs('
          var accordionBody = $("#connect-connectAccPanel");
          var accordionPanel = accordionBody.parent();
          accordionPanel.collapse("hide");')
          },
          error=function(e){
            showNotification("Check url, token and/or cropDb", type = "error", duration = notification_duration)
          })
        })
      })
      
          observeEvent(input$program,{
            if (input$program!=""){
              tr <-brapi_get_trials(rv$con, programDbId = input$program)
              setDT(tr)
              updateSelectInput(session = session, inputId = "trial", choices = tr$trialName, selected = NULL)
              rv_samp$tr <- tr
            }
          })
          
          observeEvent(input$trial,{
            if (input$trial!=""){
              selectedtr <- as.character(rv_samp$tr[trialName==input$trial, trialDbId])
              st <- brapi_get_studies(rv$con, trialDbId = selectedtr)
              setDT(st)
              st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
              #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
              #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
              locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
                brapi_get_locations(rv$con, locationDbId = l)
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
            }
          })
          observeEvent(input$get_samples,{
            if (is.null(input$studies)){
              selectedst <- unique(rv_samp$st$studyDbId)
            } else {
              selectedst <- input$studies
            }
            
            stdata <- lapply(selectedst, function(s){
              samp_srDbId <- brapi_post_search_samples(rv$con, studyDbIds = s)
              samples <- setDT(brapi_get_search_samples_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId$searchResultsDbId))
              samples.pag <- attr(samples, which = "pagination")
              if (samples.pag$pageSize < samples.pag$totalCount){
                for (p in (1:(samples.pag$totalPages-1))){
                  nextp <- setDT(brapi_get_search_samples_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId$searchResultsDbId, page = p))
                  samples <- rbind(samples, nextp, fill=T)
                }
              }
              if (nrow(samples>0)){
                os <- setDT(brapi_get_search_observationunits_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_observationunits(rv$con, observationUnitDbIds = samples$observationUnitDbId)$searchResultsDbId))
                os <- os[,.(observationUnitDbId,trialName,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
                gp <- setDT(brapi_get_search_germplasm_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_germplasm(rv$con, germplasmDbIds = samples$germplasmDbId)$searchResultsDbId))
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
            #}
          })
          observeEvent(input$sample_UID,{
            req(input$sample_UID)
            samp_srDbId <- brapi_post_search_samples(rv$con, sampleDbIds = input$sample_UID)
            samples <- setDT(brapi_get_search_samples_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId$searchResultsDbId))
            if (nrow(samples>0)){
              
              os <- setDT(brapi_get_search_observationunits_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_observationunits(rv$con, observationUnitDbIds = samples$observationUnitDbId)$searchResultsDbId))
              os <- os[,.(observationUnitDbId,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
              gp <- setDT(brapi_get_search_germplasm_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_germplasm(rv$con, germplasmDbIds = unique(samples$germplasmDbId))$searchResultsDbId))
              gp <- unique(gp[,.(germplasmDbId, germplasmName, pedigree)])
              bc1 <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][samples, on=.(observationUnitDbId)]
              bc2 <- os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT",.(observationUnitDbId,PLOT=observationUnitPosition.observationLevelRelationships.levelCode)][bc1, on=.(observationUnitDbId)]
              bc3 <- gp[bc2, on=.(germplasmDbId)]
              #bc4 <- gs[,.(germplasmUUID,names)][bc3, on=.(germplasmUUID=germplasmDbId)]
              
              st <- as.data.table(do.call(gtools::smartbind,lapply(unique(bc3$studyDbId), function(s) brapi_get_studies(rv$con, studyDbId = s))))
              st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
              #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
              #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
              locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
                brapi_get_locations(rv$con, locationDbId = l)
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
          }
          )
          
          observeEvent(input$get_samples_byg,{
            if (!is.null(input$germp_search)){
              nf <- switch(input$germp_s_how,
                           `Starts with`="STARTSWITH",
                           `Ends with`="ENDSWITH",
                           `Exact Match`="EXACTMATCH",
                           `Contains`="CONTAINS")
              gs_srdbid <- bmsapi_post_germplasm_search(rv$con, nameFilter = list(type=nf,value=input$germp_search))
              gs <- bmsapi_get_germplasm_search_searchResultsDbId(rv$con, gs_srdbid)
              if (nrow(gs)>0){
                gs <- gs[,-which(names(gs)%in%c("attributeTypesValueMap","nameTypesValueMap"))]
                setDT(gs)
                guids <- gs$germplasmUUID
                samp_srDbId <- brapi_post_search_samples(rv$con, germplasmDbIds = guids)
                samples <- setDT(brapi_get_search_samples_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId$searchResultsDbId))
                samples.pag <- attr(samples, which = "pagination")
                if (samples.pag$pageSize < samples.pag$totalCount){
                  for (p in (1:(samples.pag$totalPages-1))){
                    nextp <- setDT(brapi_get_search_samples_searchResultsDbId(rv$con, searchResultsDbId = samp_srDbId$searchResultsDbId, page = p))
                    samples <- rbind(samples, nextp, fill=T)
                  }
                }
                if (nrow(samples>0)){
                  os <- setDT(brapi_get_search_observationunits_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_observationunits(rv$con, observationUnitDbIds = samples$observationUnitDbId)$searchResultsDbId))
                  os <- os[,.(observationUnitDbId,studyName,locationName,germplasmName,observationUnitPosition.observationLevelRelationships.levelName,observationUnitPosition.observationLevelRelationships.levelCode)]
                  gp <- setDT(brapi_get_search_germplasm_searchResultsDbId(rv$con, searchResultsDbId = brapi_post_search_germplasm(rv$con, germplasmDbIds = unique(samples$germplasmDbId))$searchResultsDbId))
                  gp <- unique(gp[,.(germplasmDbId, germplasmName, pedigree)])
                  bc1 <- os[observationUnitPosition.observationLevelRelationships.levelName=="REP",.(observationUnitDbId,REP=observationUnitPosition.observationLevelRelationships.levelCode)][samples, on=.(observationUnitDbId)]
                  bc2 <- os[observationUnitPosition.observationLevelRelationships.levelName=="PLOT",.(observationUnitDbId,PLOT=observationUnitPosition.observationLevelRelationships.levelCode)][bc1, on=.(observationUnitDbId)]
                  bc3 <- gp[bc2, on=.(germplasmDbId)]
                  bc4 <- gs[,.(germplasmUUID,names)][bc3, on=.(germplasmUUID=germplasmDbId)]
                  
                  st <- as.data.table(do.call(gtools::smartbind,lapply(unique(bc3$studyDbId), function(s) brapi_get_studies(rv$con, studyDbId = s))))
                  st <- unique(st[,.(studyDbId,studyName, locationDbId, locationName)])
                  #locs <- brapi_get_search_locations_searchResultsDbId(rv$con,
                  #                                                     searchResultsDbId = brapi_post_search_locations(rv$con, locationDbIds = unique(st$locationDbId))$searchResultsDbId)
                  locs <- do.call(gtools::smartbind, lapply(unique(st$locationDbId), function(l){
                    brapi_get_locations(rv$con, locationDbId = l)
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
                }} else {
                  showModal(modalDialog("No germplasm found", fade = F))
                  rv_samp$stdatadt_byg <- data.table()
                  
                }
              
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
