#' @export
mod_banner_ui <- function(id){
  ns <- NS(id)
    uiOutput(ns("banner"))
}

#' @export
mod_banner_server <- function(id, rv, appname){
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      output$banner <- renderUI({
        req(rv$data)
        tags$div(
          div(img(src="img/ibpcirad.png", height="34px", border.radius="6px",
                  style="border-radius: 6px;
                width:129px;
                height:34px;
                margin-right:10px"),paste(appname," - Study: ", paste(rv$data[,unique(trialName)], collapse = ", ")),
          style="font: 500 20px/32px Roboto,Helvetica Neue,sans-serif;display: flex;justify-content: center;position: relative; z-index: 30;",
          div(style="margin-left: auto;",
              dropdown(
                #style = "unite", 
                status = "royal", width = "200px",
                #inputId = "mydropdown",
                label = "Download data",
                icon = icon("fas fa-download"),
                size="sm",
                right=TRUE,
                #status = "royal",
                #circle = FALSE,
                #downloadBttn(ns("dwnld1"),label = "As csv", style = "minimal", size="xs"),
                downloadButton(ns("dwnld1"),label = "As csv file"),
                if (appname!="BMS trial data explorer") downloadButton(ns("dwnld2"),label = "As TD object")
              ))),
        style = "padding: 2px 0 2px 10px;
                    width:100%;
                    font-weight: 700;
                    min-height: auto;
                    color: #fff;
                    background-color: #225691;
                    margin-bottom: 5px;
                    border-color: #357ebd;
                    position: absolute;
                    top: 0;
                    left: 3px;")
      })
      output$dwnld1 <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          if (appname=="DSBrAPP"){
            write.csv(rv$extradata, file, row.names = F)
          } else {
            if (appname=="STABrAPP"){
              write.csv(rv$data, file, row.names = F)
            } else {
              if (appname=="BMS trial data explorer"){
                write.csv(rv$data[,-c("facetrows", "facetcols"), with=F], file, row.names = F)
              }
            }
          }
        }
      )
      output$dwnld2 <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".Rdata", sep="")
        },
        content = function(file) {
          if (appname=="DSBrAPP"){
            write.csv(rv$data, file, row.names = F)
            if (is.null(rv$TD)){
              showNotification("You must select a Trait and a Variable to use as Environment Name to form the TD object (all traits will be included)", type = "error", duration = notification_duration)
            } else {
              TD <- rv$TD
              TDb <- rbindlist(TD)
              TDb <- TDb[,.SD,.SDcols = colnames(TDb)[!colnames(TDb)%in%rv$column_datasource[source=="Means", cols]]]
              TDb <- rv$extradata[,.SD, .SDcols = c("observationUnitDbId", rv$column_datasource[source=="Means", cols])][TDb, on=.(observationUnitDbId)]
              TD <- split(TDb, f = TDb$trial)
              save(TD, file = file)
            }
          } else {
            if (appname=="STABrAPP"){
              if (is.null(rv$TD)){
                showNotification("Please fit a model first", type = "error", duration = notification_duration)
              } else {
                TD <- rv$TD
                save(TD, file = file)
              }
            }
          }
        }
      )
      
    }
  )
}
