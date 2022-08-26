#' @export
mod_banner_ui <- function(id){
  ns <- NS(id)
    uiOutput(ns("banner"))
}

#' @export
mod_banner_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      output$banner <- renderUI({
        req(rv$data)
        tags$div(
          paste("STUDY: ", paste(rv$data[,unique(trialName)], collapse = ", ")),
          style = "
      padding: 10px 0 10px 34px;
      font-weight: 700;
min-height: auto;
      color: #fff;
background-color: #428bca;
margin-bottom: 5px;
border-color: #357ebd;")
      })
    }
  )
}
