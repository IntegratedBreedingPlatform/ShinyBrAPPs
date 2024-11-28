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
      output$banner <- renderUI({
        req(rv$data)
        tags$div(
          div(img(src="img/ibpcirad.png", height="34px", border.radius="6px",
                  style="border-radius: 6px;
                width:129px;
                height:34px;
                margin-right:10px"),paste(appname," - Study: ", paste(rv$data[,unique(trialName)], collapse = ", ")),
          style="font: 500 20px/32px Roboto,Helvetica Neue,sans-serif;"),
        style = "padding: 2px 0 2px 10px;
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
    }
  )
}
