library(shinybrapps)
source("config.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css")
  ),
  # Static App banner
  tags$div(div(img(src="img/ibpcirad.png",
                   height="34px",
                   border.radius="6px",
                   style="border-radius: 6px;
                          width:129px;
                          height:34px;
                          margin-right:10px"),
               appname,
               style="font: 500 20px/32px Roboto,Helvetica Neue,sans-serif;"),
           style = "padding: 2px 0 2px 10px;
                    font-weight: 700;
                    min-height: auto;
                    color: #fff;
                    background-color: #225691;
                    margin-bottom: 5px;
                    border-color: #357ebd;"),
  shinyjs::useShinyjs(),
  #shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-left", margins = c(110, 10), color = "#225691", height = 25, width =25, timeout = 200), 
  
  div(class = "container-fluid",
      mod_banner_ui("banner"),
      mod_get_studydata_ui("get_studydata"),
      tabsetPanel(id = "tabsetPanel_main",
                  # selected = "Model",
                  selected = "Data Quality",
                  tabPanel(
                    "Data Quality",
                    mod_dataquality_ui("dataquality")
                  ),
                  tabPanel(
                    "Model",
                    mod_model_ui('model')
                  )
      )
  )
)
