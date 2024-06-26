library(shinybrapps)
source("config.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css")
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
  shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),
  div(class = "container-fluid",
      tags$section(
        mod_banner_ui("banner"),
        mod_get_studydata_ui("get_studydata"),
        mod_get_extradata_ui("get_extradata"),
        mod_scatterplot_ui("scatterplot")
      )
  ),
)
