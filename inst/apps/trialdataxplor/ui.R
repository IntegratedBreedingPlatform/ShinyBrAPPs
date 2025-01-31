library(shinybrapps)
source("config.R")

ui <- bslib::page_fluid(theme = bslib::bs_theme(preset = "bootstrap"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css")
  ),
  # Static App banner
  tags$div(div(img(src="img/sticker.png",
                   border.radius="6px",
                   style="border-radius: 6px;
                          width:29px;
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
  #shinysky::busyIndicator(wait = 200, text = NULL),
  rclipboard::rclipboardSetup(),
  
  div(class = "container-fluid",
      mod_banner_ui("banner"),
      mod_get_studydata_ui("get_studydata"),
      mod_trialdataxplor_ui("xplor")
      )

  )
