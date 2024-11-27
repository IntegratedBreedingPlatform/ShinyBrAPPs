library(shinybrapps)
source("config.R")
ui <- fluidPage(
  #theme = bslib::bs_theme(version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css"),
    tags$style(HTML("
    
      /* Ensure that the 'Deselect All' button stays on top */
      .inner.show {
        overflow: visible !important;
      }
      .dropdown-menu .bs-actionsbox {
        z-index: 1000;
      }
      /* Adjust the behavior of individual items (dropdown-item) */
      .dropdown-menu .dropdown-item {
        z-index: 10;  /* keep elements below the action box */
        position: relative;  /* Ensure that the checkmark is well positioned */
      }
      /* Modify the position of the checkmark so it doesn't overlap other elements */
      .dropdown-menu .check-mark {
        position: relative;
        margin-left: 5px;  /* Adjust the margin for better alignment */
        z-index: 5;
      }
      /* Ensure that the 'Deselect All' button is not covered */
      .dropdown-menu .bs-actionsbox button {
        z-index: 999;
        position: relative;
      }
      
      .shiny-options-group .checkbox-inline {
        padding-left: 0;
        margin-right: 5px;
      }
      .shiny-options-group .checkbox-inline input {
        margin-right: 5px;
      }
      .nav .nav-item .nav-link { font-size: 20px; }
    "))
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
      bslib::page_navbar(
        bslib::nav_panel(
          title = "Data Quality",
          mod_dataquality_ui("dataquality")
        ),
        bslib::nav_panel(
          title = "Model",
          mod_model_ui('model')
        )
      )
  )
)
