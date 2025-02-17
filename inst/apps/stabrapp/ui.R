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
  #shinyjs::useShinyjs(),
  #shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-left", margins = c(110, 10), color = "#225691", height = 25, width =25, timeout = 200), 
  
  div(class = "container-fluid",
      mod_banner_ui("banner"),
      mod_get_studydata_ui("get_studydata"),
      div(style="position: relative; z-index: 10;",
      bslib::page_navbar(
        bslib::nav_panel(
          title = "Data Quality",
          mod_dataquality_ui("dataquality")
        ),
        bslib::nav_panel(
          title = "Model",
          mod_model_ui('model')
        ),
        bslib::nav_spacer(),
        bslib::nav_panel(
          title = "About",
          h1("STABrAPP"),
          img(src='img/sticker.png', height="178px", width="154px",  align = "right"),
          p("Single Trial Analysis BrAPP"),
          h2("Contributors"),
          p("Jean-François Rami (Maintainer) - rami 'at' cirad.fr"),
          p("Alice Boizet (Author) - alice.boizet 'at' cirad.fr"),
          p("Léo Valette (Author)"),
          p("Mariano Crimi (Author)"),
          img(src='img/ibpcirad.png', height="61px", width="231px",  align = "left"),
          br(),hr(),
          h2(a("github",href="https://github.com/IntegratedBreedingPlatform/ShinyBrAPPs", target="_blank", icon("github")), align="right"),
          hr(),hr(),
          h2("Funded by"),
          p("STABrAPP development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en"), ", under the DESIRA initiative of the European Union"),
          img(src='img/ABEE_logo_trspbckgd.png', height="57px", width="84px",  align = "right"),
          hr(),hr(),
          img(src='img/desira.png', height="56px", width="252px",  align = "right"),
          hr(),hr(),
          h2("Session info"),
          verbatimTextOutput("Rsi")
        )
      ),
      bslib::accordion(
        id = "excluded_accordion",
        bslib::accordion_panel(
          value = "excluded_panel",
          title = "Excluded observations",
          actionButton(
            inputId = "set_non_excluded_obs",
            label = "Include selected observations",
            disabled = T,
            class = "btn btn-info",
            style = "margin-bottom: 10px;"
          ),
          DT::dataTableOutput("excluded_obs_table")
        )
      )
    )
  )
)
