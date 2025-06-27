library(shinybrapps)
source("config.R")

ui <- fluidPage(
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
  #shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-left", margins = c(110, 10), color = "#225691", height = 25, width =25, timeout = 200), 

  div(class = "container-fluid",
      tags$section(
        mod_banner_ui("banner"),
        mod_connect_ui("connect"),
        mod_get_studydata_ui("get_studydata"),
        mod_get_extradata_ui("get_extradata"),
        div(style="position: relative; z-index: 10;",
        bslib::page_navbar(
          sidebar = bslib::sidebar(
            id = "groups_sidebar",
            position = "right",
            open = F,
            width = 350,
            mod_groups_sidebar_ui("groups_sidebar")
          ),
          selected = "GxE Analysis",
          #position = "fixed-top",
          bslib::nav_panel(
            title = "GxE Analysis",
            mod_gxe_ui("gxe")
          ),
          bslib::nav_panel(
            title = "Correlate",
            mod_scatterplot_ui("scatterplot")
          ),
          bslib::nav_spacer(),
          bslib::nav_panel(
            title = "About",
            h1("BrAVISE"),
            img(src='img/sticker.png', height="178px", width="154px",  align = "right"),
            p("Decoding Data, Driving Decisions"),
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
            p("BrAVISE development was funded by the ", a("ABEE project", href="https://capacity4dev.europa.eu/projects/desira/info/abee_en", target="_blank"), ", under the DESIRA initiative of the European Union"),
            img(src='img/ABEE_logo_trspbckgd.png', height="57px", width="84px",  align = "right"),
            hr(),hr(),
            img(src='img/desira.png', height="56px", width="252px",  align = "right"),
            hr(),hr(),
            h2("Session info"),
            verbatimTextOutput("Rsi")
          )
        ))
      )
  ),
)
