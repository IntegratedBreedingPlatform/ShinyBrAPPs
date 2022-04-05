library(shinybrapps)
source("config.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom_styles.css")
  ),

  shinyjs::useShinyjs(),
  shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),

  div(class = "container-fluid",
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
