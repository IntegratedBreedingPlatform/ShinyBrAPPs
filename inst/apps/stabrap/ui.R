library(shinybrapps)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/fieldbook.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),

  shinyjs::useShinyjs(),
  shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),

  mod_get_studydata_ui("get_studydata"),
  tabsetPanel(id = "tabsetPanel_main", selected = "Data Quality",
    tabPanel(
      "Data Quality",
      mod_dataquality_ui("dataquality")
    ),
    tabPanel(
      "Model",
      mod_model_ui('model')
    ),
    tabPanel("BLUES/BLUPS"),
    tabPanel("Diagnosis")
  )
)
