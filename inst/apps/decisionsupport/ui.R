library(shinybrapps)
source("config.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/fieldbook.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),

  shinyjs::useShinyjs(),
  shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),

  mod_get_studydata_ui("get_studydata"),
  mod_get_extradata_ui("get_extradata"),
  mod_scatterplot_ui("scatterplot")
)
