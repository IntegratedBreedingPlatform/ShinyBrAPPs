library(shinybrapps)
library(shiny)
library(shinyjs)
library(shinyBS)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(magrittr)
library(RColorBrewer)
library(e1071)
library(brapirv1)

ui <- fluidPage(
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "css/fieldbook.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),

  shinyjs::useShinyjs(),
  shinysky::busyIndicator(wait = 200, text = NULL, img = "img/loading-animation.gif"),

  mod_get_studydata_ui("get_studydata"),
  tabsetPanel(selected = "Raw Data",
    tabPanel(
      "Raw Data",
      mod_rawdata_ui("rawdata")
    ),
    tabPanel("Model"),
    tabPanel("BLUES/BLUPS"),
    tabPanel("Diagnosis")
  )
)

