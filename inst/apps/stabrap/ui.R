library(shinybrapps)
library(shiny)
library(shinyjs)
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
    tags$link(rel = "stylesheet", type = "text/css", href = "css/fieldbook.css")
  ),

  shinyjs::useShinyjs(),

  mod_get_studydata_ui("get_studydata"),
  tabsetPanel(
    tabPanel(
      "Raw Data",
      mod_rawdata_ui("rawdata")
    ),
    tabPanel("Model"),
    tabPanel("BLUES/BLUPS"),
    tabPanel("Diagnosis")
  )
)

