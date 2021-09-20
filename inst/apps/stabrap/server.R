source("config.R")

rv <- reactiveValues()

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
  ## rv$
  #   - con: brapi connection information
  #   - TD: trial data object

  rv <- mod_dataquality_server("dataquality", rv)
  ## rv$
  #   - con: brapi connection information
  #   - TD: trial data object
  #   - TD_dq: trial data object filtered by trait and environments in the data quality UI
}
