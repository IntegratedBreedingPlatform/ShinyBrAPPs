source("config.R")

rv <- reactiveValues()

server <- function(input, output, session){
  studiesTD <- mod_get_studydata_server("get_studydata", rv)
  studiesTD_postQA <- mod_dataquality_server("dataquality", studiesTD)
}
