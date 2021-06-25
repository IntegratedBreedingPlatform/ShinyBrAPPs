source("config.R")

rv <- reactiveValues()

server <- function(input, output, session){
  trialdata <- mod_get_studydata_server("get_studydata", rv)
  mod_rawdata_server("rawdata", trialdata)
}
