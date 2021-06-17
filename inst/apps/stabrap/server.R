source("config.R")

rv <- reactiveValues()

server <- function(input, output, session){
  trialdata <- mod_get_trialdata_server("get_trialdata", rv)
  mod_rawdata_server("rawdata", trialdata)
}
