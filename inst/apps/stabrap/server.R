source("config.R")

rv <- reactiveValues()

# foo <- fread("../../../../bacasable/sprint2/study21.csv")
# foo <- fread("../bacasable/sprint2/study81.csv")
# foo <- foo[observations.observationVariableName=="PH_M_cm"]
# foo[,is.excluded:=F]

server <- function(input, output, session){
  studiesTD <- mod_get_studydata_server("get_studydata", rv)
  # mod_rawdata_server("rawdata", studiesTD)
}
