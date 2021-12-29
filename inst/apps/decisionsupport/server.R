rv <- reactiveValues()

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
}
