rv <- reactiveValues(appname = appname)

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
  rv <- mod_get_extradata_server("get_extradata", rv)
  mod_banner_server("banner", rv)
  mod_scatterplot_server("scatterplot", rv)
}
