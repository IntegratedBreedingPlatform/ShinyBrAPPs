server <- function(input, output, session){
  rv <- reactiveValues(
    con = NULL,                 # brapi connection information
    trial_metadata = NULL,      # trial metadata
    study_metadata = NULL,      # study metadata
    data = NULL                # trial data as a data.table object
  )
  output$Rsi <- renderPrint(sessionInfo())
  
  mod_get_studydata_server("get_studydata", rv)
  mod_banner_server("banner", rv, appname)
  mod_connect_server("server", rv)
  mod_trialdataxplor_server("xplor", rv)
}
