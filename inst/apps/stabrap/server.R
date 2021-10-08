rv <- reactiveValues(
  con = NULL,                 # brapi connection information
  data = NULL,                # trial data as a data.table object
  excluded_obs = NULL         # vector of excluded observations.observationDbId (data quality module)
)

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
  rv <- mod_dataquality_server("dataquality", rv)
  mod_model_server("model", rv)
}
