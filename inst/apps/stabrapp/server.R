rv <- reactiveValues(
  con = NULL,                 # brapi connection information
  trial_metadata = NULL,      # trial metadata
  study_metadata = NULL,      # study metadata
  data = NULL,                # trial data as a data.table object
  excluded_obs = NULL,        # vector of excluded observations.observationDbId (data quality module)
  pushOK = FALSE              # to avoid pushing BLUES/BLUPS to easily
)

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
  mod_banner_server("banner", rv)
  rv <- mod_dataquality_server("dataquality", rv)
  mod_model_server("model", rv)
}
