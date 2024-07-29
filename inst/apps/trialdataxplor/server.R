rv <- reactiveValues(
  con = NULL,                 # brapi connection information
  trial_metadata = NULL,      # trial metadata
  study_metadata = NULL,      # study metadata
  data = NULL,                # trial data as a data.table object
  appname = appname,
  tr=NULL,
  st = NULL,
  stdatadt=NULL,
  variables=NULL,
  data_dq=NULL,
  locs=NULL,
  study_no_dat=NULL,
  var_no_dat=NULL,
  candidat_out=NULL
)


server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv, obs_unit_level = "PLOT")
  mod_banner_server("banner", rv)
  mod_trialdataxplor_server("xplor", rv)
}
