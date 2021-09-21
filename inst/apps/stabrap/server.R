source("config.R")

rv <- reactiveValues(
  con = NULL,                 # brapi connection information
  data = NULL,                # trial data as a data.table object
  TD = NULL,                  # trial data object
  TD_qd = NULL,               # trial data object filtered by trait and environments in the data quality UI
  excluded_obs = NULL         # vector of excluded observations.observationDbId (data quality module)
)

# load(file = "../bacasable/sprint3/studiesTD.RData")
# setDT(foo$`101`)
# setDT(foo$`102`)
# setDT(foo$all)
# foo$all[,environment_label:=trials]
# foo$all[,environment_label_abbrev:=trials]
# foo$`101`[,environment_label:=trial]
# foo$`102`[,environment_label:=trial]
# foo$`101`[,environment_label_abbrev:=trial]
# foo$`102`[,environment_label_abbrev:=trial]

server <- function(input, output, session){
  rv <- mod_get_studydata_server("get_studydata", rv)
  rv <- mod_dataquality_server("dataquality", rv)
  mod_model_server("model", rv)
}
