# brapi_protocol <- "https://"
# brapi_db <- "brapi.bms-uat-test.net"
# brapi_port <- 80
# brapi_apipath <- "bmsapi"
brapi_version <- "2.0"


# hidden_columns_observationunits <- c(
#   "studyDbId","studyName","studyLocationDbId", "studyLocation",
#   "programName", "programDbId",
#   "trialDbId", "trialName",
#   "observations.observationVariableName",
#   "environment_label_abbrev"
#   )
visible_columns_selected_obs <- c(
  "study_name_abbrev_app",
  "entryType",
  "entryNumber",
  "germplasmName",
  "blockNumber",
  "replicate",
  "plotNumber",
  "positionCoordinateX",
  "positionCoordinateY"
)

## correspondence of experimental designs
exp_designs_corresp <- data.table(
  StatGenSTA_code = c(
    "ibd","res.ibd", "rcbd", "rowcol", "res.rowcol"
  ),
  statGenSTA = c(
    "incomplete block",
    "resolvable incomplete block",
    "randomized complete block",
    "row column",
    "resolvable row column"
    ),
  BMS_pui = c(
    NA, "10130", "10110", NA, "10145"
  )
)

choices_model_design <- exp_designs_corresp$StatGenSTA_code
names(choices_model_design) <- exp_designs_corresp$statGenSTA

notification_duration = 10 # in seconds

## columns to hide in widgets/table (columns created by the app)
hidden_col_names <- c("study_name_BMS", "study_name_app", "study_name_abbrev_app", "location_abbrev", "location_name")


