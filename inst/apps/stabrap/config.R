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
    "rowcol", "ibd","res.ibd","rcbd", "res.rowcol"
  ),
  statGenSTA = c(
    "row column design",
    "incomplete block design",
    "resolvable incomplete block design",
    "randomized complete block design",
    "resolvable row column design"
    ),
  BMS_pui = c(
    NA, NA, "10130", "10110", "10145"
  )
)

choices_model_design <- exp_designs_corresp$StatGenSTA_code
names(choices_model_design) <- exp_designs_corresp$statGenSTA

notification_duration = 10 # in seconds




