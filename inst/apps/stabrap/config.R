brapi_protocol <- "https://"
brapi_db <- "brapi.bms-uat-test.net"
brapi_port <- 80
brapi_apipath <- "bmsapi"
brapi_version <- "1.3"

hidden_columns_observationunits <- c(
  "studyDbId","studyName","studyLocationDbId", "studyLocation",
  "programName", "programDbId",
  "trialDbId", "trialName",
  "observations.observationVariableName",
  "environment_label_abbrev"
  )
