#' column_types
#'
#' Dataset that describes the data types of all the columns found in the decision support app datasets. These columns come from:
#'  - 1/ Trial metadata: GET /brapi/v2/studies
#'  - 2/ Environment observations: GET /brapi/v1/studies/{studyDbId}/observationunits
#'  - 3/ Location metadata: GET /brapi/v2/locations
#'  - 4/ Germplasm metadata: GET /brapi/v2/germplasm
#'  - 5/ BMS ontology variables: GET&POST /{crop}/brapi/v2/search/variables
#'
#' A data type is assigned by hand except for the BMS ontology variables where the type is given by "scale.dataType".
#' The BMS ontology variables are crop specific and a variable name can be used by several crops (e.g. PH_M_cm CO_321:0001301 and PH_M_cm CO_346:0000063). When it happens, the assumption is that the data type is the same.
#'
#' @format A data table 4 columns:
#' \describe{
#'   \item{origin_dataset}{where column/variable comes from}
#'   \item{column}{column name as found in the app}
#'   \item{type}["Text, "Numerical", "Nominal" or "Date"]
#' }
#'
#' XXX table made by hand in a non-reproducible way (for now)
#'
#' stored in R/sysdata.rda via 'usethis::use_data(column_types, internal = TRUE)'
