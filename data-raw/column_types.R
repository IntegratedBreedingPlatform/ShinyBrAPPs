#' column_types
#'
#' Dataset that describes the data types of common columns found in the decision support app datasets. These columns come from:
#'  - 1/ Trial metadata: GET /brapi/v2/studies
#'  - 2/ Environment observations: GET /brapi/v1/studies/{studyDbId}/observationunits
#'  - 3/ Location metadata: GET /brapi/v2/locations
#'  - 4/ Germplasm metadata: GET /brapi/v2/germplasm
#'
#' NB data types of BMS ontology variables are retrieved by the app
#'
#' @format A data table 2 columns:
#' \describe{
#'   \item{column}{column name as found in the app}
#'   \item{type}["Text, "Numerical", "Nominal" or "Date"]
#' }
#'
#'
#' stored in R/sysdata.rda via 'usethis::use_data(column_types, internal = TRUE)'
#'


column_types <- as.data.table(read.table(header = TRUE, text = "
                           column      type
                  accessionNumber      Text
                      blockNumber      Text
               breedingMethodDbId      Text
              countryOfOriginCode      Text
               defaultDisplayName      Text
                      entryNumber      Text
                        entryType      Text
               environment_number      Text
                    germplasmDbId      Text
                    germplasmName      Text
                   instanceNumber      Text
                    instituteName      Text
                  location_abbrev      Text
                    location_name      Text
                     locationName      Text
                             NREP Numerical
                 observationLevel      Text
                observationLevels      Text
                        collector      Text
             observationTimeStamp      Text
                           season      Text
              observationUnitDbId      Text
              observationUnitName      Text
              observationUnitXRef      Text
                         pedigree      Text
                      plantNumber      Text
                       plotNumber      Text
              positionCoordinateX Numerical
              positionCoordinateY Numerical
                      programDbId      Text
                      programName      Text
                        replicate      Text
            study_name_abbrev_app      Text
                   study_name_app      Text
                   study_name_BMS      Text
                        studyDbId      Text
                     locationName      Text
                     locationDbId      Text
                        studyName      Text
                         synonyms      Text
                        trialDbId      Text
                        trialName      Text
                                x Numerical
                                y Numerical
"))

# usethis::use_data(column_types, overwrite = T, internal = TRUE)
