#' @export
mod_get_extradata_ui <- function(id){}

#' @export
mod_get_extradata_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      observe({
        req(rv$data)
        req(rv$study_metadata)

        if(!isTruthy("observationVariableName"%in%names(rv$data))){
          showNotification("Data set without observations", type = "warning", duration = notification_duration)
          req(F)
        }

        isolate({
          data_tmp <- rv$data

          ### Data source 1: GET /brapi/v1/studies/{studyDbId}/observationunits
          ## 1 trait per column
          formul <- paste(
            paste(
              names(data_tmp)[!names(data_tmp)%in%c("observationVariableName", "observationValue", "observationDbId", "observationVariableDbId")],
              collapse = " + "
            ),
            " ~ observationVariableName"
          )

          data_plot <- dcast(
            data = data_tmp[!is.na(observationVariableName)],#[studyDbId %in% input$studies],
            formula = formul,
            value.var = "observationValue"
          )
          
          # remove observationunits with observationLevels=SUMMARY_STATISTICS (because no germplasmDbId for these observations)
          #data_plot <- data_plot[observationLevel != "SUMMARY_STATISTICS"]

          ### Data source 2: Environment  GET /brapi/v2/studies
          ## extract envrionment parameters from rv$study_metadata
          if(length(grep("environmentParameters", names(rv$study_metadata)))){
            environmentParameters <- dcast(data = unique(rv$study_metadata[,grep("environmentParameters|studyDbId", names(rv$study_metadata)), with = F]),
                                           formula = "studyDbId ~ environmentParameters.parameterName",
                                           value.var = "environmentParameters.value")
            environmentParameters[,studyDbId:=as.numeric(studyDbId)]
            data_plot <- merge(data_plot, environmentParameters, by = "studyDbId")
          }else{
            environmentParameters <- unique(rv$study_metadata[,studyDbId])
            showNotification("No environment parameters in study metadata", type = "warning", duration = notification_duration)
          }

          ### make data source register
          # - Germplasm
          # - Cross Environment Means (GxE)
          # - Means (BLUxs)
          # - Environment
          # - Plot

          ## plot (default)
          column_datasource <- data.table(
            cols = names(data_plot),
            source = "plot"
          )

          ## Observations
          traits <- rv$data[, unique(observationVariableName)]
          column_datasource[cols %in% traits, source := "GxE"]
          column_datasource[grepl("BLUE", cols) | grepl("BLUP", cols) , source := "Means"]

          ## environment
          column_datasource[cols %in% names(environmentParameters) & cols != "studyDbId", source := "environment"]
          column_datasource[grep("study|location|trial",cols), source := "environment"]

          ## germplasm
          column_datasource[grepl("germplasm", cols), source := "germplasm"]

          ## assign column types
          # - to all columns except observationVariableNames
          # - to observationVariableNames

          studyIds <- rv$data[,unique(studyDbId)]
          withProgress(message = "GET brapi/v2/variables?studyDbId", value = 0, {
            ontology_variables <- rbindlist(l = lapply(studyIds, function(studyId){
              incProgress(
                1/length(studyIds),
                detail = paste0("studyDbId=", studyId)
              )
              tryCatch({
                brapirv2::brapi_get_variables(con = rv$con, studyDbId = as.character(studyId))
              }, error = function(e)({
                showNotification("Could not get ontology data", type = "error", duration = notification_duration)
              }))
            }), use.names = T, fill = T)
          })

          column_types2 <- rbindlist(list(column_types[,.(column, type)], unique(ontology_variables[,.(column = observationVariableName, type = scale.dataType)])), fill = T)
          column_datasource <- merge.data.table(column_datasource, column_types2[,.(column, type)], by.x = "cols", by.y = "column", all.x = T)

          ### data source 3: GET /brapi/v1/germplasm
          ## add germplasm info
          germplasms <- data_plot[,unique(germplasmDbId)]
          withProgress(message = "POST brapi/v2/search/attributevalues/", value = 0, {
            # get study_metadata
            tryCatch({
              incProgress(
                1/2,
                detail = paste("POST brapi/v2/search/attributevalues/ of", length(germplasms), "genotypes")
              )
              searchResultsDbId <- brapirv2::brapi_post_search_attributevalues(con = rv$con, germplasmDbIds = germplasms)
              incProgress(
                2/2,
                detail = paste0("GET brapi/v2/search/attributevalues/", searchResultsDbId)
              )
              # res <- brapirv2::brapi_get_search_attributevalues_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(1344))
              # # res <- brapirv2::brapi_get_search_attributevalues_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId))
              germplasm_data <- as.data.table(brapirv2::brapi_get_search_attributevalues_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId)))
            }, error = function(e)({
              showNotification("Could not get germplasm data", type = "error", duration = notification_duration)
            }))
          })
          
          if(exists("germplasm_data") && nrow(germplasm_data>0)){
            req("attributeName" %in% names(germplasm_data))
            germplasm_data_2 <- dcast(germplasm_data, "germplasmDbId ~ attributeName", value.var = "value")
            if(!any(duplicated(germplasm_data_2))){
              data_plot <- merge.data.table(
                data_plot,
                germplasm_data_2,
                by = "germplasmDbId")
            }

            ## update rv$column_datasource with new column names
            newcols <- data.table(
              cols = germplasm_data_2[, names(germplasm_data_2)[!names(germplasm_data_2)%in% column_datasource[, unique(cols)] ]],
              source = "germplasm"
            )
            column_datasource <- rbindlist(list(column_datasource, newcols), use.names = T, fill = T)
          }

          ##
          # s <-  brapirv2::brapi_post_search_variables(con = rv$con) #studyDbId = rv$study_metadata[,unique(studyDbId)])
          # study_ontology <- brapirv2::brapi_get_search_variables_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(s))

          # for observationVariableNames that are not typed (e.g. not in the ontology) assign type manually
          # check if the variable can be safely converted to num and then convert. Assign type "Text" otherwise
          nothing <- lapply(column_datasource[is.na(type), cols], function(col){
            if(all(check.numeric(data_plot[,eval(as.name(col))]))){
              data_plot[,eval(col) := as.numeric(eval(as.name(col)))]
              column_datasource[cols == col, type := "Numerical"]
            }else{
              column_datasource[cols == col, type := "Text"]
            }
          })

          ## force numerical variables to be numerical
          # (it is not the case for environmentParameters)
          nothing <- lapply(column_datasource[type=="Numerical", cols], function(col){
            data_plot[,eval(col) := as.numeric(eval(as.name(col)))]
          })
          
          hidden_cols <- c("scale.dataType", "entryNumber", "environmentNumber", "germplasmDbId", "observationUnitDbId", "studyDbId", "programDbId",
                           "programName", "study_name_BMS", "study_name_abbrev_app", "study_name_app", "environment_number", "entryNumber", 
                           "locationDbId", "location_name", "location_abbrev", "trialDbId", "trialName")
          column_datasource <- column_datasource[, visible := T]
          column_datasource <- column_datasource[cols %in% hidden_cols, visible := F]
          
          rv$environmentParameters <- environmentParameters
          rv$data_plot <- data_plot

          rv$column_datasource <- column_datasource
          rv$ontology_variables <- ontology_variables
        })
      })

      return(rv)
    }
  )
}
