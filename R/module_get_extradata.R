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
          data_tmp <- rv$data[,!c("observationTimeStamp")]
          
          browser()
          
          ### make data source register
          # - Germplasm
          # - Cross Environment Means (GxE)
          # - Means (BLUxs)
          # - Environment
          # - Plot
          
          column_datasource <- unique(data_tmp[, .(cols = observationVariableName, type = scale.dataType)])[, source := "GxE"]
          column_datasource[grepl("BLUE", cols) | grepl("BLUP", cols) , source := "Means"]
          column_datasource[, visible := T]
          
          # data source "plot"
          # standard columns characterising the observation units
          plot_column <- data.table(cols = c("blockNumber", "studyName", "entryType", "observationUnitName", "germplasmName", "germplasmDbId",
                                             "replicate", "plotNumber","positionCoordinateX", "positionCoordinateY", "observationLevel",
                                             "observationCode", "locationName")
                                    )[, source := "plot"][, visible := T]
          plot_column[cols %in% c("studyName", "locationName"), source := "environment"]
          types <- data.table(cols = colnames(data_tmp) , type = sapply(data_tmp, class))
          types[type =="character", type := "Text"]
          types[type =="numeric", type := "Numerical"]
          types[cols %in% c("replicate", "plotNumber","blockNumber"), type := "Text"]
          plot_column <- merge(plot_column, types)

          column_datasource <- rbindlist(list(column_datasource, plot_column), use.names = T)

          ### observations POST /brapi/v2/search/observationunits (with include_observations)
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
          
          ### Data source "environment"
          ## extract environment parameters from rv$study_metadata
          if(length(grep("environmentParameters", names(rv$study_metadata)))){
            environmentParameters <- dcast(data = unique(rv$study_metadata[,grep("environmentParameters|studyDbId", names(rv$study_metadata)), with = F]),
                                           formula = "studyDbId ~ environmentParameters.parameterName",
                                           value.var = "environmentParameters.value")
            environmentParameters[,studyDbId:=as.numeric(studyDbId)]
            env_cols <- unique(rv$study_metadata[, .(cols = environmentParameters.parameterName, type = NA, source = "environment", visible = T)])
            data_plot <- merge(data_plot, environmentParameters, by = "studyDbId")
          }else{
            environmentParameters <- unique(rv$study_metadata[,studyDbId])
            showNotification("No environment parameters in study metadata", type = "warning", duration = notification_duration)
          }
          
          column_datasource <- rbindlist(list(column_datasource, env_cols), use.names = T)
          
          ### data source "germplasm"
          column_datasource[grepl("germplasm", cols), source := "germplasm"]
          column_datasource[cols == "germplasmDbId", visible := F]  #used in custom_input_genotype_groups
          ## add germplasm info
          germplasms <- data_plot[,unique(germplasmDbId)]
          germplasm_cols <- NULL
          withProgress(message = "POST brapi/v2/search/attributevalues/", value = 0, {
            # get study_metadata
            tryCatch({
              # get attributeValues
              incProgress(
                1/2,
                detail = paste("POST brapi/v2/search/attributevalues/ of", length(germplasms), "genotypes")
              )
              searchResultsDbId <- brapirv2::brapi_post_search_attributevalues(con = rv$con, germplasmDbIds = germplasms)
              incProgress(
                2/2,
                detail = paste0("GET brapi/v2/search/attributevalues/", searchResultsDbId)
              )
              germplasm_data <- as.data.table(brapirv2::brapi_get_search_attributevalues_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId)))
            
              if (nrow(germplasm_data) > 0) {
                # get attributes datatype
                incProgress(
                  1/2,
                  detail = paste("POST brapi/v2/search/attributes/")
                )
                searchResultsDbId <- brapirv2::brapi_post_search_attributes(con = rv$con, attributeDbIds = unique(germplasm_data$attributeDbId))
                incProgress(
                  2/2,
                  detail = paste0("GET brapi/v2/search/attributes/", searchResultsDbId)
                )
                server_url <- paste0(rv$con$protocol, rv$con$db, ":", rv$con$port, "/", rv$con$apipath, "/", rv$con$commoncropname, "/brapi/v2")
                callurl <- paste0(server_url, "/search/attribute/", as.character(searchResultsDbId))
                resp <-   httr::GET(url = callurl,
                                    httr::timeout(25),
                                    httr::add_headers(
                                      "Authorization" = paste("Bearer", rv$con$token),
                                      "Content-Type"= "application/json",
                                      "accept"= "*/*"
                                    )
                )
                cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
                if (resp$status_code == 200) {
                  res <- jsonlite::fromJSON(cont, flatten = T)$result$data
                  germplasm_cols <- as.data.table(res)[,.(cols = attributeName, type = scale.dataType)
                                                    ][, source := "germplasm"
                                                      ][, visible := T]
                } else {
                  germplasm_cols <- data.table(cols = unique(germplasm_data$attributeName), type = NA, source = "germplasm", visible = T)
                }
              }
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
            if (!is.null(germplasm_cols)) {
              column_datasource <- rbindlist(list(column_datasource, germplasm_cols), use.names = T, fill = T)
            }
          }

          # for columns that are not typed (environmentParameters for example) assign type manually
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
          
          rv$environmentParameters <- environmentParameters
          rv$data_plot <- data_plot
          
          browser()

          rv$column_datasource <- column_datasource
          #rv$ontology_variables <- ontology_variables
        })
      })

      return(rv)
    }
  )
}
