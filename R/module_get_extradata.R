#' @export
mod_get_extradata_ui <- function(id){}

#' @importFrom varhandle check.numeric
#' @export
mod_get_extradata_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      notification_duration <- conf$notification_duration

      observeEvent(rv$data, {
        req(rv$study_metadata)

        # no need to reload extradata if we get it from user session
        # needed in ui_mode when loading environments one by one
        req(rv$ui_mode | is.null(rv$extradata))   

        if(!isTruthy("observationVariableName"%in%names(rv$data))){
          showNotification("Data set without observations", type = "warning", duration = notification_duration)
          req(F)
        }

        isolate({
          data_tmp <- rv$data[,!c("observationTimeStamp")]
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
              names(data_tmp)[!names(data_tmp)%in%c("observationVariableName", "observationValue", "observationDbId", "observationVariableDbId", "scale.dataType")],
              collapse = " + "
            ),
            " ~ observationVariableName"
          )

          extradata <- dcast(
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
            env_cols <- unique(rv$study_metadata[, .(cols = environmentParameters.parameterName, type = NA, source = "environment", visible = T)])
            comcols <- setdiff(colnames(environmentParameters)[colnames(environmentParameters)%in%colnames(extradata)],"studyDbId")
            if (length(comcols)>0){
              setnames(environmentParameters, old = comcols, new = paste0(comcols,".env"))
              setnames(extradata, old = comcols, new = paste0(comcols,".obs"))
              column_datasource[cols%in%comcols, cols:=paste0(cols,".obs")]
              env_cols[cols%in%comcols, cols:=paste0(cols,".obs")]
            }
            extradata <- merge(extradata, environmentParameters, by = "studyDbId")
          }else{
            environmentParameters <- unique(rv$study_metadata[,studyDbId])
            showNotification("No environment parameters in study metadata", type = "warning", duration = notification_duration)
          }
          if(length(grep("coordinates", names(rv$study_metadata)))){
            geo <- rv$study_metadata[,c("studyDbId",grep("coordinates", names(rv$study_metadata),value = T)), with = F]
            geo <- geo[!is.na(coordinates.type)]
            latlon <- unique(geo[,.(studyDbId,geo.lat=unlist(lapply(coordinates.geometry.coordinates,function(a) a[1])),geo.lon=unlist(lapply(coordinates.geometry.coordinates,function(a) a[2])))])
            environmentParameters <- latlon[environmentParameters,on=.(studyDbId)]
            env_cols <- rbind(env_cols, data.table(cols=c("geo.lat","geo.lon"), type = "Numerical", source = "environment", visible = T))
            extradata <- merge(extradata, latlon, by = "studyDbId", all.x=TRUE)
          }

          locols <- c( "locationType", "locationName","abbreviation", "countryCode", "countryName", "parentLocationName")
          if(sum(names(rv$study_metadata)%in%locols)){
            locs <- unique(rv$study_metadata[,c("studyDbId",locols[locols%in%names(rv$study_metadata)]), with = F])
           environmentParameters <- locs[environmentParameters,on=.(studyDbId)]
            locs[,locationName:=NULL]
            locols <- names(locs)
            env_cols <- rbind(env_cols, data.table(cols=locols[locols%in%names(rv$study_metadata)], type = "Text", source = "environment", visible = T))
            extradata <- merge(extradata, locs, by = "studyDbId", all.x=TRUE)
          }
          
          column_datasource <- rbindlist(list(column_datasource, env_cols), use.names = T)
          
          ### data source "germplasm"
          column_datasource[grepl("germplasm", cols), source := "germplasm"]
          column_datasource[cols == "germplasmDbId", visible := F]  #used in custom_input_genotype_groups
          ## add germplasm info
          germplasms <- extradata[,unique(germplasmDbId)]
          germplasm_cols <- NULL
          withProgress(message = "POST brapi/v2/search/attributevalues/", value = 0, {
            # get study_metadata
            tryCatch({
              # get attributeValues
              incProgress(
                1/2,
                detail = paste("POST brapi/v2/search/attributevalues/ of", length(germplasms), "genotypes")
              )
              searchResultsDbId <- brapir::germplasm_attributevalues_post_search(con = rv$con, germplasmDbIds = germplasms)$data$searchResultsDbId
              incProgress(
                2/2,
                detail = paste0("GET brapi/v2/search/attributevalues/", searchResultsDbId)
              )
              germ_resp <- brapir::germplasm_attributevalues_get_search_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId))
              germplasm_data <- germ_resp$data
               if (germ_resp$metadata$pagination$totalPages >1){
                germplasm_data <- rbind(germplasm_data,rbindlist(lapply(2:germ_resp$metadata$pagination$totalPages, function(p){
                  brapir::germplasm_attributevalues_get_search_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId), page = p-1)$data
                })))
              } 
              germplasm_data <- as.data.table(germplasm_data)
            
              if (nrow(germplasm_data) > 0) {
                # get attributes datatype
                incProgress(
                  1/2,
                  detail = paste("POST brapi/v2/search/attributes/")
                )
                searchResultsDbId <- brapir::germplasm_attributes_post_search(con = rv$con, attributeDbIds = unique(germplasm_data$attributeDbId))$data$searchResultsDbId
                incProgress(
                  2/2,
                  detail = paste0("GET brapi/v2/search/attributes/", searchResultsDbId)
                )
                server_url <- paste0(rv$con$protocol, rv$con$db, ":", rv$con$port, "/", rv$con$apipath, "/", rv$con$commoncropname, "/brapi/v2")
                callurl <- paste0(server_url, "/search/attribute/", as.character(searchResultsDbId))
                resp <- brapir::germplasm_attributes_get_search_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(searchResultsDbId))
                if (resp$status_code == 200) {
                  germplasm_cols <- as.data.table(resp$data)[,.(cols = attributeName, type = scale.dataType)
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
          
          if(exists("germplasm_data") && nrow(germplasm_data)>0){
            req("attributeName" %in% names(germplasm_data))
            # In the unexpected case there are duplicated attributes for the same germplasm, take the first one
            # to avoid dcast returning counts instead of values
            germplasm_data_2 <- dcast(germplasm_data[,.(value=value[1]),.(germplasmDbId,attributeName)], "germplasmDbId ~ attributeName", value.var = "value")
            if(!any(duplicated(germplasm_data_2))){
              extradata <- merge.data.table(
                extradata,
                germplasm_data_2,
                by = "germplasmDbId", all.x=TRUE)
            }
            if (!is.null(germplasm_cols)) {
              column_datasource <- rbindlist(list(column_datasource, germplasm_cols), use.names = T, fill = T)
            }
          }

          # for columns that are not typed (environmentParameters for example) assign type manually
          # and try to convert nominal observations variables as numeric
          # check if the variable can be safely converted to num and then convert. Assign type "Text" otherwise
          nothing <- lapply(column_datasource[is.na(type) | type == "Nominal", cols], function(col){
            var <- extradata[, base::get(col)]
            var_is_num <- all(check.numeric(var[!is.na(var) & var != "NA"]))
            if (var_is_num) {
              extradata[, (col) := as.numeric(extradata[[col]])]
              column_datasource[cols == col, type := "Numerical"]
            }else{
              column_datasource[cols == col, type := "Text"]
            }
          })

          ## force numerical variables to be numerical
          # (it is not the case for environmentParameters)
          nothing <- lapply(column_datasource[type=="Numerical", cols], function(col){
            extradata[,eval(col) := as.numeric(eval(as.name(col)))]
          })
          extradata[, label_study:=paste(locationName, studyDbId, sep="-")]
          
          rv$environmentParameters <- environmentParameters
          rv$extradata <- extradata

          rv$column_datasource <- column_datasource
          #rv$ontology_variables <- ontology_variables
          
          save_user_data(rv, conf)
        })
      })
    }
  )
}
