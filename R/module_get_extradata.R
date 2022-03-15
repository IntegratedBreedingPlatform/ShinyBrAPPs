#' @export
mod_get_extradata_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("load_germplasm_data"),
      label = "Load germplasm data"
    )
  )
}


#' @export
mod_get_extradata_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){

      observe({
        req(rv$data)

        req(rv$study_metadata)
        if(!isTruthy("observations.observationVariableName"%in%names(rv$data))){
          showNotification("Data set without observations", type = "warning", duration = notification_duration)
          req(F)
        }

        isolate({
          data_tmp <- rv$data

          ### Data source 1: GET /brapi/v1/studies/{studyDbId}/observationunits
          ## 1 trait per column
          formul <- paste(
            paste(
              names(data_tmp)[!names(data_tmp)%in%c("observations.observationVariableName", "observations.value", "observations.observationDbId", "observations.observationVariableDbId")],
              collapse = " + "
            ),
            " ~ observations.observationVariableName"
          )

          data_plot <- dcast(
            data = data_tmp[!is.na(observations.observationVariableName)],#[studyDbId %in% input$studies],
            formula = formul,
            value.var = "observations.value"
          )

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

          ## GxE
          traits <- rv$data[, unique(observations.observationVariableName)]
          column_datasource[cols %in% traits, source := "GxE"]

          ## environment
          column_datasource[cols %in% names(rv$environmentParameters) & cols != "studyDbId", source := "environment"]
          column_datasource[grep("study|location|trial",cols), source := "environment"]

          ## germplasm
          column_datasource[grepl("germplasm", cols), source := "germplasm"]

          ## assign column types
          # - to all columns except observationVariableNames
          # - to observationVariableNames

          studies <- rv$study_metadata[,unique(studyDbId)]
          withProgress(message = "GET brapi/v2/variables?studyDbId", value = 0, {
            k <- 0
            ontology_variables <- rbindlist(l = lapply(studies, function(id){
              incProgress(
                k/length(studies),
                detail = paste0("studyDbId=", id)
              )
              tryCatch({
                brapirv2::brapi_get_variables(con = rv$con, studyDbId = id)
              }, error = function(e)({
                showNotification("Could not get ontology data", type = "error", duration = notification_duration)
              }))
            }), use.names = T, fill = T)
          })

          column_types2 <- rbindlist(list(column_types[origin_dataset == "NON-CROP",.(column, type)], unique(ontology_variables[,.(column = observationVariableName, type = scale.dataType)])), fill = T)
          column_datasource <- merge.data.table(column_datasource, column_types2[,.(column, type)], by.x = "cols", by.y = "column", all.x = T)


          ##
          # s <-  brapirv2::brapi_post_search_variables(con = rv$con) #studyDbId = rv$study_metadata[,unique(studyDbId)])
          # study_ontology <- brapirv2::brapi_get_search_variables_searchResultsDbId(con = rv$con, searchResultsDbId = as.character(s))

          # for observationVariableNames that are not typed (e.g. not in the ontology) assign type manually
          # check if the variable can be safely converted to num and the convert. Assign type "Text" otherwise
          nothing <- lapply(column_datasource[is.na(type), cols], function(col){
            if(all(check.numeric(data_plot[,eval(as.name(col))]))){
              data_plot[,eval(col) := as.numeric(eval(as.name(col)))]
              column_datasource[cols == col, type := "Numerical"]
            }else{
              column_datasource[cols == col, type := "Text"]
            }
          })

          rv$environmentParameters <- environmentParameters
          rv$data_plot <- data_plot

          rv$column_datasource <- column_datasource
        })
      })


      observeEvent(input$load_germplasm_data, {
        req(rv$data_plot)
        data_plot <- rv$data_plot
        ### data source 3: GET /brapi/v1/germplasm

        ## add germplasm info
        germplasms <- data_plot[,unique(germplasmDbId)]

        withProgress(message = "GET /brapi/v1/germplasm/{germplasmDbID}/attribute", value = 0, {
          n_germplasms <- length(germplasms)
          k <- 0
          # get study_metadata
          tryCatch({
            germplasm_data <- rbindlist(l = lapply(1:length(germplasms), function(k){
              incProgress(
                1/n_germplasms,
                detail = paste(k, "/", n_germplasms, "\n", data_plot[germplasmDbId == germplasms[k], unique(germplasmName)])
              )
              brapirv1::brapi_get_germplasm_germplasmDbId_attributes(con = rv$con, germplasmDbId = germplasms[k])
              # brapirv1::brapi_get_germplasm(con = rv$con, germplasmDbId = germplasms[k])
            }), use.names = T, fill = T)
          }, error = function(e)({
            showNotification("Could not get germplasm data", type = "error", duration = notification_duration)
          }))
        })

        req(germplasm_data)
        germplasm_data_2 <- dcast(germplasm_data, "germplasmDbId ~ attributeCode", value.var = "value")
        if(!any(duplicated(germplasm_data_2))){
          data_plot <- merge.data.table(
            data_plot,
            germplasm_data_2,
            # germplasm_data[,-setdiff(intersect(names(data_tmp), names(germplasm_data)), "germplasmDbId"), with = F], # remove columns that are already in data_tmp (e.g. germplasmName) but not germplasmDbId
            by = "germplasmDbId")
        }


        ## update rv$column_datasource with new column names
        newcols <- data.table(
          cols = germplasm_data_2[, names(germplasm_data_2)[!names(germplasm_data_2)%in% rv$column_datasource[, unique(cols)] ]],
          source = "germplasm"
        )
        rv$column_datasource <- rbindlist(list(rv$column_datasource, newcols), use.names = T, fill = T)

        # check if the variable can be safely converted to num and the convert. Assign type "Text" otherwise
        nothing <- lapply(rv$column_datasource[is.na(type), cols], function(col){
          if(all(check.numeric(data_plot[,eval(as.name(col))])) & any(!is.na(data_plot[,eval(as.name(col))]))){
            data_plot[,eval(col) := as.numeric(eval(as.name(col)))]
            rv$column_datasource[cols == col, type := "Numerical"]
          }else{
            rv$column_datasource[cols == col, type := "Text"]
          }
        })

        rv$data_plot <- data_plot
      })

      return(rv)
    }
  )
}
