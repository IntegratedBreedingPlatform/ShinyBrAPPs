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
        req("observations.observationVariableName"%in%names(rv$data))
        req(rv$study_metadata)
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

        data_tmp_2 <- dcast(
          data = data_tmp[!is.na(observations.observationVariableName)],#[studyDbId %in% input$studies],
          formula = formul,
          value.var = "observations.value"
        )

        study_metadata_tmp <- rv$study_metadata

        ### Data source 2: Environment  GET /brapi/v2/studies
        ## extract envrionment parameters from rv$study_metadata
        environmentParameters <- unique(study_metadata_tmp[,grep("environmentParameters|studyDbId", names(study_metadata_tmp)), with = F])
        environmentParameters_casted <- dcast(data = environmentParameters,
                                              formula = "studyDbId ~ environmentParameters.parameterName",
                                              value.var = "environmentParameters.value")
        environmentParameters_casted[,studyDbId:=as.numeric(studyDbId)]
        data_tmp_2 <- merge(data_tmp_2, environmentParameters_casted, by = "studyDbId")


        ### make data source register
        # - Germplasm
        # - Cross Environment Means (GxE)
        # - Means (BLUxs)
        # - Environment
        # - Plot

        ## plot (default)
        column_datasource <- data.table(
          cols = names(data_tmp_2),
          source = "plot"
        )

        ## GxE
        traits <- data_tmp[, unique(observations.observationVariableName)]
        column_datasource[cols %in% traits, source := "GxE"]

        # environment
        column_datasource[cols %in% names(environmentParameters_casted) & cols != "studyDbId", source := "environment"]

        ## germplasm
        column_datasource[grepl("germplasm", cols), source := "germplasm"]

        rv$data_plot <- data_tmp_2
        rv$column_datasource <- column_datasource
      })


      observeEvent(input$load_germplasm_data, {
        req(rv$data_plot)
        data_tmp <- rv$data_plot
        ### data source 3: GET /brapi/v1/germplasm

        ## add germplasm info
        germplasms <- data_tmp[,unique(germplasmDbId)]

        withProgress(message = "GET /brapi/v1/germplasm", value = 0, {
          n_germplasms <- length(germplasms)
          k <- 0
          # get study_metadata
          tryCatch({
            germplasm_data <- rbindlist(l = lapply(1:length(germplasms), function(k){
              incProgress(
                1/n_germplasms,
                detail = paste(k, "/", n_germplasms, "\n", data_tmp[germplasmDbId == germplasms[k], unique(germplasmName)])
              )
              brapirv1::brapi_get_germplasm(con = rv$con, germplasmDbId = germplasms[k])
            }), use.names = T, fill = T)
          }, error = function(e)({
            showNotification("Could not get germplasm data", type = "error", duration = notification_duration)
          }))
        })

        req(germplasm_data)
        if(!any(duplicated(germplasm_data))){
          data_tmp_2 <- merge.data.table(
            data_tmp,
            germplasm_data[,-setdiff(intersect(names(data_tmp), names(germplasm_data)), "germplasmDbId"), with = F], # remove columns that are already in data_tmp (e.g. germplasmName) but not germplasmDbId
            by = "germplasmDbId")
        }


        ## update rv$column_datasource with new column names
        newcols <- data.table(
          cols = germplasm_data[, names(germplasm_data)[!names(germplasm_data)%in% rv$column_datasource[, unique(cols)] ]],
          source = "germplasm"
          )
        # germplasm_data[, names(germplasm_data)[!names(germplasm_data)%in% rv$column_datasource[, unique(cols)] ]
        # rv$column_datasource[cols %in% names(germplasm_data), source := "germplasm"]
        rv$column_datasource <- rbindlist(list(rv$column_datasource, newcols), use.names = T, fill = T)

        rv$data_plot <- data_tmp_2
      })

      return(rv)
    }
  )
}
