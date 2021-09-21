#' @export
mod_get_studydata_ui <- function(id){
  ns <- NS(id)

  tagList(
    # UI for study selection (if no GET parameters)
    div(
      style = "display:none",
      id = ns("study_selection_UI"),
      shinyBS::bsCollapse(
        id = ns("dataImportCollapse"),
        open = "Data import",
        # style = "width : 50%",
        bsCollapsePanel("Data import",
                        textInput(ns("token"), "Token", placeholder = "Enter Token", width = "100%"),
                        textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames", width = "100%"),
                        selectizeInput(
                          ns("trials"), label = "Study", choices = NULL, multiple = FALSE, width = "100%",
                          options = list(
                            placeholder = '',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
                        )
        )
      ),
    )
  )
}

#' @export
mod_get_studydata_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)

      if(!is.null(dataset_4_dev)){ # XXX
        rv$TD <- dataset_4_dev
      }else{
        parse_GET_param  <- reactive({
          pars <- parseQueryString(session$clientData$url_search)
        })

        observeEvent(parse_GET_param(),{

          if(!(is.null(parse_GET_param()$token) | is.null(parse_GET_param()$cropDb) | is.null(parse_GET_param()$trialDbId))){
            rv$con <- brapirv2::brapi_connect(
              secure = TRUE,
              protocol = brapi_protocol,
              db = brapi_db,
              port = brapi_port,
              apipath = brapi_apipath,
              multicrop = TRUE,
              token = parse_GET_param()$token,
              commoncropname = parse_GET_param()$cropDb,
              granttype = "token",
              clientid = "brapir",
              bms = TRUE
            )
            rv$trialDbId <- parse_GET_param()$trialDbId
          }else{
            shinyjs::show(id = "study_selection_UI")
            if(is.null(parse_GET_param()$token)){
              updateTextInput("token", session = session, value = parse_GET_param()$token)
            }
          }
        })

        observeEvent(c(input$token,input$cropDb),{
          req(input$token)
          req(input$cropDb)
          rv$con <- brapirv2::brapi_connect(
            secure = TRUE,
            protocol = brapi_protocol,
            db = brapi_db,
            port = brapi_port,
            apipath = brapi_apipath,
            multicrop = TRUE,
            commoncropname = input$cropDb,
            token = input$token,
            granttype = "token",
            clientid = "brapir",
            bms = TRUE
          )
          try({
            # get the brapi::trials for the crop
            trials <- as.data.table(brapirv2::brapi_get_trials(con = rv$con))
            trial_choices <- trials[,trialDbId]
            names(trial_choices) <- trials[,trialName]
            updateSelectizeInput(
              inputId = "trials", session = session, choices = trial_choices,
              options = list(
                placeholder = 'Select a study',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
        })

        observeEvent(input$trials,{
          req(input$trials)
          rv$trialDbId <- input$trials
        })

        observeEvent(rv$trialDbId,{
          req(rv$trialDbId)
          try({
            # get all the studies of a trial
            trial_studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, trialDbId = rv$trialDbId))
            data_studies <- rbindlist(l = lapply(trial_studies$studyDbId, function(study_id){
              try({
                study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = study_id))
                study[,observations.value:=as.numeric(observations.value)] # XXX this should not always be the case
                return(study)
              })
            }), use.names = T, fill = T)
            data_studies[,environment_label := paste0(
              studyDbId, " - ",
              studyName, " AT ",
              studyLocation
            )]
            maxchar <- 8
            data_studies[,environment_label_abbrev :=
                           paste0(
                             studyDbId, " - ",
                             ifelse(
                               nchar(studyName)>maxchar,
                               paste0(
                                 substr(studyName,1,maxchar/2 - 1),
                                 "...",
                                 substr(studyName,nchar(studyName)- maxchar/2 - 1,nchar(studyName))
                               ),
                               studyName
                             ),
                             " AT ",
                             ifelse(
                               nchar(studyLocation)>20,
                               paste0(
                                 substr(studyLocation,1,8),
                                 "...",
                                 substr(studyLocation,nchar(studyLocation)-8,nchar(studyLocation))
                               ),
                               studyLocation
                             )
                           )
            ]
            rv$data <- data_studies

            studiesTD <- createTD(
              data = data_studies,
              genotype = "germplasmDbId",
              trial = "studyDbId",
              loc = "studyLocationDbId",
              repId = "replicate",
              subBlock = "observationUnitDbId",
              rowCoord = "positionCoordinateY",
              colCoord = "positionCoordinateX")

            studiesTD <- lapply(studiesTD, as.data.table)

            TDall <- rbindlist(l = studiesTD, use.names = T, fill = T)
            TDall[,trials:=trial]
            TDall[,trial:="all"]
            studiesTD <- addTD(TD = studiesTD, data = TDall)
            setDT(studiesTD$all)

            shinyBS::updateCollapse(
              id = "dataImportCollapse",
              session = session,
              close = "Data import"
            )
            rv$TD <- studiesTD
          })
        })
      }
      return(rv)
    }
  )
}
