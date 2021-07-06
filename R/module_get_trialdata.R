#' @export
mod_get_studydata_ui <- function(id){
  ns <- NS(id)

  tagList(
    # title
    htmlOutput(ns("title_study_name")),

    # UI for study selection (if no GET parameters)
    div(
      style = "display:none",
      id = ns("study_selection_UI"),
      textInput(ns("token"), "Token", placeholder = "Enter Token"),
      textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames")
    ),
    selectizeInput(
      ns("trait"), label = "Trait", choices = NULL,
      options = list(
        placeholder = '',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      ns("studies"), label = "Environments", choices = NULL,
      multiple = T,
      options = list(
        placeholder = '',
        onInitialize = I('function() { this.setValue(""); }')
      )
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
        dataset_4_dev[,is.excluded:=F]
        studydata <- reactive(dataset_4_dev)
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
            try({
              ## get the observed variables for the trial
              studies <- brapirv2::brapi_get_studies(con = con, trialDbId = parse_GET_param()$trialDbId)
              trial_vars <- rbindlist(l = lapply(studies[,"studyDbId"], function(study_id){
                as.data.table(brapirv1::brapi_get_studies_studyDbId_observationvariables(con = con, studyDbId = study_id))
              }), use.names = T, fill = T)
              variables <- unique(trial_vars[,.(observationVariableDbId, observationVariableName)])

              trait_choices <- variables[,observationVariableDbId]
              names(trait_choices) <- variables[,observationVariableName]
              updateSelectizeInput(
                inputId = "trait", session = session, choices = trait_choices,
                options = list(
                  placeholder = 'Select a trait',
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            })
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
            # get all the observed traits (regardless of the trials)
            variables <- as.data.table(brapirv1::brapi_get_variables(con = rv$con))
            trait_choices <- variables[,observationVariableDbId]
            names(trait_choices) <- variables[,observationVariableName]
            updateSelectizeInput(
              inputId = "trait", session = session, choices = trait_choices,
              options = list(
                placeholder = 'Select a trait',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
        })

        observeEvent(input$trait,{
          # 4/ get the studies with observations for a given trait
          req(input$trait)
          try({
            if(is.null(parse_GET_param()$trialDbId)){
            studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, observationVariableDbId = as.character(input$trait)))
            }else{
            studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, observationVariableDbId = as.character(input$trait), trialDbId = parse_GET_param()$trialDbId))
          }
            studies <- unique(studies[,.(studyDbId,studyName)])
            study_choices <- studies[,studyDbId]
            names(study_choices) <- studies[,studyName]
            updateSelectizeInput(
              inputId = "studies", session = session, choices = study_choices,
              options = list(
                placeholder = 'Select 1 or more environments',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
        })

        studiesTD <- eventReactive(input$studies,{
          # 5/ data of the selected studies
          req(input$studies)

          studies <- rbindlist(l = lapply(input$studies, function(study_id){
            try({
              study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = study_id))
              return(as.data.table(study[observations.observationVariableDbId == input$trait]))
            })
          }), use.names = T, fill = T)

          studiesTD <- createTD(data = studies,
                                genotype = "germplasmDbId",
                                trial = "studyDbId",
                                loc = "studyLocationDbId",
                                repId = "replicate",
                                subBlock = "observationUnitDbId",
                                rowCoord = "positionCoordinateY",
                                colCoord = "positionCoordinateX")

          return(studiesTD)
        })
      }
      return(studiesTD)
    }
  )
}
