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
      textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames"),
      selectizeInput(
        ns("trials"), label = "Study", choices = NULL, multiple = FALSE,
        options = list(
          placeholder = '',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
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
        studiesTD <- reactive(dataset_4_dev)
      }else{
        parse_GET_param  <- reactive({
          pars <- parseQueryString(session$clientData$url_search)
        })

        observeEvent(parse_GET_param(),{

          if(!(is.null(parse_GET_param()$token) | is.null(parse_GET_param()$cropDb) | is.null(parse_GET_param()$trialDbId))){
            print("is not null")
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
            print("parse_GET_param()$trialDbId")
            print(parse_GET_param()$trialDbId)
            rv$trialDbId <- parse_GET_param()$trialDbId
            print(rv$trialDbId)
            # rv$trigger_get_data_studies <- ifelse(is.null(rv$trigger_get_data_studies), 1, rv$trigger_get_data_studies + 1)
            # try({
            #   ## get the observed variables for the trial
            #   studies <- brapirv2::brapi_get_studies(con = rv$con, trialDbId = parse_GET_param()$trialDbId)
            #   trial_vars <- rbindlist(l = lapply(studies[,"studyDbId"], function(study_id){
            #     as.data.table(brapirv1::brapi_get_studies_studyDbId_observationvariables(con = rv$con, studyDbId = study_id))
            #   }), use.names = T, fill = T)
            #   variables <- unique(trial_vars[,.(observationVariableDbId, observationVariableName)])
            #
            #   trait_choices <- variables[,observationVariableDbId]
            #   names(trait_choices) <- variables[,observationVariableName]
            #   updateSelectizeInput(
            #     inputId = "trait", session = session, choices = trait_choices,
            #     options = list(
            #       placeholder = 'Select a trait',
            #       onInitialize = I('function() { this.setValue(""); }')
            #     )
            #   )
            # })
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
        # observe({
        #   print(paste(input$trials, parse_GET_param()$trialDbId, collapse = ""))
        #   # rv$trialDbId <- paste(input$trials, parse_GET_param()$trialDbId, collapse = "")
        # })
        observeEvent(input$trials,{
          req(input$trials)
          rv$trialDbId <- input$trials
        })

        observeEvent(rv$trialDbId,{
          req(rv$trialDbId)
          try({
            # get all the studies of a trial
            trial_studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, trialDbId = rv$trialDbId))
            trial_studies$studyDbId %>% print()
            rv$data_studies <- rbindlist(l = lapply(trial_studies$studyDbId, function(study_id){
              try({
                study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = study_id))
                study[,is.excluded:=F]
                study[,observations.value:=as.numeric(observations.value)] # XXX this should not always be the case
                return(study)
              })
            }), use.names = T, fill = T)

            updateSelectizeInput(
              inputId = "trait", session = session,
              choices = rv$data_studies[,unique(observations.observationVariableName)],
              options = list(
                placeholder = 'Select a trait'
                # onInitialize = I('function() { this.setValue(""); }')
              )
            )
            study_names <- rv$data_studies[,.(id = unique(studyDbId), name = unique(studyName))]
            study_choices <- study_names[,id]
            names(study_choices) <- study_names[, name]
            updateSelectizeInput(
              inputId = "studies", session = session, choices = study_choices,
              options = list(
                placeholder = 'Select 1 or more environments',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
        })
        # observeEvent(input$trait,{
        #   # 4/ get the studies with observations for a given trait
        #   req(input$trait)
        #   try({
        #     if(is.null(parse_GET_param()$trialDbId)){
        #       studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, observationVariableDbId = as.character(input$trait)))
        #     }else{
        #       studies <- as.data.table(brapirv2::brapi_get_studies(con = rv$con, observationVariableDbId = as.character(input$trait), trialDbId = parse_GET_param()$trialDbId))
        #     }
        #     studies <- unique(studies[,.(studyDbId,studyName)])
        #     study_choices <- studies[,studyDbId]
        #
        #     names(study_choices) <- studies[,studyName]
        #     updateSelectizeInput(
        #       inputId = "studies", session = session, choices = study_choices,
        #       options = list(
        #         placeholder = 'Select 1 or more environments',
        #         onInitialize = I('function() { this.setValue(""); }')
        #       )
        #     )
        #   })
        # })

        studiesTD <- eventReactive(input$studies,{
          req(input$studies)
          req(input$trait)
          print(input$trait)
          print(input$studies)
          print(rv$data_studies[studyDbId%in%input$studies])
          studiesTD <- createTD(
            data = rv$data_studies[
              studyDbId%in%input$studies &
                observations.observationVariableName == input$trait
            ],
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
          TDall[,is.excluded:=F]
          studiesTD <- addTD(TD = studiesTD, data = TDall)
          setDT(studiesTD$all)

          return(studiesTD)
        })
      }
      # observe({
      #   foo <<- studiesTD()
      # })
      return(studiesTD)
    }
  )
}
