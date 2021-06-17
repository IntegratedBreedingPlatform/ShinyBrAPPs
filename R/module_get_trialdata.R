#' @export
mod_get_trialdata_ui <- function(id){
  ns <- NS(id)

  tagList(
    # title
    htmlOutput("title_trial_name"),

    # UI for study selection (if no GET parameters)
    div(
      style = "display:none",
      id = ns("study_selection_UI"),
      textInput(ns("token"), "Token", placeholder = "Enter Token"),
      textInput(ns("cropDb"), "CropDb", value = "wheat", placeholder = "Enter cropDb -- or selectinput with GET /commoncropnames"),
      selectizeInput(
        ns("study"), label = "studyDbId", choices = NULL,
        options = list(
          placeholder = 'Select StudyDbId',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    selectInput(ns("trial"), label = "Trial", choices = NULL),
    selectInput(ns("trait"), label = "Trait", choices = NULL)
  )
}

#' @export
mod_get_trialdata_server <- function(id, rv, dataset_4_dev = NULL){
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)

      if(!is.null(dataset_4_dev)){
        dataset_4_dev[,is.missing:=F]
        trialdata <- reactive(dataset_4_dev)
      }else{
        # parses the url
        parse_GET_param  <- reactive({
          pars <- parseQueryString(session$clientData$url_search)
        })

        observeEvent(parse_GET_param(),{

          if(!(is.null(parse_GET_param()$token) | is.null(parse_GET_param()$cropDb) | is.null(parse_GET_param()$studyDbId))){

            rv$con <- brapirv1::brapi_connect(
              secure = TRUE,
              protocol = brapi_protocol,
              db = brapi_db,
              port = brapi_port,
              apipath = brapi_apipath,
              multicrop = TRUE,
              token = parse_GET_param()$token,
              # commoncropname = "wheat",
              commoncropname = parse_GET_param()$cropDb,
              granttype = "token",
              clientid = "brapir",
              bms = TRUE
            )
            study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = parse_GET_param()$studyDbId))
            bar <<- study
            rv$study <- study
          }else{

            shinyjs::show(id = "study_selection_UI")

            if(is.null(parse_GET_param()$token)){
              updateTextInput("token", session = session, value = parse_GET_param()$token)
            }

          }
        })


        # token -> cropdb -> study -> trial -> trait
        observeEvent(c(input$token,input$cropDb),{
          req(input$token)
          rv$con <- brapirv1::brapi_connect(
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
          studies <- brapirv1::brapi_get_studies(con = rv$con)

          updateSelectizeInput(inputId = "study", session = session, choices = unique(studies$studyDbId))
        })
        observeEvent(input$study,{
          req(input$study)
          study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = input$study))
          rv$study <- study
        })

        observeEvent(rv$study,{
          updateSelectInput(inputId = "trial", session = session, choices = unique(rv$study$trialName))
        })

        observeEvent(input$trial,{
          req(input$trial)
          output$titleTrialName <- renderUI({
            titlePanel(title = paste0("Trial: ", input$trial))
          })
          updateSelectInput(
            inputId = "trait", session = session,
            choices = rv$study[trialName==input$trial, unique(observations.observationVariableName)]
          )
        })

        trialdata <- eventReactive(input$trait,{
          req(input$trial)
          req(input$trait)
          return(rv$study[trialName==input$trial & observations.observationVariableName == input$trait])
        })
      }

      observeEvent(trialdata(),{
        trialdata()[,is.missing:=F]
        trialdata()[,observations.value:=as.numeric(observations.value)]
        trialdata()[(observations.value>90),is.missing:=T] ### XX fonction qui détecte les missing values (NA, "", ...)
      })

      return(trialdata)
    }
  )
}
