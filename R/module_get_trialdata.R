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
        ns("study"), label = "study", choices = NULL,
        options = list(
          placeholder = 'Select Study',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    selectInput(ns("trait"), label = "Trait", choices = NULL)
  )
}

#' @export
mod_get_studydata_server <- function(id, rv, dataset_4_dev = NULL){ # XXX dataset_4_dev = NULL
  moduleServer(
    id,
    function(input, output, session){

      ns <- NS(id)

      if(!is.null(dataset_4_dev)){ # XXX
        dataset_4_dev[,is.missing:=F]
        studydata <- reactive(dataset_4_dev)
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
              commoncropname = parse_GET_param()$cropDb,
              granttype = "token",
              clientid = "brapir",
              bms = TRUE
            )
            study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = parse_GET_param()$studyDbId))
            rv$study <- study
          }else{

            shinyjs::show(id = "study_selection_UI")

            if(is.null(parse_GET_param()$token)){
              updateTextInput("token", session = session, value = parse_GET_param()$token)
            }

          }
        })


        # token -> cropdb -> study -> trait
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
          try({
            studies <- as.data.table(brapirv1::brapi_get_studies(con = rv$con))
            studies <- unique(studies[,.(studyDbId,studyName)])
            study_choices <- studies[,studyDbId]
            names(study_choices) <- studies[,studyName]
            updateSelectizeInput(inputId = "study", session = session, choices = study_choices)
          })
        })

        observeEvent(input$study,{
          req(input$study)
          study <- as.data.table(brapirv1::brapi_get_studies_studyDbId_observationunits(con = rv$con, studyDbId = input$study))

          updateSelectInput(
            inputId = "trait", session = session,
            choices = study[, unique(observations.observationVariableName)]
          )
          rv$study <- study
        })



        studydata <- eventReactive(input$trait,{
          req(input$trait)
          return(rv$study[observations.observationVariableName == input$trait])
        })

      }

      observeEvent(studydata(),{
        studydata()[,is.missing:=F]
        studydata()[,observations.value:=as.numeric(observations.value)]

        output$title_study_name <- renderUI({
          h1(rv$study[studyDbId==input$study, unique(studyName)])
        })
      })

      return(studydata)
    }
  )
}
