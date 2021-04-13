library(shiny)
library(shinyjs)

shinyapp_name <- "shiny app test auth"
shinyapp_url <- "http://127.0.0.1:7152" # runApp("app.R", launch.browser = T, port = 7152)

authenticate_bms <- function(){
  ## re-directs to the bms logging page
  runjs(
    paste0(
      'window.location.replace("',
      'https://brapi.bms-uat-test.net/ibpworkbench/controller/auth/login?display_name=', shinyapp_name,
      '&return_url=',shinyapp_url,'")'
    )
  )
}

rv <- reactiveValues()

ui <- fluidPage(
  useShinyjs(),
  uiOutput("body")
)

server <- function(input, output, session){
  token_parse  <- reactive({
    ## listens the url
    # if url/?token=<TOKEN>&..., returns <TOKEN>
    pars <- parseQueryString(session$clientData$url_search)
    if(length(pars$token) > 0){
      return(pars$token)
    } 
  })
  
  observeEvent(token_parse(),{
    ## stores the token in a reactive value
    if(length(token_parse())>0){
      rv$token <- token_parse()
    }
  })
  
  observe({
    if(is.null(rv$token)){
      output$body <- renderUI({
        tagList(h3("Not logged in"))
      })
      authenticate_bms()
    }else{
      output$body <- renderUI({
        tagList(
          tags$label("token: "),
          tags$code(rv$token),
          tags$br(),
          actionButton("go_authenticate", "Authenticate again", icon=icon("sign-in"))
        )
      })
    }
  })
  
  observeEvent(input$go_authenticate,authenticate_bms())
}

shinyApp(ui = ui, server = server)
