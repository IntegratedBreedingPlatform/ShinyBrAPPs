ui <- fluidPage(
  tags$p("Hello world")
)

server <- function(input, output, session){
  print("retrieving OAUTH_CLIENT_ID")
  print(Sys.getenv("OAUTH_CLIENT_ID"))
  print("retrieving OAUTH_CLIENT_REDIRECT_URI")
  print(Sys.getenv("OAUTH_CLIENT_REDIRECT_URI"))
}

shinyApp(ui = ui, server = server)
