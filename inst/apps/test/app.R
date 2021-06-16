ui <- fluidPage(
  tags$p("Hello world")
)

server <- function(input, output, session){
}

shinyApp(ui = ui, server = server)
