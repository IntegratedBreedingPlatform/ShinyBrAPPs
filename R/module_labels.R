#' @import tableHTML
#' @import DT
#' @export
mod_samplr_ui <- function(id){
  ns <- NS(id)
      layout_sidebar(
        sidebar=bslib::sidebar(width = 350,
                               selectInput(ns("program"),label = "Program", choices = NULL, selected = NULL),
                               selectInput(ns("trial"),label = "Study name", choices = NULL),
                               selectizeInput(ns("studies"),label = "Environments", choices = NULL, multiple=TRUE),
                               div(style="display:flex;gap: 10px;",
                                   actionButton(ns("get_samples"),"Get samples"),
                                   uiOutput(ns("dbutton_container"))),
                               tags$style(make_css(list('.container-fluid', 'padding-left', '0px'))),
                               tags$style(make_css(list('.container-fluid', 'padding-right', '0px')))
        ),layout_columns(col_widths = c(6, 6),
                         card(full_screen = TRUE, height = "100%",
                              card_body(min_height = "600px",
                                        fluidPage(
                                          fluidRow(
                                            
                                            column(width = 1),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("1_1",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("1_1_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 2),
                                            column(width = 2,
                                                   div(style='margin-bottom:-2em;', selectizeInput("1_2",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("1_2_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 1)
                                          ),
                                          fluidRow(
                                            column(width = 1),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("2_1",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("2_1_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 2),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("2_2",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("2_2",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 1)
                                          ),
                                          fluidRow(
                                            column(width = 1),
                                            column(width = 2),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("3_1",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("3_1_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 2),
                                            column(width = 1)
                                          ),
                                          fluidRow(
                                            column(width = 1),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("4_1",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("4_1_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 2),
                                            column(width = 2, 
                                                   div(style='margin-bottom:-2em;', selectizeInput("4_2",label="",choices=collabs)),
                                                   div(style='margin-bottom:1em;',numericInput("4_2_fontsize",label="",min = 3, max = 12, value = 5) )),
                                            column(width = 1)
                                          ),
                                          fluidRow(
                                            column(width = 1),
                                            column(width = 2, 
                                                   selectizeInput("5_1",label="",choices=collabs)),
                                            column(width = 2),
                                            column(width = 2, 
                                                   selectizeInput("5_3",label="",choices=collabs)),
                                            column(width = 1)
                                          )
                                        )
                              )
                         ),
                         card(full_screen = TRUE, height = "100%"

                         )
                         )

      )
  
}

#' @export
mod_samplr_server <- function(id, rv){
  
  moduleServer(
    
    id,
    function(input, output, session){
      rv_labs <- reactiveValues(
      )
      
      ns <- session$ns
  
      output$download_data <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(rv_samp$stdatadt, file, row.names = F)
        }
      )
      
      output$download_data_byg <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(rv_samp$stdatadt_byg, file, row.names = F)
        }
      )      
      
    })
}
