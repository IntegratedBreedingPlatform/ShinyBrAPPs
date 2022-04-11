#' list_selector
#'
#' input with the detail of lists selectable by checkboxes
#'
#' @description creates a list_selector component
#' @param inputId
#' @param label
#' @param list_table
#'
#' @export
list_selector <- function(input_id, list_table, ...) {
  shinyhtml <- tagList(
    singleton(
      tags$head(
        # tags$link(rel="stylesheet", type="text/css", href="list_selector"),
        tags$script(src="js/custom_inputs/list_selector.js")
      )
    ),
    div(id=input_id, class = "my_selector",
        tagList(
          tags$h3("Lists of genotypes"),
          lapply(list_table[,list_id], function(id){
            fluidRow(
              column(11,
                     bsCollapsePanel(
                       title = list_table[list_id==id, list_name],
                       tags$label(paste(list_table[list_id==id, N], "genotypes: ")),
                       tags$p(list_table[list_id==id, germplasmNames_label]),
                       tags$label("Description: "),
                       tags$p(list_table[list_id==id, list_desc])
                       # style = "info"
                     )
              ),
              column(1,
                     tags$input(id = id, type = "checkbox", class = "list_checkbox", value = id)
              )
            )
          })
        )
    )
  )
  return(shinyhtml)
}
