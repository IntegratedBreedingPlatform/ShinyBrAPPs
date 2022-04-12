#' group_selector
#'
#' input with the detail of genotype groups selectable by checkboxes
#'
#' @description creates a group_selector component
#' @param inputId
#' @param label
#' @param group_table
#'
#' @export
group_selector <- function(input_id, group_table, ...) {
  ns <- NS(input_id)
  shinyhtml <- tagList(
    singleton(
      tags$head(
        tags$script(src="js/custom_inputs/InputBinding_genotype_group_checkbox.js")
      )
    ),
    div(id=input_id, class = "my_selector",
        tagList(
          tags$h3("Groups of Genotypes"),
          lapply(group_table[,group_id], function(id){
            fluidRow(
              column(11,
                     bsCollapsePanel(
                       title = group_table[group_id==id, group_name],
                       tags$label(paste(group_table[group_id==id, N], "genotypes: ")),
                       tags$p(group_table[group_id==id, germplasmNames_label]),
                       tags$label("Description: "),
                       tags$p(group_table[group_id==id, group_desc])
                       # style = "info"
                     )
              ),
              column(1,
                     tags$input(id = ns(id), type = "checkbox", class = "group_checkbox", group_id = id, value = id)
              )
            )
          })
        )
    )
  )
  return(shinyhtml)
}
