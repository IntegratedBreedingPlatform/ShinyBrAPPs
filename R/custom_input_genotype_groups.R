#' group_selector
#'
#' input with the detail of genotype groups selectable by checkboxes
#'
#' @description creates a group_selector component
#' @param inputId
#' @param label
#' @param group_table
#' @param panel_style default, info, primary, warning, danger
#'
#' @export
group_selector <- function(input_id, group_table, panel_style = "default", ...) {
  ns <- NS(input_id)
  custom_panel <- div(
    id=input_id,
    class = "genotypes_groups",
    tagList(
      tags$h3("Groups of Genotypes"),
      lapply(group_table[,group_id], function(id){
        tagList(
          shiny::tags$div(
            class = paste0("panel panel-", panel_style),
            value = paste0("value-",id),
            shiny::tags$div(
              class = "panel-heading",
              role = "tab",
              id = paste0("heading_", ns(id)),
              tags$input(id = ns(id), type = "checkbox", class = "group_checkbox", group_id = id, value = id),
              shiny::tags$h4(
                class = "panel-title",
                style = "display:inline",
                shiny::tags$a(
                  `data-toggle` = "collapse",
                  href = paste0("#div-",ns(id)),
                  group_table[group_id==id, group_name]
                )
              )
            ),
            shiny::tags$div(
              id = paste0("div-", ns(id)),
              class = "panel-collapse collapse",
              role = "tabpanel",
              shiny::tags$div(
                class = "panel-body",
                tags$label(paste(group_table[group_id==id, N], "genotypes: ")),
                tags$p(group_table[group_id==id, germplasmNames_label]),
                tags$label("Description: "),
                tags$p(group_table[group_id==id, group_desc])
              )
            )
          )
        )
      })
    )
  )

  shinyhtml <- tagList(
    singleton(
      tags$head(
        tags$script(src="js/custom_inputs/InputBinding_genotype_group_checkbox.js")
      )
    ),
    custom_panel
  )

  return(shinyhtml)
}
