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
group_selector <- function(input_id, group_table, column_datasource, data_plot, panel_style = "default", ...) {
  ns <- NS(input_id)
  custom_panel <- div(
    id=input_id,
    class = "genotypes_groups",
    tagList(
      tags$h3("Groups of Genotypes"),
      lapply(group_table[,group_id], function(id){
        group_detail <- unique(data_plot[
          germplasmDbId %in% unlist(group_table[group_id==id, germplasmDbIds]),
          .SD, .SD = column_datasource[source == "germplasm", cols]
        ])
        setcolorder(group_detail, c("germplasmDbId", "germplasmName"))
        tagList(
          div(
            class = paste0("panel panel-", panel_style),
            value = paste0("value-",id),
            div(
              class = "panel-heading",
              role = "tab",
              id = paste0("heading_", ns(id)),
              tags$input(id = ns(id), type = "checkbox", class = "group_checkbox", group_id = id, value = id),
              tags$h4(
                class = "panel-title space-left space-right",
                style = "display:inline",
                tags$a(
                  `data-bs-toggle` = "collapse",
                  href = paste0("#div-",ns(id)),
                  group_table[group_id==id, group_name]
                )
              ),
              div(class = "d-flex",
              tags$p(
                class = "me-auto",
                paste0(group_table[group_id==id, N], " genotype", ifelse(group_table[group_id==id, N]>1,"s", "")),
                # style = "float: right"
              ),
              a(
                icon('copy'),
                class = "ms-2",
                onclick = paste0('copyToClipboard(',
                                 paste0('"genotype-names-', id), 
                                 '","',paste(unlist(group_detail$germplasmName), collapse = "\t")
                                 ,'")'),
                style = "color: inherit; text-decoration: none; cursor: grab;",
                span(class = "copiedNotif", id = paste0("copied-notif-genotype-names-", id), `aria-hidden`="true", "Copied!", style = "display:none;"),
              ))
              
              # tags$button(
              #   class = "btn btn-primary", type = "button",
              #   `data-bs-toggle` = "collapse", `data-bs-target` = "#collapseCardBody",
              #   tags$i(class = "fas fa-caret-down")
              # ),
            ),
            div(
              id = paste0("div-", ns(id)),
              class = "panel-collapse collapse",
              role = "tabpanel",
              div(
                class = "panel-body",
                tags$label("Description:"),
                tags$div(HTML(group_table[group_id==id, group_desc])),
                #tags$label("Group Detail:"),
                DT::datatable(
                  group_detail[,.(germplasmName)],
                  rownames = F,
                  height = "250px",
                  options = list(
                    paging = F,
                    scrollX = T,
                    scrollY = "200px",
                    scrollCollapse = T,
                    dom = 't'
                  )
                )
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
        tags$script(src="js/custom_inputs/InputBinding_genotype_group_checkbox.js"),
        tags$script('
function copyToClipboard(id, names) {
  //var names = document.getElementById(id).textContent,
      copiedNotif = document.getElementById("copied-notif-"+id);
  names = names.replace(/[\t]+/g, \'\\n\');
  var dummy = document.createElement("textarea");
  document.body.appendChild(dummy);
  dummy.value = names;
  dummy.select();
  document.execCommand("copy");

  if (document.execCommand("copy")) {
      copiedNotif.style.display = "inline";
      var temp = setInterval( function(){
      copiedNotif.style.display = "none";
        clearInterval(temp);
      }, 2000 );
  } else {
    console.info( "document.execCommand went wrong…" )
  }

  document.body.removeChild(dummy);
  return false;
}
                    ')
      )
    ),
    custom_panel
  )

  return(shinyhtml)
}
