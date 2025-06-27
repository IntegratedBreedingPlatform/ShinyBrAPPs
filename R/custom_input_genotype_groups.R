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
  
  # Split the table by clustering_id
  group_list <- split(group_table, by = "clustering_id", drop = FALSE)
  cluster_ids <- unique(group_table[, clustering_id])
  groups_list <- list()
  clusters <- c()
  for (grId in group_table[, group_id]) {
    if (is.na(group_table[group_id == grId, clustering_id])) {
      groups_list <- append(groups_list, list(group_table[group_id == grId,]))
    } else {
      clId <- group_table[group_id == grId, clustering_id]
      if (!clId %in% clusters) {
        groups_list <- append(groups_list, list(group_table[clustering_id == clId,]))
        clusters <- append(c, clId)
      }
    }
  }

  custom_panel <- div(
    id = input_id,
    class = "genotypes_groups",
    tagList(
      tags$h3("Groups of Genotypes"),
      
      # Global checkbox
      tags$div(
        class = "form-check mb-3 ms-2",
        tags$input(
          type = "checkbox",
          class = "form-check-input group-global-select-all",
          id = ns("select_all_groups")
        ),
        tags$label(
          class = "form-check-label ms-2",
          `for` = ns("select_all_groups"),
          "Select all"
        )
      ),
      
      # Loop over clustering_id groups
      lapply(groups_list, function(groups) {
        clustering_checkbox <- NULL
        cl_id <- unique(groups[,clustering_id])
        if (!is.na(cl_id)) { 
          # Checkbox for same cluster groups
          clustering_checkbox <- tags$div(
            class = "form-check mb-2",
            tags$input(
              type = "checkbox",
              class = "form-check-input group-cluster-select-all",
              id = ns(paste0("select_all_clust_", cl_id)),
              `data-clustering-id` = cl_id
            ),
            tags$label(
              class = "form-check-label fw-bold ms-2",
              `for` = ns(paste0("select_all_clust_", cl_id)),
              paste0("Groups from clustering ", cl_id)
            )
          )
        }
        
        panels <- lapply(groups[, group_id], function(id) {
          group_info <- groups[group_id == id]
          group_detail <- unique(data_plot[
            germplasmDbId %in% unlist(group_info$germplasmDbIds),
            .SD, .SD = column_datasource[source == "germplasm", cols]
          ])
          setcolorder(group_detail, c("germplasmDbId", "germplasmName"))
          
          div(
            class = paste0("panel panel-", panel_style),
            value = paste0("value-", id),
            div(
              class = "panel-heading",
              role = "tab",
              id = paste0("heading_", ns(id)),
              tags$input(
                id = ns(id),
                type = "checkbox",
                class = "group_checkbox",
                `data-clustering-id` = ifelse(is.na(cl_id), "manual", cl_id),
                group_id = id,
                value = id
              ),
              tags$h4(
                class = "panel-title space-left space-right",
                style = "display:inline",
                tags$a(
                  `data-bs-toggle` = "collapse",
                  href = paste0("#div-", ns(id)),
                  group_info$group_name
                )
              ),
              div(
                class = "d-flex",
                tags$p(
                  class = "me-auto",
                  paste0(group_info$N, " genotype", ifelse(group_info$N > 1, "s", ""))
                ),
                a(
                  icon('copy'),
                  class = "ms-2",
                  onclick = paste0(
                    'copyToClipboard("genotype-names-', id, '","',
                    paste(unlist(group_detail$germplasmName), collapse = "\t"), '")'
                  ),
                  style = "color: inherit; text-decoration: none; cursor: grab;",
                  span(
                    class = "copiedNotif",
                    id = paste0("copied-notif-genotype-names-", id),
                    `aria-hidden` = "true",
                    "Copied!", style = "display:none;"
                  )
                )
              )
            ),
            div(
              id = paste0("div-", ns(id)),
              class = "panel-collapse collapse",
              role = "tabpanel",
              div(
                class = "panel-body",
                tags$label("Description:"),
                tags$div(HTML(group_info$group_desc)),
                DT::datatable(
                  group_detail[, .(germplasmName)],
                  rownames = FALSE,
                  height = "250px",
                  options = list(
                    paging = FALSE,
                    scrollX = TRUE,
                    scrollY = "200px",
                    scrollCollapse = TRUE,
                    dom = 't'
                  )
                )
              )
            )
          )
        })
        
        tagList(
          tags$hr(style = "margin-top: 1em; margin-bottom: 1em; border-color: gray"),
          clustering_checkbox,
          panels
        )
      }),
      
      # JS for cascading checkboxes
      tags$script(HTML(sprintf("
        $(document).on('change', '.group-global-select-all', function() {
          var checked = $(this).is(':checked');
          $('.group_checkbox, .group-cluster-select-all').prop('checked', checked).trigger('change');
        });

        $(document).on('change', '.group-cluster-select-all', function() {
          var cl_id = $(this).data('clustering-id');
          var checked = $(this).is(':checked');
          $('.group_checkbox[data-clustering-id=\"' + cl_id + '\"]').prop('checked', checked).trigger('change').trigger('input');
        });
      ")))
    )
  )
  
  shinyhtml <- tagList(
    singleton(
      tags$head(
        tags$script(src = "js/custom_inputs/InputBinding_genotype_group_checkbox.js"),
        tags$script('
function copyToClipboard(id, names) {
  var copiedNotif = document.getElementById("copied-notif-" + id);
  names = names.replace(/[\\t]+/g, \'\\n\');
  var dummy = document.createElement("textarea");
  document.body.appendChild(dummy);
  dummy.value = names;
  dummy.select();
  document.execCommand("copy");

  if (document.execCommand("copy")) {
    copiedNotif.style.display = "inline";
    var temp = setInterval(function () {
      copiedNotif.style.display = "none";
      clearInterval(temp);
    }, 2000);
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
