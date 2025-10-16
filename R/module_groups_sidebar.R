#' @import bslib
#' @export
mod_groups_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        label.required:after {
          content: ' *';
          color: red;
        }
      "))
    ),
    fluidRow(
      column(12,uiOutput(ns("ui_groups")))
    ),
    fluidRow(
      column(12,
             card(
               #class = ns("at_least_one_group_selected"),
               card_header(
                 h4('Actions ', icon('screwdriver-wrench'))
               ),
               card_body(
                 #actionButton(ns("action_groups_plot_creation_params"),label = "Visualize like at group creation", block = T, class = paste("btn btn-info", ns("one_group_selected"))),
                 actionButton(ns("action_groups_create"),label = "Add new group", block = T, class = "btn btn-info"),
                 actionButton(ns("action_groups_union"),label = "Union", block = T, class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                 actionButton(ns("action_groups_intersect"),label = "Intersect", block = T, class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                 actionButton(ns("action_groups_complement"),label = "Complement", block = T, class = paste("btn btn-info", ns("at_least_one_group_selected"))),
                 actionButton(ns("action_groups_rename"),label = "Rename", block = T, class =paste("btn btn-info", ns("one_group_selected"))),
                 actionButton(ns("action_groups_delete"),label = "Delete", block = T, class =paste("btn btn-info", ns("all_groups_selected")))
               )
             ),
             card(
               id = ns("export_box"),
               card_header(
                 h4('Export ')
               ),
               card_body(
                 downloadButton(ns("action_groups_export_group_details"),label = "Export Group Details", class = "btn-block btn-primary"),
                 actionButton(ns("action_groups_export_as_list"),label = "Export as List", block = T, class = "btn btn-primary", icon = icon("cloud"), icon.library = "font awesome"),
                 actionButton(ns("action_groups_mark_as_selection"),label = "Mark as Selection", block = T, class = "btn btn-primary", icon = icon("cloud"), icon.library = "font awesome")
               )
             )
      )
    )
  )
}

#' @export
# SERVER ####
mod_groups_sidebar_server <- function(id, rv, parent_session){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      parent_ns <- parent_session$ns
      observeEvent(rv$groups$group_id,{
        req(rv$groups)
        req(length(rv$groups) > 0)
        output$ui_groups <- renderUI({
          group_selector(input_id = ns("group_sel_input"), group_table = rv$groups, column_datasource = rv$column_datasource, data_plot = rv$extradata, panel_style = "info")
        })
        save_user_data(rv)
      })
      
      ## Displaying buttons ####
      observeEvent(input$group_sel_input, {
        #selected_groups <- rv$groups[group_id %in% input$group_sel_input,]
        shinyjs::toggle(selector = paste0(".",ns("at_least_one_group_selected")), condition = length(input$group_sel_input)>0 && length(input$group_sel_input)<nrow(rv$groups))
        shinyjs::toggle(selector = paste0(".",ns("create_new_groups_from_groups")), condition = length(input$group_sel_input)>1 && length(input$group_sel_input)<nrow(rv$groups))
        shinyjs::toggle(selector = paste0(".",ns("one_group_selected")), condition = length(input$group_sel_input)==1)
        shinyjs::toggle(id = "export_box", condition = length(input$group_sel_input)==1)
        shinyjs::toggle(selector = paste0(".",ns("all_groups_selected")), condition = length(input$group_sel_input)>0)
        
        req(input$group_sel_input)
        req(rv$groups)
        sel_groups <- rv$groups[group_id %in% input$group_sel_input,]
        clusterings <- sel_groups[, clustering_id]
        shinyjs::enable("action_groups_delete")
        for (id in clusterings) {
          if (nrow(sel_groups[clustering_id == id, ]) < nrow(rv$groups[clustering_id == id, ])) {
            # can't delete clustering group if not all selected
            shinyjs::disable("action_groups_delete")
            break
          }
        }
      }, ignoreNULL = F)
      
      ## Union ####
      observeEvent(input$action_groups_union,{
        union_germplasms_id <- rv$groups[group_id %in% input$group_sel_input, unlist(germplasmDbIds)]
        germplasms <- unique(rv$data[germplasmDbId %in% union_germplasms_id, .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv$groups$group_id), 1, max(rv$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv$selection <- selection_data
        
        showModal(groupModal(rv, parent_session, "Create new group from Union", 
                             paste(rv$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∪ ")))
      })
      
      ## Intersect ####
      observeEvent(input$action_groups_intersect,{
        intersect_germplasms_id <- Reduce(intersect, rv$groups[group_id %in% input$group_sel_input, germplasmDbIds])
        germplasms <- unique(rv$data[germplasmDbId %in% intersect_germplasms_id, .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv$groups$group_id), 1, max(rv$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv$selection <- selection_data
        
        if(rv$selection[1,N>0]){
          showModal(
            groupModal(rv, parent_session,
              "Create new group from Intersection",
              paste(rv$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∩ ")
            )
          )
        } else {
          showModal(modalDialog(tags$label("This intersection results in an empty group."), fade = F))
        }
        
      })
      
      ## Complement ####
      observeEvent(input$action_groups_complement,{
        union_germplasms_id <- rv$groups[group_id %in% input$group_sel_input, unlist(germplasmDbIds)]
        germplasms <- unique(rv$data[!(germplasmDbId %in% union_germplasms_id), .(germplasmDbId, germplasmName)])
        group_id <- ifelse(is.null(rv$groups$group_id), 1, max(rv$groups$group_id) + 1)
        selection_data <- data.table(
          group_id = group_id,
          N = germplasms[,.N],
          germplasmDbIds = list(germplasms$germplasmDbId),
          germplasmNames = list(germplasms$germplasmName)
        )
        selection_data[,germplasmNames_label := if(N>6){
          paste(
            paste(unlist(germplasmNames)[1:5], collapse = ", "),
            paste("and", N - 5, "others")
          )
        }else{
          paste(unlist(germplasmNames), collapse = ", ")
        }]
        rv$selection <- selection_data
        
        showModal(
          groupModal(rv, parent_session,
            "Create new group from Complement",
            paste("Complement of (", paste(rv$groups[group_id %in% input$group_sel_input, group_name], collapse = " ∪ "), ")")
          )
        )
      })
      
      ## Visualize with group param ####
      observeEvent(input$action_groups_plot_creation_params,{
        rv$visu_as_group <- input$group_sel_input
      })      
      
      ## Delete ####
      observeEvent(input$action_groups_delete,{
        ## update selectors (shape, colour)
        for(k in input$group_sel_input){
          rv$extradata[,eval(rv$groups[group_id == k, group_name]) := NULL]
        }
        if ("clustering_name" %in% colnames(rv$groups)) {
          rv$column_datasource <- rv$column_datasource[!(cols %in% c(rv$groups[group_id %in% input$group_sel_input & is.na(clustering_name), group_name], unique(rv$groups[group_id %in% input$group_sel_input & !is.na(clustering_name), clustering_name])))]
        } else {
          rv$column_datasource <- rv$column_datasource[!(cols %in% rv$groups[group_id %in% input$group_sel_input, group_name])]
        }
        ## delete groups
        rv$groups <- rv$groups[!(group_id %in% input$group_sel_input)]
      })
      
      ## Rename a group ####
      observeEvent(input$action_groups_rename,{
        req(length(input$group_sel_input) == 1)
        rv$selected_group_id <- input$group_sel_input
        showModal(
          renameGroupModal(rv, parent_session)
        )
      })
      
      ## Export as list ####
      observeEvent(input$action_groups_export_as_list,{
        showModal(modalDialog(
          fade = F,
          title = "Export Group as List",
          tagList(
            textInput(
              ns("listName"),
              label = "List Name",
              value = rv$groups[group_id == input$group_sel_input, group_name],
              placeholder = "Human readable name of a List",
              width = "100%"
            ),
            textAreaInput(
              ns("listDescription"),
              label = "List description",
              value = rv$groups[group_id == input$group_sel_input, group_desc],
              placeholder = "Description of a List",
              width = "100%"
            )
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("go_create_list"), "Create list", class = "btn btn-info")
          )
        ))
      })
      
      # observeEvent(c(input$listDescription, input$listName), {
      #   if(input$listName != "" & input$listDescription != ""){
      #     output$go_create_list_ui <- renderUI({actionButton(ns("go_create_list"), "Create list", class = "btn btn-primary")})
      #   }else{
      #     output$go_create_list_ui <- renderUI({actionButton(ns("go_create_list"), "Create list", class = "btn btn-primary", disabled = "")})
      #   }
      # })
      
      ## Go export list ####
      observeEvent(input$go_create_list,{
        removeModal()
        req(length(input$group_sel_input)==1)
        tryCatch({
          userinfo <- whoami_bmsapi(rv$con)
          # brapirv2::brapi_post_lists(
          #   con = rv$con,
          #   data = rv$groups[group_id == input$group_sel_input, germplasmDbIds][[1]],
          #   listSize = rv$groups[group_id == input$group_sel_input, N],
          #   dateCreated = as.character(Sys.Date()), # XXX
          #   dateModified = as.character(Sys.Date()), # XXX
          #   listName = input$listName,
          #   listDescription= input$listDescription,
          #   listOwnerName = paste(userinfo$firstName,userinfo$username), # XXX
          #   listOwnerPersonDbId = as.character(userinfo$id), # XXX
          #   listSource = "test", # XXX
          #   listType = "germplasm"
          # )
          resp <- brapir::core_lists_post(
            con = rv$con,
            data = rv$groups[group_id == input$group_sel_input, germplasmDbIds][[1]],
            dateCreated = as.character(Sys.Date()),
            dateModified = as.character(Sys.Date()),
            listDescription = input$listDescription,
            listName = input$listName,
            listOwnerName = paste(userinfo$firstName,userinfo$username),
            listOwnerPersonDbId = as.character(userinfo$id),
            listSize = rv$groups[group_id == input$group_sel_input, N],
            listSource = "test",
            listType = "germplasm"
          )
          if (resp$status_code == 200) {
            showNotification("List posted", type = "message", duration = notification_duration)
          } else {
            showNotification(resp$status_code, type = "error", duration = notification_duration)
          }
        }, error = function(e)({
          showNotification("Could not post list", type = "error", duration = notification_duration)
        }))
      })
      
      #Function to retrieve variables of selection type :
      # First, filter on traitClass = selection_trait_class (config param) in WS 
      # Then, get variables with traitName = selection_trait_name (config param)
      getSelectionVariables <- function(studyDbIds) {
        withProgress(message = "Looking for variables of selection type", min=1, max=1, {
          res <- brapir::phenotyping_variables_post_search(
            con = rv$con, 
            studyDbId = as.character(studyDbIds),
            traitClasses = selection_traitClass
          )
          if (res$status_code == 200) {
            resp <- brapir::phenotyping_variables_get_search_searchResultsDbId(rv$con, res$data$searchResultsDbId)$data
            var <- data.table(brapir::phenotyping_variables_get_search_searchResultsDbId(rv$con, res$data$searchResultsDbId)$data)
            if (nrow(var) > 0) {
              var <- var[trait.traitName == selection_traitName, .(observationVariableDbId, observationVariableName)]
            }
            return(var)
          }
          
        })
      }
      
      ## Mark as selection ####
      observeEvent(input$action_groups_mark_as_selection,{
        req(length(input$group_sel_input)==1)
        envs <- unique(rv$extradata[,.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]
        #propose only variables that are selection type
        variables <- getSelectionVariables(env_choices)
       
        if (nrow(variables) > 0) {
          variable_choices <- variables[, observationVariableDbId]
          names(variable_choices) <- variables[, observationVariableName]
          
          showModal(modalDialog(
            fade = F,
            title = "Mark group as selection",
            tagList(
              awesomeCheckboxGroup(
                inputId = ns("mark_as_sel_envs"),
                label = "Environments",
                choices = env_choices,
                selected = env_choices,
                width = "100%"
              ),
              pickerInput(
                inputId = ns("mark_as_sel_var_to_use"),
                label = "Variable to use",
                choices = variable_choices,
                inline = T
              ),
              # pickerInput(
              #   inputId = ns("mark_as_sel_trait_classes"),
              #   label = "Filter Variable by Trait Class",
              #   choices = trait_classes,
              #   inline = T
              # ),
              awesomeRadio(
                ns("mark_as_sel_all_plots_radio"),
                label = "",
                choices = list(`Mark all plots` = "all", `Mark the first replicate of each plot` = "rep1"),
                selected = "rep1",
                status = "primary"
              ),
              #uiOutput(ns("mark_as_sel_info")),
            ),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("action_groups_mark_as_selection_go"),
                           "OK", style = "primary")
            )
          ))
        } else {
          showModal(modalDialog(
            fade = F,
            title = "Mark group as selection",
            p("there is no variable of selection type in the system"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Ok")
            )
          ))
        }
      })
      
      ## Go mark as selection ####
      observeEvent(input$action_groups_mark_as_selection_go,{
        req(input$mark_as_sel_var_to_use, input$mark_as_sel_all_plots_radio, input$mark_as_sel_envs)
        
        varname = unique(rv$data[observationVariableDbId == input$mark_as_sel_var_to_use,.(observationVariableName)])
        envs <- unique(rv$extradata[,.(studyDbId, study_name_app)])
        
        brapir_con <- brapir::brapi_connect(
          secure = rv$con$secure, 
          db = rv$con$db, 
          port = rv$con$port, 
          apipath = rv$con$apipath, 
          multicrop = rv$con$multicrop, 
          commoncropname = rv$con$commoncropname,
          token = rv$con$token
        )
        
        withProgress(message = paste("Post", varname, "observations"), value = 0, {
          for (i in 1:length(input$mark_as_sel_envs)) {
            env_name <- envs[studyDbId == input$mark_as_sel_envs[i], study_name_app]
            incProgress(1/length(input$mark_as_sel_envs), detail = env_name)
            as_sel_data <- rv$data[
              observationLevel == "PLOT" & studyDbId %in% input$mark_as_sel_envs[i] &
                germplasmDbId %in% rv$groups[group_id == input$group_sel_input, germplasmDbIds][[1]]
            ]
            if(input$mark_as_sel_all_plots_radio=="rep1" & as_sel_data[replicate=="1",.N]>0){
              as_sel_data <- as_sel_data[replicate == "1"]
            }
            as_sel_data <- unique(as_sel_data[,.(germplasmDbId, observationUnitDbId, studyDbId)])
            
            as_sel_data[,observationVariableDbId := input$mark_as_sel_var_to_use]
            as_sel_data[,value := "1"]
            
            # Building body POST request
            body <- apply(as_sel_data,1,function(a){
              list(
                germplasmDbId = jsonlite::unbox(as.character(a["germplasmDbId"])),
                observationUnitDbId = jsonlite::unbox(as.character(a["observationUnitDbId"])),
                studyDbId = jsonlite::unbox(as.character(a["studyDbId"])),
                observationVariableDbId = jsonlite::unbox(as.character(a["observationVariableDbId"])),
                value = jsonlite::unbox(as.numeric(a["value"]))
              )
            })
            
            resp <- brapir::phenotyping_observations_post_batch(con = brapir_con, data = body)
            if (resp$status_code == 200) {
              created_observations_df <- resp$data
              showNotification(paste(nrow(created_observations_df), varname, "observations were pushed to BMS"), type = "message", duration = notification_duration)
            } else {
              showNotification(paste0("An error occured while creating BLUES/BLUPS observations for ", var_name), type = "error", duration = notification_duration)
              showNotification(paste0(resp$metadata), type = "error", duration = notification_duration)
            }
          }
        })
        removeModal()
      })
      
      
      ## download group details ####
      output$action_groups_export_group_details <- downloadHandler(
        filename = function() {
          paste0("group_", input$group_sel_input, ".csv")
        },
        content = function(file) {
          group_detail <- unique(rv$extradata[
            germplasmDbId %in% unlist(rv$groups[group_id%in%input$group_sel_input, germplasmDbIds]),
            .SD, .SD = rv$column_datasource[source == "germplasm", cols]
          ])
          write.csv(group_detail, file, row.names = F)
        }
      )
      
      ## create group manually ####
      observeEvent(input$action_groups_create, {
        germplasm <- ""
        if (!is.null(rv$data)) {
          germplasm <- unique(rv$data[, .(germplasmDbId, germplasmName)])$germplasmName
        } 
        showModal(modalDialog(
          title = "Create a new group",
          fade = F,
          tagList(
            # tags$label(paste(rv$selection[,N]," selected germplasms")),
            # tags$p(rv$selection[,germplasmNames_label]),
            textInput(
              ns("modal_create_group_text_input_label"), 
              label = tags$label("Group Name", class = "required"), 
              value = "", 
            placeholder = "Group Label"),
            textAreaInput(
              ns("modal_create_group_text_input_desc"), 
              label = tags$label("Group Description", class = "required"), 
              placeholder = "Group Description", 
              resize = "vertical",
              value = "",
              width = "100%"
            ),
            layout_columns(
              col_widths = c(8,4),
              pickerInput(
                ns("germplasm_list"), 
                label = bslib::tooltip(
                    trigger = list(
                      tags$label("Select germplasm", class = "required"),
                      icon("info-circle")
                    ),
                    "You can upload a txt file with germplasm names in one column",
                    options = list(container = ".modal-content") 
                ),
                choices = germplasm, 
                multiple = T,
                options = pickerOptions(
                  liveSearch = TRUE
                )
              ),
              fileInput(
                inputId = ns("file"),
                "Upload names"
              )
            ),
            textOutput(ns("selection")),
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("modal_create_group_manually_go"), label = "Create", class = "btn btn-info")
          )
        ))
      })
      
      ## upload germplasm names file ####
      observeEvent(input$file, {
        file <- input$file_names
        names <- NULL
        names <- readLines(input$file$datapath)
        updateTextAreaInput(
          session,
          "names",
          value = paste(names, collapse = "\n")
        )
        
        germplasm <- unique(rv$data[germplasmName %in% names, .(germplasmDbId, germplasmName)])
        
        if (nrow(germplasm)>0) {
          updatePickerInput(
            session = session, 
            inputId = "germplasm_list",
            selected = germplasm$germplasmName
          )
        }
        
      })
      
      output$selection <- renderText({
        req(input$germplasm_list)
        paste0(length(input$germplasm_list), " selected germplasm")
      })
      
      ## Action when clicking on button create group in manual modal
      observeEvent(input$modal_create_group_manually_go, {
        req(rv$data)
        germplasm <- unique(rv$data[germplasmName %in% input$germplasm_list, .(germplasmDbId, germplasmName)])
        new_group_id <- 1
        if (!is.null(rv$groups) && nrow(rv$groups) > 0) {
          new_group_id <- max(rv$groups$group_id)+1
        }
        new_group <- data.table(
          group_id = new_group_id, 
          group_name = input$modal_create_group_text_input_label,
          group_desc = input$modal_create_group_text_input_desc,
          germplasmDbIds = list(germplasm$germplasmDbId),
          germplasmNames = list(germplasm$germplasmName),
          clustering_id = NA,
          N = nrow(germplasm))
        rv$groups <- rbindlist(list(
          rv$groups,
          new_group
        ), fill = T, use.names = T)
        
        update_selectors_with_groups(rv, new_group)
        
        removeModal()
      })
      
      observeEvent(c(input$germplasm_list, input$modal_create_group_text_input_label, input$modal_create_group_text_input_desc), {
        if (!is.null(input$germplasm_list)
            & input$modal_create_group_text_input_label != "" & input$modal_create_group_text_input_desc != "") {
          shinyjs::enable("modal_create_group_manually_go")
        } else {
          shinyjs::disable("modal_create_group_manually_go")
        }
      }, ignoreInit = )
      
    }
  )
}