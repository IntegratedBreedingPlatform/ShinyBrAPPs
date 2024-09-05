#' @export
mod_groups_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,uiOutput(ns("ui_groups")))
    ),
    fluidRow(
      column(12,
             bslib::card(
               class = ns("at_least_one_group_selected"),
               bslib::card_header(
                 h4('Actions ', icon('screwdriver-wrench'))
               ),
               bslib::card_body(
                 #title = span('Options ', icon('screwdriver-wrench')),
                 #width = 12,
                 #h4('Actions ', icon('screwdriver-wrench')),
                 actionButton(ns("action_groups_plot_creation_params"),label = "Visualize like at group creation", block = T, class = paste("btn btn-info", ns("one_group_selected"))),
                 actionButton(ns("action_groups_union"),label = "Union", block = T, class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                 actionButton(ns("action_groups_intersect"),label = "Intersect", block = T, class = paste("btn btn-info", ns("create_new_groups_from_groups"))),
                 actionButton(ns("action_groups_complement"),label = "Complement", block = T, class = paste("btn btn-info", ns("at_least_one_group_selected"))),
                 actionButton(ns("action_groups_delete"),label = "Delete", block = T, class =paste("btn btn-info", ns("at_least_one_group_selected")))
               )
             ),
             bslib::card(
               id = ns("export_box"),
               bslib::card_header(
                 h4('Export ')
               ),
               bslib::card_body(
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
          group_selector(input_id = ns("group_sel_input"), group_table = rv$groups, column_datasource = rv$column_datasource, data_plot = rv$data_plot, panel_style = "info")
        })
      })
      
      ## Displaying buttons ####
      observe({
        shinyjs::toggle(selector = paste0(".",ns("at_least_one_group_selected")), condition = length(input$group_sel_input)>0)
        shinyjs::toggle(selector = paste0(".",ns("create_new_groups_from_groups")), condition = length(input$group_sel_input)>1)
        shinyjs::toggle(selector = paste0(".",ns("one_group_selected")), condition = length(input$group_sel_input)==1)
        shinyjs::toggle(id = "export_box", condition = length(input$group_sel_input)==1)
      })
      
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
        toggleModal(session, "modal_create_group")
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
          showModal(modalDialog(tags$label("This intersection results in an empty group.")))
        }
        
      })
      
      ## Complement ####
      observeEvent(input$action_groups_complement,{
        toggleModal(session, "modal_create_group")
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
          rv$data_plot[,eval(rv$groups[group_id == k, group_name]) := NULL]
        }
        rv$column_datasource <- rv$column_datasource[!(cols %in% rv$groups[group_id %in% input$group_sel_input, group_name])]
        
        ## delete groups
        rv$groups <- rv$groups[!(group_id %in% input$group_sel_input)]
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
          brapirv2::brapi_post_lists(
            con = rv$con,
            data = rv$groups[group_id == input$group_sel_input, germplasmDbIds][[1]],
            listSize = rv$groups[group_id == input$group_sel_input, N],
            dateCreated = as.character(Sys.Date()), # XXX
            dateModified = as.character(Sys.Date()), # XXX
            listName = input$listName,
            listDescription= input$listDescription,
            listOwnerName = paste(userinfo$firstName,userinfo$username), # XXX
            listOwnerPersonDbId = as.character(userinfo$id), # XXX
            listSource = "test", # XXX
            listType = "germplasm"
          )
          showNotification("List posted", type = "message", duration = notification_duration)
        }, error = function(e)({
          showNotification("Could not post list", type = "error", duration = notification_duration)
        }))
      })
      
      #Function to retrieve variables of selection type :
      # First, filter on traitClass = selection_trait_class (config param) in WS 
      # Then, get variables with traitName = selection_trait_name (config param)
      getSelectionVariables <- function(studyDbIds) {
        res <- brapirv2::brapi_post_search_variables(
          con = rv$con, 
          studyDbId = as.character(studyDbIds),
          traitClasses = selection_traitClass
        )
        var <- data.table(brapirv2::brapi_get_search_variables_searchResultsDbId(rv$con, res$searchResultsDbId))
        if (nrow(var) > 0) {
          var <- var[trait.traitName == selection_traitName, .(observationVariableDbId, observationVariableName)]
        }
        return(var)
      }
      
      ## Mark as selection ####
      observeEvent(input$action_groups_mark_as_selection,{
        req(length(input$group_sel_input)==1)
        envs <- unique(rv$data_plot[,.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]
        
        #propose only variables that are selection type
        variables <- getSelectionVariables(env_choices)
       
        if (nrow(variables) > 0) {
          variable_choices <- variables[, observationVariableDbId]
          names(variable_choices) <- variables[, observationVariableName]
          
          showModal(modalDialog(
            fade = F,
            title = "Export Group as List",
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
            title = "Export Group as List",
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
        req(rv_plot$as_sel_data[,.N]>0)
        withProgress(message = "POST brapi/v2/observations", value = 0, {
          lapply(rownames(rv_plot$as_sel_data), function(x){
            row_id <- as.numeric(x)
            incProgress(1/length(rownames(rv_plot$as_sel_data)))
            a <- tryCatch({
              brapirv2::brapi_post_observations(
                con = rv$con,
                studyDbId = as.character(rv_plot$as_sel_data[row_id, studyDbId]),
                germplasmDbId = as.character(rv_plot$as_sel_data[row_id, germplasmDbId]),
                observationUnitDbId = as.character(rv_plot$as_sel_data[row_id, observationUnitDbId]),
                observationVariableDbId = as.character(rv_plot$as_sel_data[row_id, observationVariableDbId]),
                value = as.character(rv_plot$as_sel_data[row_id, observationValue]),
                additionalInfo = list() # otherwise error message: "Argument: \"additionalInfo\" should be provided as a list, see the help page on how the list should be constructed."
              )
            }, error = function(e)({e})
            )
            mess <- a$message
            if(!is.null(mess)){
              showNotification(
                ui =
                  tagList(
                    tags$p("Could not post observation"),
                    tags$code(paste0(
                      '{ "studyDbId":"', as.character(rv_plot$as_sel_data[row_id, studyDbId]),'",',
                      '"germplasmDbId":"', as.character(rv_plot$as_sel_data[row_id, germplasmDbId]),'",',
                      '"observationUnitDbId":"', as.character(rv_plot$as_sel_data[row_id, observationUnitDbId]),'",',
                      '"observationVariableDbId":"', as.character(rv_plot$as_sel_data[row_id, observationVariableDbId]),'",',
                      '"value":"', as.character(rv_plot$as_sel_data[row_id, observationValue]),'"'
                    )),
                    tags$code(mess)
                  ),
                type = "error", duration = notification_duration
              )
            }
          })
        })
        removeModal()
        rv_plot$as_sel_data <- NULL
      })
      
      
      ## download group details ####
      output$action_groups_export_group_details <- downloadHandler(
        filename = function() {
          paste0("group_", input$group_sel_input, ".csv")
        },
        content = function(file) {
          group_detail <- unique(rv$data_plot[
            germplasmDbId %in% unlist(rv$groups[group_id%in%input$group_sel_input, germplasmDbIds]),
            .SD, .SD = rv$column_datasource[source == "germplasm", cols]
          ])
          write.csv(group_detail, file, row.names = F)
        }
      )
      
    }
  )
}