#' @import data.table
server <- function(input, output, session){
  rv <- reactiveValues(
    con = NULL,                 # brapi connection information
    trial_metadata = NULL,      # trial metadata
    study_metadata = NULL,      # study metadata
    data = NULL,                # trial data as a data.table object
    excluded_obs = NULL,        # vector of excluded observations.observationDbId (data quality module)
    pushOK = FALSE,             # to avoid pushing BLUES/BLUPS to easily
    groups = data.table(),
    visu_as_group = NULL,
    new_group_created = F,
    hash = NULL                 # to track user query in browser sessionStorage
  )
  mod_connect_server("connect",rv)  
  mod_get_studydata_server("get_studydata", rv)
  mod_get_extradata_server("get_extradata", rv)
  print("appname")
  print(appname)
  mod_banner_server("banner", rv, appname)
  mod_scatterplot_server("scatterplot", rv, session)
  mod_gxe_server("gxe", rv, session)
  mod_groups_sidebar_server("groups_sidebar", rv, session)

  output$Rsi <- renderPrint(sessionInfo())
  
  ## user session hash ####
  observeEvent(input$hash, {
    rv$hash <- input$hash
  }, priority = 1) #to be triggered before other observeEvents
  
  ## Action when clicking on button create group in modal
  observeEvent(input$modal_create_group_go, {
    rv$selection[, group_name := input$modal_create_group_text_input_label]
    rv$selection[, group_desc := input$modal_create_group_text_input_descr]
    rv$selection[, clustering_id := NA]
    new_group <- rv$selection
    rv$groups <- rbindlist(list(
      rv$groups,
      new_group
    ), fill = T, use.names = T)
    
    update_selectors_with_groups(rv, new_group)

    #TODO reset plot selection after group creation ?
    #rv_plot$plot_selection <- data.table()
    
    rv$selection <- data.table()
    removeModal()
  })
  
  ## Action when clicking on button rename group in modal
  observeEvent(input$modal_rename_group_go, {
    req(length(rv$selected_group_id) == 1)
    rv$groups[group_id == rv$selected_group_id, group_name := input$modal_rename_group_text_input_label]
    rv$groups[group_id == rv$selected_group_id, group_desc := input$modal_rename_group_text_input_descr]
    removeModal()
    #save_user_data(rv)
  })

  # open or close the groups sidebar
  observeEvent(rv$groups$group_id,{
    if (!is.null(rv$groups) && nrow(rv$groups)>0) {
      bslib::toggle_sidebar(
        id = "groups_sidebar",
        open = T
      )
    } else {
      bslib::toggle_sidebar(
        id = "groups_sidebar",
        open = F
      )
    }
    #save_user_data(rv)
  }, ignoreNULL = TRUE)
  
}
