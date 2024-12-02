server <- function(input, output, session){
  rv <- reactiveValues(
    con = NULL,                 # brapi connection information
    trial_metadata = NULL,      # trial metadata
    study_metadata = NULL,      # study metadata
    data = NULL,                # trial data as a data.table object
    excluded_obs = NULL,        # vector of excluded observations.observationDbId (data quality module)
    pushOK = FALSE,             # to avoid pushing BLUES/BLUPS to easily
  )
  
  mod_get_studydata_server("get_studydata", rv)
  mod_get_extradata_server("get_extradata", rv)
  mod_banner_server("banner", rv, appname)
  mod_scatterplot_server("scatterplot", rv, session)
  mod_gxe_server("gxe", rv, session)
  mod_groups_sidebar_server("groups_sidebar", rv, session)

  output$Rsi <- renderPrint(sessionInfo())
  
  ## Action when clicking on button create group in modal
  observeEvent(input$modal_create_group_go, {
    rv$selection[, group_name := input$modal_create_group_text_input_label]
    rv$selection[, group_desc := input$modal_create_group_text_input_descr]
    rv$selection[, clustering_id := NA]
    rv$groups <- rbindlist(list(
      rv$groups,
      rv$selection
    ), fill = T, use.names = T)

    ## update selectors (shape, colour)
    data_plot <- copy(rv$data_plot) # to avoid issues related to assignment by reference
    data_plot[germplasmDbId %in% rv$selection[,unlist(germplasmDbIds)], eval(input$modal_create_group_text_input_label) := paste0('In "', input$modal_create_group_text_input_label,'"')]
    data_plot[!(germplasmDbId %in% rv$selection[,unlist(germplasmDbIds)]), eval(input$modal_create_group_text_input_label) := paste0('Not in "', input$modal_create_group_text_input_label,'"')]
    rv$column_datasource <- rbindlist(
      list(
        rv$column_datasource,
        data.table(cols = input$modal_create_group_text_input_label, source = "group", type = "Text", visible = T)
      ),
      use.names = T
    )

    rv$new_group_created <- T #to avoid environments selection reset
    rv$data_plot <- data_plot


    rv$selection <- data.table()

    #TODO reset plot selection after group creation ?
    #rv_plot$plot_selection <- data.table()

    removeModal()
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
  }, ignoreNULL = TRUE)
  
}
