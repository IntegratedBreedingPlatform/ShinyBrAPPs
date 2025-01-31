library(tidyr)
server <- function(input, output, session){
  rv <- reactiveValues(
    con = NULL,                 # brapi connection information
    trial_metadata = NULL,      # trial metadata
    study_metadata = NULL,      # study metadata
    data = NULL,                # trial data as a data.table object
    excluded_obs = data.table(observationDbId = character(), reason = character()), # data.table of excluded observations
    pushOK = FALSE,             # to avoid pushing BLUES/BLUPS to easily
  )
  
  mod_get_studydata_server("get_studydata", rv)
  mod_banner_server("banner", rv, appname)
  mod_dataquality_server("dataquality", rv)
  mod_model_server("model", rv)

  output$Rsi <- renderPrint(sessionInfo())
  
  ## output excluded_obs_table ####
  output$excluded_obs_table <- DT::renderDT({
    validate(
      need(nrow(rv$excluded_obs)>0, "no excluded observations")
    )
    req(rv$data)

    columns <- c("reason", visible_columns_selected_obs)

    selected_excl_obs <- rv$data[observationDbId %in% rv$excluded_obs$observationDbId]
    selected_excl_obs <- merge(rv$excluded_obs, rv$data, by = "observationDbId")
    setcolorder(selected_excl_obs, columns)
    
    DT::datatable(
      selected_excl_obs,
      colnames = c("study" = "study_name_abbrev_app"),
      extensions = c('Select', 'Buttons'),
      selection = 'none',
      options = list(
        columnDefs = list(list(
          visible = FALSE,
          targets = match(names(selected_excl_obs)[!(names(selected_excl_obs) %in%
                                                       columns)], names(selected_excl_obs))
        )),
        paging = F,
        scrollX = T,
        scrollY = "300px",
        scrollCollapse = T,
        dom = 'Bt',
        buttons = c('selectAll', 'selectNone', I('colvis')),
        select = list(style = 'os', items = 'row')
      )
    ) %>%
      DT::formatStyle(0, target = 'row', lineHeight = '90%')
  }, server = F)
  
  ## observe input$set_non_excluded_obs ####
  observeEvent(input$set_non_excluded_obs, {
    non_excluded_obs <- rv$data[observationDbId %in% rv$excluded_obs$observationDbId][input$excluded_obs_table_rows_selected, observationDbId]
    rv$excluded_obs <- rv$excluded_obs[!(observationDbId %in% non_excluded_obs)]
  })
  
  observeEvent(rv$excluded_obs, {
    if (nrow(rv$excluded_obs) > 0) {
      disable_btn = F # show button
      # show number of excluded observation in accordion panel header
      bslib::accordion_panel_update(
        id = "excluded_accordion",
        value = "excluded_panel",
        title = paste0("Excluded observations (", nrow(rv$excluded_obs), ")"),
        target = "excluded_panel"
      )
      bslib::accordion_panel_open(
        id = "excluded_accordion",
        value = "excluded_panel"
      )
    } else {
      disable_btn = T # hide button
      bslib::accordion_panel_update(
        id = "excluded_accordion",
        value = "excluded_panel",
        title = "Excluded observations",
        target = "excluded_panel"
      )
      bslib::accordion_panel_close(
        id = "excluded_accordion",
        value = "excluded_panel"
      )
    }
    
    # enable/disable "include observations" button
    updateActionButton(
      inputId = "set_non_excluded_obs",
      disabled = disable_btn
    )

  })
  
}
