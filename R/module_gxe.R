#' @export
mod_gxe_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_tab(
      bslib::nav_panel(
        title = "Data preparation", 
        bslib::card(
          # bslib::card_header(
          #   h4('Options ', icon('screwdriver-wrench'))
          # ),
          pickerInput(ns("picker_trait"), label = "Trait", choices = c()),
          pickerInput(ns("picker_env"), label = "Environments", choices = c(), multiple = T),
          pickerInput(ns("picker_scenario"), label = "Choose environment details to use as scenario", choices = c(), multiple = T),
          pickerInput(ns("picker_location"), label = "Choose environment detail to use as location", choices = c()),
          pickerInput(ns("picker_year"), label = "Choose environment detail to use as year", choices = c()),
          pickerInput(ns("picker_germplasm_attr"), label = "Select germplasm attributes", choices = c())
        )
      ),
      bslib::nav_panel(
        title = "Mixed model",
        p("TODO")
      )
    )
  )
}

#' @export
mod_gxe_server <- function(id, rv){
  moduleServer(
    id,
    function(input, output, session){
      
      observe({
        req(rv$data_plot)
        req(rv$column_datasource)
        
        #update trait dropdown
        trait_choices <- rv$column_datasource[source == "GxE", .(cols)]
        updatePickerInput(
          session, "picker_trait",
          choices = trait_choices,
          selected = NULL
        )
      })
      
      observeEvent(input$picker_trait,{
        ## update environments dropdown
        envs <- unique(rv$data_plot[!is.na(get(input$picker_trait)),.(studyDbId, study_name_app)])
        env_choices <- envs[,studyDbId]
        names(env_choices) <- envs[,study_name_app]

        updatePickerInput(
          session, "picker_env",
          choices = env_choices
        )
       
      })
      
      observeEvent(input$picker_env,{
        ## update env details dropdowns
        colnames(rv$data_plot)
        env_details <- rv$column_datasource[source == "environment" & visible == T,]$cols
        
        updatePickerInput(
          session, "picker_scenario",
          choices = env_details
        )
        
        updatePickerInput(
          session, "picker_location",
          choices = env_details
        )
        
        updatePickerInput(
          session, "picker_year",
          choices = env_details
        )
        
        germplasm_attr <- rv$column_datasource[source == "germplasm" & visible == T,]$cols
        updatePickerInput(
          session, "picker_germplasm_attr",
          choices = germplasm_attr
        )
        
      })
      
    }
  )
}