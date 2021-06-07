#' climate_projections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_climate_projections_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' climate_projections Server Functions
#'
#' @noRd 
mod_climate_projections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_climate_projections_ui("climate_projections_1")
    
## To be copied in the server
# mod_climate_projections_server("climate_projections_1")
