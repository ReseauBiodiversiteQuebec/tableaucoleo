#' environment_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_environment_display_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      plotOutput(ns("plot"))
    )
 
  )
}
    
#' environment_display Server Functions
#'
#' @noRd 
mod_environment_display_server <- function(id, sites, region){
  assertthat::assert_that(shiny::is.reactive(region))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    plot_to_show <- reactive(
      plot_one_site(site_clicked = region(), site_df = sites)
    )
    
    output$plot = renderPlot(plot_to_show())
    
    
  })
}
    
## To be copied in the UI
# mod_environment_display_ui("environment_display_ui_1")
    
## To be copied in the server
# mod_environment_display_server("environment_display_ui_1")
