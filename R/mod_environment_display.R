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
      textOutput(ns("blurb")),
      plotOutput(ns("rain")),
      plotOutput(ns("heat"))
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
    
    output$blurb = renderText("Les différents sites de surveillance du MFFP connaissent des précipitations et des températures différentes. 
Chaque cercle ci-dessous montre la variation annuelle de la pluie et de la température pour un seul site. Le site sur lequel vous avez cliqué est représenté par une ligne plus épaisse.")
    
    output$rain = renderPlot(plot_to_show()$precip)
    
    
    output$heat = renderPlot(plot_to_show()$temper)
    
    
  })
}
    
## To be copied in the UI
# mod_environment_display_ui("environment_display_ui_1")
    
## To be copied in the server
# mod_environment_display_server("environment_display_ui_1")
