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
      ggiraph::girafeOutput(ns("rain")),
      ggiraph::girafeOutput(ns("heat"))
    )
 
  )
}
    
#' environment_display Server Functions
#'
#' @noRd 
mod_environment_display_server <- function(id, sites, region,  lookup_vec){
  assertthat::assert_that(shiny::is.reactive(region))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    plot_to_show <- reactive(
      plot_one_site(site_clicked = region(), site_df = sites,  lookup_vec = lookup_vec)
    )
    
    output$blurb = renderText("Les différents sites de surveillance du MFFP connaissent des précipitations et des températures différentes. 
Chaque cercle ci-dessous montre la variation annuelle de la pluie et de la température pour un seul site. Le site sur lequel vous avez cliqué est représenté par une ligne plus épaisse.")
    
    output$rain = ggiraph::renderGirafe(plot_to_show()$precip)
    output$heat = ggiraph::renderGirafe(plot_to_show()$temper)
    
    
  })
}
    
## To be copied in the UI
# mod_environment_display_ui("environment_display_ui_1")
    
## To be copied in the server
# mod_environment_display_server("environment_display_ui_1")


env_ui <- function(request) {
  tagList(
    mod_environment_display_ui("environment_display_ui_1")
  )
}  

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @importFrom magrittr %>%
#' @noRd
env_server <- function(input, output, session) {
  
  downloaded_sites <- rcoleo::download_sites_sf()
  downloaded_sites_names <- add_site_name_df(downloaded_sites)
  
  cell_name_lookup <- make_lookup_vector(downloaded_sites, 
                                         value_col = "cell.name",
                                         name_col = "cell_id")
  
  mod_environment_display_server("environment_display_ui_1",
                                 sites = downloaded_sites_names, 
                                 region = reactive("Mékinac (B) -- marais"),
                                 lookup_vec = cell_name_lookup)
}


env_app <- function(){
  shinyApp(
    ui = env_ui,
    server = env_server)
}
