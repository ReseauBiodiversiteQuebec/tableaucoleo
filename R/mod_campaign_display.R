#' campaign_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_campaign_display_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(width = 4, tableOutput(ns("obs_tbl"))),
    column(width = 8,
           div(
             class = "tinymap",
             leaflet::leafletOutput(ns("map"))
           )
    )
  )
}
    
#' campaign_display Server Functions
#'
#' @noRd 
mod_campaign_display_server <- function(id, region, dl_sites_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # plant <- renderUI
    
    icos <- tibble::tribble(~type, ~ico, 
                    "végétation",     HTML("<i class='finature-collection nature-collection-plant-2'>Végétation</i>"),
                    "papilionidés",   HTML("<i class='fianimals animals-036-butterfly'>Papillons</i>"),
                    "acoustique",     HTML("<i class='fianimals animals-007-bat'>Chauves-souris</i>"),
                    "odonates",       HTML("<i class='ficustom custom-dragonfly'>Odonates</i>"),
                    "insectes_sol",   HTML("<i class='finature nature-012-beetle'>Insectes du sol</i>"),
                    "zooplancton",    HTML("<i class='ficustom custom-shrimp'>Zooplancton</i>")
    )
    
    
    # to_show <- reactive({(rcoleo::get_campaign_summary(site_code  = region()))})
    
    to_show <- reactive({dplyr::left_join(rcoleo::get_campaign_summary(site_code  = region()),
                                          icos)})
    
    
    output$obs_tbl = renderTable(dplyr::select(to_show(),
                                               Type = ico, `Année` = yr, Nombre = count),
                                 sanitize.text.function = function(x) x)
    
    plot_one_point <- function(dl_sites, chosen){
      
      subset(dl_sites, dl_sites$site_code == chosen) %>% 
        leaflet::leaflet(options = leaflet::leafletOptions()) %>%  
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) %>% 
        leaflet::addMarkers()
      
    }
    
    output$map <- leaflet::renderLeaflet(plot_one_point(dl_sites = dl_sites_df, chosen = region()))
})}
    
## To be copied in the UI
# mod_campaign_display_ui("campaign_display_1")
    
## To be copied in the server
# mod_campaign_display_server("campaign_display_1")
