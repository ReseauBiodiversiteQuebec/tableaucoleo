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
    column(width = 4, tableOutput(ns("obs_tbl")), tableOutput(ns("env_tbl"))),
    column(width = 8,
           div(
             class = "tinymap",
             leaflet::leafletOutput(ns("map"), height=550)
           )
    )
  )
}
    
#' campaign_display Server Functions
#'
#' @noRd 
mod_campaign_display_server <- function(id, region, dl_sites_df, site_env){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # plant <- renderUI
    
    icos <- tibble::tribble(~type, ~ico, 
                    "végétation",     HTML("<i class='finature-collection nature-collection-plant-2'>Végétation</i>"),
                    "papilionidés",   HTML("<i class='fianimals animals-036-butterfly'>Papillons</i>"),
                    "acoustique",     HTML("<i class='fianimals animals-007-bat'>Chauves-souris</i>"),
                    "odonates",       HTML("<i class='ficustom custom-dragonfly'>Odonates</i>"),
                    "insectes_sol",   HTML("<i class='finature nature-cute-012-beetle'>Insectes du sol</i>"),
                    "zooplancton",    HTML("<i class='ficustom custom-shrimp'>Zooplancton</i>"),
                    "décomposition_sol",  HTML("<i class='finature nature-cute-028-tree'>Décomposition du sol</i>"),
    )
    
    
    # to_show <- reactive({(rcoleo::get_campaign_summary(site_code  = region()))})
    
    to_show <- reactive({dplyr::left_join(rcoleo::get_campaign_summary(site_code  = region()),
                                          icos)})
    
    
    output$obs_tbl = renderTable(dplyr::select(to_show(),
                                               Type = ico, `Année` = yr, Nombre = count),
                                 sanitize.text.function = function(x) x)
    
    se <- site_env |> dplyr::select(site_code,GHMTS,bio5,bio6,bio12) |> 
      dplyr::mutate(bio5=bio5*0.1-273.15,bio6=bio6*0.1-273.15,bio12=bio12*0.1)
    
    si <- reactive({
      se |>
        dplyr::filter(site_code==region()) |>
        dplyr::select(-site_code) |>
        dplyr::rename('Indice de modifications humaines'=GHMTS, 'Température moyenne du mois le plus chaud (°C)'=bio5, 'Température moyenne du mois le plus froid (°C)'=bio6,'Précipitation annuelle (kg/m²)'=bio12) |> 
        tidyr::pivot_longer(everything(),names_to='variable')
      })

    me <- se |> dplyr::summarise('Indice de modifications humaines'=mean(GHMTS,na.rm=TRUE),'Température moyenne du mois le plus chaud (°C)'=mean(bio5),'Température moyenne du mois le plus froid (°C)'=mean(bio6),'Précipitation annuelle (kg/m²)'=mean(bio12)) |> 
      tidyr::pivot_longer(everything(),names_to='Var',values_to="Moyenne")

    
    table <- reactive({
      si() |> dplyr::left_join(me, by=c("variable"="Var")) |> dplyr::select(Variable=variable, `Valeur` = value, Moyenne) 
    })
    output$env_tbl = renderTable(data.frame(dplyr::select(table(),
                                               `Variable`=Variable, `Valeur`=Valeur, `Moyenne`=Moyenne)),
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
