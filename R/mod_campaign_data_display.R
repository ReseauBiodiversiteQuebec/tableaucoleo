
mod_campaign_type_radio <- function(id, start_sel = "odonates"){
  ns <- NS(id)
  tagList(
    shinyWidgets::radioGroupButtons(ns("selected_campaigns"),
                       label = "Groupe d'espèces",
                       choiceValues = list("végétation","papilionidés","acoustique","odonates","insectes_sol","zooplancton"),
                       choiceNames = list(HTML("<i class='finature-collection nature-collection-plant-2'>Végétation</i>"),
                                           HTML("<i class='fianimals animals-036-butterfly'>Papillons</i>"),
                                           HTML("<i class='fianimals animals-007-bat'>Chauves-souris</i>"),
                                           HTML("<i class='ficustom custom-dragonfly'>Odonates</i>"),
                                           HTML("<i class='finature nature-cute-012-beetle'>Insectes du sol</i>"),
                                           HTML("<i class='ficustom custom-shrimp'>Zooplancton</i>")),
                       direction = "vertical",
                       status = 'primary fibuttons',
                       selected = start_sel)
  )
}

## need the map output module component
mod_campaign_type_vis_output <-  function(id, start_sel = "odonates"){
  ns <- NS(id)
  tagList(plotOutput(ns("plot")))
}

mod_campaign_data_display <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ## composed module for selecting a site or sites from the coleo database
    
    # react <- reactiveValues(click = NULL, id = "mapclick")
    # 
    # observeEvent(input$map_marker_click,{
    #   react$click <- input$map_marker_click$id
    # })
    # 
    # ## if different campaigns are selected, then set map_marker_click to NULL
    # 
    # observeEvent(input$selected_campaigns,{
    #   react$click <- NULL})
    
    SxS <- reactive(rcoleo::get_species_site(campaign_type = input$selected_campaigns))
    
    return(SxS)
    
  })
}