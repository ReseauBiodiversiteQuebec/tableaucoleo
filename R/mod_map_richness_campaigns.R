#' map_richness_campaigns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_richness_campaigns_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

#' map_richness_campaigns Server Functions
#'
#' @param downloaded_site_name data.frame of coleo sites. Must have specific
#'   column names: "type" for the habitat type. "richness" for the species
#'   richness and "display_name" for the name you want users to see
#'
#' @importFrom magrittr `%>%`
mod_map_richness_campaigns_server <- function(id, downloaded_site_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_df <- mapselector::drop_empty_campaigns(downloaded_site_name) 
    
    choice <- function(m) leaflet::addControl(m,
                                              radioButtons(ns("var_chosen"), 
                                                           label = NULL, 
                                                           choices = c("Type de site" = "type",
                                                                       "Nombre d'espèces" = "richness")),
                                              className = "markerchoice",
                                              position = "topright"
    )
    
    output$map <- leaflet::renderLeaflet({
      make_leaflet_empty() %>% 
        choice(.)
    })
    
    # necessary because (apparently) the control isn't present until the map is
    rv <- reactiveValues(value_chosen = "type")
    
    observeEvent(input$var_chosen,{
      rv$value_chosen <- input$var_chosen
    })
    
    # create the functions to add to the map whichever kind of marker corresponds to the choice.
    
    ics <- mapselector::make_site_icons()
    
    map_marker_reactive <- reactive({
      switch(rv$value_chosen,
             "richness" = 
               function(x) leaflet::addCircleMarkers(x,
                                                     radius = ~(as.integer(richness)/4),
                                                     group = ~type,
                                                     color = "#497e9f",
                                                     stroke = TRUE,
                                                     weight = 5, 
                                                     opacity = 0.75,
                                                     label = ~display_name,
                                                     layerId = ~display_name,
                                                     data = data_df),
             "type" =
               function(x) leaflet::addAwesomeMarkers(x,
                                                      icon = ~ics[type],
                                                      group = ~type,
                                                      label = ~display_name,
                                                      layerId = ~display_name,
                                                      data = data_df) 
      )
    })
    
    observe({
      req(map_marker_reactive)
      leaflet::leafletProxy("map") %>%
        leaflet::clearMarkers() %>% 
        map_marker_reactive()(.) %>% 
        leaflet::addLayersControl(overlayGroups = ~type,
                                  options = leaflet::layersControlOptions(collapsed = FALSE),
                                  data = data_df)
    })
    
    site_code_from_display_name_vec <- mapselector::make_lookup_vector(some_df = data_df, 
                                                                       value_col = "site_code", 
                                                                       name_col = "display_name")
    
    return(list(
      display_name = reactive(input$map_marker_click$id),
      site_code = reactive(site_code_from_display_name_vec[input$map_marker_click$id])
    ))
    
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
