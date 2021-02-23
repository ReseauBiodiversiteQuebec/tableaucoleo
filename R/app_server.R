#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @importFrom magrittr %>%
#' @noRd
app_server <- function( input, output, session ){
                                         
  # make a map of your own
  # needs and id
  

# sites -------------------------------------------------------------------

  downloaded_sites <- rcoleo::download_sites_sf()
  
  got_clicked_site <- mod_map_select_server("sitemap",
                                            what_to_click = "marker", 
                                            fun = plot_rcoleo_sites,
                                            rcoleo_sites_sf = downloaded_sites)
  
  # calculations for a modal triggered by this map
  mod_observation_display_server("siteobs", 
                                 site = downloaded_sites, 
                                 region = got_clicked_site)
  
  mod_environment_display_server("siteenv",
                                 sites = downloaded_sites,
                                 region = got_clicked_site
                                 )
  
  # display of the modal
  mod_modal_make_server("modal_make_ui_1", 
                        # this reactive value is passed inside the module
                        # note you but the reactive value here, not its value, 
                        # which you would get with chosen_region()
                        region = got_clicked_site,
                        # give the title that you want for the modal
                        title_format_pattern = "Visualization for %s",
                        tabPanel(title = "Observations",
                                 mod_observation_display_ui("siteobs")
                        ),
                        tabPanel(title = "Environment",
                                 mod_environment_display_ui("siteenv"))
  )

# ouranos regions --------------------------------------------------------
  
  got_clicked_our <- mod_map_select_server("ouranos_map",what_to_click = "shape",
                        fun = make_leaflet_map,
                        # these are arguments to make_leaflet_map
                        mapdata = mapselector::regions_simplified_Ouranos,
                        label = TRUE,
                        region_name = "Region")

  
  mod_ouranos_display_server("projection", got_clicked_our)
  
  mod_modal_make_server("modal_our",
                        region = got_clicked_our,
                        title_format_pattern = "Climate projection for %s",
                        tabPanel(title = "Ouranos",
                                 mod_ouranos_display_ui("projection")))
}
