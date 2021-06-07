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
  
  # add a display name column
  downloaded_sites_names <- add_site_name_df(downloaded_sites)
  
  # match to Ouranos
  site_region_joined <- site_region_join(downloaded_sites)
  
  # make lookup vecs
  site_code_lookup <- make_lookup_vector(downloaded_sites_names, "site_code", "display_name")
  cell_name_lookup <- make_lookup_vector(downloaded_sites, 
                                         value_col = "cell.name", name_col = "cell_id")
  
  ouranos_region_lookup <- make_lookup_vector(site_region_joined, value_col = "Region", name_col = "display_name")
  
  got_clicked_site <- mod_map_select_server("sitemap",
                                            what_to_click = "marker", 
                                            fun = plot_rcoleo_sites,
                                            rcoleo_sites_sf = downloaded_sites_names,
                                            site_id_col = "display_name")
  
  # reactive that takes got_clicked_site and gives back the technical code
  clicked_site_code <- reactive({
    req(got_clicked_site())
    make_site_name(got_clicked_site_val = got_clicked_site(), site_code_lookup)
    })
  
  clicked_ouran_name <- reactive({
    req(got_clicked_site())
    make_site_name(got_clicked_site_val = got_clicked_site(), ouranos_region_lookup)
  })
  
  # calculations for a modal triggered by this map
  mod_observation_display_server("siteobs", 
                                 site = downloaded_sites_names, 
                                 region = clicked_site_code)
  
  mod_environment_display_server("siteenv",
                                 sites = downloaded_sites_names,
                                 region = got_clicked_site, lookup_vec = cell_name_lookup
                                 )
  
  mod_ouranos_display_server("projection", clicked_ouran_name)
  
  
  # display of the modal
  mod_modal_make_server("modal_make_ui_1", 
                        # this reactive value is passed inside the module
                        # note you but the reactive value here, not its value, 
                        # which you would get with chosen_region()
                        region = got_clicked_site,
                        # give the title that you want for the modal
                        title_format_pattern = "Informations disponibles pour %s",
                        tabPanel(title = "Observations",
                                 mod_observation_display_ui("siteobs")
                        ),
                        tabPanel(title = "Pluie et température",
                                 mod_environment_display_ui("siteenv")),
                        tabPanel(title = "Changements climatiques",
                                 mod_ouranos_display_ui("projection"))
  )

# ouranos regions --------------------------------------------------------

  got_clicked_our <- mod_map_select_server("ouranos_map",what_to_click = "shape",
                        fun = make_leaflet_map,
                        # these are arguments to make_leaflet_map
                        mapdata = mapselector::regions_simplified_Ouranos,
                        label = TRUE,
                        region_name = "Region")



  mod_modal_make_server("modal_our",
                        region = got_clicked_our,
                        title_format_pattern = "Projections climatiques pour %s",
                       )
}
