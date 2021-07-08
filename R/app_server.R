#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @noRd
app_server <- function( input, output, session ){
                                         
  # make a map of your own
  # needs and id
  

# sites -------------------------------------------------------------------

  downloaded_sites <- rcoleo::download_sites_sf()
  
  # add a display name column
  downloaded_sites_names <- mapselector::add_site_name_df(downloaded_sites)
  
  # match to Ouranos
  site_region_joined <- site_region_join(downloaded_sites)
  
  # make lookup vecs
  site_code_lookup <- mapselector::make_lookup_vector(downloaded_sites_names, "site_code", "display_name")
  cell_name_lookup <- mapselector::make_lookup_vector(downloaded_sites, 
                                         value_col = "cell.name", name_col = "cell_id")
  ouranos_region_lookup <- mapselector::make_lookup_vector(site_region_joined, value_col = "Region", name_col = "display_name")
  
  got_clicked_site <- mapselector::mod_map_select_server("sitemap",
                                            what_to_click = "marker", 
                                            fun = plot_rcoleo_sites,
                                            rcoleo_sites_sf = downloaded_sites_names,
                                            site_id_col = "display_name")
  
  
  
  
  # reactive that takes got_clicked_site and gives back the technical code
  clicked_site_code <- reactive({
    req(got_clicked_site())
    mapselector::make_site_name(got_clicked_site_val = got_clicked_site(), site_code_lookup)
    })
  
  clicked_ouran_name <- reactive({
    req(got_clicked_site())
    mapselector::make_site_name(got_clicked_site_val = got_clicked_site(), ouranos_region_lookup)
  })
  
  mod_environment_display_server("siteenv",
                                 sites = downloaded_sites_names,
                                 region = got_clicked_site,
                                 lookup_vec = cell_name_lookup
                                 )
  
  mod_ouranos_display_server("projection", clicked_ouran_name)
  
  mod_campaign_display_server("camps", region = clicked_site_code, dl_sites_df = downloaded_sites_names)
  
  
  # display of the modal
  mapselector::mod_modal_make_server("modal_make_ui_1", 
                        # this reactive value is passed inside the module
                        # note you put the reactive value here, not its value, 
                        # which you would get with chosen_region()
                        region = got_clicked_site,
                        # give the title that you want for the modal
                        title_format_pattern = "Informations disponibles pour %s",
                        tabPanel(title = "Campagnes sur ce site",
                                 mod_campaign_display_ui("camps")
                                 ),
                        tabPanel(title = "Pluie et température",
                                 mod_environment_display_ui("siteenv")),
                        tabPanel(title = "Projections climatiques",
                                 mod_ouranos_display_ui("projection"))
  )

}
