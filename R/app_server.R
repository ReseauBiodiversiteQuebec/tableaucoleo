#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny mapselector
#' @importFrom magrittr %>%
#' @noRd
app_server <- function(input, output, session ){
  
  # sites -------------------------------------------------------------------
  sf::sf_use_s2(FALSE)
  downloaded_sites=rcoleo::get_gen('/sites_with_campaigns') |> dplyr::filter(number_of_campaigns>0) |> dplyr::rename(cell.name=name)
  downloaded_sites$geom.coordinates <- lapply(downloaded_sites$geom.coordinates, 
                                          sf::st_point)
  downloaded_sites <- sf::st_as_sf(downloaded_sites) 

    refresh<-FALSE
  
  s<-readRDS('data/sites.RDS')
  if(nrow(s)==nrow(downloaded_sites) & !refresh){
    refresh_sites=FALSE
  }else{
    refresh_sites=TRUE
    saveRDS(downloaded_sites,'data/sites.RDS')
  }
  campaign_ids<-readRDS('data/campaign_ids.RDS')
  cids<-unlist(lapply(downloaded_sites$campaigns,function(x){x$id}))
  if(all(campaign_ids %in% cids & !refresh)) {
    refresh_campaigns=FALSE
  }else{
    refresh_campaigns=TRUE
    saveRDS(cids,'data/campaign_ids.RDS')
  }
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
  
  if(refresh_campaigns){
    species_data <- rcoleo::get_gen('/species_abundance_count',query=list('by_site'=TRUE))
    saveRDS(species_data,'data/species_data.RDS')
  }else{
    species_data <- readRDS('data/species_data.RDS')
  }
  site_lcbd <- calculate_lcbd(species_data, refresh_campaigns)
  rarefaction <- rarefaction_create_data(species_data, refresh_campaigns)
  site_env <- get_site_environment(downloaded_sites, refresh_sites)
  
  # reactive that takes got_clicked_site and gives back the technical code
  clicked_site_code <- reactive({
    req(got_clicked_site())
    mapselector::make_site_name(got_clicked_site_val = got_clicked_site(), site_code_lookup)
  })
  
  clicked_ouran_name <- reactive({
    req(got_clicked_site())
    mapselector::make_site_name(got_clicked_site_val = got_clicked_site(), ouranos_region_lookup)
  })
  
  mod_main_stats_server('main_stats', downloaded_sites_names, species_data=species_data)
  mod_random_photo_server('random_photo',species_data)
  mod_environment_display_server("siteenv",
                                 sites = downloaded_sites_names,
                                 region = got_clicked_site,
                                 lookup_vec = cell_name_lookup
  )
  
  mod_ouranos_display_server("projection", clicked_ouran_name)
  
  mod_campaign_display_server("camps", region = clicked_site_code, dl_sites_df = downloaded_sites_names, site_env)
  
  # help modules ------------------------------------------------------------
  
  mod_modal_observeEvent_tutorial_server("info1",
                                         title_text = "title for help",
                                         md_file = "demo_help.md")  
  
  downloaded_sites <- rcoleo::download_sites_sf() %>% 
    dplyr::left_join(rcoleo::get_richness(by_site = TRUE),by = c("site_code"))
  
  # add a display name column
  # match to Ouranos
  downloaded_sites_names <- add_site_name_df(downloaded_sites)
  
  userclick <- mod_map_richness_campaigns_server("sitemap", 
                                                 downloaded_site_name = downloaded_sites_names)

  mod_sunburst_server("sunburst",species_data, refresh_campaigns)
  
# download richness info ------------------------------------------------
  
  this_rich_spcat <- reactive({
    rcoleo::get_richness(site_code = userclick$site_code(), by_species_category=TRUE)
  })
  # 
  this_rich_campaign <- reactive({
    rcoleo::get_richness(site_code = userclick$site_code(), by_campaign_type=TRUE)
  })
  
  
  # 
  site_select_options <- reactive({
    req(userclick$site_code())
    ds<-subset(downloaded_sites_names,type==downloaded_sites_names[downloaded_sites_names$site_code==userclick$site_code(),"type"]$type)
    opts<-c("mean",ds$site_code)
    names(opts) <- c("Moyenne",ds$display_name)
    opts
  })   
  #all_rich_spcat <- rcoleo::get_richness(by_species_category=TRUE)

  all_rich_campaign <- rcoleo::get_richness(by_campaign_type=TRUE)
  
  all_rich_campaign_mean <- rcoleo::get_richness(mean=TRUE, by_campaign_type=TRUE)

  all_rich_site_campaign <- rcoleo::get_richness(by_site=TRUE, by_campaign_type=TRUE)
  
  # richness displays ----------------------------------------------------------

  mod_site_richness_campaign_display_server("site_richness_campaign",
                                   sites = downloaded_sites_names,
                                   site = userclick$site_code,
                                   rich = this_rich_campaign,
                                   all_rich = all_rich_campaign,
                                   rarefaction = rarefaction
  )

  mod_site_comparison_display_server("site_comparison",
                                            sites = downloaded_sites_names,
                                            site = userclick$site_code,
                                            rich = this_rich_campaign,
                                            all_rich_mean = all_rich_campaign_mean,
                                            all_rich_site_campaign = all_rich_site_campaign
  )
  
  mod_site_lcbd_display_server("site_lcbd",
                                     sites = downloaded_sites_names,
                                     site = userclick$site_code,
                                     lcbd = site_lcbd, 
                                     species_data = species_data
  )
  
  mod_sunburst_site_server("sunburst_site",site = userclick$site_code, species_data, FALSE)
  # 
  #   
  mod_modal_make_server("modal_make_ui_1",
                        # this reactive value is passed inside the module
                        # note you put the reactive value here, not its value,
                        # which you would get with chosen_region()
                        region = userclick$display_name,
                                # give the title that you want for the modal
                        title_format_pattern = "Site %s",
                        tabPanel(title = "Campagnes sur ce site",
                                 mod_campaign_display_ui("camps")
                        ),
                        tabPanel(title = "Pluie et température",
                                 mod_environment_display_ui("siteenv")),
                        tabPanel(title = "Projections climatiques",
                                 mod_ouranos_display_ui("projection")),
                        tabPanel(title = "Espèces",
                                 mod_site_richness_display_ui("site_richness_campaign", this_rich_campaign)),
                        tabPanel(title = 'Composition',
                                 mod_site_lcbd_display_ui("site_lcbd")),
                        tabPanel(title = 'Hiérarchie taxonomique',
                                 mod_sunburst_site_ui("sunburst_site")),
                        tabPanel(title = "Comparer avec d'autres sites",
                                 mod_site_comparison_display_ui("site_comparison", site_select_options))
  )

}
