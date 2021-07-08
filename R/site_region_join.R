
#' join ouranos regions and the downloaded sites
#'
#' @param site_sf_df dataframe from [rcoleo::download_sites_sf()]
#'
#' @return
#' @export
site_region_join <- function(site_sf_df){
  regions_simplified_Ouranos <- mapselector::regions_simplified_Ouranos
  
  # @importFrom mapselector regions_simplified_Ouranos
  ## better to do it that way, but would require mapselector reinstall
  
  # need to set the crs for downloaded sites
  sf::st_crs(site_sf_df) <- sf::st_crs(regions_simplified_Ouranos)
  joined_sites_our <- sf::st_join(site_sf_df, regions_simplified_Ouranos)
  
  
  joined_nice_names <- mapselector::add_site_name_df(joined_sites_our)

  return(joined_nice_names)
}