#' Get climate data from STAC catalogue
#'
#' @param sites_df 
#'
#' @return
#' @export
get_site_environment<-function(sites_df, refresh=FALSE){
  if(refresh){
    s = rstac::stac("http://io.biodiversite-quebec.ca/stac/")
    
    it_obj <- s |>
      rstac::stac_search(bbox=c(-120,30,-40,68), collections=c('chelsa-clim'),limit=5000) |> rstac::get_request()
    
    vars=c('bio1','bio5','bio6','bio12')
    assets=unlist(lapply(it_obj$features,function(x){names(x$assets)}))
    
    coll = gdalcubes::stac_image_collection(it_obj$features,asset_names=vars)
    
    #bio1 = Mean annual air temperature
    #bio5 = Mean daily maximum air temperature of the warmest month
    #bio6 = Mean daily minimum air temperature of the coldest month
    #bio12 = Annual precipitation amount
    
    v = gdalcubes::cube_view(srs = "EPSG:32198",extent = list(t0 = "1981-01-01", t1 = "1981-01-01",
                             left = -2009488, right = 1401061,  top = 2597757, bottom = -715776),
                             dx = 1000, dy = 1000, dt = "P1D",aggregation = "mean", resampling = "near")
    
    gdalcubes::gdalcubes_options(threads = 4)
    x<-gdalcubes::raster_cube(coll, v)
    coo<-sf::st_coordinates(sites_df)
    p<-gdalcubes::query_points(x,coo[,1],coo[,2], rep(as.Date('1981-01-01'),nrow(coo)), 'EPSG:4326')

    it_obj2 <- s |>
      rstac::stac_search(bbox=c(-120,30,-40,68), collections=c('ghmts'),limit=5000) |> rstac::get_request()
    
    vars=c('GHMTS')

    coll = gdalcubes::stac_image_collection(it_obj2$features,asset_names=vars)
    
    
    v = gdalcubes::cube_view(srs = "EPSG:32198",extent = list(t0 = "2016-01-01", t1 = "2016-01-01",
                                                              left = -2009488, right = 1401061,  top = 2597757, bottom = -715776),
                             dx = 3000, dy = 3000, dt = "P1D",aggregation = "mean", resampling = "near")
    x<-gdalcubes::raster_cube(coll, v)
    p2<-gdalcubes::query_points(x,coo[,1],coo[,2], rep(as.Date('2016-01-01'),nrow(coo)), 'EPSG:4326')
    p<-cbind(site_code=sites_df$site_code,p,p2)
    saveRDS(p,'data/site_climate.RDS')
  }else{
    p<-readRDS('data/site_climate.RDS')
  }
  return(p)
}
