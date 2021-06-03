
#' Plot environmental  variables at at site
#'
#' @param type type of environment. one of "precip" or "temp"
#' @param site three digit code -- is probably actually the CELL id
#'
#' @return
#' @export
#'
plot_site_env <- function(type, site){
  if (!(type %in% c("precip", "temp"))) stop("type must be either precip or temp")
  
  dat <- switch(type,
                precip = mapselector::meteo_precipitation_ECMWF,
                temp   = mapselector::mean_temperature)
  
  lightcol <- switch(type,
                     precip = "#3182bd30",
                     temp   = "#f03b2030")
  
  
  boldcol <- switch(type,
                    precip = "#3182bd",
                    temp   = "#f03b20")
  
  
  # brutal simplicity: rename the response to a constant name, but alter the labels
  names(dat)[3] <- "yvar"
  
  ylab <- switch(type,
                 precip = "Millimètres de précipitation",
                 temp = "Température (degrés C)")
  
  lightplot <- 
    ggplot2::ggplot(dat) + 
    ggplot2::aes(x = Month, y = yvar, group = nn) + 
    ggplot2::geom_polygon(fill = NA, col = lightcol) +
    ggplot2::coord_polar(start = -pi * 1/12) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(title = ylab, x = "Mois", y = NULL, x = NULL)
  
  ## everything above here could actually be stored as app data! 
  lightplot + 
    ggplot2::geom_polygon(fill = NA, lwd = 2, col = boldcol,
                 data = subset(dat,
                               dat$nn == site))
}


#' Plot one site
#'
#' @param site_clicked the site_id returned by clicking the map
#' @param site_df the data-frame of downloaded sites
#'
#' @return a two-panel plat
#' @export
#'
#' @import patchwork
plot_one_site <- function(site_clicked, site_df){
  stopifnot(is.data.frame(site_df))
  
  cell <- site_df[["cell_id"]][which(site_df[["site_code"]] == site_clicked)]
  plot_site_env("precip", cell) + plot_site_env("temp", cell)
}


# plot_one_site( "123_89_L01", rcoleo::download_sites_sf())
