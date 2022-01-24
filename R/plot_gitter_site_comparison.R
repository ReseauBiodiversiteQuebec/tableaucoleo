#' @export
gitter_site_comparison <- function(sites,this_site_richness, other_site_richness, all_rich_mean, all_rich_site_campaign){

#print(other_site_richness)
    all_richness<-subset(all_rich_site_campaign, campaign_type %in% this_site_richness$campaign_type) %>% dplyr::left_join(this_site_richness, by=("campaign_type"), suffix=c("","_this")) %>% dplyr::left_join(sites %>% dplyr::select(site_code,display_name,type,cell.name), by=c('site_code')) %>% dplyr::left_join(all_rich_mean, by=("campaign_type")) 

    if(nrow(other_site_richness>0)){
      all_richness <- all_richness %>% dplyr::left_join(other_site_richness, by=("campaign_type"), suffix=c("","_other"))
    }

  all_richness$campaign_type<-sapply(all_richness$campaign_type,campaign_types_format, USE.NAMES = FALSE)
  this_site_richness$campaign_type<-sapply(this_site_richness$campaign_type,campaign_types_format, USE.NAMES = FALSE)
  this_site_richness$display_name=NULL

    g <-
    ggplot2::ggplot(all_richness, ggplot2::aes(x = campaign_type, y = as.numeric(richness),color = type, data_id = site_code, tooltip=paste0(display_name,' : ',richness))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Nombre d'espèces") +
    ggplot2::theme(
      legend.position = "topright",
      axis.title = ggplot2::element_text(family = "Montserrat", size = 14),
      axis.text.x = ggplot2::element_text(family = "Montserrat", size = 12),
      axis.text.y = ggplot2::element_text(family = "Montserrat", size = 12),
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::theme_minimal()+
    ggiraph::geom_jitter_interactive(position = ggplot2::position_jitter(seed = 2021, width = 0.2), size = 2, alpha = 0.45)+
    ggplot2::stat_summary(ggplot2::aes(x=campaign_type,y=as.numeric(richness_this)), fun = mean, geom = "point", size = 4)+
    ggiraph::geom_segment_interactive(
      ggplot2::aes(x = campaign_type, xend = campaign_type,
          #y = as.numeric(average_richness), yend = as.numeric(richness_this)),
          y = as.numeric(average_richness), yend = as.numeric(richness_this)),
      size = 0.8)
    
    if(nrow(other_site_richness>0)){
     g<- g + ggplot2::stat_summary(ggplot2::aes(x=campaign_type,y=as.numeric(richness_other)), fun = mean, geom = "point", size = 4, col="black")
    }
  g<-ggiraph::ggiraph(code=print(g))
  g<-ggiraph::girafe_options(g, ggiraph::opts_tooltip(zindex = 99999),ggiraph::opts_selection(type = "single", only_shiny = FALSE))
}
