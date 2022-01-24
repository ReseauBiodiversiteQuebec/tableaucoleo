plot_spp_site_tile <- function(raw_resp) {
  raw_resp %>% 
    dplyr::filter(!is.na(taxa_name)) %>% 
    dplyr::add_count(site_code, name = "sn") %>% 
    dplyr::add_count(taxa_name, name = "tn") %>% 
    dplyr::mutate(site_code = forcats::fct_reorder(site_code, sn),
           taxa_name = forcats::fct_reorder(taxa_name, tn)) %>% 
    ggplot2::ggplot() + 
    ggplot2::aes(y = site_code, x = taxa_name) + 
    ggplot2::geom_tile() + 
    ggplot2::coord_fixed() + 
    ggplot2::theme_classic() + 
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45))
}