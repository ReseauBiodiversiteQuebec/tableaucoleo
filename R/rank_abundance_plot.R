

calc_ranks <- function(site_spp) {
  
  if (nrow(site_spp) == 0) stop("no data here")
  
  assertthat::has_name(site_spp, "taxa_name")
  
  rank_species <- site_spp %>% 
    dplyr::filter(!is.na(taxa_name)) %>% 
    dplyr::count(taxa_name) %>% 
    dplyr::mutate(rank = rank(dplyr::desc(n), ties.method = "random")) 
  
  return(rank_species)
}

rank_abundance_plot <- function(SxS){
  
  rank_species <- calc_ranks(SxS)
  
  rank_species %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = rank, y = n) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "rang des espèces", y = "Frequence des especes")
}