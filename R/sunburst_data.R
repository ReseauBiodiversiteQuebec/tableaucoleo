#' Sunburst data
#'
#' @param species_data 
#'
#' @return
#' @export
sunburst_create_data <- function(species_data, refresh=FALSE){
  if(refresh){
    names <- unique(species_data[!grepl('|',species_data$taxa_name, fixed = TRUE),'taxa_name'])
    taxo <- taxize::gnr_resolve(names$taxa_name,fields='all',data_source_ids = c(1),best_match_only = TRUE)
    long_hier <- taxo |> tidyr::separate_rows(classification_path,sep="\\|")
    sun_data <- long_hier |> 
      dplyr::group_by(classification_path) |> 
      dplyr::summarise(n=dplyr::n(), names = paste0(submitted_name, collapse = ","))
    for (i in 1:nrow(sun_data)){
      s <- as.character(sun_data[i,'classification_path'])
      sp <- long_hier |> dplyr::filter(classification_path==s) |> dplyr::select(submitted_name)
      sp <- as.character(sp[1,1])
      h <- taxo |> dplyr::filter(submitted_name==sp) |> dplyr::select(classification_path) |> as.character() |> strsplit(split='\\|')
      ind <- which(h[[1]]==s)[1]
      if(ind==1){
        sun_data[i,'parent'] <- ""
      }else{
        sun_data[i,'parent'] <- h[[1]][ind-1]
      }
    }
    sun_data<-sun_data[!sun_data$classification_path=='Biota',]
    sun_data[sun_data$parent=='Biota','parent'] <- ''
    saveRDS(sun_data,'data/sun_data.RDS')
  }else{
    sun_data<-readRDS('data/sun_data.RDS')
  }
  return(sun_data)
}