#'
#' @param species_data 
#'
#' @return
#' @export
rarefaction_create_data <- function(species_data, refresh=FALSE) {
  if(refresh){
    for (c in unique(species_data$campaign_type)){
      # Remove complexes
      species_data<-species_data[!grepl('|',species_data$taxa_name, fixed = TRUE),] 
      if(c=='végétation'){
        #Fix vegetation data on different scales
        species_data[species_data$value>0 & species_data$value<1 & species_data$campaign_type==c  & !is.na(species_data$value),'value'] = species_data[species_data$value>0 & species_data$value<1 & species_data$campaign_type==c  & !is.na(species_data$value),'value']*100 
        d=species_data |> dplyr::filter(campaign_type==c) |> tidyr::pivot_wider(id_cols=taxa_name, names_from=site_code,values_from=value,values_fill=0)
      }else{
        species_data$cnt <- as.integer(species_data$cnt)
        d=species_data |> dplyr::filter(campaign_type==c) |> tidyr::pivot_wider(id_cols=taxa_name, names_from=site_code,values_from=cnt,values_fill=0)
      }
      d=d[,-1]
      d=data.frame(d)
      i=iNEXT::iNEXT(d,datatype="abundance")$AsyEst
      i$campaign_type=c
      if(c!=unique(species_data$campaign_type)[1]){
        out=rbind(out,i)
      }else{
        out=c
      }
    }
    out$Site <- gsub('X', '', out$Site)
    saveRDS(out,'data/rarefaction_data.RDS')
  }else{
    out<-readRDS('data/rarefaction_data.RDS')
  }
  return(out)
}