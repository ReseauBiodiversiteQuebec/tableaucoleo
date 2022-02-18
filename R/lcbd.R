#' lcbd calculations
#'
#' @param species_data 
#'
#' @return
#' @export
calculate_lcbd <- function(species_data, refresh=FALSE){
  if(refresh){
    campaign_types=unique(species_data$campaign_type)
    species_data$cnt=as.integer(species_data$cnt)
    out=list()
    for (camp in campaign_types){
      this <- species_data |> dplyr::filter(campaign_type==camp)
      if(camp=='végétation'){ 
        mat <- this |> tidyr::pivot_wider(site_code, names_from=taxa_name, values_from=value,values_fill=0) |> data.frame()
      }else{
        mat <- this |> tidyr::pivot_wider(site_code, names_from=taxa_name, values_from=cnt,values_fill=0) |> data.frame()
      }
      row.names(mat) <- mat$site_code
      mat <- mat[,-1]
      beta <- adespatial::beta.div.comp(mat, coef="S", quant=TRUE)
      out[[camp]]=data.table::data.table(site_code=row.names(mat),campaign_type=camp,lcbd=adespatial::LCBD.comp(beta$rich, sqrt.D=FALSE)$LCBD)
    }
    out=data.table::rbindlist(out)
    saveRDS(out,'data/lcbd_data.RDS')
    return(out)
  }else{
    return(readRDS('data/lcbd_data.RDS'))
  }
}