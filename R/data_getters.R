
#' retrieve all observations 
#'
#' @param campaign_type campaign type to retrieve
#'
#' @return
#' @export
get_observations_by_campaign_type <- function(campaign_type=NULL){
  if(!is.null(campaign_type)){
    resp <- rcoleo::get_gen('/campaigns',list('type'=campaign_type))  
    camps=data.table::rbindlist(resp$body)
  }
  camp_id=as.list(camps$id)
  names(camp_id)=rep('campaign_ids',length(camp_id))
  resp <- rcoleo::get_gen('/observations',camp_id)  
  obs=data.table::rbindlist(resp$body)
  return(obs)
}