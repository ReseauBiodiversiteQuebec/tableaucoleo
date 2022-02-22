#'
#' @param number text icon color_class 
#'
#' @return
#' @export
# export
stats_card <- function(number, text, icon, color_class='main-1'){
  div(class='col-md-12', div(class='stati card-left',icon(icon,class=color_class), div(strong(number,class=color_class),span(text))))
}

#'
#' @param number text icon color_class 
#'
#' @return
#' @export
# export
photo_card <- function(url,name,text=''){
  div(class="photo-cards",style="",div(class="top_photos",style=paste0('background: url("',url,'")')),div(class="photo-card-body",h5(class="photo_species_name",name),div(class='photo_main_text',text)))
}