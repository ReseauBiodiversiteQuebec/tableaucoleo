

# take a clicked name and spit out the site name

make_site_name <- function(got_clicked_site_val, cell_data){
  
  req(got_clicked_site_val)
  # is.reactive(got_clicked_site)
  
  stopifnot(any(c("type", "cell.name") %in% names(cell_data)))
  
  click_info <- subset(cell_data, site_code == got_clicked_site_val)
  
  stopifnot(nrow(click_info) == 1)
  
  cellname <- click_info$cell.name
  habitat <-  click_info$type
  
  good_name <- paste0(cellname," -- ", habitat)
  
  return(good_name)
}