

# take a clicked name and spit out the site name

make_site_name <- function(got_clicked_site_val, cell_lookup_vec){
  
  good_name <- cell_lookup_vec[[got_clicked_site_val]]
  
  return(good_name)
}


add_site_name_df <- function(cell_data){
  
  stopifnot(any(c("type", "cell.name") %in% names(cell_data)))
  
  outdata <- transform(cell_data, display_name = paste0(cell.name," -- ", type))

  return(outdata)
}


make_lookup_vector <- function(some_df, value_col, name_col){
  # check for error
  stopifnot(any(c(value_col, name_col) %in% names(some_df)))
  
  names_df <- as.data.frame(some_df)[,c(value_col, name_col)]
  
  dedup_names <- names_df[!duplicated(names_df), ]
  
  xx <- setNames(dedup_names[[value_col]], dedup_names[[name_col]])
  return(xx)
}