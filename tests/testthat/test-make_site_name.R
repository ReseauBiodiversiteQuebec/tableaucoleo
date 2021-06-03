test_that("name formatting works fine", {
  
  # example data from dput. NOTE it is partial
  cell_data_onerow <- structure(list(id = 1L, cell_id = 134L, off_station_code_id = NA_character_, 
                                     site_code = "135_104_H01", type = "marais", opened_at = "2016-06-23", 
                                     notes = NA_character_, created_at = "2018-11-07T18:49:59.242Z", 
                                     updated_at = "2018-11-07T18:49:59.242Z", cellId = 134L, campaigns = list(
                                       structure(list(id = c(1L, 580L, 657L), site_id = c(1L, 
                                                                                          1L, 1L), type = c("végétation", "papilionidés", "acoustique"
                                                                                          ), technicians = list(c("Caroline Dubé", "Pierre-Alexis Drolet"
                                                                                          ), c("Caroline Dube", "Pierre-Alexis Drolet"), "Joelle Spooner"), 
                                                      opened_at = c("2016-06-23", "2016-06-23", "2016-04-25"
                                                      ), closed_at = c("2016-06-23", "2016-06-23", "2016-07-12"
                                                      ), notes = c(NA, NA, NA), created_at = c("2018-11-07T18:50:06.355Z", 
                                                                                               "2020-12-09T15:43:51.683Z", "2021-03-09T21:28:07.505Z"
                                                      ), updated_at = c("2018-11-07T18:50:06.355Z", "2020-12-09T15:43:51.683Z", 
                                                                        "2021-03-09T21:28:07.505Z"), siteId = c(1L, 1L, 1L
                                                                        )), class = "data.frame", row.names = c(NA, 3L))), 
                                     cell.id = 134L, cell.name = "Mékinac (B)", cell.cell_code = "135_104", 
                                     cell.created_at = "2018-11-07T18:49:18.690Z", cell.updated_at = "2018-11-07T18:49:18.690Z", 
                                     geom.coordinates = structure(list(structure(c(-72.2994, 46.8088
                                     ), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT", 
                                                                                     "sfc"), precision = 0, bbox = structure(c(xmin = -72.2994, 
                                                                                                                               ymin = 46.8088, xmax = -72.2994, ymax = 46.8088), class = "bbox"), crs = structure(list(
                                                                                                                                 input = NA_character_, wkt = NA_character_), class = "crs"), n_empty = 0L)), row.names = c(NA, 
                                                                                                                                                                                                                            -1L), sf_column = "geom.coordinates", agr = structure(c(id = NA_integer_, 
                                                                                                                                                                                                                                                                                    cell_id = NA_integer_, off_station_code_id = NA_integer_, site_code = NA_integer_, 
                                                                                                                                                                                                                                                                                    type = NA_integer_, opened_at = NA_integer_, notes = NA_integer_, 
                                                                                                                                                                                                                                                                                    created_at = NA_integer_, updated_at = NA_integer_, cellId = NA_integer_, 
                                                                                                                                                                                                                                                                                    campaigns = NA_integer_, cell.id = NA_integer_, cell.name = NA_integer_, 
                                                                                                                                                                                                                                                                                    cell.cell_code = NA_integer_, cell.created_at = NA_integer_, 
                                                                                                                                                                                                                                                                                    cell.updated_at = NA_integer_), .Label = c("constant", "aggregate", 
                                                                                                                                                                                                                                                                                                                               "identity"), class = "factor"), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                                                                                                                               ))

  # throw error if no names
  expect_error(make_site_name("135_104_H01", cell_data_onerow[,1:3]))
  
  expect_equal(make_site_name("135_104_H01", cell_data_onerow), "Mékinac (B) -- marais")
  
  
})
