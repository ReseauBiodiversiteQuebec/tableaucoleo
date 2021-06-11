test_that("multiplication works", {
  skip_if_not(interactive())
  
  
  
  env_ui <- function(request) {
    tagList(
      mod_environment_display_ui("environment_display_ui_1")
    )
  }  
  
  env_server <- function(input, output, session) {
    
    downloaded_sites <- rcoleo::download_sites_sf()
    downloaded_sites_names <- add_site_name_df(downloaded_sites)
    
    cell_name_lookup <- make_lookup_vector(downloaded_sites, 
                                           value_col = "cell.name",
                                           name_col = "cell_id")
    
    mod_environment_display_server("environment_display_ui_1",
                                   sites = downloaded_sites_names, 
                                   region = reactive("Mékinac (B) -- marais"),
                                   lookup_vec = cell_name_lookup)
  }
  
  
  env_app <- function(){
    shinyApp(
      ui = env_ui,
      server = env_server)
  }
  
  env_app()
  
})
