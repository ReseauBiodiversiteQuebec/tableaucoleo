#' @import mapselector 
#' @import shiny
testapp_bat_maps <- function(){
  
  ui_fn <- function(request) {
    tagList(
      marcel(filename = "marcel.md"),
      golem_add_external_resources(),
      tableau_de_bord(
        dash_title(title = "Explorateur des sites"), 
        dash_sidebar(
          badge(text_badge = "Voila un survol"),
          tableOutput("sel")
        ), 
        dash_tabs(
          #maybe a little strange, but here we pass in the UI of a modal and the id that defines it.
          tab_map(title = "Site Map", id = "bat_map", outputFunction = mod_map_select_ui)
          )
      )
    )
  }  
  
  server <-  function(input, output, session) {
    
    downloaded_sites <- rcoleo::download_sites_sf()
    
    selsite <- mod_map_select_server("bat_map",
                                     what_to_click = "marker",
                                     fun = plot_rcoleo_sites,
                                     rcoleo_sites_sf = downloaded_sites)
    
    
    ff <- reactive({mapselector::get_subset_site(site = downloaded_sites,
                                                  site_code_sel = selsite())})
    
    output$sel <- renderTable(head(ff()))
  }
  shinyApp(ui_fn, server)
}

testapp_bat_maps()


miniapp <- function(){
  shinyApp(
    ui = app_ui,
    server = app_server)
}

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @noRd
small_ui <- function(request) {
  tagList(
    tableau_de_bord(
      dash_title(title = "Explorateur des sites"), 
      dash_sidebar(
        badge(text_badge = "Voila un survol"),
        tableOutput("sel")
      ), 
      dash_tabs(
        #maybe a little strange, but here we pass in the UI of a modal and the id that defines it.
        tab_map(title = "Site Map", id = "bat_map", outputFunction = mod_map_select_ui),
        tab_gen()
      )
    )
  )
}  

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @importFrom magrittr %>%
#' @noRd
small_server <- function(input, output, session) {
  
  downloaded_sites <- rcoleo::download_sites_sf()
  
  selsite <- mod_map_select_server("bat_map",
                                   what_to_click = "marker",
                                   fun = plot_rcoleo_sites,
                                   rcoleo_sites_sf = downloaded_sites)
  
  
  ff <- reactive({mapselector::get_subset_site(site = downloaded_sites,
                                               site_code_sel = selsite())})
  
  output$sel <- renderTable(head(ff()))
}


smallapp <- function(){
  shinyApp(
    ui = small_ui,
    server = small_server)
}