#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import mapselector
#' @noRd
app_server <- function( input, output, session ){
  # Your application server logic 
  # mod_map_select_server("map_select_ui_1")
  chosen_site <- mod_map_select_server("map",
                                       what_to_click = "marker",
                                       fun = mapselector::plot_rcoleo_sites)
  
  mod_modal_make_server("site_modal", 
                        # this reactive value is passed inside the module
                        # note you but the reactive value here, not its value, 
                        # which you would get with chosen_region()
                        region = chosen_site,
                        # give the title that you want for the modal
                        title_format_pattern = "Visualization for %s",
                        tabPanel(title = "ou suis-je",
                                 renderText({paste("tu est sur", chosen_site())})
                        )
  )

}
