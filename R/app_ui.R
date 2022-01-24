#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny mapselector
#' @noRd
app_ui <- function(request) {
  tagList(
    mapselector::marcel(filename = "marcel.md"),
    golem_add_external_resources(),
    tableau_de_bord(
      dash_sidebar(
        dash_title(title = "Réseau de suivi de la biodiversité", icon="nature-collection-trees-1"),
        badge(text_badge="Cet interface permet d'explorer la diversité des espèces répertoriées dans les inventaires effectués dans le cadre du réseau de suivi de la biodiversité du Québec.")
        #mod_campaign_type_radio("selcamp",start_sel = "odonates"),
        #mapselector::mod_modal_helpbutton_ui("info1", "Jargon")
      ), 
      dash_tabs(
        #maybe a little strange, but here we pass in the UI of a modal and the id that defines it.
        # tab_map(title = "Vis", id = "selcamp", outputFunction = mapselector::mod_campaign_type_map_plot),
        mapselector::tab_gen(title = "Sites",
                             outputFunction = mod_map_richness_campaigns_ui, 
                             id = "sitemap"),
        mapselector::tab_gen(title = "Espèces",
                             outputFunction = plotOutput,
                             id = "spp_by_site_plot")
        )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'tableaucoleo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

