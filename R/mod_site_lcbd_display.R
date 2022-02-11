#' environment_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_lcbd_display_ui <- function(id){
  ns <- NS(id)
  tagList(bootstrapPage(uiOutput(ns('plots'))))
}

#' site_richness_display Server Functions
#'
#' @noRd 
mod_site_lcbd_display_server <- function(id, sites, site, lcbd) {
  assertthat::assert_that(shiny::is.reactive(site))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plots <- renderUI({
      plot_output_list=list()
      lc <- lcbd[lcbd$site_code==site(),]
      for(i in lc$campaign_type){
        value <- 100*lc[lc$campaign_type==i,'lcbd']
        comp <- 100
        plot_name <- paste0("species_category","_lcbd_", i)
        plot_output_list<-append(plot_output_list,list(plot_name=gauge_plot(i, "campaign_type", value, comp)))  
      }
      tagList(plot_output_list)
    })
  })
}
