#' site_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_comparison_display_ui <- function(id, site_select_options){
  ns <- NS(id)
  #
  tagList(renderUI({selectInput(ns("other_site"),"Comparer avec...",site_select_options())}),uiOutput(ns('plots')))
}

#' site_richness_display Server Functions
#'
#' @noRd 
mod_site_comparison_display_server <- function(id, sites, site, rich, all_rich_mean, all_rich_site_campaign) {
    moduleServer(id, function(input, output, session){
    other_rich <- reactiveValues()
    observe({
      req(input$other_site)
      other_rich$richness <-rcoleo::get_richness(site_code = input$other_site, by_campaign_type=TRUE)
    })
    #output$titles <- renderUI({
    #  tagList(renderUI({
    #    div(h3(other_rich$other))
    #    }))
    #})
      output$plots <- renderUI({
        tagList(ggiraph::renderggiraph(gitter_site_comparison(sites, rich(), other_rich$richness, all_rich_mean, all_rich_site_campaign)))
      })
  })
}