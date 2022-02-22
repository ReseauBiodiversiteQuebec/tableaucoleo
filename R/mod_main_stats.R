#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::htmlOutput(ns("stats"))
  )
}

#'
#' @param species_data sites x species count summary table
#'
mod_main_stats_server <- function(id, sites_df, species_data){
  moduleServer( id, function(input, output, session){
    output$stats<-renderUI({div(
      stats_card(nrow(sites_df),'Sites échantillonnés','map-marker','main-1'),
      stats_card(length(unique(species_data$taxa_name)),'Taxons inventoriés','bug','main-2'))
    })
  })
}

