#' Sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sunburst_site_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("sunburst_site_plot"),width='95%',height='95%'),proxy.height = '100%',color='#538887',type=7)
  )
}

#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_sunburst_site_server <- function(id, site, species_data, refresh){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sunburst_site_plot<-plotly::renderPlotly({
      print(species_data)
      ns <- session$ns
      sp <- species_data |> dplyr::filter(site_code == site())
      sun_data <- sunburst_create_data(species_data, FALSE)
      sun_data <- sun_data |> tidyr::separate_rows(names,sep=",") |> dplyr::filter(names %in% sp$taxa_name) |> dplyr::group_by(classification_path, parent) |> dplyr::summarise()
      plotly::plot_ly() |>
        plotly::add_trace(
          labels = sun_data$classification_path,
          parents = sun_data$parent,
          type = 'sunburst',
          maxdepth = 5
        ) |>
        plotly::layout(
          sunburstcolorway = c("#538887", "#7bb5b1"),
          extendsunburstcolors = FALSE
        )
    })
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
