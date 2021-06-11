#' observation_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @export
mod_ouranos_display_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("blurb")),
    plotly::plotlyOutput((ns("plot")))
    )
}

#' observation_display Server Functions
#'
#' @noRd 
#' @export
mod_ouranos_display_server <- function(id, region){
  assertthat::assert_that(shiny::is.reactive(region))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$blurb <- renderUI({
        tags$div(id = "blurbid", class = "blurbtext",
                 tags$h2(paste0("Projections climatiques pour ", region())),
                 tags$p("Cette figure montre les projections climatiques 
                 créées par Ouranos pour chaque province du Québec. 
                 Vous pouvez explorer les données plus en détail sur le",
                        tags$a(href = "https://www.ouranos.ca/portraits-climatiques/#/",
                               "site d'Ouranos.")
      ))
      })
    output$plot = plotly::renderPlotly(plotly::ggplotly(
      plot_ouranos_one_region(reg = region()))
    )
  })
}




subset_our <- function(dd, reg){
  subset(dd, dd$region == reg)
}

#' @export
plot_ouranos_one_region <- function(reg){
  project_plot <- subset_our(mapselector::ouranos_rcp, reg) %>%
    ggplot2::ggplot(ggplot2::aes(x = Annee, y = Avg, colour = rcp, fill = rcp,  ymin = Min, ymax = Max)) + 
    ggplot2::geom_line() + 
    ggplot2::facet_wrap(~var, scales = "free") + 
    ggplot2::geom_ribbon(alpha = 0.1)+ 
    ggplot2::theme_minimal()
  
  project_plot + 
    ggplot2::geom_line(ggplot2::aes(x = Annee, y = Obs),inherit.aes = FALSE, 
                       data = subset_our(mapselector::ouranos_observed, reg))
}
