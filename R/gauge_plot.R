#' @export
gauge_plot <- function(category, by, value, comp, output){
  if(by=='campaign_type'){
    color <- mapselector::campaign_types_colors(category)
    icon <- mapselector::campaign_types_icons(category)
    ptitle <- campaign_types_format(category)
  }else{
    color <- mapselector::species_colors(category)
    icon <- mapselector::species_icons(category)
    ptitle <- stringr::str_to_title(category)
  }
  plot_name <- paste0(by,"_richness_", category)
    
  plotly::plotlyOutput(plot_name, width="200px", height="150px", inline = TRUE) %>% 
    tagAppendAttributes(class = plot_name)
  fig <- plotly::plot_ly(
    value = as.integer(value),
    gauge = list(
      axis=list(
        range = list(NULL,as.integer(comp))
      ),
      bar = list(color=color)
    ),
    title = list(text = ptitle),
    type = "indicator",
    mode = "gauge+number", 
    height = 150, 
    width = 200) %>% plotly::layout(margin = list(l = 50,r = 50,b = 50,t = 50,pad=20))
  renderUI({div(class="gauge",div(class="icon",HTML(icon)),plotly::renderPlotly(fig))})
}

