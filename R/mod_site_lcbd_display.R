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
        lcc <- lcbd |> dplyr::filter(campaign_type==i)
        value <- 100*lc[lc$campaign_type==i,'lcbd']
        comp <- 100/nrow(lcbd[lcbd$campaign_type==i,])
        plot_name <- paste0("species_category","_lcbd_", i)
        #plot_output_list<-append(plot_output_list,list(plot_name=gauge_plot(i, "campaign_type", value, comp)))
        colors=rep('#ececec',nrow(lcbd))
        colors[lcc$site_code==site()]='#55aabb'
        pl <- lcc |> plotly::plot_ly(labels = ~site_code, values = ~lcbd,width=200,height=200) |> 
          plotly::add_pie(hole=0.6,marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1), textinfo='none'), textinfo='none') |>
          plotly::layout(title = mapselector::campaign_types_format(i),  showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) |>
          plotly::add_text(x=0.5,y=0.5,text=icon(name=NULL, class="fianimals animals-036-butterfly",verify_fa=FALSE))
        plot_output_list<-append(plot_output_list,list(
          plot_name=div(pl,class='lcdb_donut',style=c('float:left;','width:200px;','height:200px;'))
        ))
      }
      tagList(plot_output_list)
    })
  })
}
