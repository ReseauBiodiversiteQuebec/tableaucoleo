#' Sunburst UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sunburst_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("sunburst_plot"),width='95%',height='95%')
  )
}

#' Sunburst Server Functions
#'
#' @param species_data sites x species count summary table
#'
mod_sunburst_server <- function(id, species_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sunburst_plot<-plotly::renderPlotly({
      print(species_data)
      ns <- session$ns
      names <- unique(species_data[!grepl('|',species_data$taxa_name, fixed = TRUE),'taxa_name'])
      taxo <- taxize::gnr_resolve(names$taxa_name,fields='all',data_source_ids = c(1),best_match_only = TRUE)
      long_hier <- taxo |> tidyr::separate_rows(classification_path,sep="\\|")
      sun_data <- long_hier |> 
        dplyr::group_by(classification_path) |> 
        dplyr::summarise(n=dplyr::n())
      for (i in 1:nrow(sun_data)){
        s <- as.character(sun_data[i,'classification_path'])
        sp <- long_hier |> dplyr::filter(classification_path==s) |> dplyr::select(submitted_name)
        sp <- as.character(sp[1,1])
        h <- taxo |> dplyr::filter(submitted_name==sp) |> dplyr::select(classification_path) |> as.character() |> strsplit(split='\\|')
        ind <- which(h[[1]]==s)[1]
        if(ind==1){
          sun_data[i,'parent'] <- ""
        }else{
          sun_data[i,'parent'] <- h[[1]][ind-1]
        }
      }
      sun_data<-sun_data[!sun_data$classification_path=='Biota',]
      sun_data[sun_data$parent=='Biota','parent'] <- ''
      plotly::plot_ly(
        labels = sun_data$classification_path,
        parents = sun_data$parent,
        type = 'sunburst',
        maxdepth = 5
      )
    })
  })
}

## To be copied in the UI
# mod_map_richness_campaigns_ui("map_richness_campaigns_1")

## To be copied in the server
# mod_map_richness_campaigns_server("map_richness_campaigns_1")
