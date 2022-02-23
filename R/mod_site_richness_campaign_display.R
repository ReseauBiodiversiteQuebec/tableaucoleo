#' environment_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_richness_campaign_display_ui <- function(id, rich){
  ns <- NS(id)
  tagList(bootstrapPage(uiOutput(ns('plots')),uiOutput(ns('photos'))))
}

#' site_richness_display Server Functions
#'
#' @noRd 
mod_site_richness_campaign_display_server <- function(id, sites, site, rich, all_rich, rarefaction) {
  assertthat::assert_that(shiny::is.reactive(site))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plots <- renderUI({
      plot_output_list=list()
      for(i in rich()$campaign_type){
        print(i)
        value <- as.integer(rich()[rich()$campaign_type==i,"richness"])
        #comp <- as.integer(all_rich[all_rich$campaign_type==i,'richness'])
        comp <- rarefaction |> dplyr::filter(campaign_type == i & Site == site() & Diversity == 'Species richness') |> dplyr::select(Estimator)
        plot_name <- paste0("species_category","_richness_", i)
        plot_output_list<-append(plot_output_list,list(plot_name=gauge_plot(i, "campaign_type", value, comp)))  
      }
      tagList(plot_output_list)
    })
    output$photos <- renderUI({
      if(nrow(rich())>0){
        phs=c()
        photos_output_list<-lapply(rich()$campaign_type, function(p){
          sp_list <- rcoleo::get_species_list(site_code=site(),campaign_type=p) |> dplyr::arrange(desc(nobs))
          photo<-mapselector::get_species_photo(sp_list[1,'taxa_name'])
          photo_name<-paste0("photo_top_", p)
          uiOutput(photo_name)
          if(!is.null(photo$thumb_url)){
            output[[photo_name]]<-renderUI({
              photo_card(photo$thumb_url,sp_list[1,'taxa_name'])
            })
          }else{
            output[[photo_name]]<-renderUI({
              photo_card(name=sp_list[1,'taxa_name'])
            })
          }
        })
        if(length(photos_output_list)!=0){
          photos_output_list=append(list(photos_title=renderUI({div(h3('Espèces fréquemment observées sur ce site.'),class="frequent_species")})),photos_output_list)
        }
      }else{
        uiOutput(ns('aucune'))
        photos_output_list=list(aucune=renderUI({div(h3('Aucune observation pour ce site.'))}))
      }
      #uiOutput(ns('photos_title'))
      tagList(photos_output_list)
    }) 
    })
}
