#' environment_display UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_richness_display_ui <- function(id, rich){
    ns <- NS(id)
    tagList(uiOutput(ns('plots')),uiOutput(ns('photos')))
}

#' site_richness_display Server Functions
#'
#' @noRd 
mod_site_richness_display_server <- function(id, sites, site, rich, all_rich) {
  assertthat::assert_that(shiny::is.reactive(site))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plots <- renderUI({
      plot_output_list=list()
      for(i in rich()$species_category){
        value <- as.integer(rich()[rich()$species_category==i,"richness"])
        comp <- as.integer(all_rich[all_rich$species_category==i,'richness'])
        plot_name <- paste0("species_category","_richness_", i)
        plot_output_list<-append(plot_output_list,list(plot_name=gauge_plot(i, "species_category", value, comp)))  
      }
      tagList(plot_output_list)
    })
      output$photos <- renderUI({
        if(nrow(rich())>0){
        sp_list <- rcoleo::get_species_list(site_code=site())
        phs=c()
        photos_output_list<-lapply(rich()$species_category, function(p){
          this_sp<-sp_list[sp_list$category==p,]
          i<-0;
          photo<-list()
          while(is.null(photo$thumb_url) & i<nrow(this_sp)){
            i<-i+1
            photo<-mapselector::get_species_photo(this_sp[i,'taxa_name'])
          }
          if(!is.null(photo$thumb_url)){
            photo_name<-paste0("photo_top_", p)
              if(! photo$thumb_url %in% phs) {
                  phs<-cbind(phs,photo$thumb_url)
                  uiOutput(photo_name)    
                  output[[photo_name]]<-renderUI({
                    div(div(id = photo_name,class="top_photos",style=paste0("background: url(",photo$thumb_url,");"),
                            div(this_sp[i,'taxa_name'], class="photo_species_name"))
                    )
                  })
              }
            }
        })
          photos_output_list=append(list(photos_title=renderUI({div(h3('Espèces fréquemment observées sur ce site.'),class="frequent_species")})),photos_output_list)
        }else{
          uiOutput(ns('aucune'))
          photos_output_list=list(aucune=renderUI({div(h3('Aucune observation pour ce site.'))}))
        }
        #uiOutput(ns('photos_title'))
        tagList(photos_output_list)
    }) 
  })
}

