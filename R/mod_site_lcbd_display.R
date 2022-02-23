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
  tagList(bootstrapPage(uiOutput(ns('plots')),uiOutput(ns('photos'),style='display:flex;flex-wrap:wrap;width:100%')))
}

#' site_richness_display Server Functions
#'
#' @noRd 
mod_site_lcbd_display_server <- function(id, sites, site, lcbd, species_data) {
  assertthat::assert_that(shiny::is.reactive(site))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$plots <- renderUI({
      plot_output_list=list(h3=h3('Contribution à la biodiversité'))
      lc <- lcbd[lcbd$site_code==site(),]
      for(i in lc$campaign_type){
        lcc <- lcbd |> dplyr::filter(campaign_type==i) |> dplyr::left_join(sites,by=c('site_code'))
        value <- 100*lc[lc$campaign_type==i,'lcbd']
        comp <- 100/nrow(lcbd[lcbd$campaign_type==i,])
        plot_name <- paste0("species_category","_lcbd_", i)
        colors=rep('#ececec',nrow(lcbd))
        colors[lcc$site_code==site()]='#7bb5b1'
        if(value>comp){
          textcolor=I("#538887")
          text='+'
        }else{
          textcolor=I("#538887")
          text='-'
        }
        pl <- lcc |> plotly::plot_ly(labels = ~display_name, values = ~lcbd,width=200,height=200) |> 
          plotly::add_pie(hole=0.6,marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1), textinfo='none'), textinfo='none') |>
          plotly::layout(title = mapselector::campaign_types_format(i),  showlegend = F,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))|>
          plotly::add_text(x=0.5,y=0.5,text=text,color=textcolor, size=2)
          
        plot_output_list<-append(plot_output_list,list(
          plot_name=div(pl,class='lcdb_donut',style=c('float:left;','width:200px;','height:200px;'))
        ))
      }
      tagList(plot_output_list)
      })
    output$photos <- renderUI({
        this <- species_data |> dplyr::filter(site_code==site()) |> dplyr::select(taxa_name) |> unique()
        other <- species_data |> dplyr::filter(site_code!=site()) |> dplyr::select(taxa_name) |> unique()
        unique_sp <- this$taxa_name[!(this$taxa_name %in% other$taxa_name)] |> head(4)
        photos_output_list<-list()
        if(length(unique_sp)>0){
          photos_output_list<-lapply(unique_sp, function(p){
            photo<-list()
            phs<-c()
            photo<-mapselector::get_species_photo(p)
            photo_name<-paste0("photo_top_", p)
            uiOutput(photo_name, style='flex:1')
            output[[photo_name]]<-renderUI({
              if(!is.null(photo$thumb_url)){
                photo_card(url=photo$thumb_url,name=p)
              }else{
                photo_card(name=p)
              }
            })
          })
          if(length(photos_output_list)!=0){
            photos_output_list=append(list('photos_title'=renderUI({
              uiOutput('photos_title',style='flex:100%');
              div(h3('Espèces observées uniquement sur ce site.'),class="frequent_species",style='flex:100%')
              })),photos_output_list)
          }
      }
      tagList(photos_output_list)
    })
  })
}
