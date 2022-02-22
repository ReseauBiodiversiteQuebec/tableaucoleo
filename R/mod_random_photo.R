#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_random_photo_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::htmlOutput(ns("photo"))
  )
}

#'
#' @param species_data sites x species count summary table
#'
mod_random_photo_server <- function(id, species_data){
  moduleServer( id, function(input, output, session){
    sp_list=unique(species_data$taxa_name)
    sp_list
    ind <- sample(length(sp_list))
    photo<-list()
    i<-0
    while(is.null(photo$thumb_url) & (i < length(sp_list))) {
      i<-i+1
      photo<-mapselector::get_species_photo(sp_list[ind[i]])
    }
    cnt<-species_data |> dplyr::filter(taxa_name ==  sp_list[ind[i]]) |> nrow()
    output$photo<-renderUI({div(style=list('float:left'),h3('Espèce en vedette'),
      photo_card(photo$thumb_url,sp_list[ind[i]],paste('Observée dans',cnt,'sites'),photo$url)
      )
    })
  })
}

