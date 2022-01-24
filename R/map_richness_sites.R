plot_site_map <- function(site_info_sf){
    ics<-mapselector::make_site_icons()
    leaflet::leaflet(site_info_sf) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      radius = ~(as.integer(richness)/4),
      group = ~type,
      color= "#497e9f",
      stroke = TRUE,
      weight=5, 
      opacity = 0.75,
      layerId =  ~paste0(site_code,"-",type,"-Richesse"),
      #label = ~richness,
      #labelOptions = c(permanent = TRUE, textOnly=TRUE, textSize="26px",style=list('text-align'='center')),
      fillOpacity = 0) %>%
    leaflet::addAwesomeMarkers(icon = ~ics[type],
                             group = ~type,
                             layerId = ~paste0(site_code,"-",type,"-Marqueurs"),
                             label = ~site_code) %>% 
    leaflet::addLayersControl(baseGroups=c("Marqueurs","Richesse"),
                              overlayGroups=~type,options=leaflet::layersControlOptions(collapsed=FALSE)) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var baseLayer = 'Marqueurs';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if (baseLayer !== id.split('-')[2]){
            layer.getElement().style.display = 'none';
          }
        }
      })
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.split('-')[2]){
                  layer.getElement().style.display = 'none';
                  if(typeof layer._shadow !== 'undefined'){
                    layer._shadow.style.display = 'none';
                  }
                }
                if (e.name === id.split('-')[2]){
                  layer.getElement().style.display = 'block';
                  if(typeof layer._shadow !== 'undefined'){
                    layer._shadow.style.display = 'block';
                  }
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                console.log(id)
                if (baseLayer !== id.split('-')[2]){
                  layer.getElement().style.display = 'none';
                  if(typeof layer._shadow !== 'undefined'){
                    layer._shadow.style.display = 'none';
                  }
                }
            }    
          })
        })
}")
    
}
