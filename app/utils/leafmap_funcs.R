make_base_bc_leafmap = function(){
  leaflet(
    options = leafletOptions(
      attributionControl = F
    )
  ) %>%
    addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
    addTiles(group = 'Streets') %>%
    addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    add_bc_home_button() %>%
    set_bc_view() %>%
    addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                     options = layersControlOptions(collapsed = F),
                     position = 'topleft')
}


update_leaflet = function(map, stations, clicked_station, shapes, pal){
  # browser()
  leafletProxy(map) %>%
    clearShapes() %>%
    clearMarkers() %>%
    addPolygons(layerId = ~shape_name,
                color = 'grey',
                fillOpacity = 0.3,
                label = ~shape_name,
                weight = 2,
                highlightOptions = highlightOptions(
                  color = 'purple',
                  opacity = 0.5
                ),
                data = shapes) %>%
    addCircleMarkers(layerId = ~STATION_NUMBER,
                     color = 'black',
                     fillColor = ~pal(trend_sig),
                     radius = 8,
                     weight = 1,
                     fillOpacity = 0.75,
                     label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                     data = stations) %>%
    addCircleMarkers(#layerId = 'selected_station',
                     color = 'orange',
                     weight = 7.5,
                     fillColor = 'transparent',
                     data = stations |> filter(STATION_NUMBER %in% clicked_station)) %>%
    removeControl("legend") %>%
    addLegend(pal = pal,
              values = ~trend_sig,
              title = 'Mann-Kendall Trend Result',
              data = stations,
              layerId = 'legend')
}
