make_my_pal = function(user_selected_metric, date_vars, mk_results){
  if(user_selected_metric %in% date_vars){
    colorFactor(palette = 'RdBu',
                domain = mk_results$trend_sig,
                levels = c("Significant Trend Earlier",
                           'Non-Significant Trend Earlier',
                           'No Trend',
                           'Non-Significant Trend Later',
                           'Significant Trend Later'),
                ordered = T)
  } else {
    colorFactor(palette = 'RdBu',
                domain = mk_results$trend_sig,
                levels = c("Significant Trend Down",
                           'Non-Significant Trend Down',
                           'No Trend',
                           'Non-Significant Trend Up',
                           'Significant Trend Up'),
                ordered = T)
  }
}


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
    leaflet.extras::addDrawToolbar(
      targetGroup = 'draw',
      polylineOptions = F,
      circleOptions = F,
      rectangleOptions = F,
      markerOptions = F,
      circleMarkerOptions = F,
      singleFeature = T,
      editOptions = leaflet.extras::editToolbarOptions()
    ) %>%
    addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                     options = layersControlOptions(collapsed = F),
                     position = 'topright')
}


update_leaflet = function(map, stations, clicked_station, shapes, clicked_shape, pal){

  # Add in average of Mann-Kendall trend analyses for each shape.
  # Left join the result dataframe (with crude average results per
  # map shape) onto the shape object.
  #if(shapes$shape_name != 'Nothing'){

  shapes = shapes %>%
    left_join(
      stations %>%
        ungroup() %>%
        # dplyr::select(-shape_name) %>%
        st_join(shapes %>% dplyr::select(shape_name), st_intersects) %>%
        st_drop_geometry() %>%
        group_by(shape_name) %>%
        count(trend_sig) %>%
        mutate(stations_for_mean = sum(n)) %>%
        mutate(val_to_sum = case_when(
          trend_sig == 'Significant Trend Down' ~ -2,
          trend_sig == 'Non-Significant Trend Down' ~ -1,
          trend_sig == "No Trend" ~ 0,
          trend_sig == 'Non-Significant Trend Up' ~ 1,
          trend_sig == 'Significant Trend Up' ~ 2,
          T ~ 100
        )) %>%
        mutate(average_trend_result = sum(n*val_to_sum)/n()) %>%
        mutate(label = paste0(trend_sig,': ',n, collapse = '; ')) %>%
        dplyr::select(shape_name,average_trend_result,stations_for_mean,label) %>%
        distinct()
    )
  #}

  shape_pal = colorNumeric(palette = 'RdBu',
                           domain = shapes$average_trend_result)

  # If the user has clicked on a shape, reduce the points / polygon fills
  # to just that one.
  stations_for_map = stations
  shapes_for_map = shapes

  #If the user has selected a shape on the leaflet map, filter stations.
  if(clicked_shape != 'no_selection'){
    stations_for_map = stations_for_map %>%
      st_join(shapes %>% filter(shape_name == clicked_shape), st_intersects) %>%
      filter(!is.na(shape_name)) %>%
      dplyr::select(-shape_name)

    shapes_for_map = shapes %>%
      mutate(average_trend_result = ifelse(shape_name == clicked_shape, average_trend_result, NA))
  }

  leafletProxy(map) %>%
    clearShapes() %>%
    clearMarkers() %>%
    addPolygons(layerId = ~shape_name,
                color = 'grey',
                fillOpacity = 0.5,
                fillColor = ~shape_pal(average_trend_result),
                label = ~lapply(paste0(shape_name,": ",
                                       round(average_trend_result,2),
                                       "<br>",
                                       "(",stations_for_mean, " stations)"),
                                htmltools::HTML
                ),
                weight = 2,
                highlightOptions = highlightOptions(
                  color = 'purple',
                  opacity = 0.5
                ),
                data = shapes_for_map) %>%
    addCircleMarkers(layerId = ~STATION_NUMBER,
                     color = 'black',
                     fillColor = ~pal(trend_sig),
                     radius = 8,
                     weight = 1,
                     fillOpacity = 0.75,
                     label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                     data = stations_for_map) %>%
    addCircleMarkers(#layerId = 'selected_station',
      color = 'orange',
      weight = 7.5,
      fillColor = 'transparent',
      data = stations_for_map |> filter(STATION_NUMBER %in% clicked_station)) %>%
    removeControl("legend") %>%
    addLegend(pal = pal,
              values = ~trend_sig,
              title = 'Mann-Kendall Trend Result',
              data = stations,
              layerId = 'legend')
}
