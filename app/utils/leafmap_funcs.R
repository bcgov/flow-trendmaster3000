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


update_leaflet = function(map, stations, clicked_station, shape_type, shapes, clicked_shape, pal){

  # Add in average of Mann-Kendall trend analyses for each shape.
  # Left join the result dataframe (with crude average results per
  # map shape) onto the shape object.
  #if(shapes$shape_name != 'Nothing'){

  # browser()
  # Determine the domain for our colour palette.
  # if(length(shapes$shape_name) == 1){
    # if(shapes$shape_name == 'Nothing'){
  if(sum(grepl('Nothing', shapes$shape_name)) >= 1){
    shapes = shapes %>%
      mutate(average_trend_result = 0,
             stations_for_mean = 0,
             label = "")
 } else {

   # If it's a drawn poly, add 'drawn_poly' to stations df.
   if(sum(grepl('drawn_poly',shapes$shape_name)) >= 1){
     stations = stations |>
       mutate(drawn_shape = 'drawn_poly')
   }

   # Summarize 'general trend' (a bit vague and arbitrary)
   # for the shape(s).
   dat_for_shapes = stations %>%
     filter(!is.na(trend_sig)) %>%
     st_drop_geometry() %>%
     dplyr::rename(shape_name = !!sym(shape_type())) %>%
     dplyr::select(trend_sig, shape_name) %>%
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
     distinct() %>%
     filter(!is.na(shape_name))

   shapes = shapes %>%
     left_join(dat_for_shapes)
 }

  shape_pal = colorNumeric(palette = 'RdBu',
                           domain = c(-6:6))

  # #If the user has selected a shape on the leaflet map, filter stations.
  # if(clicked_shape != 'no_selection'){
  #   # If it is a custom drawn polygon, we must do a spatial match here.
  #   if(clicked_shape == 'drawn_poly'){
  #     # stations = stations %>%
  #     #   st_join(shapes %>% filter(shape_name == clicked_shape), st_intersects) %>%
  #     #   filter(!is.na(shape_name)) %>%
  #     #   dplyr::select(-shape_name)
  #
  #     # Add the average trend result to the shape(s)
  #     shapes = shapes %>%
  #       mutate(average_trend_result = ifelse(shape_name == clicked_shape, average_trend_result, NA))
  #
  #   }
  # }

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
                data = shapes) %>%
    addCircleMarkers(#layerId = 'selected_station',
      color = 'orange',
      weight = 7.5,
      fillColor = 'transparent',
      data = stations |> filter(STATION_NUMBER %in% clicked_station)) %>%
    addCircleMarkers(layerId = ~STATION_NUMBER,
                     color = 'black',
                     fillColor = ~pal(trend_sig),
                     radius = 8,
                     weight = 1,
                     fillOpacity = 0.75,
                     label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                     data = stations) %>%
    removeControl("legend") %>%
    addLegend(pal = pal,
              values = ~trend_sig,
              title = 'Mann-Kendall Trend Result',
              data = stations,
              layerId = 'legend')
}
