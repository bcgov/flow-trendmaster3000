make_my_pal = function(user_selected_metric, date_vars, mk_results){
  if(user_selected_metric %in% date_vars){
    leaflet::colorFactor(palette = 'RdBu',
                domain = mk_results$trend_sig,
                levels = c("Significant Trend Earlier",
                           'Non-Significant Trend Earlier',
                           'No Trend',
                           'Non-Significant Trend Later',
                           'Significant Trend Later'),
                ordered = T)
  } else {
    leaflet::colorFactor(palette = 'RdBu',
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
  leaflet::leaflet(
    options = leafletOptions(
      attributionControl = F
    )
  ) %>%
    leaflet::addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
    leaflet::addTiles(group = 'Streets') %>%
    leaflet::addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
    envreportutils::add_bc_home_button() %>%
    envreportutils::set_bc_view() %>%
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
    leaflet::addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                     options = leaflet::layersControlOptions(collapsed = F),
                     position = 'topright')
}


update_leaflet = function(map, stations, clicked_station, shape_type, shapes, clicked_shape, pal){

  if(sum(grepl('Nothing', shapes$shape_name)) >= 1){

 } else {

   # If it's a drawn poly, add 'drawn_poly' to stations df.
   # This helps with spatial matching, which we only have to do in the shiny app
   # for a drawn polygon (all other shapes included have been matched prior to the
   # shiny app code)
   if(sum(grepl('drawn_poly',shapes$shape_name)) >= 1){
     stations = stations |>
       dplyr::mutate(drawn_shape = 'drawn_poly')

     shapes = shapes |> dplyr::select(-average_trend_result,
                                      -stations_for_mean)

     # Just keep stations inside our shape.
     stations = stations |>
       sf::st_join(shapes) |>
       dplyr::filter(!is.na(shape_name))
   }

  # Note the janky data.table code I wrote below: to pass in a reactive variable
  # as one of the grouping variables in the 'by = ' argument, I had to use
  # get(...); this results in a grouping variable called 'get', so that is how
  # I refer to it in the 2nd and 3rd chained data.table calls

   shape_name_for_dt = shape_type()

   means_for_shapes = data.table::as.data.table(stations)[,
                                              trend_as_number := data.table::fcase(trend_sig %in% c('Significant Trend Down','Significant Trend Earlier'), -2,
                                                                       trend_sig %in% c('Non-Significant Trend Down','Non-Significant Trend Earlier'), -1,
                                                                       trend_sig %in% c("No Trend"), 0,
                                                                       trend_sig %in% c('Non-Significant Trend Up','Non-Significant Trend Later'), 1,
                                                                       trend_sig %in% c('Significant Trend Up','Significant Trend Later'), 2)
   ][,
     .(average_trend_result = mean(trend_as_number),
       stations_for_mean = .N),
     by = eval(shape_name_for_dt)][, .(shape_name = get(shape_name_for_dt),
                               average_trend_result,
                               stations_for_mean)]

   shapes = dplyr::left_join(shapes, means_for_shapes)
 }

  shape_pal = leaflet::colorNumeric(palette = 'RdBu',
                           domain = c(-2:2))

  leaflet::leafletProxy(map) %>%
    leaflet::clearShapes() %>%
    leaflet::clearMarkers() %>%
    leaflet::addPolygons(layerId = ~shape_name,
                color = 'grey',
                fillOpacity = 0.5,
                fillColor = ~shape_pal(average_trend_result),
                label = ~lapply(paste0(shape_name,": ",
                                       round(average_trend_result,2),
                                       "<br>",
                                       "(",stations_for_mean, " station(s))"),
                                htmltools::HTML
                ),
                weight = 2,
                highlightOptions = leaflet::highlightOptions(
                  color = 'purple',
                  opacity = 0.5
                ),
                data = shapes) %>%
    leaflet::addCircleMarkers(#layerId = 'selected_station',
      color = 'orange',
      weight = 7.5,
      fillColor = 'transparent',
      data = stations |> dplyr::filter(STATION_NUMBER %in% clicked_station)) %>%
    leaflet::addCircleMarkers(layerId = ~STATION_NUMBER,
                     color = 'black',
                     fillColor = ~pal(trend_sig),
                     radius = 8,
                     weight = 1,
                     fillOpacity = 0.75,
                     label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                     data = stations) %>%
    leaflet::removeControl("legend") %>%
    leaflet::addLegend(pal = pal,
              values = ~trend_sig,
              title = 'Mann-Kendall Trend Result',
              data = stations,
              layerId = 'legend')
}
