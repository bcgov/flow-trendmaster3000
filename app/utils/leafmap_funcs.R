make_my_pal = function(user_selected_metric, user_selected_param, date_vars, mk_results){
  if(user_selected_metric %in% date_vars){
    output = leaflet::colorFactor(palette = 'RdBu',
                                  domain = mk_results$trend_sig,
                                  levels = c("Significant Trend Earlier",
                                             'Non-Significant Trend Earlier',
                                             'No Trend',
                                             'Non-Significant Trend Later',
                                             'Significant Trend Later'),
                                  ordered = T)
  } else {
    output = leaflet::colorFactor(palette = 'RdBu',
                                  domain = mk_results$trend_sig,
                                  levels = c("Significant Trend Down",
                                             'Non-Significant Trend Down',
                                             'No Trend',
                                             'Non-Significant Trend Up',
                                             'Significant Trend Up'),
                                  ordered = T)
  }
  output
}


make_base_bc_leafmap = function(){
  leaflet::leaflet(
    options = leafletOptions(
      attributionControl = F
    )
  ) %>%
    leaflet::addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
    leaflet::addTiles(group = 'Streets') %>%
    # leaflet::addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
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
    leaflet::addLayersControl(baseGroups = c("CartoDB","Streets"),
                              options = leaflet::layersControlOptions(collapsed = F),
                              position = 'topright')
}


update_leaflet = function(map, stations, clicked_station, shape_type, shapes, clicked_shape, pal, chosen_parameter, chosen_var,date_vars){

  # Make some static-ish things for the leaflet map.
  #1. 5 colours based on red-blue scale.
  colours = RColorBrewer::brewer.pal(5, 'RdBu')

  #2. Numeric pal for the p-value.
  pvalue_shape_pal = leaflet::colorNumeric(palette = 'RdBu',
                                           domain = c(-2:2))

  #3. Labels for legend if p-value selected.
  if(chosen_var %in% date_vars){
    pvalue_legend_labels = c("Significant Trend Earlier",
                             'Non-Significant Trend Earlier',
                             'No Trend',
                             'Non-Significant Trend Later',
                             'Significant Trend Later')
  } else {
    pvalue_legend_labels = c("Significant Trend Down",
                             'Non-Significant Trend Down',
                             'No Trend',
                             'Non-Significant Trend Up',
                             'Significant Trend Up')
  }

  # #4. Labels for legend if slope selected as parameter to show.
  # slope_legend_labels = c("0-20%",
  #                         "20-40%",
  #                         "40-60%",
  #                         "60-80%",
  #                         "80-100%")

  if(sum(grepl('Nothing', shapes$shape_name)) >= 1){

  } else {

    shape_name_for_dt = shape_type()

    # If the user has used a drawn polygon, we have to refer to
    # a column with a specific name... otherwise, use
    # the variable passed in, called 'shape_name_for_dt'
    # (this could be cleaned up!)

    if(chosen_parameter == 'slope'){
      means_for_shapes = data.table::as.data.table(stations)[,
                                                             .(average_slope = mean(Slope),
                                                               stations_for_mean = .N),
                                                             by = shape_name][, .(shape_name,
                                                                                  average_slope,
                                                                                  stations_for_mean)]

      shapes = dplyr::left_join(shapes |> dplyr::select(shape_name), means_for_shapes) |>
        dplyr::mutate(slope_bin = as.numeric(santoku::chop_equally(average_slope, 5))) |>
        dplyr::mutate(my_colour = colours[average_slope])
    }
    if(chosen_parameter == 'p_value'){
      if(shape_name_for_dt != 'drawn_shape'){
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

      } else {
        means_for_shapes = data.table::as.data.table(stations)[,
                                                               trend_as_number := data.table::fcase(trend_sig %in% c('Significant Trend Down','Significant Trend Earlier'), -2,
                                                                                                    trend_sig %in% c('Non-Significant Trend Down','Non-Significant Trend Earlier'), -1,
                                                                                                    trend_sig %in% c("No Trend"), 0,
                                                                                                    trend_sig %in% c('Non-Significant Trend Up','Non-Significant Trend Later'), 1,
                                                                                                    trend_sig %in% c('Significant Trend Up','Significant Trend Later'), 2)
        ][,
          .(average_trend_result = mean(trend_as_number),
            stations_for_mean = .N),
          by = shape_name][, .(shape_name,
                               average_trend_result,
                               stations_for_mean)]

        shapes = dplyr::left_join(shapes |> dplyr::select(shape_name), means_for_shapes)
      }
    }
  }

  l = leaflet::leafletProxy(map) %>%
    leaflet::clearShapes() %>%
    leaflet::clearMarkers()

  if(chosen_parameter == 'slope'){
    l = l |>
      leaflet::addPolygons(
        layerId = ~shape_name,
        color = 'grey',
        fillOpacity = 0.5,
        fillColor = ~my_colour,
        label = ~lapply(paste0(shape_name,": ",
                               round(average_slope,2),
                               "<br>",
                               "(",stations_for_mean, " station(s))"),
                        htmltools::HTML
        ),
        weight = 2,
        highlightOptions = leaflet::highlightOptions(
          color = 'purple',
          opacity = 0.5
        ),
        data = shapes)
  }
  if(chosen_parameter == 'p_value'){
    l = l |>
      leaflet::addPolygons(
      layerId = ~shape_name,
      color = 'grey',
      fillOpacity = 0.5,
      fillColor = ~pvalue_shape_pal(average_trend_result),
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
      data = shapes)
  }

  l = l |>
    leaflet::addCircleMarkers(#layerId = 'selected_station',
      color = 'orange',
      weight = 7.5,
      fillColor = 'transparent',
      data = stations |> dplyr::filter(STATION_NUMBER %in% clicked_station))

  if(chosen_parameter == 'slope'){
    l = l |>
      leaflet::addCircleMarkers(
        layerId = ~STATION_NUMBER,
        color = 'black',
        fillColor = ~my_colour,
        radius = 8,
        weight = 1,
        opacity = ~ifelse(stations$P_value < 0.05, 0.75, 0.25),
        fillOpacity = ~ifelse(stations$P_value < 0.05, 0.75, 0.25),
        label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
        data = stations)
  }
  if(chosen_parameter == 'p_value'){
    l = l |>
      leaflet::addCircleMarkers(
        layerId = ~STATION_NUMBER,
        color = 'black',
        fillColor = ~my_colour,
        radius = 8,
        weight = 1,
        fillOpacity = 0.75,
        label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
        data = stations)
  }
  l = l |>
    leaflet::removeControl("legend")
  if(chosen_parameter == 'slope'){
    l = l |>
      leaflet::addLegend(colors = colours,
                         labels = levels(stations$slope_bin),
                         title = 'Mann-Kendall Trend Slope',
                         data = stations,
                         layerId = 'legend')
  }
  if(chosen_parameter == 'p_value'){
    l = l |>
      leaflet::addLegend(colors = colours,
                         labels = levels(stations$trend_sig),
                         title = 'Mann-Kendall Trend Slope',
                         data = stations,
                         layerId = 'legend')
  }
  l
}
