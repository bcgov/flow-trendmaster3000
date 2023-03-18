station_flow_plot = function(data,variable_choice,clicked_station,stations_shapefile,slopes,caption_label){

  label.frame = data.frame(varname = c('Mean','Median',
                                       'DoY_50pct_TotalQ','Min_7_Day',
                                       'Min_7_Day_DoY','Min_30_Day',
                                       'Min_30_Day_DoY','Total_Volume_m3'),
                           labels = c('Mean Flow','Median Flow',
                                      'Date of 50% Annual Flow',
                                      'Minimum Flow (7day)',
                                      'Date of Minimum Flow (7day)',
                                      'Minimum Flow (30day)',
                                      'Date of Minimum Flow (30day)',
                                      'Total Flow'))

  if(clicked_station == 'no_selection'){
    ggplot() +
      geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
      ggthemes::theme_map()
  } else {

    plot_units = fcase(
      variable_choice %in% c('Mean','Median','Total_Volume_m3','Min_7_Day','Min_30_Day') , '(m<sup>3</sup>/second)',
      variable_choice %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Min_30_Day_DoY'), " "
    )

    station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER == clicked_station,]$STATION_NAME)

    data %>%
      ungroup() %>%
      filter(STATION_NUMBER == clicked_station) %>%
      left_join(stations_shapefile %>%
                  st_drop_geometry() %>%
                  dplyr::select(STATION_NUMBER,STATION_NAME),
                by = 'STATION_NUMBER') %>%
      ggplot() +
      geom_point(aes(y = values, x = Year)) +
      # geom_line(aes(y = values, x = Year)) +
      geom_line(aes(y = SlopePreds, x = Year),
                colour = 'darkblue',
                linetype = 1,
                linewidth = 2,
                alpha = 0.75,
                data = slopes) +
      labs(title = paste0(station_name," (",unique(clicked_station),")"),
           subtitle = paste0(unique(slopes$trend_sig),
                             " (Sen slope: ",round(slopes$Slope,3),
                             ", p-value ~ ",round(unique(slopes$P_value),2),")"),
           caption = caption_label) +
      labs(y = paste(label.frame[label.frame$varname == variable_choice,]$labels,plot_units,sep = " ")) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme_minimal() +
      theme(axis.title.y = element_markdown(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 11))
  }
}

hydrograph_plot = function(dat, clicked_station,stations_shapefile){

  if(clicked_station == 'no_selection'){
    ggplot() +
      geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
      ggthemes::theme_map()
  } else {

    # plot_units = fcase(
    #   variable_choice %in% c('Mean','Median','Total_Volume_m3','Min_7_Day','Min_30_Day') , '(m<sup>3</sup>/second)',
    #   variable_choice %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Min_30_Day_DoY'), " "
    # )

    station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER == clicked_station,]$STATION_NAME)

    plotting_df = dat %>%
      dplyr::select(-Symbol) %>%
      filter(STATION_NUMBER == clicked_station) %>%
      mutate(date_for_plot = lubridate::ymd(paste0('2023-',month(Date),'-',day(Date)))) %>%
      group_by(date_for_plot) %>%
      reframe(median_flow = median(Value, na.rm=T),
              percentiles = list(quantile(Value, probs = c(0.05,0.25,0.75,0.95)))) %>%
      unnest_wider(percentiles) %>%
      filter(!is.na(date_for_plot)) %>%
      # Convert from calendar year to 'water year'
      slice(
        # Slice for October to December
        which(month(date_for_plot) %in% c(10:12)),
        # Slice for January to September
        which(month(date_for_plot) %in% c(1:9))
      ) %>%
      mutate(month_label = ifelse(day(date_for_plot) == 15, month(date_for_plot), NA)) %>%
      # mutate(date_for_plot = forcats::fct_inorder(factor(date_for_plot))) %>%
      mutate(back_to_doy = 1:365) %>%
      mutate(median_line_label = 'Median Flow') %>%
      mutate(fifty_pct_label = '"Normal" range (50%) of flow') %>%
      mutate(ninety_pct_label = 'Range of 90% of flow')

    plotting_df %>%
      ggplot() +
      geom_ribbon(aes(x = back_to_doy, ymin = `5%`, ymax = `95%`, fill = ninety_pct_label)) +
      geom_ribbon(aes(x = back_to_doy, ymin = `25%`, ymax = `75%`, fill = fifty_pct_label)) +
      geom_line(aes(x = back_to_doy, y = median_flow, colour = median_line_label),
                linewidth = 1) +
      scale_colour_manual(values = c("Median Flow" = "#2d7ca1")) +
      scale_fill_manual(values = c("Range of 90% of flow" = "#ceeaed",
                                   '"Normal" range (50%) of flow' = 'lightblue')) +
      scale_x_continuous(breaks = plotting_df[!is.na(plotting_df$month_label),]$back_to_doy,
                         labels = month(plotting_df[!is.na(plotting_df$month_label),]$month_label, abbr = T, label = T)) +
      labs(y = 'Average Discharge (m<sup>3</sup>/s)',
           x = '',
           title = '*Daily Stream or River Discharge*',
           subtitle = station_name,
           col = '',
           fill = '') +
      theme(axis.title.y = element_markdown(size = 15),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.position = 'top',
            plot.title = element_markdown(hjust = 0.5),
            panel.background = element_rect(fill = 'transparent'),
            panel.grid.major = element_line(colour = 'grey'))
  }
}
