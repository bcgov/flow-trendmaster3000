station_flow_plot = function(data,variable_choice,clicked_station,stations_shapefile,slopes,
                             caption_label){

  label.frame = data.frame(varname = c('Average',
                                       'DoY_50pct_TotalQ','Min_7_Day',
                                       'Min_7_Day_DoY','Min_30_Day',
                                       'Min_30_Day_DoY','Max_7_Day',
                                       'Max_7_Day_DoY'),
                           labels = c('Average Flow',
                                      'Date of \n50% Annual Flow',
                                      'Minimum Flow (7day)',
                                      'Date of \nMinimum Flow (7day)',
                                      'Minimum Flow (30day)',
                                      'Date of \nMinimum Flow (30day)',
                                      'Maximum Flow (7day)',
                                      'Date of \nMaximum Flow (7day)'))

  if(sum(str_detect(clicked_station, 'no_selection')) == 1){
    ggplot() +
      geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
      ggthemes::theme_map()
  } else {

    plot_units = fcase(
      variable_choice %in% c('Average','Total_Volume_m3','Min_7_Day','Min_30_Day','Max_7_Day') , '\n(m<sup>3</sup>/second)',
      variable_choice %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Min_30_Day_DoY','Max_7_Day_DoY'), " "
    )

    # Establish station name (or names) for plot title.
    if(length(clicked_station) == 1){
      station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER %in% clicked_station,]$STATION_NAME)
      name_for_plot = paste0(station_name," (",unique(clicked_station),")")
      name_for_plot
    } else {
      name_for_plot = paste0("Stations: ",str_flatten_comma(clicked_station))
    }

    data |>
      ggplot() +
      geom_point(aes(y = values, x = Year, col = STATION_NUMBER)) +
      geom_line(aes(y = SlopePreds, x = Year, col = STATION_NUMBER),
                linetype = 1,
                linewidth = 2,
                alpha = 0.75
                ) +
      # ggrepel::geom_label_repel(aes(y = SlopePreds,
      #                               x = Year,
      #                               label = paste0(STATION_NUMBER,
      #                                              "\nSen Slope: ",
      #                                              round(Slope, 3),
      #                                              "\n P-value: ",
      #                                              round(P_value,2)))) +
      labs(title = name_for_plot,
           # subtitle = paste0(unique(slopes$trend_sig),
           #                   " (Sen slope: ",round(slopes$Slope,3),
           #                   ", p-value ~ ",round(unique(slopes$P_value),2),")"),
           caption = caption_label) +
      labs(y = paste(label.frame[label.frame$varname == variable_choice,]$labels,plot_units,sep = " ")) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme_minimal() +
      theme(axis.title.y = element_markdown(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 11),
            plot.caption = element_text(size = 11),
            legend.position = 'none')
  }
}

hydrograph_plot = function(dat, clicked_station, stations_shapefile){

  if(sum(str_detect(clicked_station, 'no_selection')) == 1){
    ggplot() +
      geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
      ggthemes::theme_map()
  } else {

    # Establish station name (or names) for plot title.
    # if(length(clicked_station) == 1){
    #   station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER %in% clicked_station,]$STATION_NAME)
    #   name_for_plot = paste0(station_name," (",unique(clicked_station),")")
    #   name_for_plot
    # } else {
      name_for_plot = paste0("Stations: ",str_flatten_comma(clicked_station))
    # }

    plotting_df = dat %>%
      filter(STATION_NUMBER %in% clicked_station) %>%
      mutate(date_for_plot = lubridate::ymd(paste0('2023-',month(Date),'-',day(Date)))) %>%
      group_by(STATION_NUMBER, date_for_plot) %>%
      reframe(median_flow = median(Value, na.rm=T),
              percentiles = list(quantile(Value, probs = c(0.05,0.25,0.75,0.95)))) %>%
      unnest_wider(percentiles) %>%
      filter(!is.na(date_for_plot)) %>%
      # # Convert from calendar year to 'water year'
      # slice(
      #   # Slice for October to December
      #   which(month(date_for_plot) %in% c(10:12)),
      #   # Slice for January to September
      #   which(month(date_for_plot) %in% c(1:9))
      # ) %>%
      # mutate(month_label = ifelse(day(date_for_plot) == 15, month(date_for_plot), NA)) %>%
      # # mutate(back_to_doy = 1:365) %>%
      # mutate(back_to_doy = 1:nrow(.)) %>%
      mutate(median_line_label = 'Median Flow') %>%
      mutate(fifty_pct_label = '"Normal" range (50%) of flow') %>%
      mutate(ninety_pct_label = 'Range of 90% of flow')

    plotting_df %>%
      ggplot() +
      geom_ribbon(aes(x = date_for_plot, ymin = `5%`, ymax = `95%`, fill = STATION_NUMBER), alpha = 0.3) +
      geom_ribbon(aes(x = date_for_plot, ymin = `25%`, ymax = `75%`, fill = STATION_NUMBER), alpha = 0.1) +
      geom_line(aes(x = date_for_plot, y = median_flow, colour = STATION_NUMBER),
                linewidth = 1) +
      # scale_color_brewer(palette = 'Set2') +
      # scale_fill_brewer(palette = 'Set2') +
      # scale_colour_manual(values = c("Median Flow" = "#2d7ca1")) +
      # scale_fill_manual(values = c("Range of 90% of flow" = "#ceeaed",
      #                              '"Normal" range (50%) of flow' = 'lightblue')) +

      # scale_x_continuous(breaks = plotting_df[!is.na(plotting_df$month_label),]$back_to_doy,
      #                    labels = month(plotting_df[!is.na(plotting_df$month_label),]$month_label, abbr = T, label = T)) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0,0))+
      labs(y = 'Average Discharge (m<sup>3</sup>/s)',
           x = '',
           title = 'Daily Stream or River Discharge',
           subtitle = name_for_plot,
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
