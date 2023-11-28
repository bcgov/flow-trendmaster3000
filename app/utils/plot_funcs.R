station_flow_plot = function(data,variable_choice,parameter_chosen,clicked_station,stations_shapefile,slopes,
                             caption_label){

  label.frame = data.frame(varname = c('Median',
                                       'DoY_50pct_TotalQ','Min_7_Day',
                                       'Min_7_Day_DoY','Min_30_Day',
                                       'Min_30_Day_DoY','Max_7_Day',
                                       'Max_7_Day_DoY'),
                           labels = c('Average (Median) Flow',
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
      variable_choice %in% c('Median','Total_Volume_m3','Min_7_Day','Min_30_Day','Max_7_Day') , '\n(m<sup>3</sup>/second)',
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

    # parameter_chosen #
    data |>
      mutate(Slope = round(Slope, 2),
             Statistic = round(Statistic, 2),
             P_value = round(P_value, 2)) |>
      mutate(Line := paste0("\nSlope: ",Slope, "\nStatistic: ", Statistic, "\nP Value:", P_value),
             Point = paste0("\nYear: ",Year,"\nValue: ",round(values,2),"\nStation Number: ",STATION_NUMBER)) |>
      ggplot() +
      geom_point(aes(y = values, x = Year, col = STATION_NUMBER, label = Point)) +
      geom_line(aes(y = SlopePreds, x = Year, col = STATION_NUMBER, label = Line),
                linetype = 1,
                linewidth = 2,
                alpha = 0.75
      ) +
      labs(title = name_for_plot,
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

    name_for_plot = paste0("Stations: ",str_flatten_comma(clicked_station))

    # We have day-level flow records.
    if("Day" %in% names(dat)){
      plotting_df = dat %>%
        filter(STATION_NUMBER %in% clicked_station) %>%
        group_by(STATION_NUMBER, Month, Day)
    } else {
      # We have month-level flow records
      plotting_df = dat %>%
        filter(STATION_NUMBER %in% clicked_station) %>%
        group_by(STATION_NUMBER, Month)
    }

    plotting_df = plotting_df |>
      reframe(median_flow = median(Value, na.rm=T),
              percentiles = list(quantile(Value, probs = c(0.05,0.25,0.75,0.95)))) %>%
      ungroup() |>
      unnest_wider(percentiles) %>%
      mutate(median_line_label = 'Median Flow') %>%
      mutate(fifty_pct_label = '"Normal" range (50%) of flow') %>%
      mutate(ninety_pct_label = 'Range of 90% of flow') |>
      arrange(STATION_NUMBER)

    if(!"Day" %in% names(dat)){
      plotting_df = plotting_df |>
        mutate(date_for_plot = lubridate::ymd(paste0('2020-',Month,'-01')))
    } else {
      plotting_df = plotting_df |>
        # mutate(Day = ) |>
        mutate(date_for_plot = lubridate::ymd(paste0('2020-',Month,'-',Day)))
    }


    plotting_df %>%
      ggplot() +
      geom_ribbon(aes(x = date_for_plot, ymin = `5%`, ymax = `95%`, fill = STATION_NUMBER), alpha = 0.3) +
      geom_ribbon(aes(x = date_for_plot, ymin = `25%`, ymax = `75%`, fill = STATION_NUMBER), alpha = 0.1) +
      geom_line(aes(x = date_for_plot, y = median_flow, colour = STATION_NUMBER),
                linewidth = 1) +
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b",
                   expand = c(0,0))+
      labs(y = 'Average \nDischarge (m<sup>3</sup>/s)',
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
