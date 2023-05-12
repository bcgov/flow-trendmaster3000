library(tidyverse)
library(sf)
library(qs)
library(EnvStats)
library(data.table)
library(profvis)
library(bench)

data = qs::qread('app/www/daily_flow_records_passes_qaqc.qs')

data.table::setDT(data)

filter_data_DT = function(data, finegrain_selector = c(3,15,6,15),
                       scale_selector = 'Select Dates'){

  start_month = finegrain_selector[1]
  start_day = finegrain_selector[2]
  end_month = finegrain_selector[3]
  end_day = finegrain_selector[4]

  # First filtering cut: Time scale ---------------------------------
  dat_filtered = data[
    Year >= 1990 & Month %in% c(start_month,end_month)
    ][,
    .(.SD[,], Day = lubridate::day(Date))
  ][ (Month == start_month & Day >= start_day) | (Month > start_month & Month < end_month) | (Month == end_month & Day <= end_day),
  ]

    return(dat_filtered)
}
#
# bench::bench_memory(filter_data_DT(data))
# bench::workout(filter_data_DT(data))

filtered_data = filter_data_DT(data)

add_metric = function(dat, user_var_choice){

  if(user_var_choice == 'Average'){

    # Using data.table notation, calculate the median flow
    # by station # and Year.
    dat = dat[, .(values = median(Value,na.rm=T)), by = .(STATION_NUMBER,Year)]
    #Update progress bar...
  }
  if(user_var_choice == 'DoY_50pct_TotalQ'){
    # Using data.table notation, calculate the day of freshet onset.
    # (first, calculate the total flow, the flow-to-date, and the row #,
    #  then, take the first row where the flow to date is greater than half
    #  the total annual flow, by station # and year;
    #  finally, calculate the day of the year (from 1 to 365) for that date.)
    dat = cbind(dat,
                dat[,
              .(TotalFlow = sum(Value),
                FlowToDate = cumsum(Value)),
              by = .(STATION_NUMBER,Year)
    ])[
      FlowToDate > TotalFlow/2
    ][,
      .SD[1,],
      by = .(STATION_NUMBER,Year)
    ][
      ,.(STATION_NUMBER,Date,Year,DoY_50pct_TotalQ = data.table::yday(Date))
    ]
  }
  if(user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)
    dat = dat[,
              RowNumber := rleid(Value),
              by = .(STATION_NUMBER,Year)
    ][ , Min_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
       by = .(STATION_NUMBER,Year)
    ][
      ,
      .SD[which.min(Min_7_Day),],
      by = .(STATION_NUMBER,Year)
    ][,
      Min_7_Day_DoY := data.table::yday(Date),
      by = .(STATION_NUMBER,Year)
    ]
    # names(dat)[c(3)] <- c("Min_7_Day_Date")
  }
  if(user_var_choice %in% c('Min_30_Day','Min_30_Day_DoY')){
    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)
    dat = dat[,
              RowNumber := rleid(Value),
              by = .(STATION_NUMBER,Year)
    ][ , Min_30_Day := data.table::frollmean(Value, 30, align = 'right', fill = NA),
       by = .(STATION_NUMBER,Year)
    ][
      ,
      .SD[which.min(Min_30_Day),],
      by = .(STATION_NUMBER,Year)
    ][,
      Min_30_Day_DoY := data.table::yday(Date),
      by = .(STATION_NUMBER,Year)
    ]
    # names(dat)[c(3)] <- c("Min_30_Day_Date")
  }
  if(user_var_choice %in% c('Max_7_Day','Max_7_Day_DoY')){
    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)

    dat = dat[,
        RowNumber := rleid(Value),
        by = .(STATION_NUMBER,Year)
    ][ , Max_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
       by = .(STATION_NUMBER,Year)
    ][
      ,
      .SD[which.max(Max_7_Day),],
      by = .(STATION_NUMBER,Year)
    ][,
      Max_7_Day_DoY := data.table::yday(Date),
      by = .(STATION_NUMBER,Year)
    ]
    # names(dat)[c(3)] <- c("Max_7_Day_Date")
  }

  # Generalize the name of the column for the chosen variable.
  names(dat)[which(names(dat) == user_var_choice)] <- 'values'
  dat = dat[,.(STATION_NUMBER, Year,Date,values)]
  return(dat)
}

# bench::bench_memory(add_metric(filtered_data,'Average'))
# bench::bench_memory(add_metric(filtered_data,'DoY_50pct_TotalQ'))
# bench::bench_memory(add_metric(filtered_data,'Min_7_Day'))

dat_with_m = add_metric(filtered_data, 'DoY_50pct_TotalQ')

# profvis::profvis({
#   filtered_data = filter_data_DT(data = data)
#   add_metric(dat = filtered_data, user_var_choice = 'Average')
#   })

# bench::bench_memory({
#   add_metric(dat = filtered_data, user_var_choice = 'Max_7_Day')
# })


calc_mk = function(data, user_var_choice){

  chosen_variable = user_var_choice
  unique_stations = unique(data$STATION_NUMBER)

  # Filter for stations with 3+ records.
  data = data[,group_count := .N, by = STATION_NUMBER
  ][group_count >= 3,]

  # This chunk does the following things:
  # 1. First, applies the Mann-Kendall trend test. Results are a list inside each row.
  # 2. Unnest the list results, putting each in its own row.
  # 3. Add a new column that indicates what each row is (e.g. p-value, or slope, or Tau, etc.)
  data = data[, .(MK_results = kendallTrendTest(values ~ Year, warn = FALSE)[c('statistic','p.value','estimate')]),
              by = STATION_NUMBER
  ][,
    .(MK_results = unlist(MK_results)), by = .(STATION_NUMBER)
  ][,
    .(.SD[,], MK_key = c('Statistic','P_value','Tau','Slope','Intercept'))
  ]

  # Lean on pivot_wider from tidyverse to easily pivot wider.
  # Then, add in what the trend significance is.
  as.data.frame(data) |>
    pivot_wider(id_cols = STATION_NUMBER, names_from = MK_key, values_from = MK_results) |>
    mutate(trend_sig = case_when(
      abs(Tau) <= 0.05 ~ "No Trend",
      Tau < -0.05 & P_value < 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) ~ "Significant Trend Earlier",
      Tau < -0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) ~ "Non-Significant Trend Earlier",
      Tau > 0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) ~ "Non-Significant Trend Later",
      Tau > 0.05 & P_value < 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) ~ "Significant Trend Later",
      Tau < -0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) ~ "Significant Trend Down",
      Tau < -0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) ~ "Non-Significant Trend Down",
      Tau > 0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) ~ "Non-Significant Trend Up",
      Tau > 0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) ~ "Significant Trend Up"
    ))
}

calc_mk_fcase = function(data, user_var_choice){


  chosen_variable = user_var_choice
  unique_stations = unique(data$STATION_NUMBER)

  # Filter for stations with 3+ records.
  data = data[,group_count := .N, by = STATION_NUMBER
  ][group_count >= 3,]

  # This chunk does the following things:
  # 1. First, applies the Mann-Kendall trend test. Results are a list inside each row.
  # 2. Unnest the list results, putting each in its own row.
  # 3. Add a new column that indicates what each row is (e.g. p-value, or slope, or Tau, etc.)
  data = data[, .(MK_results = kendallTrendTest(values ~ Year, warn = FALSE)[c('statistic','p.value','estimate')]),
              by = STATION_NUMBER
  ][,
    .(MK_results = unlist(MK_results)), by = .(STATION_NUMBER)
  ][,
    .(.SD[,], MK_key = c('Statistic','P_value','Tau','Slope','Intercept'))
  ]

  # Lean on pivot_wider from tidyverse to easily pivot wider.
  # Then, add in what the trend significance is.
  as.data.frame(data) |>
    pivot_wider(id_cols = STATION_NUMBER, names_from = MK_key, values_from = MK_results) |>
    mutate(trend_sig = fcase(
      abs(Tau) <= 0.05 , "No Trend",
      Tau < -0.05 & P_value < 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) , "Significant Trend Earlier",
      Tau < -0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) , "Non-Significant Trend Earlier",
      Tau > 0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) , "Non-Significant Trend Later",
      Tau > 0.05 & P_value < 0.05 & grepl(pattern = '(TotalQ|DoY)$)', chosen_variable) , "Significant Trend Later",
      Tau < -0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) , "Significant Trend Down",
      Tau < -0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) , "Non-Significant Trend Down",
      Tau > 0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) , "Non-Significant Trend Up",
      Tau > 0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$)', chosen_variable)) , "Significant Trend Up"
    ))
}

dat_with_mk = calc_mk_fcase(dat_with_m, 'DoY_50pct_TotalQ')

stations = read_sf('app/www/stations.gpkg')

stations_with_dat = stations |>
  left_join(dat_with_mk)

og_calc = function(stations_with_dat, shapes, shape_type){
  means_for_shapes = as.data.table(stations_with_dat)[,
                                             trend_as_number := fcase(trend_sig == 'Significant Trend Down', -2,
                                                                      trend_sig == 'Non-Significant Trend Down', -1,
                                                                      trend_sig == "No Trend", 0,
                                                                      trend_sig == 'Non-Significant Trend Up', 1,
                                                                      trend_sig == 'Significant Trend Up', 2)
  ][,
    .(average_trend_result = mean(trend_as_number),
      stations_for_mean = .N),
    by = get(shape_type)][, .(shape_name = get,
                         average_trend_result,
                         stations_for_mean)]

  merge(shapes, means_for_shapes)

}

og_calc(stations_with_dat = stations_with_dat, shapes = read_sf('app/www/ecosections.gpkg'), shape_type = 'ecosec')


new_calc = function(){
  interm_data = filtered_data[,Min_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
     by = .(STATION_NUMBER,Year)
  ] |>
    group_by(STATION_NUMBER,Year) |>
    group_split() |>
    map(\(df) df[which.min(df$Min_7_Day),]) |>
    bind_rows() |>
    as.data.table()

  interm_data[,
    .(STATION_NUMBER,Date,Year,values = Min_7_Day, Min_7_Day_DoY = data.table::yday(Date))
  ]
}

bench::bench_memory({
  og_calc(stations_with_dat = stations_with_dat, shapes = read_sf('app/www/ecosections.gpkg'), shape_type = 'ecosec')
})

bench::workout({
  og_calc(stations_with_dat = stations_with_dat,
          shapes = read_sf('app/www/ecosections.gpkg'),
          shape_type = 'ecosec')
  })

bench::bench_memory({
  new_calc()
})
