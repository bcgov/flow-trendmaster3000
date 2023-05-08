library(tidyverse)
library(sf)
library(qs)
library(EnvStats)
library(data.table)
library(profvis)
library(bench)

data = qs::qread('app/www/daily_flow_records_passes_qaqc.qs')

# data = data %>%
#   group_by(STATION_NUMBER, Year) %>%
#   reframe(values = median(values,na.rm=T))

# Filter.

filter_data = function(data, finegrain_selector = c(3,15,6,15),
                       scale_selector = 'Select Dates'){

  # data = qs::qread('app/www/daily_flow_records_passes_qaqc.qs')

  # First filtering cut: Time scale ---------------------------------
  dat_filtered = data |>
    filter(Year >= 1990)

  #If custom time scale, use it here to filter data.
  if(scale_selector == 'Select Dates'){
    start_month = finegrain_selector[1]
    start_day = finegrain_selector[2]
    end_month = finegrain_selector[3]
    end_day = finegrain_selector[4]

    req(start_month, start_day, end_month, end_day)

    dat_select_months = dat_filtered %>%
      # mutate(Month = lubridate::month(Date)) %>%
      filter(Month %in% c(start_month,end_month)) %>%
      mutate(Day = lubridate::day(Date)) %>%
      filter((Month == start_month & Day >= start_day) | (Month > start_month & Month < end_month) | (Month == end_month & Day <= end_day))

    return(dat_select_months)
  }
}


filter_data_DT = function(data, finegrain_selector = c(3,15,6,15),
                       scale_selector = 'Select Dates'){

  # data = qs::qread('app/www/daily_flow_records_passes_qaqc.qs')

  data.table::setDT(data)

  start_month = finegrain_selector[1]
  start_day = finegrain_selector[2]
  end_month = finegrain_selector[3]
  end_day = finegrain_selector[4]

  # First filtering cut: Time scale ---------------------------------
  dat_filtered = data[
    Year >= 1990 & Month %in% c(start_month,end_month),
    .(.SD[,], Day = mday(Date))
      ][ (Month == start_month & Day >= start_day) | (Month > start_month & Month < end_month) | (Month == end_month & Day <= end_day),
         ]

  setDF(dat_filtered)
    return(dat_filtered)
}

add_metric = function(dat, user_var_choice){

  if(user_var_choice == 'Average'){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      summarise(Average = median(Value,na.rm=T))

  }
  if(user_var_choice == 'DoY_50pct_TotalQ'){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      mutate(RowNumber = row_number(),
             TotalFlow = sum(Value),
             FlowToDate = cumsum(Value)) %>%
      filter(FlowToDate > TotalFlow/2) %>%
      slice(1) %>%
      mutate(DoY_50pct_TotalQ = lubridate::yday(Date))

  }
  if(user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      mutate(my_row = row_number()) %>%
      ungroup()

    dat$Min_7_Day = RcppRoll::roll_mean(dat$Value, n = 7, align = 'right', fill = NA)

    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      slice_min(Min_7_Day) %>%
      group_by(STATION_NUMBER,Year,Min_7_Day) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(STATION_NUMBER,Year,Min_7_Day,Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)

  }
  if(user_var_choice %in% c('Min_30_Day','Min_30_Day_DoY')){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      mutate(my_row = row_number()) %>%
      ungroup()

    dat$Min_30_Day = RcppRoll::roll_mean(dat$Value, n = 30, align = 'right', fill = NA)


    dat = as_tibble(dat) %>%
      group_by(STATION_NUMBER,Year) %>%
      slice_min(Min_30_Day) %>%
      group_by(STATION_NUMBER,Year,Min_30_Day) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(STATION_NUMBER,Year,Min_30_Day,Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

  }
  if(user_var_choice %in% c('Max_7_Day','Max_7_Day_DoY')){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      mutate(my_row = row_number()) %>%
      ungroup()

    dat$Max_7_Day = RcppRoll::roll_mean(dat$Value, n = 7, align = 'right', fill = NA)

    dat = as_tibble(dat) %>%
      group_by(STATION_NUMBER,Year) %>%
      slice_min(Max_7_Day) %>%
      group_by(STATION_NUMBER,Year,Max_7_Day) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(STATION_NUMBER,Year,Max_7_Day,Max_7_Day_DoY = my_row, Max_7_Day_Date = Date)


  }
  return(dat)
}

filtered_data = filter_data(dat = data)

f_data_with_metric = add_metric(dat = filtered_data, user_var_choice = 'Max_7_Day')

profvis::profvis({
  filtered_data = filter_data(dat = data)
  add_metric(dat = filtered_data, user_var_choice = 'Average')
  })

bench::bench_memory({
  filter_data(data = data)
})

bench::bench_memory({
  filter_data_DT(data = data)
})

bench::bench_memory({
  filtered_data = filter_data(dat = data)
  add_metric(dat = filtered_data, user_var_choice = 'Average')
})

# bench::system_time(case_when_example(dataset = data))
# bench::system_time(case_when_split_example(dataset = data))

bench::workout({
  filter_data(data = data)
  filter_data_DT(data = data)
})
