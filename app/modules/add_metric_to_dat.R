
add_metric_to_dat_mod <- function(id, flow_dat_daily = flow_dat_daily,
                                  data, user_var_choice) {
  moduleServer(
    id,
    function(input, output, session) {
      # browser()
      dat_with_metric = reactive({

        req(exists('flow_dat_daily'))
        # browser()
        withProgress(message = 'calculating metric', {

          dat = data
          #Update progress bar...
          incProgress(1 / 2)

          if(user_var_choice == 'Mean'){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              summarise(Mean = mean(Value,na.rm=T))
            #Update progress bar...
            incProgress(1 / 2)
          }
          if(user_var_choice == 'Median'){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              summarise(Median = median(Value,na.rm=T))
            #Update progress bar...
            incProgress(1 / 2)
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
            #Update progress bar...
            incProgress(1 / 2)
          }
          if(user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              mutate(my_row = row_number()) %>%
              ungroup()

            daily_flows_dt = data.table::data.table(dat, key = c('STATION_NUMBER','Year'))

            daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

            #Update progress bar...
            incProgress(1 / 4)

            dat = as_tibble(daily_flows_dt) %>%
              group_by(STATION_NUMBER,Year) %>%
              slice_min(Min_7_Day) %>%
              group_by(STATION_NUMBER,Year,Min_7_Day) %>%
              slice(1) %>%
              ungroup() %>%
              dplyr::select(STATION_NUMBER,Year,Min_7_Day,Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)

            #Update progress bar...
            incProgress(1 / 4)
          }
          if(user_var_choice %in% c('Min_30_Day','Min_30_Day_DoY')){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              mutate(my_row = row_number()) %>%
              ungroup()

            daily_flows_dt = data.table::data.table(dat, key = c('STATION_NUMBER','Year'))

            daily_flows_dt$Min_30_Day = frollmean(daily_flows_dt[, Value], 30, align = 'right')

            #Update progress bar...
            incProgress(1 / 4)

            dat = as_tibble(daily_flows_dt) %>%
              group_by(STATION_NUMBER,Year) %>%
              slice_min(Min_30_Day) %>%
              group_by(STATION_NUMBER,Year,Min_30_Day) %>%
              slice(1) %>%
              ungroup() %>%
              dplyr::select(STATION_NUMBER,Year,Min_30_Day,Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

            #Update progress bar...
            incProgress(1 / 4)
          }
          if(user_var_choice == 'Total_Volume_m3'){
            dat = dat %>%
              # The flow parameter here is a flow rate, i.e. m^3/second.
              # Multiply by number of seconds in a day to get volume.
              mutate(Volume = Value*86400) %>%
              group_by(STATION_NUMBER,Year) %>%
              summarise(Total_Volume_m3 = sum(Volume))
            #Update progress bar...
            incProgress(1 / 2)
          }
          dat = dat %>%
            dplyr::select(everything(), values = !!sym(user_var_choice)) |>
            ungroup()
          return(dat)
        })
      })
      return(reactive(dat_with_metric()))
    }
  )
}
