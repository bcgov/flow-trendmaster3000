
add_metric_to_dat_mod <- function(id, flow_dat_daily = flow_dat_daily,
                                  data, user_var_choice) {
  moduleServer(
    id,
    function(input, output, session) {

      dat_with_metric = reactive({

        withProgress(message = 'calculating metric', {

          req(!is.null(data()))

          dat = data()
          user_var_choice = user_var_choice()

          #Update progress bar...
          incProgress(1 / 2)

          if(user_var_choice == 'Average'){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              summarise(Average = median(Value,na.rm=T))
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

            dat$Min_7_Day = RcppRoll::roll_mean(dat$Value, n = 7, align = 'right', fill = NA)

            #Update progress bar...
            incProgress(1 / 4)

            dat = dat %>%
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

            dat$Min_30_Day = RcppRoll::roll_mean(dat$Value, n = 30, align = 'right', fill = NA)

            #Update progress bar...
            incProgress(1 / 4)

            dat = as_tibble(dat) %>%
              group_by(STATION_NUMBER,Year) %>%
              slice_min(Min_30_Day) %>%
              group_by(STATION_NUMBER,Year,Min_30_Day) %>%
              slice(1) %>%
              ungroup() %>%
              dplyr::select(STATION_NUMBER,Year,Min_30_Day,Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

            #Update progress bar...
            incProgress(1 / 4)
          }
          if(user_var_choice %in% c('Max_7_Day','Max_7_Day_DoY')){
            dat = dat %>%
              group_by(STATION_NUMBER,Year) %>%
              mutate(my_row = row_number()) %>%
              ungroup()

            dat$Max_7_Day = RcppRoll::roll_mean(dat$Value, n = 7, align = 'right', fill = NA)

            #Update progress bar...
            incProgress(1 / 4)

            dat = as_tibble(dat) %>%
              group_by(STATION_NUMBER,Year) %>%
              slice_min(Max_7_Day) %>%
              group_by(STATION_NUMBER,Year,Max_7_Day) %>%
              slice(1) %>%
              ungroup() %>%
              dplyr::select(STATION_NUMBER,Year,Max_7_Day,Max_7_Day_DoY = my_row, Max_7_Day_Date = Date)

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
      return(dat_with_metric)
    }
  )
}
