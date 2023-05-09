
add_metric_to_dat_mod <- function(id, flow_dat_daily = flow_dat_daily,
                                  data, user_var_choice,
                                  user_period_choice,
                                  scale_selector_radio,
                                  finegrain_reactives_list) {
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

            # Using data.table notation, calculate the median flow
            # by station # and Year.
            dat = dat[, .(values = median(Value,na.rm=T)), by = .(STATION_NUMBER,Year)]
            #Update progress bar...
            incProgress(1 / 2)
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
            incProgress(1 / 2)
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
            incProgress(1 / 2)
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
            incProgress(1 / 2)
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
            incProgress(1 / 2)
          }

          names(dat)[which(names(dat) == user_var_choice)] <- 'values'
          dat = dat[,.(STATION_NUMBER, Year,Date,values)]
          return(dat)
        })
      }) %>%
        bindCache(user_var_choice(),
                  user_period_choice(),
                  scale_selector_radio(),
                  finegrain_reactives_list())
      return(dat_with_metric)
    }
  )
}
