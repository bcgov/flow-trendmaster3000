
add_metric_to_dat_mod <- function(id, flow_dat_daily = flow_dat_daily,
                                  data, user_var_choice,
                                  user_period_choice,
                                  scale_selector_radio,
                                  finegrain_reactives_list) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      dat_with_metric = shiny::reactive({

        shiny::withProgress(message = 'calculating metric', {

          req(!is.null(data()))

          dat = data()
          user_var_choice = user_var_choice()
          scale_chosen = scale_selector_radio()
          period_chosen = user_period_choice()

          #Update progress bar...
          shiny::incProgress(1 / 2)

          # If the user is using the Annual data, all of these metrics
          # have already been calculated! Just need to rename the
          # column of choice.
          if(scale_chosen == 'Annual'){

            dat = dat[,.(STATION_NUMBER,Year,values = get(user_var_choice))]
          } else {

            # browser()

            if(user_var_choice == 'Median'){

              # Using data.table notation, calculate the median flow
              # by station # and Year.
              dat = dat[, .(values = median(Value,na.rm=T), stations_per_group = .N), by = .(STATION_NUMBER,Year)]
              #Update progress bar...
              shiny::incProgress(1 / 2)
            }
            if(user_var_choice == 'DoY_50pct_TotalQ'){
              # Using data.table notation, calculate the day of freshet onset.
              # (first, calculate the total flow, the flow-to-date, and the row #,
              #  then, take the first row where the flow to date is greater than half
              #  the total annual flow, by station # and year;
              #  finally, calculate the day of the year (from 1 to 365) for that date.)
              data_output = dat[,
                                `:=`(
                                  TotalFlow = sum(Value*7),
                                  FlowToDate = cumsum(Value*7)),
                                by = .(STATION_NUMBER,Year)
              ] |>
                dplyr::group_by(STATION_NUMBER,Year) |>
                dplyr::group_split() |>
                purrr::map(\(df) df[which.min(abs(df$TotalFlow/2 - df$FlowToDate)),]) |>
                dplyr::bind_rows() |>
                data.table::as.data.table()

              dat = data_output[
                ,.(STATION_NUMBER,Date,Year,DoY_50pct_TotalQ = data.table::yday(Date))
              ]

              shiny::incProgress(1 / 2)
            }
            if(user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
              # Using data.table notation,
              # 1. Calculate row numbers by station # and Year.
              # 2. Calculate 7-day rolling average of flow.
              # 3. Just take lowest 7-day flow (to find the low-flow)
              # interm_data = dat[,Min_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
              #                   by = .(STATION_NUMBER,Year)
              # ]

              dat = dat[,.SD[which.min(.SD$Value)], by = .(STATION_NUMBER,Year)
                                ][,`:=`(Min_7_Day = Value, Min_7_Day_DoY = data.table::yday(Date)), by = .(STATION_NUMBER,Year)]

              shiny::incProgress(1 / 2)
            }
            if(user_var_choice %in% c('Min_30_Day','Min_30_Day_DoY')){
              # Using data.table notation,
              # 1. Calculate row numbers by station # and Year.
              # 2. Calculate 7-day rolling average of flow.
              # 3. Just take lowest 7-day flow (to find the low-flow)

              dat = dat[,thirty_day_window := data.table::frollmean(Value, 4, align = 'right', fill = NA),
                        by = .(STATION_NUMBER,Year)][,.SD[which.min(.SD$thirty_day_window)], by = .(STATION_NUMBER,Year)
              ][,`:=`(Min_30_Day = thirty_day_window, Min_30_Day_DoY = data.table::yday(Date)), by = .(STATION_NUMBER,Year)]

              shiny::incProgress(1 / 2)
            }
            if(user_var_choice %in% c('Max_7_Day','Max_7_Day_DoY')){
              # Using data.table notation,
              # 1. Calculate row numbers by station # and Year.
              # 2. Calculate 7-day rolling average of flow.
              # 3. Just take lowest 7-day flow (to find the low-flow)

              dat = dat[,.SD[which.max(.SD$Value)], by = .(STATION_NUMBER,Year)
              ][,`:=`(Max_7_Day = Value, Max_7_Day_DoY = data.table::yday(Date)), by = .(STATION_NUMBER,Year)]

              shiny::incProgress(1 / 2)
            }
            # Do we need to update the variable name to 'values'? If so, do that here:
            if(!str_detect(paste0(names(dat), collapse = ', '), 'values')){
              names(dat)[which(names(dat) == user_var_choice)] <- 'values'
            }
            dat = dat[!is.na(values),.(STATION_NUMBER, Year,Date,values)]
          }

          return(dat)

        })
      }) #%>%
      # bindCache(user_var_choice(),
      #           user_period_choice(),
      #           scale_selector_radio(),
      #           finegrain_reactives_list())
      return(dat_with_metric)
    }
  )
}
