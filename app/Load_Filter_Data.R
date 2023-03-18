# Workflow of filtering data as follows:

# 1. Read in data => 'flow_dat_daily'
# 2. Filter years to include: recent (last 10 years), medium-term (1990), or all data => 'dat_filtered'
# 2. Filter portions of each year to include => 'dat_filteredTwo'
# 3. Filter: variable of choice => 'dat_with_metric'


# First filtering cut: time periods -------------------------------
dat_filtered = reactive({
  req(exists('flow_dat_daily'))
  switch(input$user_period_choice,
         `2010+` = flow_dat_daily %>% filter(Year >= 2010),
         `1990+` = flow_dat_daily %>% filter(Year >= 1990),
         `all` = flow_dat_daily
  )
})

dat_filteredTwo = reactive({

  withProgress(message = 'applying date filter', {

    # req() tells the app to not start solving this reactive expression until
    # we have a value for the scale selection radio buttons! This delay
    # avoids errors while these buttons are loading.
    req(input$scale_selector_radio)
    if(input$scale_selector_radio == 'Select Dates'){
      req(input$start_month, input$start_day, input$end_month, input$end_day)
    }
    #In the case of annual timescale, do no filtering here.
    dat = dat_filtered()

    #Update progress bar...
    incProgress(1 / 2)

    #In the case of monthly timescale, filter down to month of interest.
    if(input$scale_selector_radio == 'Monthly'){
      req(input$time_selector)
      dat = dat %>%
        filter(Month == input$time_selector)
      #Update progress bar...
      incProgress(1 / 2)
    }

    if(input$scale_selector_radio == 'Seasonal'){
      req(input$season_selector)
      dat = dat %>%
        mutate(season = case_when(
          Month %in% c(12,1,2) ~ 'winter',
          Month %in% c(3:5) ~ 'spring',
          Month %in% c(6:8) ~ 'summer',
          Month %in% c(9:11) ~ 'autumn'
        )) %>%
        filter(season == input$season_selector)
      #Update progress bar...
      incProgress(1 / 2)
    }

    #If custom time scale, use it here to filter data.
    if(input$scale_selector_radio == 'Select Dates'){
      # Only start calculating this reactive once we have all 4 inputs.
      req(input$start_month, input$start_day, input$end_month, input$end_day)

      # Use {lubridate} to calculate the start and end periods. We use these to filter the data.
      start_period = (months(as.numeric(input$start_month)) + days(input$start_day))
      end_period = (months(as.numeric(input$end_month)) + days(input$end_day))

      # Perform check that end period is later than start period
      date_check = start_period < end_period
      # If it's not, give a warning.
      shinyFeedback::feedbackWarning("end_month", !date_check, "End date must be later than start date")
      # Date check must be TRUE to proceed.
      req(date_check)

      # Filter data.
      dat = dat %>%
        mutate(Year = year(Date),
               Month = month(Date),
               Day = day(Date),
               this_period = c(months(Month) + days(Day))) %>%
        filter(this_period >= start_period,
               this_period <= end_period) %>%
        dplyr::select(-this_period,-Day)
      #Update progress bar...
      incProgress(1 / 2)
    }
    return(dat)
  })
})

# Now that data has been filtered, calculate metric.
dat_with_metric = reactive({

  withProgress(message = 'calculating metric', {

    dat = dat_filteredTwo()
    #Update progress bar...
    incProgress(1 / 2)

    if(input$user_var_choice == 'Mean'){
      dat = dat %>%
        group_by(STATION_NUMBER,Year) %>%
        summarise(Mean = mean(Value,na.rm=T))
      #Update progress bar...
      incProgress(1 / 2)
    }
    if(input$user_var_choice == 'Median'){
      dat = dat %>%
        group_by(STATION_NUMBER,Year) %>%
        summarise(Median = median(Value,na.rm=T))
      #Update progress bar...
      incProgress(1 / 2)
    }
    if(input$user_var_choice == 'DoY_50pct_TotalQ'){
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
    if(input$user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
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
    if(input$user_var_choice %in% c('Min_30_Day','Min_30_Day_DoY')){
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
    if(input$user_var_choice == 'Total_Volume_m3'){
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
      dplyr::select(everything(), values = !!sym(input$user_var_choice))
    return(dat)
  })
})

