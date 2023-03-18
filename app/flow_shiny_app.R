# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source('UI.R')
source('utils/first_run_funcs.R')
source('utils/stats_funcs.R')
source('utils/plot_funcs.R')
source('utils/leafmap_funcs.R')
source('utils/load_stations.R')
source('modules/ask_user_for_dir.R')

ui = shiny::fluidPage(
  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.5;}
         #trend_selector:hover{opacity:0.9;}'))),

  shinyFeedback::useShinyFeedback(),

  titlePanel("Flow Indicator"),

  ask_user_for_server_ui('dir'),
  # uiOutput('ask_user_for_dir'),

  map_abs_panel,

  trend_select_abs_panel
)

server <- function(input, output) {

  # Module that asks the user for their choice of directory.
  ask_user_for_dir_server('dir')

  # Set path to data; create folder 'Trendmaster3000_tmpfiles' in user-selected folder.
  tempfiles_folder = eventReactive(input$submit_filepath, {
    path = input$text_filepath
    # Make sure last character of path is a '/'
    if(str_detect(path, '.{1}$') != '/') path = paste0(path,'/')

    # Tack on name of folder for temporary files: "Trendmaster3000_tmpfiles"
    paste0(path,"Trendmaster3000_tmpfiles/")

    # Close modal.
    removeModal()
  })

  # Create reactive for path to Hydat database.
  hydat_path = reactive(paste0(tempfiles_folder(),'Hydat.sqlite3'))

  # observeEvent(input$submit_filepath, {
  #
  #   # Drop the modal dialogue box.
  #   removeModal()
  #
  #   #  Create the daily_flow_records.feather file, if not yet made.
  #   if(!file.exists(paste0(tempfiles_folder(),"daily_flow_records.feather"))){
  #
  #     print("need to make feather file!")
  #
  #     first_time_file_generator(temporary_folder = tempfiles_folder())
  #
  #     print("Finished making temporary files!")
  #   }
  # })

  # Load in data ----------------------------------------------------
  ## flow data.
  flow_dat_daily = eventReactive(input$submit_filepath, {
    req(file.exists(paste0(tempfiles_folder(),"daily_flow_records.feather")))
    feather::read_feather(paste0(tempfiles_folder(),"daily_flow_records.feather"))
  })

  ## List of stations to include.
  stations_list = reactive({
    read.csv(paste0(tempfiles_folder(),'filtered_station_list.csv')) |>
    pull(STATION_NUMBER)
  })

  # Get the stations
  stations_sf = eventReactive(input$submit_filepath, {
    #Utils function to read in stations from HYDAT, convert to sf.
    get_station_sf(stations_list(), hydat_path = hydat_path())
  })

  # source(file.path('Load_Filter_Data.R'), local = T)$value

  # First filtering cut: time periods -------------------------------
  dat_filtered = reactive({
    # req(nrow(flow_dat_daily())>1)
    switch(input$user_period_choice,
           `2010+` = flow_dat_daily() %>% filter(Year >= 2010),
           `1990+` = flow_dat_daily() %>% filter(Year >= 1990),
           `all` = flow_dat_daily()
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
    req(exists('flow_dat_daily'))

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

  source(file.path('Render_UI_elements.R'), local = T)$value

  date_vars = c("Min_7_Day_DoY","DoY_50pct_TotalQ")

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$scale_selector_radio, {
    if(input$scale_selector_radio == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Median')
      )
    } else {
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Median',
                                       'Date of 50% Flow' = 'DoY_50pct_TotalQ',
                                       'Minimum Flow (7day)' = 'Min_7_Day',
                                       'Date of Minimum Flow (7day)' = 'Min_7_Day_DoY',
                                       'Minimum Flow (30day)' = 'Min_30_Day',
                                       'Date of Minimum Flow (30day)' = 'Min_30_Day_DoY')
      )
    }

  })

  # Has the tidyhydat database been downloaded?
  output$db_version = renderText({
    paste0("Tidyhydat Version ",
           hy_version(hydat_path = paste0(tempfiles_folder(),'Hydat.sqlite3'))$Version,
           " (",as.Date(hy_version(hydat_path = paste0(tempfiles_folder(),'Hydat.sqlite3'))$Date),")")
  })

  mk_results = reactive({
    calculate_MK_results(data = dat_with_metric(),
                         chosen_variable = input$user_var_choice)
  })

  flow_dat_with_mk = reactive({
    dat_with_metric() %>%
      left_join(mk_results())
  })

  # output$test_dat = DT::renderDT({flow_dat_daily()})
  output$test_text = renderText(nrow(flow_dat_daily()))

  stations_sf_with_trend = reactive({

    req(exists('stations_sf'))

    # browser()

    dat = stations_sf() %>%
      left_join(mk_results())

    if(input$user_var_choice %in% date_vars){
      dat = dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Earlier",
                                                        'Non-Significant Trend Earlier',
                                                        'No Trend',
                                                        'Non-Significant Trend Later',
                                                        'Significant Trend Later')))
    } else {
      dat = dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Down",
                                                        'Non-Significant Trend Down',
                                                        'No Trend',
                                                        'Non-Significant Trend Up',
                                                        'Significant Trend Up')))
    }
    return(dat)
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    if(!input$tabset == 'Station Hydrograph'){
      shiny::updateTabsetPanel(
        inputId = 'tabset',
        selected = 'Station Trend Plot')
    }
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  date_choice_label = reactive({
    switch(input$scale_selector_radio,
           Annual = 'Based on data from: entire year',
           Monthly = paste0('Based on data from: ',month.name[as.numeric(input$time_selector)]),
           Seasonal = paste0('Based on data from: ',str_to_title(input$season_selector)),
           `Select Dates` = paste0('Based on data from: ',month.abb[input$start_month],'-',input$start_day,' to ',month.abb[input$end_month],'-',input$end_day)
    )
  })

  output$myplot = renderPlot({
    station_flow_plot(data = dat_with_metric(),
                      variable_choice = input$user_var_choice,
                      clicked_station = click_station(),
                      stations_shapefile = stations_sf(),
                      slopes = senslope_dat(),
                      caption_label = date_choice_label())
  })

  output$my_hydrograph = renderPlot({
    hydrograph_plot(dat = flow_dat_daily,
                    clicked_station = click_station(),
                    stations_shapefile = stations_sf())
  })

  output$test = DT::renderDT(dat_with_metric())
  output$test_text = renderText(input$filter_interval)

  mypal = reactive({
    if(input$user_var_choice %in% date_vars){
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Earlier",
                             'Non-Significant Trend Earlier',
                             'No Trend',
                             'Non-Significant Trend Later',
                             'Significant Trend Later'),
                  ordered = T)
    } else {
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Down",
                             'Non-Significant Trend Down',
                             'No Trend',
                             'Non-Significant Trend Up',
                             'Significant Trend Up'),
                  ordered = T)
    }
  })

  output$leafmap <- renderLeaflet({
    make_base_bc_leafmap()
  })

  observe({
    update_leaflet(map = 'leafmap',
                   stations = stations_sf_with_trend(),
                   clicked_station = click_station(),
                   pal = mypal())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
