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



# Here we load in other R scripts that we use in the shiny UI or server code.
# We split code into multiple scripts like this to keep each R script
# (hopefully) bite-sized and possible to read / debug.
source('UI.R')
source('utils/stats_funcs.R')
source('utils/plot_funcs.R')
source('utils/leafmap_funcs.R')
source('utils/load_stations.R')
source('utils/make_poly_from_drawing.R')
source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')
source('modules/calc_mk_results.R')
source('modules/access_wsurvey_canada_data.R')


server <- function(input, output) {

  # Load in data ----------------------------------------------------

  # Probably unnecessary once the app is published...
  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  # ## Raw flow data.
  all_flow_records_for_hydrograph = qs::qread("weekly_flow_records.qs")
  # flow_dat_all = qs::qread('daily_flow_records.qs')

  # flow_dat_daily = qs::qread("small_dat.qs")
  #
  # # set data as a data.table object.
  # setDT(flow_dat_daily)

  ## BC spatial file.
  bc_boundary = read_sf('bc_bound.gpkg') %>%
    st_transform(crs = 4326) %>%
    mutate(shape_name = 'unselected area')

  ## Stations spatial file.
  stations_sf = read_sf('stations.gpkg')

  # Set up reactives -------------------------------------------------

  # Set up reactive values that store the clicked station and map shape
  click_station <- reactiveVal('no_selection')
  click_shape <- reactiveVal('no_selection')

  # Get user variable choice, make sure it's reactive.
  user_var = reactive(input$user_var_choice)
  user_param = reactive(input$user_param_choice)

  # Stations subsetted by a selected shape on the map.
  stations_to_include = reactive({
    stations = stations_sf
    if(!click_shape() %in% c('no_selection','drawn_poly')){
      stations = stations |> filter(!!sym(input$user_shape_choice) == click_shape())
    }
    if(click_shape() %in% c('drawn_poly') | shape_for_map()$shape_name[1] == 'drawn_poly'){
      stations = stations |>
        st_join(drawn_poly(), st_intersects) |>
        filter(!is.na(shape_name))
    }
    if(!input$include_low_qual_data){
      stations = stations |>
        dplyr::filter(meets_dat_qual_check)
    }
    return(stations)
  })

  # flow_dat_daily_starting_filter = reactive({
  #   flow_dat_daily[STATION_NUMBER %in% stations_to_include()$STATION_NUMBER]
  # })

  # Modules ----------------------------------------------------------

  # Use the data filtering module on the raw data.
  # The result is a list with 4 elements: the filtered data,
  # the user period choice (e.g. all data, or just 2010 -> recent),
  # the scale selector (e.g. annual, or monthly, etc.),
  # and the finegrained reactive filter.
  filtering_mod_output <- filter_data_Mod_Server("data",
                                                 reactive(input$include_low_qual_data),
                                                 stations_to_include,
                                                 number_station_cutoff = 50)

  # Use the metric-adding module on the filtered data.
  # Whichever metric the user has chosen will be calculated
  # across the time scale selected and joined to the data.
  dat_with_metric <- add_metric_to_dat_mod("data2",
                                           data = filtering_mod_output$dat_filtered,
                                           user_var_choice = reactive(input$user_var_choice),
                                           user_period_choice = filtering_mod_output$user_period_choice,
                                           scale_selector_radio = filtering_mod_output$scale_selector_radio,
                                           finegrain_reactives_list = filtering_mod_output$finegrain_reactives_list)

  # min_max_years = reactive({
  #   dat_with_metric()[, c("min_year","max_year") := list(min(Year),max(Year)), by = STATION_NUMBER][,c('STATION_NUMBER','min_year','max_year')]
  # })

  # observe({
  #   browser()
  #   mk_results = merge.data.table(mk_results(),
  #                    min_max_years()
  #   )
  # })
  # Run the Mann-Kendall trend analysis on the data with metric.
  mk_results <- calculate_mk_mod('mk_res',
                                 data = dat_with_metric,
                                 user_var_choice = user_var)

  # # Use the detailed data download to snag daily flow records from
  # # the Water Survey of Canada website.
  # download_results = access_wsurvey_canada_Server('pull_data',
  #                                                 station_number_list = c('07EA004','07EA006'))


  # Attach the MK results to the data with metric.
  flow_dat_with_mk = reactive({
    # dat_with_metric() %>%
    # left_join(mk_results())
    merge.data.table(dat_with_metric(),
                     mk_results())
  })

  # If the user draws a poly, create a reactive polygon spatial object from it.
  drawn_poly = eventReactive(input$leafmap_draw_new_feature, {
    return(
      make_poly_from_drawing(input$leafmap_draw_new_feature$geometry$coordinates) |>
        rename('geom' = geometry) |>
        mutate(average_trend_result = 0,
               stations_for_mean = 0,
               label = "")
    )
  })

  # And once a polygon is drawn, switch the shape dropdown to 'drawn_shape'
  observeEvent(input$leafmap_draw_new_feature, {
    updateSelectInput(inputId = 'user_shape_choice',
                      selected = 'drawn_shape')
    click_shape('drawn_poly')
    # print(paste0('click_shape is:', click_shape()))
  })

  # Specify which variables are 'Date' variables (for plot labelling)
  date_vars = c("Min_7_Day_DoY","Min_30_Day_DoY",
                "Max_7_Day_DoY","DoY_50pct_TotalQ")

  # Colours for leaflet map.
  colours = RColorBrewer::brewer.pal(5, 'RdBu')

  # Add the flow data with MK trend results to the
  # spatial object of all the stations.
  stations_sf_with_trend = reactive({

    stations.dt = as.data.table(stations_to_include())

    dat = st_as_sf(
      # data.table::merge.data.table(
        data.table::merge.data.table(
          stations.dt, mk_results()
        # ),
        # min_max_years()
      )
    )

    if(input$user_param_choice == 'slope'){

      # STILL UNSURE ABOUT SLOPE CALCULATION BELOW! *
      dat = dat |>
        # Calculate the percent increase/decrease per year.
        # mutate(max_year_value = (max_year-min_year) * Slope) |>
        # mutate(slope_perc = ((max_year_value / Intercept) - 1)/(max_year-min_year)) |>
        # Option 1: equal number of stations in each bin, 5 bins.
        # dplyr::mutate(slope_bin = as.numeric(santoku::chop_equally(Slope, 5))) |>
        # dplyr::mutate(slope_bin = factor(slope_bin, levels = c(1:5))) |>
        # Option 2: manual thresholds.
        # dplyr::mutate(slope_perc = 100*Slope / Intercept) |>
        # dplyr::mutate(slope_bin = case_when(
        #   slope_perc <= -1 ~ 'x <= -1',
        #   slope_perc > -1 & Slope <= -0.25 ~ '-1 < x <= -0.25',
        #   slope_perc > -0.25 & Slope <= 0.25 ~ '-0.25 < x <= 0.25',
        #   slope_perc > 0.25 & Slope <= 1 ~ '0.25 < x <= 1',
        #   slope_perc > 1 ~ '1 < x',
        # )) |>
        # dplyr::mutate(slope_bin = factor(slope_bin,
        #                                  levels = c('x <= -1',
        #                                             '-1 < x <= -0.25',
        #                                             '-0.25 < x <= 0.25',
        #                                             '0.25 < x <= 1',
        #                                             '1 < x'))) |>
        dplyr::mutate(slope_bin = case_when(
          Slope <= -1 ~ 'x <= -1',
          Slope > -1 & Slope <= -0.25 ~ '-1 < x <= -0.25',
          Slope > -0.25 & Slope <= 0.25 ~ '-0.25 < x <= 0.25',
          Slope > 0.25 & Slope <= 1 ~ '0.25 < x <= 1',
          Slope > 1 ~ '1 < x',
        )) |>
        dplyr::mutate(slope_bin = factor(slope_bin,
                                         levels = c('x <= -1',
                                                    '-1 < x <= -0.25',
                                                    '-0.25 < x <= 0.25',
                                                    '0.25 < x <= 1',
                                                    '1 < x'))) |>
        dplyr::mutate(my_colour = colours[slope_bin])
    }
    if(input$user_param_choice == 'p_value'){
      if(user_var() %in% date_vars){
        dat = dat %>%
          mutate(
            trend_sig = factor(
              trend_sig,
              levels = c("Significant Trend Earlier",
                         'Non-Significant Trend Earlier',
                         'No Trend',
                         'Non-Significant Trend Later',
                         'Significant Trend Later'),
            ),
            my_colour = colours[as.numeric(trend_sig)]
          )
      } else {
        dat = dat %>%
          mutate(
            trend_sig = factor(
              trend_sig,
              levels = c("Significant Trend Down",
                         'Non-Significant Trend Down',
                         'No Trend',
                         'Non-Significant Trend Up',
                         'Significant Trend Up')),
            my_colour = colours[as.numeric(trend_sig)])
      }
    }
    return(dat)
  })

  ## Shapes for leaflet map station filtering.
  shape_for_map = reactive({
    switch(input$user_shape_choice,
           none = st_set_geometry(
             tibble(shape_name = 'Nothing',
                    average_trend_result = 0,
                    average_slope = 0,
                    stations_for_mean = 0,
                    my_colour = 'transparent',
                    label = ""),
             tibble(lon = c(1,0), lat = c(0, 1)) %>%
               st_as_sf(coords = c('lon','lat'), crs = 4326) %>%
               st_bbox() %>%
               st_as_sfc()
           ),
           ecoprov = read_sf('ecoprovinces.gpkg') %>% st_transform(crs = 4326),
           ecoreg = read_sf('ecoregions.gpkg') %>% st_transform(crs = 4326),
           ecosec = read_sf('ecosections.gpkg') %>% st_transform(crs = 4326),
           nr_dist = read_sf('nr_districts.gpkg') %>% st_transform(crs = 4326),
           nr_reg = read_sf('nr_regions.gpkg') %>% st_transform(crs = 4326),
           subw = read_sf('subw.gpkg') %>% st_transform(crs = 4326),
           drawn_shape = drawn_poly())
  })

  # If the user clicks on a shape on the map,
  # just keep the stations within that shape.
  stations_sf_with_trend_in_shape = reactive({
    stations_sf_with_trend()
  })

  # Reactive of just the clicked (selected) stations, with MK results.
  selected_stations = reactive({
    stations_sf_with_trend() |>
      filter(STATION_NUMBER %in% click_station()) |>
      dplyr::filter(!duplicated(STATION_NUMBER))
  })

  output$selected_station_DT = DT::renderDT({
    DT::datatable(stations_sf_with_trend() |>
                    dplyr::filter(STATION_NUMBER %in% click_station()) |>
                    st_drop_geometry() |>
                    dplyr::select(STATION_NUMBER,STATION_NAME,
                                  Status = HYD_STATUS, #meets_criteria,
                                  Trend = trend_sig),
                  extensions = 'Buttons',
                  options = list(
                    pageLength = 5,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'ftsp',
                    buttons = c('copy', 'csv', 'excel')
                  ))
  })

  # Render summary values for sidebar.
  output$num_stations_on_plot = renderText({
    nrow(stations_sf_with_trend() %>% filter(!is.na(trend_sig)))
  })

  output$num_stations_dec = renderText({
    # browser()
    if(input$user_param_choice == 'p_value'){
      output = nrow(stations_sf_with_trend() |>
             dplyr::filter(P_value <= 0.05) |>
             filter(trend_sig %in% c('Significant Trend Earlier',
                                     'Significant Trend Down')))
    }
    if(input$user_param_choice == 'slope'){
      output = nrow(stations_sf_with_trend() |>
             dplyr::filter(P_value <= 0.05) |>
             filter(slope_bin == 'x <= -1'))
    }
  as.character(output)
  })

  output$num_stations_inc = renderText({
    # nrow(stations_sf_with_trend() |>
    #        filter(trend_sig %in% c('Significant Trend Later',
    #                                'Significant Trend Up')))
    if(input$user_param_choice == 'p_value'){
      output = nrow(stations_sf_with_trend() |>
             dplyr::filter(P_value <= 0.05) |>
             filter(trend_sig %in% c('Significant Trend Later',
                                     'Significant Trend Up')))
    }
    if(input$user_param_choice == 'slope'){
      output = nrow(stations_sf_with_trend() |>
             dplyr::filter(P_value <= 0.05) |>
             filter(slope_bin == '1 < x'))
    }
    as.character(output)
  })

  output$data_recency = renderText({
    paste0("Most recent WSC data included: ",max(dat_with_metric()$Year))
  })
  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    sel_stations_dt = as.data.table(selected_stations() |>
                                      dplyr::filter(!duplicated(STATION_NUMBER)))
    # mk_results_dt = as.data.table(mk_results())

    # flow_dat_with_mk() %>%
    #   filter(STATION_NUMBER %in% click_station()) %>%
    #   left_join(stations_sf %>%
    #               st_drop_geometry() %>%
    #               dplyr::select(STATION_NUMBER,STATION_NAME),
    #             by = 'STATION_NUMBER') %>%
    # #   left_join(mk_results()) %>%
    #   mutate(SlopePreds = Intercept+Slope*Year)
    # data.table::merge.data.table(merge.data.table(dat_with_metric(),sel_stations_dt)[STATION_NUMBER %chin% STATION_NUMBER],mk_results_dt)[,SlopePreds := Intercept+Slope*Year]
    merge.data.table(dat_with_metric(),sel_stations_dt)[STATION_NUMBER %chin% STATION_NUMBER][,SlopePreds := Intercept+Slope*Year]
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection of marker.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    # print(click_station())
    # Note - are we in 'multiple station select mode', or
    # 'single station select mode'? If multiple,
    # keep station IDs as we click them.
    if(input$multi_station == T){
      # Did the user click a station that's already selected? If so, drop that station.
      if(input$leafmap_marker_click$id %in% click_station()){
        click_station(click_station()[click_station() != input$leafmap_marker_click$id])
      } else {
        new_click_station_value = c(click_station(), input$leafmap_marker_click$id)
        click_station(unique(new_click_station_value[new_click_station_value != 'no_selection']))
      }
    } else {
      click_station(input$leafmap_marker_click$id)
    }
    #  # Update the tab that is selected, unless we're already
    #  # looking at either the flow metric plot, the hydrograph plot, or
    #  # tabular data.
    # if(!input$tabset %in% c("Flow Metric Plot","Hydrograph"))
    #  nav_select('tabset',
    #             selected = 'Flow Metric Plot')
  })

  # 1. AND/OR Update selection of shape.
  observeEvent(input$leafmap_shape_click, {

    # Capture the info of the clicked polygon.
    # If different from current selection, update selection.
    if(click_shape() != input$leafmap_shape_click$id){
      click_shape(input$leafmap_shape_click$id)
    }
    # Reset station selection (good idea?)
    click_station('no_selection')
  })

  # Also, when the user changes the region shape, reset the clicked shape to 'no_selection'
  observeEvent(input$user_shape_choice, {
    click_station('no_selection')
    click_shape('no_selection')
  })

  # React to 'Undo Station Selection' Button
  observeEvent(input$reset_station_sel, {
    click_station('no_selection')
  })

  # React to 'Undo Shape Selection' Button
  observeEvent(input$reset_shape_sel,{
    click_shape('no_selection')
  })

  # React to 'Select All' Button
  observeEvent(input$select_all_stats_in_shape, {

    updateSwitchInput(inputId = 'multi_station', value = TRUE)

    click_station(c(stations_sf_with_trend_in_shape()$STATION_NUMBER))

  })

  # output$selected_station = renderText({paste0("Station: ",click_station())})

  # Generate label depending on which time frame the data is based.
  date_choice_label = reactive({
    switch(filtering_mod_output$scale_selector_radio(),
           Annual = 'Based on data from: entire year',
           Monthly = paste0('Based on data from: ',month.name[as.numeric(input$time_selector)]),
           Seasonal = paste0('Based on data from: ',str_to_title(input$season_selector)),
           `Select Dates` = paste0('Based on data from: ',month.abb[input$start_month],'-',input$start_day,' to ',month.abb[input$end_month],'-',input$end_day)
    )
  })

  output$myplot = renderPlotly({
    # p = station_flow_plot(data = dat_with_metric(),
    #                       variable_choice = user_var(),
    #                       clicked_station = click_station(),
    #                       stations_shapefile = stations_to_include(),
    #                       slopes = senslope_dat(),
    #                       caption_label = date_choice_label())
    # ggplotly(p)
    p = station_flow_plot(data = senslope_dat(),
                          variable_choice = user_var(),
                          parameter_chosen = user_param(),
                          clicked_station = click_station(),
                          # stations_shapefile = stations_to_include(),
                          stations_shapefile = stations_sf_with_trend_in_shape(),
                          slopes = senslope_dat(),
                          caption_label = date_choice_label())
    ggplotly(p, tooltip = c('label'))
  })

  output$my_hydrograph = renderPlotly({
    # Trigger this renderPlotly again.
    # unique(click_stations_ddat$web_download()$STATION_NUMBER)

    # data_for_hydrograph = flow_dat_all |>
    #   dplyr::filter(STATION_NUMBER %in% click_station()) |>
    #   dplyr::mutate(Year = lubridate::year(Date),
    #                 Week = floor(yday(Date)/7),
    #                 Month = lubridate::month(Date),
    #                 Day = lubridate::day(Date),
    #                 PARAM = 1) |>
    #   dplyr::rename(record_date = Date)

    # browser()
    if(is.null(click_stations_ddat$web_download())){
      data_for_hydrograph = all_flow_records_for_hydrograph |> filter(STATION_NUMBER %chin% click_station())
    } else {
      # We have at least SOME data. Include daily data for any and all
      # stations for which we've performed a download.
      data_for_hydrograph = purrr::map(click_station(), ~ {
        if(.x %in% unique(click_stations_ddat$web_download()$STATION_NUMBER)){
          # print('a station is using downloaded daily data')

          click_stations_ddat$web_download() |>
            dplyr::filter(STATION_NUMBER == .x) |>
            dplyr::select(-SYM) |>
            dplyr::mutate(Year = lubridate::year(record_date),
                          Week = floor(yday(record_date)/7),
                          Month = lubridate::month(record_date),
                          Day = lubridate::day(record_date))
        } else {
          # print('a station is using monthly summary data')
          all_flow_records_for_hydrograph |>
            filter(STATION_NUMBER %chin% .x) |>
            dplyr::rename(record_date = Date) |>
            dplyr::mutate(PARAM = 1,
                          Day = 1)
        }
      }) |>
        dplyr::bind_rows()

    # if(!click_station() %in% unique(click_stations_ddat$web_download()$STATION_NUMBER)){
    #   data_for_hydrograph = all_flow_records_for_hydrograph |> filter(STATION_NUMBER %chin% click_station())
    # } else {
    #   data_for_hydrograph = click_stations_ddat$web_download() |>
    #     dplyr::select(-SYM) |>
    #     dplyr::mutate(Year = lubridate::year(record_date),
    #                   Week = floor(yday(record_date)/7),
    #                   Month = lubridate::month(record_date),
    #                   Day = lubridate::day(record_date))
    # }
    }

    h = hydrograph_plot(
      dat = data_for_hydrograph,
      clicked_station = click_station(),
      stations_shapefile = stations_sf)

    ggplotly(h)
  })

  # GET DAILY DATA FROM WSC #
  # 1. Set up reactive flag for whether or not data's been downloaded.
  # ddat_downloaded_for_selection = reactiveVal('No')

  output$current_selected_stations = renderUI({

    output_text = paste0(click_station(), collapse = ', ')

    if(str_count(output_text,', ') >= 3){
      output_text = as.character(length(click_station()))
    }

    if('no_selection' %in% click_station()) {
      output_text = 'None'
    }
    bslib::card(
      p(paste0("Currently Selected Stations: "),
        style = 'margin-bottom:0px;text-align:center;display:inline;'),
      p(paste0(output_text),
        style = 'margin-bottom:0px;text-align:center;display:inline;color:purple;')
    )
  })

  output$current_downloaded_data_UI = renderUI({

    # Trigger this renderUI again.
    # unique(click_stations_ddat$web_download()$STATION_NUMBER)

    if(!is.null(click_stations_ddat$web_download())) {
      text_to_add = unique(click_stations_ddat$web_download()$STATION_NUMBER)
      # text_to_add = paste0(click_station(), collapse = ', ')
    } else {
      text_to_add = "None"
    }
    tagList(
      h5(HTML("Data from WSC Loaded:<br>")),
      p(paste0(text_to_add, collapse = ', '))
    )
  })

  click_stations_ddat = access_wsurvey_canada_Server('pull_data',
                                                     click_station)

  # TEST #
  # click_stations_ddat = reactive(access_wsurvey_canada_Server('pull_data',click_station))

  # RAW DAILY DATA DOWNLOAD #
  output$download_flow_data_ui = renderUI({
    # if(is.null(click_stations_ddat$web_download()) == 'No'){
    #   div(
    #     actionButton("placeholder_download_flow_data",paste0("Download Daily Flow Data for ",length(unique(click_stations_ddat$web_download()$STATION_NUMBER))," loaded station",ifelse(length(unique(click_stations_ddat$web_download()$STATION_NUMBER)) >= 2, '(s)',''))),
    #     style = 'color: grey; opacity: 0.1;'
    #   )
    # } else {
    #   downloadButton("download_flow_data",paste0("Download Daily Flow Data for ",length(unique(click_stations_ddat$web_download()$STATION_NUMBER))," loaded station",ifelse(length(unique(click_stations_ddat$web_download()$STATION_NUMBER)) >= 2, '(s)','')))
    # }
    if(!is.null(click_stations_ddat$web_download())){
      downloadButton("download_flow_data",
                     h4("Download Daily Data"),
                     style = 'background-color:#2c3e50;height:12vh;width:100%;')
    } else {
      actionButton("download_flow_data_spaceholder",
                     h4("Download Daily Data"),
                     style = 'background-color:grey;height:12vh;width:100%;') |>
        bslib::tooltip('Download daily data from WSC first; button is on the sidebar!')
    }

  })

  output$download_flow_data <- downloadHandler(
    filename = function() {
      paste0("HYDAT_flow_data_", Sys.Date(), ".csv")
    },
    content = function(file) {

      write.csv(
        # flow_dat_all |>
        #   dplyr::filter(STATION_NUMBER %in% click_station()),
        click_stations_ddat$web_download(),
        file,
        row.names = F
      )
    })
  # write.csv(filtering_mod_output$dat_filtered() %>%
  #             filter(STATION_NUMBER %in% click_station()), file, row.names = F)

  # # TREND ANALYSIS DOWNLOAD #
  # output$download_data_with_results_ui = renderUI({
  #   downloadButton("download_data_with_results",
  #                  # h4(paste0("Download Trend Analysis Results (",
  #                  #           length(click_station()[click_station() != 'no_selection']),
  #                  #           " station",ifelse(length(click_station()[click_station() != 'no_selection']) >= 2,
  #                  #                             '(s)','')," selected)")))
  #                  h4("Download Trend Analysis Results"),
  #                  style = 'background-color:#2c3e50;height:10vh;'
  #   )
  # })

  output$download_data_with_results <- downloadHandler(
    filename = function() {
      paste0("HYDAT_data_with_MK_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(senslope_dat(), file, row.names = F)
    }
  )

  mypal = reactive({
    make_my_pal(user_selected_metric = user_var(),
                user_selected_param = user_param(),
                date_vars = date_vars,
                mk_results = mk_results())
  })

  # my_pal_shape = eventReactive(input$user_shape_choice, {
  #
  #   if(input$user_shape_choice == 'none') return(colorFactor(palette = '#808080', domain = 1))
  #
  #   if(user_param() == 'slope'){
  #     output = colorNumeric(palette = 'RdBu',
  #                          domain = shape_for_map()$average_slope)
  #   }
  #   if(user_param() == 'p_value'){
  #     output = colorNumeric(palette = 'RdBu',
  #                           domain = shape_for_map()$average_trend_result)
  #   }
  #   output
  # })

  output$leafmap <- renderLeaflet({
    make_base_bc_leafmap()
  })

  observe({
    update_leaflet(map = 'leafmap',
                   stations = stations_sf_with_trend_in_shape(),
                   clicked_station = click_station(),
                   shape_type = reactive(input$user_shape_choice),
                   shapes = shape_for_map(),
                   clicked_shape = click_shape(),
                   # shape_pal = my_pal_shape(),
                   chosen_parameter = user_param(),
                   chosen_var = user_var(),
                   date_vars = date_vars)
  })

  runjs('
bottombar_button_state = "showing";

bottombar_button = document.getElementById("toggle_bottombar");
bottombar = document.getElementById("trend_selector");
bottombar.classList.add("bottombar");

//bottombar_button.style.left = "-10px";

bottombar_button.addEventListener("click",
function() {
    if(bottombar_button_state == "showing"){
          bottombar.style.transform = "translateY(50%) scale(1, 0)";
          bottombar_button.style.transform = "rotate(180deg)";
          bottombar_button_state = "hidden";
    } else {
          bottombar.style.transform = "translateY(0%) scale(1, 1)";
          bottombar_button.style.transform = "rotate(0deg)";
          bottombar_button_state = "showing";
    }
});
')
}

# Run the application
shinyApp(ui = ui, server = server)
# shiny::runApp(myapp, port = 6104)
