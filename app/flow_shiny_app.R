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
source('utils/stats_funcs.R')
source('utils/plot_funcs.R')
source('utils/leafmap_funcs.R')
source('utils/load_stations.R')
source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')
source('modules/calc_mk_results.R')

my_theme = bs_theme(bootswatch = 'flatly',
                    # danger = "#cc0000",
                    # primary = '#3399ff',
                    # "sidebar-bg" = '#ADD8E7',
                    font_scale = 0.75) %>%
  # For below, we had '#trend_selector:hover{opacity:0.95;}'
  bs_add_rules("#trend_selector {opacity:0.5;}
                #trend_selector:hover{opacity:1;}
                #reset_shape_sel{background-color:#2c3e50;}
                #select_all_stats_in_shape{background-color:#2c3e50;}")

ui = page_fillable(

  theme = my_theme,

  shinyFeedback::useShinyFeedback(),

  # ask_user_for_dir_ui('dir'),

  layout_sidebar(
    sidebar = the_sidebar,
    main_bit,
    textOutput('stations_selected')
  )
)



server <- function(input, output) {

  # Module that asks the user for their choice of directory.
  # tempfiles_folder = ask_user_for_dir_server('dir')
  tempfiles_folder = reactiveVal('./')

  # Probably unnecessary once the app is published...
  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  # # Once user has selected a filepath, remove modal.
  # observeEvent(input$submit_filepath, removeModal())

  # # Create reactive for path to Hydat database.
  # hydat_path = reactive(paste0(tempfiles_folder(),'Hydat.sqlite3'))

  # Get user variable choice
  user_var = reactive(input$user_var_choice)

  # Load in data ----------------------------------------------------

  ## raw flow data.
  flow_dat_daily = feather::read_feather("daily_flow_records.feather")

  bc_boundary = read_sf('bc_bound.gpkg') %>%
    st_transform(crs = 4326) %>%
    mutate(shape_name = 'unselected area')

  # output$number_rows_raw_dat = renderText(nrow(filtering_mod_output$dat_filtered()))

  # ## List of stations to include.
  # stations_list = reactive({
  #   read.csv('filtered_station_list.csv') |>
  #     pull(STATION_NUMBER)
  # })

  # # Get the stations
  # stations_sf = reactive({
  #
  #   stations = read_sf('stations.gpkg')
  #
  #   #If the user has selected a shape on the leaflet map, filter stations.
  #   if(click_shape() != 'no_selection'){
  #     stations = stations %>%
  #       st_join(shape_for_map() %>% filter(shape_name == click_shape()), st_intersects) %>%
  #       filter(!is.na(shape_name)) %>%
  #       dplyr::select(-shape_name)
  #   }
  #
  #   stations
  # })
  stations_sf = read_sf('stations.gpkg') %>%
    mutate(internal_id = c(1:nrow(.)))

  # Use the data filtering module on the raw data.
  filtering_mod_output <- filter_data_Mod_Server("data",
                                                 flow_dat_daily,
                                                 shape = click_shape)

  # Use the metric-adding module on the filtered data.
  dat_with_metric <- add_metric_to_dat_mod("data2",
                                           data = filtering_mod_output$dat_filtered,
                                           user_var_choice = reactive(input$user_var_choice),
                                           user_period_choice = filtering_mod_output$user_period_choice,
                                           scale_selector_radio = filtering_mod_output$scale_selector_radio,
                                           finegrain_reactives_list = filtering_mod_output$finegrain_reactives_list)

  # Run the Mann-Kendall trend analysis on the data with metric.
  mk_results <- calculate_mk_mod('mk_res',
                                 data = dat_with_metric,
                                 user_var_choice = user_var)

  # Attach the MK results to the data with metric.
  flow_dat_with_mk = reactive({
    dat_with_metric() %>%
      left_join(mk_results())
  })

  # If the user draws a poly, draw it here.
  drawn_poly = eventReactive(input$leafmap_draw_new_feature, {

    drawn_poly = as.data.frame(matrix(unlist(input$leafmap_draw_new_feature$geometry$coordinates), ncol = 2, byrow = T))

    names(drawn_poly) = c("lon","lat")

    drawn_poly = st_as_sf(drawn_poly, coords = c("lon","lat"), crs = 4326)

    drawn_poly = drawn_poly %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON") %>%
      mutate(shape_name = 'drawn_poly')

    # And update the drop-down shape selector to 'drawn poly'
    # as well as updating the selected polygon to our drawn poly.
    updateSelectInput(inputId = 'user_shape_choice',
                      selected = 'drawn_shape')
    click_shape('drawn_poly')
    # browser()
    return(
      sf::st_difference(bc_boundary, drawn_poly) %>%
        bind_rows(
        drawn_poly %>% dplyr::rename('geom' = geometry)
      )
    )


  })

  # Specify which variables are 'Date' variables (for plot labelling)
  date_vars = c("Min_7_Day_DoY","DoY_50pct_TotalQ")

  # Add the flow data with MK trend results to the
  # spatial object of all the stations.
  stations_sf_with_trend = reactive({

    req(exists('stations_sf'),flow_dat_daily)

    dat = stations_sf %>%
      left_join(mk_results())

    # Is the chosen variable one of the 'date' variables?
    # If so, update the MK trend labels to say 'earlier' or 'later'
    # rather than 'up' or 'down'.
    if(user_var() %in% date_vars){
      dat = dat %>%
        mutate(trend_sig = factor(
          trend_sig,
          levels = c("Significant Trend Earlier",
                     'Non-Significant Trend Earlier',
                     'No Trend',
                     'Non-Significant Trend Later',
                     'Significant Trend Later')))
    } else {
      dat = dat %>%
        mutate(trend_sig = factor(
          trend_sig,
          levels = c("Significant Trend Down",
                     'Non-Significant Trend Down',
                     'No Trend',
                     'Non-Significant Trend Up',
                     'Significant Trend Up')))
    }
    return(dat)
  })

  # If the user clicks on a shape on the map,
  # just keep the stations in within that shape.
  stations_sf_with_trend_in_shape = reactive({

    stations_sf = stations_sf_with_trend()

    #If the user has selected a shape on the leaflet map, filter stations.
    if(click_shape() != 'no_selection'){
      stations_sf = stations_sf %>%
        st_join(shape_for_map() %>% filter(shape_name == click_shape()), st_intersects) %>%
        filter(!is.na(shape_name)) %>%
        dplyr::select(-shape_name)
    }

    #Otherwise, all good!
    stations_sf
  })

  ## Shapes for leaflet map station filtering.
  shape_for_map = reactive({
    switch(input$user_shape_choice,
           none = st_set_geometry(
             tibble(shape_name = 'Nothing',
                    average_trend_result = 0,
                    stations_for_mean = 0),
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

  # Render summary values for sidebar.
  output$num_stations_on_plot = renderText({nrow(stations_sf_with_trend_in_shape() %>% filter(!is.na(trend_sig)))})

  output$num_stations_dec = renderText({nrow(stations_sf_with_trend_in_shape() |>
                                               filter(trend_sig %in% c('Significant Trend Earlier',
                                                                       'Significant Trend Down')))})

  output$num_stations_inc = renderText({nrow(stations_sf_with_trend_in_shape() |>
                                               filter(trend_sig %in% c('Significant Trend Later',
                                                                       'Significant Trend Up')))})
  # Set up reactive values that store the clicked station and map shape
  click_station <- reactiveVal('no_selection')
  click_shape <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    #
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER %in% click_station()) %>%
      left_join(stations_sf %>%
                  st_drop_geometry() %>%
                  dplyr::select(STATION_NUMBER,STATION_NAME),
                by = 'STATION_NUMBER') %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection of marker.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.

    # Note - are we in 'multiple station select mode', or
    # 'single station select mode'? If multiple,
    # keep station IDs as we click them.
    if(input$multi_station == T){
      new_click_station_value = c(click_station(), input$leafmap_marker_click$id)
      click_station(new_click_station_value[new_click_station_value != 'no_selection'])
    } else {
      click_station(input$leafmap_marker_click$id)
    }
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Station Plot')
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
    click_shape('no_selection')
  })

  # React to 'Undo Selection' Button
  observeEvent(input$reset_shape_sel,{
    click_shape('no_selection')
  })

  # React to 'Select All' Button
  observeEvent(input$select_all_stats_in_shape, {

    req(input$multi_station == TRUE)

    click_station(stations_sf$STATION_NUMBER)
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

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
    p = station_flow_plot(data = dat_with_metric(),
                          # variable_choice = input$user_var_choice,
                          variable_choice = user_var(),
                          clicked_station = click_station(),
                          stations_shapefile = stations_sf,
                          slopes = senslope_dat(),
                          caption_label = date_choice_label())
    ggplotly(p)
  })

  output$my_hydrograph = renderPlotly({
    h = hydrograph_plot(
      #dat = flow_dat_daily(),
      dat = filtering_mod_output$dat_filtered(),
      clicked_station = click_station(),
      stations_shapefile = stations_sf)

    ggplotly(h)
  })

  # DATA DOWNLOAD #
  output$download_flow_data_ui = renderUI({
    downloadButton("download_flow_data",paste0("Download Daily Flow Data (",length(click_station()[click_station() != 'no_selection'])," station(s) selected)"))
  })

  output$download_flow_data <- downloadHandler(
    filename = function() {
      paste0("HYDAT_flow_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtering_mod_output$dat_filtered() %>%
                  filter(STATION_NUMBER %in% click_station()), file, row.names = F)
    }
  )

  # DATA DOWNLOAD #
  output$download_data_with_results_ui = renderUI({
    downloadButton("download_data_with_results", paste0("Download Trend Analysis Results (",length(click_station()[click_station() != 'no_selection'])," station(s) selected"))
  })

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
                date_vars = date_vars,
                mk_results = mk_results())
  })

  my_pal_shape = eventReactive(input$user_shape_choice, {
    if(input$user_shape_choice == 'none') return(colorFactor(palette = '#808080', domain = 1))

    colorNumeric(palette = 'RdBu',
                 domain = shape_for_map()$average_trend_result)

  })

  output$leafmap <- renderLeaflet({
    make_base_bc_leafmap()
  })

  observe({
    update_leaflet(map = 'leafmap',
                   stations = stations_sf_with_trend(),
                   clicked_station = click_station(),
                   shapes = shape_for_map(),
                   clicked_shape = click_shape(),
                   pal = mypal())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
# shiny::runApp(myapp, port = 6104)
