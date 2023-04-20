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
source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')
source('modules/calc_mk_results.R')

my_theme = bs_theme(bootswatch = 'flatly',
                    danger = "#cc0000",
                    primary = '#3399ff',
                    font_scale = 0.75)

sidebar_content = tagList(
  var_choice_bit,
  filter_data_Mod_UI('data'),
  number_stations_vb,
  number_stations_declining,
  number_stations_increasing
)

the_sidebar = sidebar(
  width = '20%',
  sidebar_content
)

ui = page_fluid(
  theme = my_theme,

  shinyFeedback::useShinyFeedback(),

  ask_user_for_dir_ui('dir'),

  layout_sidebar(
    sidebar = the_sidebar,
    main_bit,
    textOutput('number_rows_raw_dat')
  )
)



server <- function(input, output) {
  # Module that asks the user for their choice of directory.
  tempfiles_folder = ask_user_for_dir_server('dir')

  # Once user has selected a filepath, remove modal.
  observeEvent(input$submit_filepath, removeModal())

  # # Set path to data; create folder 'Trendmaster3000_tmpfiles' in user-selected folder.
  # tempfiles_folder = reactive({
  #   browser()
  #   req(input$text_filepath)
  #   path = input$text_filepath
  #   # Make sure last character of path is a '/'
  #   if(str_detect(path, '.{1}$') != '/') path = paste0(path,'/')
  #
  #   # Tack on name of folder for temporary files: "Trendmaster3000_tmpfiles"
  #   paste0(path,"Trendmaster3000_tmpfiles/")
  # })

  # Create reactive for path to Hydat database.
  hydat_path = reactive(paste0(tempfiles_folder(),'Hydat.sqlite3'))

  # Get user variable choice
  user_var = reactive(input$user_var_choice)

  # Load in data ----------------------------------------------------
  ## raw flow data.
  flow_dat_daily = reactive({
    req(file.exists(paste0(tempfiles_folder(),"daily_flow_records.feather")))
    feather::read_feather(paste0(tempfiles_folder(),"daily_flow_records.feather"))
  })

  output$number_rows_raw_dat = renderText(nrow(filtering_mod_output$dat_filtered()))

  ## List of stations to include.
  stations_list = reactive({
    read.csv(paste0(tempfiles_folder(),'filtered_station_list.csv')) |>
    pull(STATION_NUMBER)
  })

  # Get the stations
  stations_sf = reactive({
    #Utils function to read in stations from HYDAT, convert to sf.
    get_station_sf(stations_list(), hydat_path = hydat_path())
  })

  # Use the data filtering module on the raw data.
  filtering_mod_output <- filter_data_Mod_Server("data",
                                        flow_dat_daily())

  # Use the metric-adding module on the filtered data.
  dat_with_metric <- add_metric_to_dat_mod("data2",
                                           data = filtering_mod_output$dat_filtered,
                                           user_var_choice = user_var)

  date_vars = c("Min_7_Day_DoY","DoY_50pct_TotalQ")

  # Run the Mann-Kendall trend analysis on the data with metric.
  mk_results <- calculate_mk_mod('mk_res',
                                data = dat_with_metric,
                                user_var_choice = user_var)

  # Attach the MK results to the data with metric.
  flow_dat_with_mk = reactive({
    dat_with_metric() %>%
      left_join(mk_results())
  })

  # Add the flow data with MK trend results to the
  # spatial object of the stations.
  stations_sf_with_trend = reactive({

    req(exists('stations_sf'),flow_dat_daily())

    dat = stations_sf() %>%
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

  # Render summary values for sidebar.
  output$num_stations_on_plot = renderText({nrow(stations_sf_with_trend())})

  output$num_stations_dec = renderText({nrow(stations_sf_with_trend() |>
                                               filter(trend_sig %in% c('Significant Trend Earlier',
                                                                       'Significant Trend Down')))})

  output$num_stations_inc = renderText({nrow(stations_sf_with_trend() |>
                                               filter(trend_sig %in% c('Significant Trend Later',
                                                                       'Significant Trend Up')))})
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
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Station Plot')
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  # Generate label of which
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
                      stations_shapefile = stations_sf(),
                      slopes = senslope_dat(),
                      caption_label = date_choice_label())
    ggplotly(p)
  })

  output$my_hydrograph = renderPlot({
    h = hydrograph_plot(dat = flow_dat_daily(),
                    clicked_station = click_station(),
                    stations_shapefile = stations_sf())
    # ggplotly(h)
    h
  })

  mypal = reactive({
    # if(input$user_var_choice %in% date_vars){
    if(user_var() %in% date_vars){
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
