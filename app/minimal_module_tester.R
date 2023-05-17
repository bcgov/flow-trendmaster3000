library(shiny)
library(shinyWidgets)
library(data.table)

source('UI.R')
source('utils/stats_funcs.R')
source('utils/plot_funcs.R')
source('utils/leafmap_funcs.R')
source('utils/load_stations.R')
source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')
source('modules/calc_mk_results.R')


ui <- fluidPage(
  filter_data_Mod_UI("data"),

  useShinyFeedback(),

  h5("Include stations that do not meeting QAQC criteria?"),

  shinyWidgets::switchInput(
    inputId = 'include_low_qual_data',
    label = '',
    onLabel = 'Include',
    offLabel = 'Exclude'
  ),

  selectizeInput(
    inputId = 'user_shape_choice',
    label = 'Add Shapes',
    choices = c('None' = 'none',
                'Subwatershed Groups' = 'subw',
                'Ecoprovinces' = 'ecoprov',
                'Ecoregions' = 'ecoreg',
                'Ecosections' = 'ecosec',
                'Natural Resource Districts' = 'nr_dist',
                'Natural Resource Regions' = 'nr_reg',
                'Drawn Shape' = 'drawn_shape'),
    selected = 'ecosec'
  ),

  selectizeInput(
    inputId = 'user_var_choice',
    label = '',
    choices = c('Average Flow' = 'Median',
                'Date of 50% Flow' = 'DoY_50pct_TotalQ',
                'Minimum Flow (7-day)' = 'Min_7_Day',
                'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
                'Minimum Flow (30-day)' = 'Min_30_Day',
                'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
                'Maximum Flow (7-day)' = 'Max_7_Day',
                'Date of Maximum Flow (7-day)' = 'Max_7_Day_DoY'),
    selected = 'Median',
    width = '100%'),

  textOutput('nrows'),

  # textOutput('shapes_nrows'),

  DT::DTOutput('dat')

  # leafletOutput('leafmap')
)

server <- function(input, output, session) {

  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  bc_boundary = read_sf('bc_bound.gpkg') %>%
    st_transform(crs = 4326) %>%
    mutate(shape_name = 'unselected area')

  stations_sf = read_sf('stations.gpkg') %>%
    mutate(internal_id = c(1:nrow(.)))

  stations_to_include = reactive({
    # stations = stations_sf[stations_sf$meets_criteria == T,]
    if(!click_shape() %in% c('no_selection','drawn_poly')){
      # if(click_shape() != 'drawn_poly'){
      stations = stations_sf |> filter(!!sym(input$user_shape_choice) == click_shape())
    }
    if(click_shape() %in% c('drawn_poly')){
      stations = stations_sf |>
        st_join(drawn_poly(), st_intersects) |>
        filter(!is.na(shape_name))
    }
    if(click_shape() %in% c("no_selection")){
      stations = stations_sf
    }
    return(stations)
  })

  observe({
    print(head(stations_to_include()))
  })

  click_shape = reactiveVal('Finlay River Trench')

  # Use the data filtering module on the raw data.
  filtering_mod_output <- filter_data_Mod_Server("data",
                                                 reactive(input$include_low_qual_data),
                                                 stations_to_include,
                                                 number_station_cutoff = 50)

  dat_with_metric <- add_metric_to_dat_mod("data2",
                                           data = filtering_mod_output$dat_filtered,
                                           user_var_choice = reactive(input$user_var_choice),
                                           user_period_choice = filtering_mod_output$user_period_choice,
                                           scale_selector_radio = filtering_mod_output$scale_selector_radio,
                                           finegrain_reactives_list = filtering_mod_output$finegrain_reactives_list)

  dat_with_mk = calculate_mk_mod('mk_res',
                                 data = dat_with_metric,
                                 user_var_choice = reactive(input$user_var_choice))

  # Adding in more pieces from the main app in an attempt to find
  # where things start to get so memory-intensive!
  user_var = reactive(input$user_var_choice)

  # date_vars = c("Min_7_Day_DoY","Min_30_Day_DoY",
  #               "Max_7_Day_DoY","DoY_50pct_TotalQ")
  #
  # stations_sf_with_trend = reactive({
  # # browser()
  #   # req(stations_to_include(),mk_results())
  #   stations.dt = as.data.table(stations_sf_to_include())
  #   mk_results.dt = as.data.frame(dat_with_mk())
  #
  #   dat = st_as_sf(data.table::merge.data.table(stations.dt, mk_results.dt))
  #
  #   # Is the chosen variable one of the 'date' variables?
  #   # If so, update the MK trend labels to say 'earlier' or 'later'
  #   # rather than 'up' or 'down'.
  #
  #   if(user_var() %in% date_vars){
  #     dat = dat %>%
  #       mutate(trend_sig = factor(
  #         trend_sig,
  #         levels = c("Significant Trend Earlier",
  #                    'Non-Significant Trend Earlier',
  #                    'No Trend',
  #                    'Non-Significant Trend Later',
  #                    'Significant Trend Later')))
  #   } else {
  #     dat = dat %>%
  #       mutate(trend_sig = factor(
  #         trend_sig,
  #         levels = c("Significant Trend Down",
  #                    'Non-Significant Trend Down',
  #                    'No Trend',
  #                    'Non-Significant Trend Up',
  #                    'Significant Trend Up')))
  #   }
  #
  #   return(dat)
  # })
  #
  # ## Shapes for leaflet map station filtering.
  # shape_for_map = reactive({
  #   switch(input$user_shape_choice,
  #          none = st_set_geometry(
  #            tibble(shape_name = 'Nothing',
  #                   average_trend_result = 0,
  #                   stations_for_mean = 0,
  #                   label = ""),
  #            tibble(lon = c(1,0), lat = c(0, 1)) %>%
  #              st_as_sf(coords = c('lon','lat'), crs = 4326) %>%
  #              st_bbox() %>%
  #              st_as_sfc()
  #          ),
  #          ecoprov = read_sf('ecoprovinces.gpkg') %>% st_transform(crs = 4326),
  #          ecoreg = read_sf('ecoregions.gpkg') %>% st_transform(crs = 4326),
  #          ecosec = read_sf('ecosections.gpkg') %>% st_transform(crs = 4326),
  #          nr_dist = read_sf('nr_districts.gpkg') %>% st_transform(crs = 4326),
  #          nr_reg = read_sf('nr_regions.gpkg') %>% st_transform(crs = 4326),
  #          subw = read_sf('subw.gpkg') %>% st_transform(crs = 4326),
  #          drawn_shape = drawn_poly())
  # })
  #
  # click_shape = reactiveVal('no_selection')
  # click_station = reactiveVal('no_selection')
  #
  # stations_sf_with_trend_in_shape = reactive({
  #   # browser()
  #   #If the user has selected a shape on the leaflet map, filter stations.
  #   if(click_shape() != 'no_selection'){
  #     # If it is a custom drawn polygon, we must do a spatial match here.
  #     if(click_shape() == 'drawn_poly'){
  #
  #       return(stations_sf_with_trend() %>%
  #                st_join(shape_for_map() %>% filter(shape_name == click_shape()), st_intersects) %>%
  #                filter(!is.na(shape_name)) %>%
  #                dplyr::select(-shape_name)
  #       )
  #
  #     } else {
  #       return(
  #         stations_sf_with_trend() %>%
  #           filter(!!sym(input$user_shape_choice) == click_shape())
  #       )
  #     }
  #   } else {
  #     return(stations_sf_with_trend())
  #   }
  #
  # })
  #
  # mypal = reactive({
  #   make_my_pal(user_selected_metric = user_var(),
  #               date_vars = date_vars,
  #               mk_results = dat_with_mk())
  # })
  #
  # my_pal_shape = eventReactive(input$user_shape_choice, {
  #
  #   if(input$user_shape_choice == 'none') return(colorFactor(palette = '#808080', domain = 1))
  #
  #   colorNumeric(palette = 'RdBu',
  #                domain = shape_for_map()$average_trend_result)
  #
  # })

  output$nrows = renderText({
    paste0(nrow(dat_with_mk()))
  })

  output$dat = DT::renderDT({
    req(input$user_var_choice)
    dat_with_mk() |> head()
    })

  # output$leafmap <- renderLeaflet({
  #   make_base_bc_leafmap()
  # })

  # observe({
  #
  #   update_leaflet(map = 'leafmap',
  #                  stations = stations_sf_with_trend_in_shape(),
  #                  clicked_station = click_station(),
  #                  shape_type = reactive(input$user_shape_choice),
  #                  shapes = shape_for_map(),
  #                  clicked_shape = click_shape(),
  #                  pal = mypal())
  # })
}

shinyApp(ui, server)
