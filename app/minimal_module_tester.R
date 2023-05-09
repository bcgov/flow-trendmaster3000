library(shiny)
library(shinyWidgets)
library(data.table)

source('utils/stats_funcs.R')
source('utils/plot_funcs.R')
source('utils/leafmap_funcs.R')
source('utils/load_stations.R')
source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')
source('modules/calc_mk_results.R')


ui <- fluidPage(
  filter_data_Mod_UI("data"),


  h5("Include data that does not meeting QAQC criteria?"),

  shinyWidgets::switchInput(
    inputId = 'include_low_qual_data',
    label = '',
    onLabel = 'Include',
    offLabel = 'Exclude'
  ),

  selectizeInput(
    inputId = 'user_var_choice',
    label = '',
    choices = c('Average Flow' = 'Average',
                'Date of 50% Flow' = 'DoY_50pct_TotalQ',
                'Minimum Flow (7-day)' = 'Min_7_Day',
                'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
                'Minimum Flow (30-day)' = 'Min_30_Day',
                'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
                'Maximum Flow (7-day)' = 'Max_7_Day',
                'Date of Maximum Flow (7-day)' = 'Max_7_Day_DoY'),
    selected = 'Average',
    width = '100%'),

  textOutput('nrows'),

  DT::DTOutput('dat')
)

server <- function(input, output, session) {

  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }


  flow_dat_daily = qs::qread("daily_flow_records.qs")

  setDT(flow_dat_daily)

  bc_boundary = read_sf('bc_bound.gpkg') %>%
    st_transform(crs = 4326) %>%
    mutate(shape_name = 'unselected area')

  stations_sf = read_sf('stations.gpkg') %>%
    mutate(internal_id = c(1:nrow(.)))

  stations_sf = reactive({
    if(input$include_low_qual_data == T){
      sample_n(stations_sf,100)
    } else {
      stations_sf
    }
  })

  # Use the data filtering module on the raw data.
  filtering_mod_output <- filter_data_Mod_Server("data",
                                                 flow_dat_daily,
                                                 shape = click_shape)


  # dat_with_metric <- add_metric_to_dat_mod("data2",
  #                                          data = filtering_mod_output$dat_filtered,
  #                                          user_var_choice = reactive(input$user_var_choice),
  #                                          user_period_choice = filtering_mod_output$user_period_choice,
  #                                          scale_selector_radio = filtering_mod_output$scale_selector_radio,
  #                                          finegrain_reactives_list = filtering_mod_output$finegrain_reactives_list)
  #
  # dat_with_mk = calculate_mk_mod('mk_res',
  #                                data = dat_with_metric,
  #                                user_var_choice = reactive(input$user_var_choice))

  output$nrows = renderText({
    req(!is.null(filtering_mod_output$dat_filtered()))
    paste0("Filtered Data: ",nrow(filtering_mod_output$dat_filtered()))
  })

  output$dat = DT::renderDT({
    req(input$user_var_choice)
    filtering_mod_output$dat_filtered()
    })
}

shinyApp(ui, server)
