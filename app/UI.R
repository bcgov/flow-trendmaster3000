library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(envreportutils)
library(sf)
library(EnvStats)
library(data.table)
library(tidyverse)
library(tidyhydat)
library(lubridate)
library(ggtext)
library(feather)
library(shinyFeedback)

source('modules/filter_data.R')

number_stations_vb = value_box(
  "Number of Stations",
  span(
    textOutput('num_stations_on_plot')
  ),
  showcase = bsicons::bs_icon("moisture", size = "300%"),
  class = "bg-secondary"
)

number_stations_declining = value_box(
  "Stations Trend Down/Earlier",
  span(
    textOutput('num_stations_dec')
  ),
  showcase = bsicons::bs_icon("droplet-half", size = "300%"),
  fill = F,
  class = 'bg-danger'
)

number_stations_increasing = value_box(
  "Stations Trend Up/Later",
  span(
    textOutput('num_stations_inc')
  ),
  showcase = bsicons::bs_icon("droplet-fill", size = "300%"),
  class = "bg-primary"
)

var_choice_bit = selectizeInput(
  inputId = 'user_var_choice',
  label = 'Trend to Display',
  choices = c('Average Flow' = 'Average',
              'Date of 50% Flow' = 'DoY_50pct_TotalQ',
              'Minimum Flow (7-day)' = 'Min_7_Day',
              'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
              'Minimum Flow (30-day)' = 'Min_30_Day',
              'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
              'Maximum Flow (7-day)' = 'Max_7_Day',
              'Date of Maximum Flow (7-day)' = 'Max_7_Day_DoY'),
  selected = 'Average',
  width = '100%')

multi_switch = shinyWidgets::switchInput(inputId = "multi_station",
                          label = 'Station Selection Mode',
                          value = FALSE,
                          onLabel = 'Multi',
                          offLabel = 'Single')

map_shape_bit = selectizeInput(
  inputId = 'user_shape_choice',
  label = 'Region Type',
  choices = c('None' = 'none',
              'Ecoprovinces' = 'ecoprov',
              'Ecoregions' = 'ecoreg',
              'Ecosections' = 'ecosec',
              'Natural Resource Districts' = 'nr_dist',
              'Natural Resource Regions' = 'nr_reg'),
  selected = 'None'
)

data_download_bit = card(
  card_body(
    p("Download data for selected stations, incorporating any filters that have been applied."),
    uiOutput('download_flow_data_ui'),
    uiOutput('download_data_with_results_ui')
  )
)
station_plot = tagList(
  plotlyOutput('myplot', height = 250)
)

hydrograph = tagList(
  plotlyOutput('my_hydrograph', height = 250)
)

map = leafletOutput('leafmap', height = '100%')

sidebar_content = tagList(
  var_choice_bit,
  filter_data_Mod_UI('data'),
  map_shape_bit,
  multi_switch,
  number_stations_vb,
  number_stations_declining,
  number_stations_increasing
)

the_sidebar = sidebar(
  width = '20%',
  sidebar_content
)

main_bit = tagList(
  map,
  absolutePanel(
    id = 'trend_selector',
    top = '60%', left = '22%', right = '2%', height = '25%',
    bslib::navs_pill(
      id = 'tabset',
      nav(title = 'Flow Metric Plot', station_plot),
      nav(title = 'Hydrograph', hydrograph),
      nav(title = 'Data Download', data_download_bit)
    )
  )
)

