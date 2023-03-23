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
  choices = c('Average Flow' = 'Median',
              'Date of 50% Flow' = 'DoY_50pct_TotalQ',
              'Minimum Flow (7day)' = 'Min_7_Day',
              'Date of Minimum Flow (7day)' = 'Min_7_Day_DoY',
              'Minimum Flow (30day)' = 'Min_30_Day',
              'Date of Minimum Flow (30day)' = 'Min_30_Day_DoY'),
  selected = 'Median',
  width = '100%')

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

station_plot = tagList(
  plotlyOutput('myplot', height = 275)
)

hydrograph = tagList(
  plotOutput('my_hydrograph', height = 275)
)

map = leafletOutput('leafmap',height = '400px')

main_bit = tagList(
  map,
  tabsetPanel(
    id = 'tabset',
    tabPanel(title = 'Flow Metric Plot', station_plot),
    tabPanel(title = 'Hydrograph', hydrograph)
  )
)

