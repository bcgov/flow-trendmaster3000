library(shiny)
library(shinyWidgets)
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
# library(feather)
library(qs)
library(shinyFeedback)

source('modules/filter_data.R')

spacer_line = HTML("<hr>")
spacer_line_no_margin = HTML("<hr style = 'margin-top:0;margin-bottom:10;'>")

number_stations_vb = value_box(
  "Stations",
  span(
    textOutput('num_stations_on_plot')
  ),
  showcase = bsicons::bs_icon("moisture", size = "100%"),
  class = "bg-secondary"
)

number_stations_declining = value_box(
  "Trending Down/Earlier",
  span(
    textOutput('num_stations_dec')
  ),
  showcase = bsicons::bs_icon("droplet-half", size = "100%"),
  fill = F,
  class = 'bg-danger'
)

number_stations_increasing = value_box(
  "Trending Up/Later",
  span(
    textOutput('num_stations_inc')
  ),
  showcase = bsicons::bs_icon("droplet-fill", size = "100%"),
  class = "bg-primary"
)

var_choice_bit = selectInput(
  inputId = 'user_var_choice',
  label = '',
  selectize = F,
  choices = c('Average Flow' = 'Median',
              'Date of 50% Flow' = 'DoY_50pct_TotalQ',
              'Minimum Flow (7-day)' = 'Min_7_Day',
              'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
              'Minimum Flow (30-day)' = 'Min_30_Day',
              'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
              'Maximum Flow (7-day)' = 'Max_7_Day',
              'Date of Maximum Flow (7-day)' = 'Max_7_Day_DoY'),
  selected = 'Median',
  width = '100%')

region_selector_bits = tagList(
  actionButton(inputId = 'select_all_stats_in_shape',
               label = 'Select Stations in Shape'),
  actionButton(inputId = 'reset_station_sel',
               label = 'Reset Station Selection'),
  actionButton(inputId = 'reset_shape_sel',
               label = 'Reset Shape Selection')
)

# The following switch could be for allowing the user to
# include all data, not just data that matches our filter criteria.
include_non_qaqc_stations_bit = shinyWidgets::awesomeCheckbox(
  inputId = 'include_low_qual_data',
  label = htmltools::HTML('Include Stations<br>with Data Gaps'),
  status = 'primary'
)

multistation_mode = shinyWidgets::awesomeCheckbox(
  inputId = 'multi_station',
  label = 'Multiple Station Mode',
  value = F,
  status = 'success'
)

map_shape_bit = selectInput(
  inputId = 'user_shape_choice',
  label = 'Add Administrative Boundaries',
  selectize = F,
  choices = c('None' = 'none',
              'Subwatershed Groups' = 'subw',
              'Ecoprovinces' = 'ecoprov',
              'Ecoregions' = 'ecoreg',
              'Ecosections' = 'ecosec',
              'Natural Resource Districts' = 'nr_dist',
              'Natural Resource Regions' = 'nr_reg',
              'Drawn Shape' = 'drawn_shape'),
  selected = 'None'
)



station_plot = tagList(
  plotlyOutput('myplot', height = 250)
)

hydrograph = tagList(
  plotlyOutput('my_hydrograph', height = 250)
)

data_table_bit = tagList(
  DT::DTOutput('selected_station_DT')
)

data_download_bit = card(
  card_body(
    p("Download data for selected stations, incorporating any filters that have been applied."),
    p("Note: In order to facilitate shiny app processing times, daily flow records have been averaged by week."),
    uiOutput('download_flow_data_ui'),
    HTML("<br>"),
    uiOutput('download_data_with_results_ui')
  )
)

map = leafletOutput('leafmap', height = '100%')

sidebar_content = tagList(
  h5("Trend to Display"),
  spacer_line_no_margin,
  var_choice_bit,
  filter_data_Mod_UI('data'),
  h5("Station Selection\nTools"),
  # spacer_line,
  spacer_line_no_margin,
  fluidRow(
    column(width = 6,
           include_non_qaqc_stations_bit
    ),
    column(width = 6,
           multistation_mode
    )
  ),
  map_shape_bit,
  region_selector_bits,
  spacer_line_no_margin,
  number_stations_vb,
  number_stations_increasing,
  number_stations_declining
)

the_sidebar = sidebar(
  width = '20%',
  open = 'always',
  # card(
  #   card_body(
  sidebar_content,
  # min_height = '100%',
  # max_height = '100%'
  # ),
  style = 'background-color:#ADD8E7'
  # )
)


main_bit = tagList(
  absolutePanel(
    map,
    top = 0, bottom = 0, left = '20%', right = 0),
  # absolutePanel(
  #   div(
  #     number_stations_vb,
  #     spacer_line,
  #     number_stations_declining,
  #     number_stations_increasing,
  #   ),
  #   top = 0, bottom = 0, left = '80%', right = 0),
  # card(
  absolutePanel(
    id = 'trend_selector',
    top = '50%', left = '20%', right = '0%', height = '50%',
    card(
      height = '100%',
      card_body(
        full_screen = TRUE,
        bslib::navs_pill_card(
          id = 'tabset',
          # full_screen = TRUE,
          nav(title = 'Flow Metric Plot', station_plot),
          nav(title = 'Hydrograph', hydrograph),
          nav(title = 'Tabular Data', data_table_bit),
          nav(title = 'Data Download', data_download_bit)
        )
      )
    )
  )
)

my_theme = bs_theme(bootswatch = 'flatly',
                    version = "5",
                    # danger = "#cc0000",
                    # primary = '#3399ff',
                    # "sidebar-bg" = '#ADD8E7',
                    font_scale = 0.75) %>%
  bs_add_rules("#trend_selector {opacity:0.5;}
                #trend_selector:hover{opacity:1;}
                #reset_station_sel{background-color:#2c3e50;}
                #reset_shape_sel{background-color:#2c3e50;}
                #select_all_stats_in_shape{background-color:#2c3e50;}")

# ui = bslib::page_fluid(
#
#   theme = my_theme,
#
#   shinyFeedback::useShinyFeedback(),
#
#   the_sidebar,
#
#   main_bit
# )
ui = bslib::page_fillable(

  theme = my_theme,

  shinyFeedback::useShinyFeedback(),

  layout_sidebar(
    sidebar = the_sidebar,
    main_bit
  )
)
