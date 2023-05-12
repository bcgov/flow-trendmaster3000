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
  width = '100%')

region_selector_bits = tagList(
  actionButton(inputId = 'reset_station_sel',
               label = 'Reset Station Selection'),
  actionButton(inputId = 'reset_shape_sel',
               label = 'Reset Shape Selection'),
  actionButton(inputId = 'select_all_stats_in_shape',
               label = 'Select Stations in Shape')
)

# The following switch could be for allowing the user to
# include all data, not just data that matches our filter criteria.
# shinyWidgets::switchInput(
#       inputId = 'include_low_qual_data',
#       label = '',
#       onLabel = 'Include',
#       offLabel = 'Exclude'
#     ),

multistation_mode = shiny::checkboxInput(
  inputId = 'multi_station',
  label = 'Multiple Station Mode',
  value = F
)
# multi_switch = shinyWidgets::switchInput(
#   inputId = "multi_station",
#   label = 'Station Selection Mode',
#   value = FALSE,
#   onLabel = 'Multi',
#   offLabel = 'Single')

map_shape_bit = selectizeInput(
  inputId = 'user_shape_choice',
  label = 'Select Spatial Delineations',
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

data_table_bit = tagList(
  DT::DTOutput('selected_station_DT')
)

station_plot = tagList(
  plotlyOutput('myplot', height = 250)
)

hydrograph = tagList(
  plotlyOutput('my_hydrograph', height = 250)
)

data_download_bit = card(
  card_body(
    p("Download data for selected stations, incorporating any filters that have been applied."),
    uiOutput('download_flow_data_ui'),
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
  # multi_switch,
  multistation_mode,
  map_shape_bit,
  region_selector_bits,
  # HTML("<br>"),
  spacer_line,
  number_stations_vb,
  number_stations_declining,
  number_stations_increasing
)

the_sidebar = sidebar(
  width = '20%',
  sidebar_content,
  bg = '#ADD8E7',
  open = 'always'
)

main_bit = tagList(
  absolutePanel(map,
                top = 0, bottom = 0, left = '20%', right = 0),
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
        nav(title = 'Tabular Data', data_table_bit),
        nav(title = 'Flow Metric Plot', station_plot),
        nav(title = 'Hydrograph', hydrograph),
        nav(title = 'Data Download', data_download_bit)
       )
       )
    )
  )
)

my_theme = bs_theme(bootswatch = 'flatly',
                    # danger = "#cc0000",
                    # primary = '#3399ff',
                    # "sidebar-bg" = '#ADD8E7',
                    font_scale = 0.75) %>%
  bs_add_rules("#trend_selector {opacity:0.5;}
                #trend_selector:hover{opacity:1;}
                #reset_station_sel{background-color:#2c3e50;}
                #reset_shape_sel{background-color:#2c3e50;}
                #select_all_stats_in_shape{background-color:#2c3e50;}")

ui = page_fillable(

  theme = my_theme,

  shinyFeedback::useShinyFeedback(),

  layout_sidebar(
    sidebar = the_sidebar,
    main_bit,
    textOutput('stations_selected')
  )
)
