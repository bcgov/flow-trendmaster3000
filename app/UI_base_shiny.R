library(shiny)
library(shinyWidgets)
# library(bslib)
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
spacer_line_no_margin = HTML("<hr style = 'margin-top:0;margin-bottom:5;'>")

number_stations_vb = shiny::actionButton(
  inputId = 'num_stations_sum',
  label = HTML(
    paste0(
      "Number of Stations:",
      span(
        textOutput('num_stations_on_plot')
      )
    )
  ),
  style = "color:#0f5132;background:#badbcc;border-color:black;"
)

number_stations_declining = shiny::actionButton(
  inputId = 'num_stations_dec_sum',
  label = HTML(
    paste0(
      "Stations Trending Down/Earlier:",
      # span(
        textOutput('num_stations_dec')
      # )
    )
  ),
  style = "color:#842029;background:#f5c2c7;border-color:black;"
)

number_stations_increasing = shiny::actionButton(
  inputId = 'num_stations_inc_sum',
  label = HTML(
    paste0(
      "Stations Trending Up/Later:",
      span(
        textOutput('num_stations_inc')
      )
    )
  ),
  style = "color:#664d03;background:#ffecb5;border-color:black;"
)

var_choice_bit = selectInput(
  selectize = F,
  inputId = 'user_var_choice',
  label = 'Trend to Display',
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

region_selector_bits = div(
  actionButton(inputId = 'reset_station_sel',
               label = 'Reset Stations'),
  actionButton(inputId = 'reset_shape_sel',
               label = 'Reset Shapes'),
  actionButton(inputId = 'select_all_stats_in_shape',
               label = 'Select Stations\nin Shape'),
  style = 'text-align:center;'
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
  label = 'Add Admin. Boundaries',
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
  plotlyOutput('myplot', height = 300)
)

hydrograph = tagList(
  plotlyOutput('my_hydrograph', height = 300)
)

data_table_bit = tagList(
  DT::DTOutput('selected_station_DT')
)

data_download_bit = tagList(
  p("Download data for selected stations, incorporating any filters that have been applied."),
  uiOutput('download_flow_data_ui'),
  uiOutput('download_data_with_results_ui')
)

map = div(
  leafletOutput('leafmap', height = '500px'),
  style = 'margin-top:10px;margin-right:-15px;margin-left:-15px'
)

the_sidebar = div(
  h3("Flow Trendmaster 3000", style = 'text-align:center;'),
  spacer_line_no_margin,
  var_choice_bit,
  filter_data_Mod_UI('data'),
  h4("Station Selection Tools", style = 'text-align:center;'),
  spacer_line_no_margin,
  column(width = 6,
         include_non_qaqc_stations_bit
  ),
  column(width = 6,
         multistation_mode
  ),
  map_shape_bit,
  fluidRow(
    region_selector_bits
  ),
  h4("Summary Values"),
  fluidRow(
    number_stations_vb,
    number_stations_declining,
    number_stations_increasing,
    style = 'text-align:center;'
  ),
  style = 'background:#ADD8E7'
)

main_bit = fluidRow(
  column(width = 3,
         the_sidebar,
         style = 'background:#ADD8E7;border-radius:10px;border:2px solid #4fa5e3'),
  column(width = 9,
         map),
  absolutePanel(
    id = 'plot_window',
    top = '50%',
    left = '25%', right = '0%', #bottom = 0,
    # height = '100%',
    tabsetPanel(
      id = 'tabset',
      tabPanel(title = 'Flow Metric Plot', station_plot),
      tabPanel(title = 'Hydrograph', hydrograph),
      tabPanel(title = 'Tabular Data', data_table_bit),
      tabPanel(title = 'Data Download', data_download_bit)
    )
  )
)

# my_theme = bs_theme(bootswatch = 'flatly',
#                     version = "5",
#                     # danger = "#cc0000",
#                     # primary = '#3399ff',
#                     # "sidebar-bg" = '#ADD8E7',
#                     font_scale = 0.75) %>%
#   bs_add_rules("#plot_window:{opacity:0.5;}
#                 #plot_window:hover{opacity:1;}
#                 #reset_station_sel{background-color:#2c3e50;color:white}
#                 #reset_shape_sel{background-color:#2c3e50;color:white}
#                 #select_all_stats_in_shape{background-color:#2c3e50;color:white;}")


# Vanilla shiny page.
ui = shiny::fluidPage(
  tags$style("
        #plot_window{
          opacity: 0.5;
        }
        #plot_window:hover{
          opacity: 1;
        }
        #reset_station_sel{background-color:#2c3e50;color:white}
        #reset_shape_sel{background-color:#2c3e50;color:white}
        #select_all_stats_in_shape{background-color:#2c3e50;color:white;}
           "
  ),

  useShinyFeedback(),

  title = 'Flow Trendmaster 3000',
  main_bit
)
