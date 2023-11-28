library(shiny)
library(shinyWidgets)
library(shinyjs)
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
library(qs)
library(shinyFeedback)
library(santoku)

source('modules/filter_data.R')
source('modules/access_wsurvey_canada_data.R')

data_last_updated = '2023-11-21'

spacer_line = HTML("<hr>")
spacer_line_no_margin = HTML("<hr style = 'margin-top:0rem;margin-bottom:-0.25rem;'>")

sidebar_width = '28%'

number_stations_vb = card(
  div(
    h5("Stations"),
    div(textOutput('num_stations_on_plot'), style = 'font-size:large;')
  ),
  div(bsicons::bs_icon('thermometer-half', size = '50%'),
      style = 'position:absolute; opacity: 0.25; left:35%; top: 50%;')
)

number_stations_declining = bslib::card(
  div(
    h5("Down / \nEarlier"),
    div(textOutput('num_stations_dec'), style = 'font-size:large;')
  ),
  div(bsicons::bs_icon('droplet-half', size = '50%'),
      style = 'position:absolute; opacity: 0.25; left:35%; top: 50%;'),
  style = 'background: darkred; opacity:0.5; color:white;'
)

number_stations_increasing = bslib::card(
  div(
    h5("Up / \nLater"),
    div(textOutput('num_stations_inc'), style = 'font-size:large;')
  ),
  div(bsicons::bs_icon('droplet-fill', size = '50%'),
      style = 'position:absolute; opacity: 0.25; left:35%; top: 50%;'),
  style = 'background: darkblue; opacity:0.5; color:white;'
)

var_choice_bit = div(
  selectInput(
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
    width = '100%') |>
    tooltip("Select flow trend"),
  style = 'margin-top: -1.5rem !important; margin-bottom: -1rem !important;'
)

param_choice_bit = div(

  selectInput(
    inputId = 'user_param_choice',
    label = '',
    selectize = F,
    choices = c('P-value' = 'p_value',
                'Slope' = 'slope'),
    selected = 'p_value',
    width = '100%') |>
    tooltip("Select model parameter")
  ,
  style = 'margin-top: -1.5rem !important; margin-bottom: -1rem !important;'
)
region_selector_bits = tagList(
  layout_column_wrap(
    1/3,
    # div(
    actionButton(inputId = 'select_all_stats_in_shape',
                 label = "Get Stn's in Bounds"),
    # style = 'width: 120%;'
    # ),
    actionButton(inputId = 'reset_station_sel',
                 label = 'Reset Station Selection'),
    actionButton(inputId = 'reset_shape_sel',
                 label = 'Reset Bounds Selection')
  )
)

var_and_param_selectors = fluidRow(
  column(width = 6,
         var_choice_bit),
  column(width = 6,
         param_choice_bit)
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

map_shape_bit = div(
  selectInput(
    inputId = 'user_shape_choice',
    label = 'Add Administrative Bounds',
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
  ),
  style = 'margin-top: -1.5rem !important; margin-bottom: -1.5rem;'
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
    bslib::layout_column_wrap(
      1/3,
      # uiOutput('current_downloaded_data_UI'),
      uiOutput('current_selected_stations'),
      uiOutput('download_flow_data_ui'),
      # downloadButton("download_flow_data",
      #                h4("Download Daily Data"),
      #                style = 'background-color:#2c3e50;height:12vh;'),
      downloadButton("download_data_with_results",
                     h4("Download Trend Analysis Results"),
                     style = 'background-color:#2c3e50;height:12vh;'),
      # uiOutput('download_flow_data_ui'),
      # uiOutput('download_data_with_results_ui')
    ),
    p("(Note: daily data for selected station(s) download does not yet incorporate any date filters you've selected... sorry!)"),
  )
)

summary_box_bits = layout_column_wrap(
  1/3,
  number_stations_vb,
  number_stations_increasing,
  number_stations_declining
)

map = leafletOutput('leafmap', height = '100%')

sidebar_content = div(
  div(
    div(
      # bsicons::bs_icon("water", size = '100%'),
      # style = 'height: 6vh; width: 4vh;'
      class = 'wave-container',
      div(class = "wave", style = "background-image: url('sine_wave_no_background.png')"),
      div(class = "wave wave2", style = "background-image: url('sine_wave_no_background.png')"),
      div(class = "wave wave3", style = "background-image: url('sine_wave_no_background.png')")
    ),
    h4(
      HTML("Flow Trendmaster 3000"),
      style = 'font-family: fantasy;margin-top: 0.5rem;margin-bottom:0px; font-size: x-large;'
    ),
    style = 'display: inline-flex; margin-top: -1rem; margin-bottom: -0.5rem;'
  ),
  # Trend to Display section
  card(
    min_height = '20vh',
    # fill = F,
    card_header("Trend to Display"),
    card_body(
      # h4("Trend to Display", style = 'margin-bottom:-0.75rem; text-align:center;'),
      # spacer_line_no_margin,
      # var_choice_bit,
      var_and_param_selectors,
      filter_data_Mod_UI('data')
    ),
    style = "background:transparent;"
  ),
  # Station Selection Tools Section
  card(
    min_height = '31vh',
    card_header("Station Selection Tools"),
    card_body(
      # h4("Station Selection Tools", style = 'margin-bottom:-0.75rem;margin-top:-2rem; text-align:center;'),
      # spacer_line_no_margin,
      # uiOutput('current_selected_stations'),
      fluidRow(
        column(width = 6,
               include_non_qaqc_stations_bit
        ),
        column(width = 6,
               multistation_mode
        )
      ),
      map_shape_bit,
      region_selector_bits
    ),
    style = "background:transparent;"
  ),
  # Summary Boxes Section
  # card(
  # min_height = '20vh',
  # card_body(
  # spacer_line_no_margin,
  div(
    access_wsurvey_canada_UI("pull_data"),
    style = 'text-align:center;margin-bottom:2vh;'
  ),
  summary_box_bits
  # ),
  # style = "background:transparent;"
  # )
)

the_sidebar = sidebar(
  width = sidebar_width,
  open = 'always',
  # card(
  #   card_body(
  sidebar_content,
  # min_height = '100%',
  # max_height = '100%'
  # ),
  style = 'background-color:#ADD8E7;'
  # )
)


main_bit = tagList(
  absolutePanel(
    map,
    top = 0, bottom = 0, left = sidebar_width, right = 0),
  absolutePanel(
    HTML("<div class = 'bottombar_button' id = 'toggle_bottombar'> v </div>"),
  ),
  absolutePanel(
    id = 'trend_selector',
    top = '50%', left = sidebar_width, right = '0%', height = '50%',
    card(
      height = '100%',
      card_body(
        full_screen = TRUE,
        bslib::navset_card_pill(
          id = 'tabset',
          nav_panel(title = 'Flow Metric Plot', station_plot),
          nav_panel(title = 'Hydrograph', hydrograph),
          nav_panel(title = 'Tabular Data', data_table_bit),
          nav_panel(title = 'Data Download', data_download_bit)
        )
      )
    )
  ),
  div(
    textOutput('data_recency'),
    style = 'left:112vh !important;top:87vh; position:relative;'
  )
)

my_theme = bs_theme(bootswatch = 'flatly',
                    version = "5",
                    # danger = "#cc0000",
                    # primary = '#3399ff',
                    # "sidebar-bg" = '#ADD8E7',
                    font_scale = 0.75) |>
  # bs_add_rules("#trend_selector {opacity:0.5;}
  #               #trend_selector:hover{opacity:1;}
  #               #reset_station_sel{background-color:#2c3e50;}
  #               #reset_shape_sel{background-color:#2c3e50;}
  #               #select_all_stats_in_shape{background-color:#2c3e50;}
  #               #pull_data-search_wsurvey_dat{background-color:#2c3e50;}
  #               .action-button{transition: background-color 0.3s;}
  #               .action-button:hover{background-color: hsl(207, 70%, 45%);}")
  bs_add_rules("
.action-button {
  background-color: #2c3e50;
  transition: background-color 0.3s !important;
}

.action-button:hover {
  background-color: hsl(207, 70%, 45%) !important;
}

.my_download_button {
  background-color: #2c3e50;
  transition: background-color 0.3s !important;
}

.my_download_button:hover {
  background-color: hsl(207, 70%, 45%) !important;
}

.hidden-bar {
  transform: translateY(45vh) scale(1, 0);
  transition: all 1s;
}

.bottombar {
  transition:all 1s;
  opacity: 0.25;
}

.bottombar:hover {
  opacity: 1;
}

.shown-bar {
  /*transform: translateY(0vh) scale(1, 1);*/
  display: hidden;
  transition: all 1s;
}

.hidden-bar-button {
  transform: rotate(180deg);
}

.bottombar_button {
  transition:all 1s;
  opacity: 0.25;
  width:40px;
  height:40px;
  border: solid 2px black;
  background-color: white;
  font-size:x-large;
  text-align:center;
  border-radius:25%;
  position:absolute;
  top:85vh;
  left: -1.5vh;
  z-index:1000;
}

.bottombar_button:hover {
  opacity: 1;
}

.sidebar {
  background-color: #ADD8E7;
  /*overflow-y:hidden !important;*/
}

.trend_selector {opacity:0.5;}
.trend_selector:hover{opacity:1;}
.reset_station_sel{background-color:#2c3e50;}
.reset_shape_sel{background-color:#2c3e50;}
.select_all_stats_in_shape{background-color:#2c3e50;}
.pull_data-search_wsurvey_dat{background-color:#2c3e50;}

.wave-container {
  position: relative;
  width: 8vh;
  height: 6vh;
  left: 1%;
  overflow: hidden;
}

.wave {
  position: absolute;
  top: 0;
  left: 0;
  height: 4vh;
  width: 10vh;
  background-repeat: no-repeat;
  background-size: contain;
  animation: animate 5s infinite;
  animation-timing-function: ease-in-out;
}

.wave2 {
 animation-delay: 1s;
 top:20%;
 left:10%;
}

.wave3 {
 animation-delay: 2s;
 top:40%;
}

.tooltiphide {
  margin-top: 50px;
  margin-top: 50px;
}

@keyframes animate {
        0%, 100% {
          transform: translateX(0vh) scaleY(1, 1);
        }
        50% {
          transform: translateX(-1vh) scale(1, 0.75);
        }
      }
")
# bs_add_rules(sass::sass_file('www/css/my_css.scss'))

# if(!str_detect(getwd(),".*www$")){
#   head_scripts = tags$head(
#     tags$script('www/js/my_js.js'),
#     tags$link(rel = 'stylesheet', type = 'text/css', href = 'www/css/my_css.css')
#   )
# } else {
#   head_scripts = tags$head(
#     tags$script('js/my_js.js'),
#     tags$link(rel = 'stylesheet', type = 'text/css', href = 'css/my_css.css')
#   )
# }

ui = bslib::page_fillable(

  # includeCSS('www/css/my_css.css'),
  # tags$head(tags$style())
  # head_script_css,

  useShinyjs(),

  title = 'Flow Trendmaster 3000',

  theme = my_theme,

  shinyFeedback::useShinyFeedback(),

  layout_sidebar(
    sidebar = the_sidebar,
    main_bit
  )
)
