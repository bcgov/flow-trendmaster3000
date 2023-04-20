library(shiny)
library(tidyverse)
library(data.table)

dat = feather::read_feather('C:/tmp/Trendmaster3000_tmpfiles/daily_flow_records.feather')

source('modules/filter_data.R')
source('modules/add_metric_to_dat.R')

ui <- fluidPage(
  filter_data_Mod_UI('dir'),

  selectizeInput(
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
    width = '100%'),

  textOutput('my_nrow'),

  DT::DTOutput('my_dat')
)

server <- function(input, output, session) {

  dat_filtered = filter_data_Mod_Server('dir', dat)

  # Take the filtered data and feed it into the 'add metric' module.
  dat_with_metric = add_metric_to_dat_mod(id = 'data',
                                          flow_dat_daily = dat,
                                          dat = dat_filtered$dat_filtered,
                                          user_var_choice = reactive(input$user_var_choice))

  output$my_nrow = renderText({
    nrow(dat_filtered$dat_filtered())
  })

  output$my_dat = DT::renderDT(dat_with_metric())
}

shinyApp(ui, server)
