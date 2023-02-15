

output$month_selector_ui = renderUI({
  req(input$scale_selector_radio)
  if(!input$scale_selector_radio == 'Monthly') return(NULL)
  selectizeInput(inputId = 'time_selector',
                 label = 'Time Selector',
                 multiple = F,
                 choices = c('Jan' = 1,'Feb' = 2,
                             'Mar' = 3,'Apr' = 4,
                             'May' = 5,'Jun' = 6,
                             'Jul' = 7,'Aug' = 8,
                             'Sep' = 9,'Oct' = 10,
                             'Nov' = 11,'Dec' = 12))
})

output$season_selector_ui = renderUI({
  req(input$scale_selector_radio)
  if(input$scale_selector_radio != 'Seasonal') return(NULL)
  selectizeInput(inputId = 'season_selector',
                 label = 'Season Selector',
                 multiple = F,
                 choices = c('Winter (Dec-Feb)' = 'winter',
                             'Spring (Mar-May)' = 'spring',
                             'Summer (Jun-Aug)' = 'summer',
                             'Autumn (Sep-Nov)' = 'autumn'),
                 )
})

output$custom_range_selector_ui = renderUI({
  req(input$scale_selector_radio)
  if(!input$scale_selector_radio == 'Custom Date Range') return(NULL)
  tagList(
  fluidRow(
    column(width = 6,
           numericInput(inputId = 'start_month',
                          label = 'Start Month',
                          min = 1, max = 12,
                          value = NA)
    ),
    column(width = 6,
           numericInput(inputId = 'start_day',
                        label = 'Start Day',
                        min = 1,
                        max = 31,
                        value = NA)
    )
  ),
  fluidRow(
    column(width = 6,
           numericInput(inputId = 'end_month',
                          label = 'End Month',
                          min = 1, max = 12,
                          value = NA)
    ),
    column(width = 6,
           numericInput(inputId = 'end_day',
                        label = 'End Day',
                        min = 1,
                        max = 31,
                        value = NA)
    )
  )
)
})
