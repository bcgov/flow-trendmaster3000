filter_data_Mod_UI = function(id){
  ns = NS(id)

  scale_selector_bit = radioButtons(
    ns('scale_selector_radio'),
    label = 'Time Scale',
    choices = c('Annual',
                'Monthly',
                'Seasonal',
                'Select Dates'),
    selected = 'Annual')

  period_choice_bit = radioButtons(
    ns('user_period_choice'),
    label = 'Oldest data to include',
    choices = c('2010' = '2010+',
                '1990' = '1990+',
                'All' = 'all'),
    selected = 'all',
    inline = F)

  tagList(
    scale_selector_bit,
    period_choice_bit,
    uiOutput(ns('finegrained_data_filter_ui')
  )
  )
}

# Module server filters data based on inputs from user.
filter_data_Mod_Server = function(id, flow_dat_daily){

  moduleServer(
    id,
    function(input, output, session){

      determine_finegrain_selector_ui = function(id, scale){

        ns <- NS(id)

        if(scale == 'Annual') out = h5("")

        if(scale == 'Monthly'){
          out = selectizeInput(ns('finegrain_selector'),
                               label = 'Month Selector',
                               multiple = F,
                               choices = c('Jan' = 1,'Feb' = 2,
                                           'Mar' = 3,'Apr' = 4,
                                           'May' = 5,'Jun' = 6,
                                           'Jul' = 7,'Aug' = 8,
                                           'Sep' = 9,'Oct' = 10,
                                           'Nov' = 11,'Dec' = 12))
        }
        if(scale == 'Seasonal'){
          out = selectizeInput(ns('finegrain_selector'),
                               label = 'Season Selector',
                               multiple = F,
                               choices = c('Winter (Dec-Feb)' = 'winter',
                                           'Spring (Mar-May)' = 'spring',
                                           'Summer (Jun-Aug)' = 'summer',
                                           'Autumn (Sep-Nov)' = 'autumn'))
        }
        if(scale == 'Select Dates'){
          out = tagList(
            fluidRow(
              column(width = 6,
                     numericInput(ns('finegrain_selector'),
                                  label = 'Start Month',
                                  min = 1, max = 12,
                                  value = NA)
              ),
              column(width = 6,
                     numericInput(ns('start_day'),
                                  label = 'Start Day',
                                  min = 1,
                                  max = 31,
                                  value = NA)
              )
            ),
            fluidRow(
              column(width = 6,
                     numericInput(ns('end_month'),
                                  label = 'End Month',
                                  min = 1, max = 12,
                                  value = NA)
              ),
              column(width = 6,
                     numericInput(ns('end_day'),
                                  label = 'End Day',
                                  min = 1,
                                  max = 31,
                                  value = NA)
              )
            )
          )
        }
        return(out)
      }

      output$finegrained_data_filter_ui = renderUI({
        determine_finegrain_selector_ui(id, input$scale_selector_radio)
      })

      finegrained_selector = reactive({
        if(input$scale_selector_radio == 'Annual') 'Annual'
        else input$finegrain_selector
      })

      finegrained_date_filter = function(dat, scale_selector, finegrain_selector){
        req(!is.null(finegrained_selector()))
        # browser()
        withProgress(message = 'applying date filter', {
          # req() tells the app to not start solving this reactive expression until
          # we have a value for the scale selection radio buttons! This delay
          # avoids errors while these buttons are loading.
          # if(scale_selector == 'Select Dates'){
          #   req(input$start_month, input$start_day, input$end_month, input$end_day)
          # }
          #In the case of annual timescale, do no filtering here.

          #Update progress bar...
          incProgress(1 / 2)

          #In the case of monthly timescale, filter down to month of interest.
          if(scale_selector == 'Monthly'){
            dat = dat %>%
              filter(Month == finegrain_selector[1])
            #Update progress bar...
            incProgress(1 / 2)
          }

          if(scale_selector == 'Seasonal'){
            dat = dat %>%
              mutate(season = case_when(
                Month %in% c(12,1,2) ~ 'winter',
                Month %in% c(3:5) ~ 'spring',
                Month %in% c(6:8) ~ 'summer',
                Month %in% c(9:11) ~ 'autumn'
              )) %>%
              filter(season == finegrain_selector[1])
            #Update progress bar...
            incProgress(1 / 2)
          }

          #If custom time scale, use it here to filter data.
          if(scale_selector == 'Select Dates'){
            start_month = finegrain_selector[1]
            start_day = finegrain_selector[2]
            end_month = finegrain_selector[3]
            end_day = finegrain_selector[4]

            req(start_month, start_day, end_month, end_day)

            # Use {lubridate} to calculate the start and end periods. We use these to filter the data.
            start_period = (months(as.numeric(start_month)) + days(start_day))
            end_period = (months(as.numeric(end_month)) + days(end_day))

            # Perform check that end period is later than start period
            date_check = start_period < end_period
            # If it's not, give a warning.
            shinyFeedback::feedbackWarning("end_month", !date_check, "End date must be later than start date")
            # Date check must be TRUE to proceed.
            req(date_check)

            # Filter data.
            dat = dat %>%
              mutate(Year = year(Date),
                     Month = month(Date),
                     Day = day(Date),
                     this_period = c(months(Month) + days(Day))) %>%
              filter(this_period >= start_period,
                     this_period <= end_period) %>%
              dplyr::select(-this_period,-Day)
            #Update progress bar...
            incProgress(1 / 2)
          }
          return(dat)
        })
      }

      # finegrain_selector_reactive = reactive({
      #   if(is.null(input$finegrain_selector)) return(NULL)
      #   input$finegrain_selector
      # })

      # First filtering cut: time periods -------------------------------
      dat_filtered = reactive({
        # req(!is.null(finegrain_selector_reactive()))
        dat = switch(input$user_period_choice,
                     `2010+` = flow_dat_daily %>% filter(Year >= 2010),
                     `1990+` = flow_dat_daily %>% filter(Year >= 1990),
                     `all` = flow_dat_daily
        )

        if(input$scale_selector_radio == 'Annual') {
          dat = finegrained_date_filter(dat, 'Annual', 'Annual')
          return(dat)
        }

        if(input$scale_selector_radio == 'Monthly') {
          dat = finegrained_date_filter(dat, input$scale_selector_radio, input$finegrain_selector)
          return(dat)
        }

        if(input$scale_selector_radio == 'Seasonal') {
          dat = finegrained_date_filter(dat, input$scale_selector_radio, input$finegrain_selector)
          return(dat)
        }

        if(input$scale_selector_radio == 'Select Dates') {
          dat = finegrained_date_filter(dat,
                                        input$scale_selector_radio,
                                        c(input$finegrain_selector,
                                          input$start_day,
                                          input$end_month,
                                          input$end_day))
          return(dat)
        }
      })

      # Make a reactive list of the finegrain_selector inputs.
      finegrain_reactive_list = reactive({
        if(input$scale_selector_radio == 'Select Dates'){
          return(
            list(finegrain_selector = input$finegrain_selector,
                 start_day = input$start_day,
                 end_month = input$end_month,
                 end_day = input$end_day)
          )
        } else {
          return(
            input$finegrain_selector
          )
        }
      })

      # Return a list of reactive outputs. If the scale selector is for
      # specific dates, return all of the start_month, start_day, end_month, and end_day
        list(
          dat_filtered = reactive(dat_filtered()),
          scale_selector_radio = reactive(input$scale_selector_radio),
          finegrain_reactives_list = reactive(finegrain_reactive_list())
      )
    }
  )
}
