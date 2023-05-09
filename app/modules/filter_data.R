filter_data_Mod_UI = function(id){
  ns = NS(id)

  scale_selector_bit = radioButtons(
    ns('scale_selector_radio'),
    label = 'Time Scale',
    choices = c('Annual',
                'Monthly',
                #'Seasonal',
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
    layout_column_wrap(
      width = 1/2,
      scale_selector_bit,
      period_choice_bit),
    uiOutput(ns('finegrained_data_filter_ui')
  )
  )
}

# Module server filters data based on inputs from user.
filter_data_Mod_Server = function(id, flow_dat_daily, stations, include_poor_qaqc_data){

  moduleServer(
    id,
    function(input, output, session){

      # Function to generate a user interface based on the
      # 'Time Scale' the user has chosen.
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

      # Reactive of user's Time Scale filter choice.
      # 1. If Annual, no filtering performed.
      # 2. If monthly, keeps data only for a given month.
      # 4. If 'Select Dates', keeps data between start month and day and end month and day.
      finegrain_selector_reactive = reactive({
        if(input$scale_selector_radio == 'Annual') return('Annual')
        input$finegrain_selector
      })

      finegrained_date_filter = function(dat, scale_selector, finegrain_selector){

        withProgress(message = 'applying date filter', {
          # req() tells the app to not start solving this reactive expression until
          # we have a value for the scale selection radio buttons! This delay
          # avoids errors while these buttons are loading.

          #Update progress bar...
          incProgress(1 / 2)

          if(scale_selector == 'Annual'){
            dat = dat[meets_dat_qual_check == T]
          }
          #In the case of monthly timescale, filter down to month of interest.
          if(scale_selector == 'Monthly'){
            dat = dat[Month == finegrain_selector[1]]
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

            dat = dat[Month %in% c(start_month,end_month),
              .(.SD[,], Day = mday(Date))
            ][ (Month == start_month & Day >= start_day) | (Month > start_month & Month < end_month) | (Month == end_month & Day <= end_day),
            ]

            # setDF(dat_filtered)
            return(dat)
            #Update progress bar...
            incProgress(1 / 2)
          }
          return(dat)
        })
      }


      # dat_tscale_filtered = reactive({
      dat_filtered = reactive({
        #req(!is.null(finegrain_selector_reactive()) | input$scale_selector_radio == 'Annual')

        # If the user has chosen to include 'poor-quality' data,
        # filter the dataset so that it only includes the selected stations.
        dat = switch(input$user_period_choice,
                     `2010+` = flow_dat_daily[Year >= 2010],
                     `1990+` = flow_dat_daily[Year >= 1990],
                     `all` = flow_dat_daily
        )

        if(input$scale_selector_radio == 'Annual') {
          return(finegrained_date_filter(dat, 'Annual', 'Annual'))
        }

        if(input$scale_selector_radio == 'Monthly') {
          req(input$finegrain_selector)
          return(finegrained_date_filter(dat, input$scale_selector_radio, input$finegrain_selector))
        }

        if(input$scale_selector_radio == 'Select Dates') {
          return(
            finegrained_date_filter(dat,
                                    input$scale_selector_radio,
                                    c(input$finegrain_selector,
                                      input$start_day,
                                      input$end_month,
                                      input$end_day))
          )
        }
      }) %>%
        bindCache(input$user_period_choice,
                  input$scale_selector_radio,
                  # unlist(input$finegrain_selector),
                  finegrain_reactive_list())

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
          user_period_choice = reactive(input$user_period_choice),
          scale_selector_radio = reactive(input$scale_selector_radio),
          finegrain_reactives_list = reactive(finegrain_reactive_list())
      )
    }
  )
}
