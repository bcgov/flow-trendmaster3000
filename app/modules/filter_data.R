filter_data_Mod_UI = function(id){
  ns = NS(id)

  scale_selector_bit = radioButtons(
    ns('scale_selector_radio'),
    label = 'Dates to Include',
    choices = c('All' = 'Annual',
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

  # tagList(
  #   layout_column_wrap(
  #     width = 1/2,
  #     scale_selector_bit,
  #     period_choice_bit),
  #   uiOutput(ns('finegrained_data_filter_ui')
  #   )
  # )
  fluidRow(
    column(width = 6,
      scale_selector_bit
      ),
    column(width = 6,
      period_choice_bit
      ),
    uiOutput(ns('finegrained_data_filter_ui')
    )
  )
}

# Module server filters data based on inputs from user.
filter_data_Mod_Server = function(id, include_low_qual_data, stations, number_station_cutoff){

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
      # 1. If All, no filtering performed.
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
            # dat = dat[meets_dat_qual_check == T]
            dat = dat
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

      ## READ IN DATASETS ##
      # Entire flow dataset (to be used only for specific subset of stations)
      # flow_dat_all = qs::qread("daily_flow_records.qs")
      flow_dat_all = qs::qread("weekly_flow_records.qs")

      # All summaries (one with all available data, one for just 1990 or
      # more recent, and one with 2010 or more recent)
      flow_dat_annual = qs::qread("ann_flow_summary.qs")
      flow_dat_annual_1990 = qs::qread("ann_flow_summary_1990.qs")
      flow_dat_annual_2010 = qs::qread("ann_flow_summary_2010.qs")

      # set data as a data.table object.
      setDT(flow_dat_all)

      dat_filtered = reactive({

        # If user has selected 'All', rely on presummarized data.
        # Respond to user period selection (all,1990 - present, or 2010 - present)
        # Pass this on as a data.table
        if(input$scale_selector_radio == 'Annual') {
          dat = data.table::as.data.table(
            switch(input$user_period_choice,
                   `all` = flow_dat_annual,
                   `1990+` = flow_dat_annual_1990,
                   `2010+` = flow_dat_annual_2010
            )
          )

          # Has the user chosen to only display stations that match
          # our filtering criteria? Or, have they chosen to include
          # so-called 'low-quality' data?
          if(include_low_qual_data() == T){
            return(dat |> filter(STATION_NUMBER %chin% stations()$STATION_NUMBER))
          } else {
            return(dat |> filter(STATION_NUMBER %chin% stations()[stations()$meets_dat_qual_check == T,]$STATION_NUMBER))
          }

        } else {
          # If user has selected something more fine-grained than 'Annual',
          # use the daily flow dataset. To make this feasible, the user
          # must have already selected an administrative delineation and thus
          # subset the stations spatial object to a list of selected stations,
          # which we use here to filter the massive daily dataset.
          req(nrow(stations()) <= number_station_cutoff)

          # Has the user chosen to only display stations that match
          # our filtering criteria? Or, have they chosen to include
          # so-called 'low-quality' data?
          if(include_low_qual_data() == T){
            dat = flow_dat_all[STATION_NUMBER %chin% stations()$STATION_NUMBER]
          } else {
            dat = flow_dat_all |> filter(STATION_NUMBER %chin% stations()[stations()$meets_dat_qual_check == T,]$STATION_NUMBER)
          }

          dat = switch(input$user_period_choice,
                       `2010+` = dat[Year >= 2010],
                       `1990+` = dat[Year >= 1990],
                       `all` = dat
          )
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
      }) #%>%
      # bindCache(input$user_period_choice,
      #           input$scale_selector_radio,
      #           # unlist(input$finegrain_selector),
      #           finegrain_reactive_list())

      # Reactive shiny Feedback: if the number of stations currently
      # selected is over our cut-off
      # but a user has chosen either monthly data
      # or custom date ranges, give a warning on the input.

      observeEvent(input$finegrain_selector, {

        if (nrow(stations() |> distinct()) > number_station_cutoff) {
          showFeedbackWarning(
            inputId = "finegrain_selector",
            text = paste0("Please use the map to reduce the number of stations for calculations ",
                          "(",nrow(stations() |> distinct())," currently in scope)")
          )
        } else {
          hideFeedback("finegrain_selector")
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
        user_period_choice = reactive(input$user_period_choice),
        scale_selector_radio = reactive(input$scale_selector_radio),
        finegrain_reactives_list = reactive(finegrain_reactive_list())
      )
    }
  )
}
