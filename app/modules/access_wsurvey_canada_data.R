library(rvest)
library(httr)
library(tidyverse)


access_wsurvey_canada_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('search_wsurvey_dat'),'Get Daily Flow Data from WSC\n for select stations'),
  )
}

access_wsurvey_canada_Server <- function(id, station_number_list) {
  moduleServer(
    id,
    function(input, output, session) {

      # Create blank reactive Val for API flow dat.
      api_flowdat = reactiveVal()

      observeEvent(input$search_wsurvey_dat, {

        stations_to_download = station_number_list()

        if(stations_to_download[1] != 'no_selection'){

        for(i in 1:length(stations_to_download)) {

          station_number = stations_to_download[i]
          # station_number = '08CA003' # Discontinued.
          # station_number = '08CA001' # Discontinued.
          # station_number = '08DA005' # Active.

          shiny::withProgress(
            message = paste0('Downloading data for ',station_number),
            detail = "Establishing session...",
            value = 0, {

              my_session = rvest::session(paste0('https://wateroffice.ec.gc.ca/search/historical_results_e.html?search_type=station_number&station_number=',station_number,'&start_year=1850&end_year=2023&minimum_years=&gross_drainage_operator=%3E&gross_drainage_area=&effective_drainage_operator=%3E&effective_drainage_area='))

              incProgress(1/5, detail = 'session established...')

              search_res_page = session_jump_to(my_session, paste0('https://wateroffice.ec.gc.ca/report/data_availability_e.html?type=historical&station=',station_number,'&parameter_type=Flow+and+Level'))

              download_page = session_jump_to(search_res_page, paste0('https://wateroffice.ec.gc.ca/download/index_e.html?results_type=historical'))

              response = session_jump_to(download_page, '/download/report_e.html?dt=dd&df=ddf&md=0&ext=csv&ext=csv')

              incProgress(1/5, detail = 'navigated to download page...')

              wsurvey_data = content(response$response, 'text')

              # TEST #
              # stringr::str_trunc(wsurvey_data, 100)

              incProgress(1/5, detail = 'downloaded data. Parsing...')

              # Trim front bit off of data.
              wsurvey_data = stringr::str_remove(wsurvey_data, '.*PARAM \\= [0-9]+\\)\\r\\n ')

              ws_data_parsed = read_csv(wsurvey_data)
              # ws_data_parsed = read_csv(substr(wsurvey_data, 78, nchar(wsurvey_data)))

              incProgress(1/5, detail = 'Data parsed')

              ws_data_parsed$STATION_NUMBER = station_number
              ws_data_parsed$ID = NULL
              ws_data_parsed$record_date = ws_data_parsed$Date
              ws_data_parsed$Date = NULL

        api_flowdat(dplyr::bind_rows(api_flowdat(), ws_data_parsed))

        incProgress(1/5, detail = 'Complete')

          }) # End of withProgress.
        } # End of for loop.
        }
      })

        # output$number_rows = renderText({nrow(api_flowdat())})

        list(web_download = reactive(api_flowdat()))
    }
  )
}
