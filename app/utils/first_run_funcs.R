
# First time running the app - generate feather data file + filtered station list.
first_time_file_generator = function(temporary_folder){

  # Download database, if it's not in the temporary folder.
  browser()
  withProgress(message = 'Performing one-time download of HYDAT database (takes 5-10 minutes)', {

    if(!file.exists(paste0(temporary_folder,"Hydat.sqlite3"))){
    tidyhydat::download_hydat(dl_hydat_here = temporary_folder,
                              ask = F)
    }

    tidyhydat::hy_set_default_db(hydat_path = temporary_folder)

    incProgress(amount = 1/5, message = 'database downloaded to folder...')

    source('Jon_G_station_filter_script.R')

    final_stations_summary = jon_g_filtering_steps(my_path = paste0(temporary_folder,'Hydat.sqlite3'))

    incProgress(1/5, message = 'one-time filtering of stations performed...')

    write.csv(final_stations_summary, paste0(temporary_folder,'filtered_station_list.csv'),
              row.names = F)

    stations_to_keep = read_csv(paste0(temporary_folder,'filtered_station_list.csv'))
    print("Stations to keep CSV read!")

    incProgress(1/5, message = 'One-time filtering complete')

    # Read in all of the daily flow records for the list of stations that we will keep
    flow_dat_daily_to_write = tidyhydat::hy_daily_flows(station_number = stations_to_keep$STATION_NUMBER,
                                                        hydat_path = paste0(temporary_folder,'Hydat.sqlite3')) %>%
      filter(Parameter == 'Flow') %>%
      filter(!is.na(Value)) %>%
      mutate(Month = month(Date),
             Year = year(Date))

    # Now apply specific year filters that vary for each station (thanks to Jon Goetz
    # for doing the heavy lifting of setting up this table! See his cited script above
    # for details).
    flow_dat_daily_to_write = unique(stations_to_keep$STATION_NUMBER) %>%
      map( ~ {
        year_for_trimming = unique(stations_to_keep[stations_to_keep$STATION_NUMBER == .x,]$Min_Year)

        station_dat = flow_dat_daily_to_write %>%
          filter(STATION_NUMBER == .x)

        filtered_dat = station_dat %>%
          filter(Year >= year_for_trimming)

        filtered_dat
      }) %>%
      bind_rows()

    incProgress(amount = 1/5, message = 'Pulled daily records for chosen stations from database')

    feather::write_feather(flow_dat_daily_to_write,
                           paste0(temporary_folder,'daily_flow_records.feather'))

    incProgress(amount = 1/5, message = 'Written daily data file to your chosen folder')
  })
}
