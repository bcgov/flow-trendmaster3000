get_station_sf = function(stations_list, hydat_path){
  tidyhydat::hy_stations(station_number = stations_list,
                       hydat_path = hydat_path) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)
}
