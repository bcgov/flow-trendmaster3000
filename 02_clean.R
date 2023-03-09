# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#Pulling in list of filtered stations from script 01_load.R (Jon Goetz's work)
station_list_filtered = read.csv('data/finalstns.csv') %>% as_tibble()

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Daily Values ------------------------------------------------

flow_dat = tidyhydat::hy_daily_flows(station_list_filtered$STATION_NUMBER) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value))

feather::write_feather(flow_dat, 'app/www/all_flow_dat.feather')

# Get station locations
stations_sf = tidyhydat::hy_stations(station_number = stations_to_keep) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)

write_sf(stations_sf, 'app/www/stations.gpkg')
