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

### Load in data that was accessed in the '01_load.R' script.

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)

if(!exists("final_stations_summary")){final_stations_summary = read_csv('data_preprocessing/data/included_stations_and_years.csv')}

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Pull out the stations to keep from the loading script.
stations_to_keep = final_stations_summary$STATION_NUMBER

# Get station locations
stations_sf = tidyhydat::hy_stations(prov_terr_state_loc = 'BC') %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)

all_bc_data = qs::qread('app/www/daily_flow_records.qs')

# Drop stations that have no data.
# stations_sf_data_rows = stations_sf %>%
#   mutate(nrows_data = map(STATION_NUMBER, ~ {
#     tryCatch(nrow(hy_daily_flows(station_number = .x)),
#              error = function(e) 0)
#   }))

stations_sf = stations_sf %>%
  filter(STATION_NUMBER %in% unique(all_bc_data$STATION_NUMBER))

# Add column indicating if station meets our filtering criteria or not.
stations_sf = stations_sf %>%
  left_join(tibble(STATION_NUMBER = stations_to_keep,
                   meets_criteria = TRUE)) %>%
  mutate(meets_criteria = replace_na(meets_criteria, FALSE))

# Add in which of each spatial delineation of BC each station is in.
# Doing this here saves us some processing power in the shiny app.

ecoprov = read_sf('app/www/ecoprovinces.gpkg')
ecoreg = read_sf('app/www/ecoregions.gpkg')
ecosec = read_sf('app/www/ecosections.gpkg')
nr_dist = read_sf('app/www/nr_districts.gpkg')
nr_reg = read_sf('app/www/nr_regions.gpkg')
subw = read_sf('app/www/subw.gpkg')

stations_sf = stations_sf %>%
  st_join(ecoprov %>% select(ecoprov = shape_name), st_intersects) %>%
  st_join(ecoreg %>% select(ecoreg = shape_name), st_intersects) %>%
  st_join(ecosec %>% select(ecosec = shape_name), st_intersects) %>%
  st_join(nr_dist %>% select(nr_dist = shape_name), st_intersects) %>%
  st_join(nr_reg %>% select(nr_reg = shape_name), st_intersects) %>%
  st_join(subw %>% select(subw = shape_name), st_intersects)
#
# stations_sf %>%
#   st_join(read_sf('app/www/ecoprovinces.gpkg'), st_intersects)
write_sf(stations_sf, 'app/www/stations.gpkg')

