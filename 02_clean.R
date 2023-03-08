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

if(!exists("number_daily_records_per_station")){load('./tmp/station_data.Rdata')}

#Pulling in list of filtered stations from Jon Goetz work (see 'trending_station_selection.R' script)
station_list_filtered = read.csv('data/finalstns.csv') %>% as_tibble()

stations_to_keep = station_list_filtered$STATION_NUMBER

stations_to_exclude = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_keep) %>%
  dplyr::select(STATION_NUMBER) %>%
  distinct() %>%
  pull(STATION_NUMBER)

# Apply station filter to data.
number_daily_records_per_station = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_exclude) %>%
  as_tibble()

# save(stations_to_keep, stations_to_exclude, number_daily_records_per_station, file = './tmp/station_data_cleaned.Rdata')
save(stations_to_keep, stations_to_exclude, number_daily_records_per_station, file = './tmp/station_data_cleaned.Rdata')
