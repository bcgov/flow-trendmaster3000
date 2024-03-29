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

# The following script was shared with me (Chris Madsen) by its developper:
# Jon Goetz. It uses various criteria to filter out flow recording stations of BC.

## Station selection script

# Annual Station filtering:
# - filter all flow stations with at least 10 years of annual data
# - filter out stations upstream of other stations on same stream
# - filter out heavily regulated systems (interpreted)
# Year filtering:
# - for regulated streams, start year of regulation start (in HYDAT or interpreted)
# - filter out years with very large data gaps before continuous data (interpreted)

library(tidyverse)
library(lubridate)
library(tidyhydat)
library(fasstr)
library(stringr)
library(EnvStats)
library(data.table)
library(sf)

calculate_metrics = function(dat){
# browser()
  if(is.data.frame(dat)) dat = data.table::as.data.table(dat)
    dat_mean = dat[, .(Median = median(Value,na.rm=T)), by = .(STATION_NUMBER,Year)]

    # Using data.table notation, calculate the day of freshet onset.
    # (first, calculate the total flow, the flow-to-date, and the row #,
    #  then, take the first row where the flow to date is greater than half
    #  the total annual flow, by station # and year;
    #  finally, calculate the day of the year (from 1 to 365) for that date.)
    freshet_dat = dat

    interim_data = freshet_dat[,
                      `:=`(
                        TotalFlow = sum(Value),
                        FlowToDate = cumsum(Value)),
                      by = .(STATION_NUMBER,Year)
    ] |>
      dplyr::group_by(STATION_NUMBER,Year) |>
      dplyr::group_split() |>
      purrr::map(\(df) df[which.min(abs(df$TotalFlow/2 - df$FlowToDate)),]) |>
      dplyr::bind_rows() |>
      data.table::as.data.table()

    freshet_dat = interim_data[
      ,.(STATION_NUMBER,Year,DoY_50pct_TotalQ = data.table::yday(Date))
    ]

    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)
    min_7_dat = dat

    interim_data = min_7_dat[,Min_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
                      by = .(STATION_NUMBER,Year)
    ] |>
      dplyr::group_by(STATION_NUMBER,Year) |>
      dplyr::group_split() |>
      purrr::map(\(df) df[which.min(df$Min_7_Day),]) |>
      dplyr::bind_rows() |>
      dplyr::rename(Min_7_Date = Date) |>
      data.table::as.data.table()

    min_7_dat = interim_data[,
                      .(STATION_NUMBER,Min_7_Date,Year, Min_7_Day, Min_7_Day_DoY = data.table::yday(Min_7_Date))
    ]

    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)
    min_30_dat = dat

    interim_data = min_30_dat[,Min_30_Day := data.table::frollmean(Value, 30, align = 'right', fill = NA),
                      by = .(STATION_NUMBER,Year)
    ] |>
      dplyr::group_by(STATION_NUMBER,Year) |>
      dplyr::group_split() |>
      purrr::map(\(df) df[which.min(df$Min_30_Day),]) |>
      dplyr::bind_rows() |>
      dplyr::rename(Min_30_Date = Date) |>
      data.table::as.data.table()

    min_30_dat = interim_data[,
                      .(STATION_NUMBER,Min_30_Date,Year, Min_30_Day, Min_30_Day_DoY = data.table::yday(Min_30_Date))
    ]

    # Using data.table notation,
    # 1. Calculate row numbers by station # and Year.
    # 2. Calculate 7-day rolling average of flow.
    # 3. Just take lowest 7-day flow (to find the low-flow)
    max_7_dat = dat

    interim_data = max_7_dat[,Max_7_Day := data.table::frollmean(Value, 7, align = 'right', fill = NA),
                      by = .(STATION_NUMBER,Year)
    ] |>
      dplyr::group_by(STATION_NUMBER,Year) |>
      dplyr::group_split() |>
      purrr::map(\(df) df[which.max(df$Max_7_Day),]) |>
      dplyr::bind_rows() |>
      dplyr::rename(Max_7_Date = Date) |>
      data.table::as.data.table()

    max_7_dat = interim_data[,
                      .(STATION_NUMBER,Max_7_Date,Year, Max_7_Day, Max_7_Day_DoY = data.table::yday(Max_7_Date))
    ]

    # Surely there's a better way to join these guys...
    return(
      left_join(dat_mean,
                left_join(freshet_dat,
                          left_join(min_7_dat,
                                    left_join(min_30_dat,max_7_dat))))
      )
}

tidyhydat::download_hydat(ask = F)

##### First pass to filter for stations with complete data

## Filter stations for last n years of data, minimum number of years
year_filt <- year(Sys.Date())-5
n_years_filt <- 10 # will likely use >10 years in final, but this reduces number for first pass

## Get all BC stations with "flow"
stations_all_bc_list <- unique(hy_annual_stats(prov_terr_state_loc = "BC") %>%
                                 filter(Parameter == "Flow") %>%
                                 pull(STATION_NUMBER))

## Get HYDAT data for all flow stations
if(!dir.exists('data')) dir.create('data')

hydat_daily_all <- hy_daily_flows(station_number = stations_all_bc_list)

## Filter stations for n complete years
stations_filt <- hydat_daily_all %>%
  #   fill_missing_dates() %>%
  mutate(Year = year(Date)) %>%
  group_by(STATION_NUMBER, Year) %>%
  summarise(Ann_Mean = mean(Value, na.rm = FALSE),
            na = sum(is.na(Value))) %>%
  group_by(STATION_NUMBER) %>%
  summarise(n_years = sum(!is.na(Ann_Mean)),
            na_years = sum(is.na(Ann_Mean)),
            year_min = min(Year),
            year_max = max(Year)) %>%
  filter(#year_max >= year_filt,
         n_years >= n_years_filt)%>%
  left_join(hy_stations(), by = "STATION_NUMBER") %>%
  left_join(hy_stn_regulation(), by = "STATION_NUMBER")

stations_filt_list <- unique(stations_filt$STATION_NUMBER)


## manual check for multiple stations on same river (choosing most downstream station)
## filter if station names
check_dup_all <- stations_filt %>%
  mutate(Name = word(STATION_NAME,1,2))%>%
  select(STATION_NUMBER, Name, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE)
check_dup_stations <- check_dup_all %>%
  filter(duplicated(Name)) %>% # for some reason doesnt keep all duplicates, so next line required to do again
  arrange(Name)
check_dup_stations <- check_dup_all %>%
  filter(Name %in% unique(check_dup_stations$Name))
# write.csv(check_dup_stations, "station_selection/dups.csv")
## Manual checking for drainage basin area size, and physical location
# stns_checking_stream <- check_dup_all %>%
#   filter(Name == toupper("SALMON RIVER"))
# stns_checking_sf <-  stns_checking_stream %>%
#   mutate(STATION= paste0(STATION_NUMBER, " - ", STATION_NAME)) %>%
#   select(STATION, LONGITUDE, LATITUDE) %>%
#   sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
#            crs = 4326)
# mapview::mapview(stns_checking_sf)


stns_dup_table <- tibble::tribble(
  ~STATION_NUMBER,               ~Name,                                      ~STATION_NAME, ~STATION_KEEP, ~REP_STATION,
  "08LB069",    "BARRIERE RIVER",               "BARRIERE RIVER BELOW SPRAGUE CREEK",         FALSE, "08LB020",
  "08LB020",    "BARRIERE RIVER",                      "BARRIERE RIVER AT THE MOUTH",          TRUE, "08LB020",
  "08LC042",    "BESSETTE CREEK",        "BESSETTE CREEK ABOVE LUMBY LAGOON OUTFALL",         FALSE, "08LC039",
  "08LC039",    "BESSETTE CREEK",            "BESSETTE CREEK ABOVE BEAVERJACK CREEK",          TRUE, "08LC039",
  "08MB007",         "BIG CREEK",                  "BIG CREEK BELOW GRAVEYARD CREEK",         FALSE, "08MB006",
  "08MB006",         "BIG CREEK",                  "BIG CREEK ABOVE GROUNDHOG CREEK",          TRUE, "08MB006",
  "08ME023",      "BRIDGE RIVER", "BRIDGE RIVER (SOUTH BRANCH) BELOW BRIDGE GLACIER",         FALSE, "08ME028",
  "08ME028",      "BRIDGE RIVER",                  "BRIDGE RIVER ABOVE DOWNTON LAKE",          TRUE, "08ME028",
  "08EE003",     "BULKLEY RIVER",                       "BULKLEY RIVER NEAR HOUSTON",         FALSE, "08EE005",
  "08EE004",     "BULKLEY RIVER",                           "BULKLEY RIVER AT QUICK",         FALSE, "08EE005",
  "08EE005",     "BULKLEY RIVER",                      "BULKLEY RIVER NEAR SMITHERS",          TRUE, "08EE005",
  "08GA072",   "CHEAKAMUS RIVER",               "CHEAKAMUS RIVER ABOVE MILLAR CREEK",         FALSE, "08GA043",
  "08GA043",   "CHEAKAMUS RIVER",                 "CHEAKAMUS RIVER NEAR BRACKENDALE",          TRUE, "08GA043",
  "08MA002",      "CHILKO RIVER",            "CHILKO RIVER AT OUTLET OF CHILKO LAKE",         FALSE, "08MA001",
  "08MA001",      "CHILKO RIVER",                       "CHILKO RIVER NEAR REDSTONE",          TRUE, "08MA001",
  "08MH016",  "CHILLIWACK RIVER",    "CHILLIWACK RIVER AT OUTLET OF CHILLIWACK LAKE",         FALSE, "08MH001",
  "08MH103",  "CHILLIWACK RIVER",              "CHILLIWACK RIVER ABOVE SLESSE CREEK",         FALSE, "08MH001",
  "08MH001",  "CHILLIWACK RIVER",              "CHILLIWACK RIVER AT VEDDER CROSSING",          TRUE, "08MH001",
  "08LG048",   "COLDWATER RIVER",                   "COLDWATER RIVER NEAR BROOKMERE",         FALSE, "08LG010",
  "08LG010",   "COLDWATER RIVER",                       "COLDWATER RIVER AT MERRITT",          TRUE, "08LG010",
  "08NA002",    "COLUMBIA RIVER",                      "COLUMBIA RIVER AT NICHOLSON",         FALSE, "08NE049",
  "08NB005",    "COLUMBIA RIVER",                         "COLUMBIA RIVER AT DONALD",         FALSE, "08NE049",
  "08NE049",    "COLUMBIA RIVER",                      "COLUMBIA RIVER AT BIRCHBANK",          TRUE, "08NE049",
  "08MF062",  "COQUIHALLA RIVER",              "COQUIHALLA RIVER BELOW NEEDLE CREEK",         FALSE, "08MF068",
  "08MF068",  "COQUIHALLA RIVER",           "COQUIHALLA RIVER ABOVE ALEXANDER CREEK",          TRUE, "08MF068",
  "08MH002",   "COQUITLAM RIVER",                "COQUITLAM RIVER AT PORT COQUITLAM",         FALSE, "08MH141",
  "08MH141",   "COQUITLAM RIVER",             "COQUITLAM RIVER ABOVE COQUITLAM LAKE",          TRUE, "08MH141",
  "08HA002",    "COWICHAN RIVER",                  "COWICHAN RIVER AT LAKE COWICHAN",         FALSE, "08HA011",
  "08HA011",    "COWICHAN RIVER",                       "COWICHAN RIVER NEAR DUNCAN",          TRUE, "08HA011",
  "08NH119",      "DUNCAN RIVER",                    "DUNCAN RIVER BELOW B.B. CREEK",         FALSE, "08NH118",
  "08NH126",      "DUNCAN RIVER",                    "DUNCAN RIVER BELOW DUNCAN DAM",         FALSE, "08NH118",
  "08NH118",      "DUNCAN RIVER",                 "DUNCAN RIVER BELOW LARDEAU RIVER",          TRUE, "08NH118",
  "08HD018",         "ELK RIVER",                    "ELK RIVER ABOVE CAMPBELL LAKE",          TRUE, "08HD018",
  "08NK002",         "ELK RIVER",                              "ELK RIVER AT FERNIE",          TRUE, "08NK002",
  "08NK016",         "ELK RIVER",                             "ELK RIVER NEAR NATAL",         FALSE, "08NK002",
  "08LB024",    "FISHTRAP CREEK",                       "FISHTRAP CREEK NEAR MCLURE",          TRUE, "08LB024",
  "08MH153",    "FISHTRAP CREEK",         "FISHTRAP CREEK AT INTERNATIONAL BOUNDARY",          TRUE, "08MH153",
  "08KA004",      "FRASER RIVER",                          "FRASER RIVER AT HANSARD",         FALSE, "08MF005",
  "08KA005",      "FRASER RIVER",                          "FRASER RIVER AT MCBRIDE",         FALSE, "08MF005",
  "08KA007",      "FRASER RIVER",                         "FRASER RIVER AT RED PASS",         FALSE, "08MF005",
  "08KB001",      "FRASER RIVER",                          "FRASER RIVER AT SHELLEY",         FALSE, "08MF005",
  "08MC018",      "FRASER RIVER",                     "FRASER RIVER NEAR MARGUERITE",         FALSE, "08MF005",
  "08MD013",      "FRASER RIVER",                    "FRASER RIVER AT BIG BAR CREEK",         FALSE, "08MF005",
  "08MF040",      "FRASER RIVER",                   "FRASER RIVER ABOVE TEXAS CREEK",         FALSE, "08MF005",
  "08MH024",      "FRASER RIVER",                          "FRASER RIVER AT MISSION",         FALSE, "08MF005",
  "08MF005",      "FRASER RIVER",                             "FRASER RIVER AT HOPE",          TRUE, "08MF005",
  "08HC001",        "GOLD RIVER",                     "GOLD RIVER BELOW UCONA RIVER",          TRUE, "08HC001",
  "08NB014",        "GOLD RIVER",                    "GOLD RIVER ABOVE PALMER CREEK",          TRUE, "08NB014",
  "08LG041",     "GUICHON CREEK",            "GUICHON CREEK AT OUTLET OF MAMIT LAKE",         FALSE, "08LG004",
  "08LG056",     "GUICHON CREEK",        "GUICHON CREEK ABOVE TUNKWA LAKE DIVERSION",         FALSE, "08LG004",
  "08LG067",     "GUICHON CREEK",                       "GUICHON CREEK AT THE MOUTH",         FALSE, "08LG004",
  "08LG004",     "GUICHON CREEK",                  "GUICHON CREEK NEAR LOWER NICOLA",          TRUE, "08LG004",
  "07FA003",     "HALFWAY RIVER",                 "HALFWAY RIVER ABOVE GRAHAM RIVER",         FALSE, "07FA006",
  "07FA006",     "HALFWAY RIVER",                 "HALFWAY RIVER NEAR FARRELL CREEK",          TRUE, "07FA006",
  "08GD008",    "HOMATHKO RIVER",        "HOMATHKO RIVER AT INLET TO TATLAYOKO LAKE",         FALSE, "08GD004",
  "08GD004",    "HOMATHKO RIVER",                      "HOMATHKO RIVER AT THE MOUTH",          TRUE, "08GD004",
  "08KH010",    "HORSEFLY RIVER",              "HORSEFLY RIVER ABOVE MCKINLEY CREEK",         FALSE, "08KH031",
  "08KH031",    "HORSEFLY RIVER",                "HORSEFLY RIVER ABOVE QUESNEL LAKE",          TRUE, "08KH031",
  "08NF001",    "KOOTENAY RIVER",              "KOOTENAY RIVER AT KOOTENAY CROSSING",         FALSE, "08NG065",
  "08NG065",    "KOOTENAY RIVER",                    "KOOTENAY RIVER AT FORT STEELE",          TRUE, "08NG065",
  "07FB006",      "MURRAY RIVER",               "MURRAY RIVER ABOVE WOLVERINE RIVER",         FALSE, "07FB002",
  "07FB002",      "MURRAY RIVER",                      "MURRAY RIVER NEAR THE MOUTH",          TRUE, "07FB002",
  "08JA017",     "NECHAKO RIVER",              "NECHAKO RIVER BELOW CHESLATTA FALLS",         FALSE, "08JC002",
  "08JC001",     "NECHAKO RIVER",                      "NECHAKO RIVER AT VANDERHOOF",         FALSE, "08JC002",
  "08JC002",     "NECHAKO RIVER",                     "NECHAKO RIVER AT ISLE PIERRE",          TRUE, "08JC002",
  "07ED001",      "NATION RIVER",                 "NATION RIVER NEAR FORT ST. JAMES",         FALSE, "07ED003",
  "07ED003",      "NATION RIVER",                      "NATION RIVER NEAR THE MOUTH",         TRUE, "07ED003",
  "08LG006",      "NICOLA RIVER",                 "NICOLA RIVER NEAR SPENCES BRIDGE",         FALSE, "08LG049",
  "08LG065",      "NICOLA RIVER",            "NICOLA RIVER AT OUTLET OF NICOLA LAKE",         FALSE, "08LG049",
  "08LG049",      "NICOLA RIVER",                   "NICOLA RIVER ABOVE NICOLA LAKE",          TRUE, "08LG049",
  "08LB047",    "NORTH THOMPSON",             "NORTH THOMPSON RIVER AT BIRCH ISLAND",         FALSE, "08LB064",
  "08LB064",    "NORTH THOMPSON",                   "NORTH THOMPSON RIVER AT MCLURE",          TRUE, "08LB064",
  "08NM002",    "OKANAGAN RIVER",                 "OKANAGAN RIVER AT OKANAGAN FALLS",         FALSE, "08NM085",
  "08NM050",    "OKANAGAN RIVER",                      "OKANAGAN RIVER AT PENTICTON",         FALSE, "08NM085",
  "08NM085",    "OKANAGAN RIVER",                       "OKANAGAN RIVER NEAR OLIVER",          TRUE, "08NM085",
  "07EF001",       "PEACE RIVER",                       "PEACE RIVER AT HUDSON HOPE",         FALSE, "07FD010",
  "07FA004",       "PEACE RIVER",                     "PEACE RIVER ABOVE PINE RIVER",         FALSE, "07FD010",
  "07FD002",       "PEACE RIVER",                          "PEACE RIVER NEAR TAYLOR",         FALSE, "07FD010",
  "07FD010",       "PEACE RIVER",                    "PEACE RIVER ABOVE ALCES RIVER",          TRUE, "07FD010",
  "08HB084",   "PUNTLEDGE RIVER",                  "PUNTLEDGE RIVER BELOW DIVERSION",         FALSE, "08HB006",
  "08HB006",   "PUNTLEDGE RIVER",                     "PUNTLEDGE RIVER AT COURTENAY",          TRUE, "08HB006",
  "08KH001",     "QUESNEL RIVER",                          "QUESNEL RIVER AT LIKELY",         FALSE, "08KH006",
  "08KH006",     "QUESNEL RIVER",                       "QUESNEL RIVER NEAR QUESNEL",          TRUE, "08KH006",
  "08HD021",     "QUINSAM RIVER",                 "QUINSAM RIVER AT ARGONAUT BRIDGE",         FALSE, "08HD005",
  "08HD027",     "QUINSAM RIVER",           "QUINSAM RIVER BELOW LOWER QUINSAM LAKE",         FALSE, "08HD005",
  "08HD005",     "QUINSAM RIVER",                "QUINSAM RIVER NEAR CAMPBELL RIVER",          TRUE, "08HD005",
  "08HD006",      "SALMON RIVER",                        "SALMON RIVER NEAR SAYWARD",          TRUE, "08HD006",
  "08HD007",      "SALMON RIVER",                 "SALMON RIVER ABOVE MEMEKAY RIVER",         FALSE, "08HD006",
  "08HD015",      "SALMON RIVER",       "SALMON RIVER ABOVE CAMPBELL LAKE DIVERSION",         FALSE, "08HD006",
  "08HD032",      "SALMON RIVER",       "SALMON RIVER BELOW CAMPBELL LAKE DIVERSION",         FALSE, "08HD006",
  "08KC001",      "SALMON RIVER",                  "SALMON RIVER NEAR PRINCE GEORGE",          TRUE, "08KC001",
  "08LE020",      "SALMON RIVER",                         "SALMON RIVER AT FALKLAND",         FALSE, "08LE021",
  "08LE021",      "SALMON RIVER",                     "SALMON RIVER NEAR SALMON ARM",          TRUE, "08LE021",
  "08MH090",      "SALMON RIVER",               "SALMON RIVER AT 72 AVENUE, LANGLEY",          TRUE, "08MH090",
  "08GA030",     "SEYMOUR RIVER",               "SEYMOUR RIVER NEAR NORTH VANCOUVER",          TRUE, "08GA030",
  "08GA077",     "SEYMOUR RIVER",                 "SEYMOUR RIVER BELOW ORCHID CREEK",         FALSE, "08GA030",
  "08GA079",     "SEYMOUR RIVER",                     "SEYMOUR RIVER ABOVE LAKEHEAD",         FALSE, "08GA030",
  "08LE027",     "SEYMOUR RIVER",                   "SEYMOUR RIVER NEAR SEYMOUR ARM",          TRUE, "08LE027",
  "08LC003",     "SHUSWAP RIVER",                         "SHUSWAP RIVER NEAR LUMBY",         FALSE, "08LC002",
  "08LC018",     "SHUSWAP RIVER",  "SHUSWAP RIVER AT OUTLET OF SUGAR LAKE RESERVOIR",         FALSE, "08LC002",
  "08LC002",     "SHUSWAP RIVER",                       "SHUSWAP RIVER NEAR ENDERBY",          TRUE, "08LC002",
  "08NL007", "SIMILKAMEEN RIVER",                   "SIMILKAMEEN RIVER AT PRINCETON",         FALSE, "08NL038",
  "08NL070", "SIMILKAMEEN RIVER",         "SIMILKAMEEN RIVER ABOVE GOODFELLOW CREEK",         FALSE, "08NL038",
  "08NL038", "SIMILKAMEEN RIVER",                    "SIMILKAMEEN RIVER NEAR HEDLEY",          TRUE, "08NL038",
  "08EE012",     "SIMPSON CREEK",                       "SIMPSON CREEK AT THE MOUTH",          TRUE, "08EE012",
  "08HF013",     "SIMPSON CREEK",               "SIMPSON CREEK NEAR KOPRINO HARBOUR",          TRUE, "08HF013",
  "08EB005",      "SKEENA RIVER",                  "SKEENA RIVER ABOVE BABINE RIVER",         FALSE, "08EF001",
  "08EF001",      "SKEENA RIVER",                              "SKEENA RIVER AT USK",          TRUE, "08EF001",
  "08LG068",       "SPIUS CREEK",                   "SPIUS CREEK BELOW SILVER CREEK",         FALSE, "08LG008",
  "08LG008",       "SPIUS CREEK",                         "SPIUS CREEK NEAR CANFORD",          TRUE, "08LG008",
  "08GC005",   "THEODOSIA RIVER",       "THEODOSIA RIVER DIVERSION ABOVE OLSEN LAKE",         FALSE, "08GC008",
  "08GC006",   "THEODOSIA RIVER",                 "THEODOSIA RIVER DIVERSION BYPASS",         FALSE, "08GC008",
  "08GC007",   "THEODOSIA RIVER",       "THEODOSIA RIVER BELOW OLSEN LAKE DIVERSION",         FALSE, "08GC008",
  "08GC008",   "THEODOSIA RIVER",               "THEODOSIA RIVER ABOVE SCOTTY CREEK",          TRUE, "08GC008",
  "08NL071",    "TULAMEEN RIVER",                 "TULAMEEN RIVER BELOW VUICH CREEK",         FALSE, "08NL024",
  "08NL024",    "TULAMEEN RIVER",                      "TULAMEEN RIVER AT PRINCETON",          TRUE, "08NL024",
  "08NN015",       "WEST KETTLE",                 "WEST KETTLE RIVER NEAR MCCULLOCH",         FALSE, "08NN003",
  "08NN003",       "WEST KETTLE",                  "WEST KETTLE RIVER AT WESTBRIDGE",          TRUE, "08NN003",
  "08HE008",    "ZEBALLOS RIVER",                      "ZEBALLOS RIVER AT MOOK PEAK",         FALSE, "08HE006",
  "08HE006",    "ZEBALLOS RIVER",                     "ZEBALLOS RIVER NEAR ZEBALLOS",          TRUE, "08HE006"
)

# Duplicated streams to keep (from manual assessment)
stns_dup_keep <- stns_dup_table %>%
  filter(STATION_KEEP) %>%
  pull(STATION_NUMBER)
# Duplicated streams to remove
stn_dup_remove <- check_dup_stations %>%
  filter(!STATION_NUMBER %in% stns_dup_keep) %>%
  pull(STATION_NUMBER)

# Remove the duplicated streams
stations_filt2 <- stations_filt %>%
  filter(!STATION_NUMBER %in% stn_dup_remove)


####### Check for station regulation (plot data and see (first pass))

# Filter for regulated stations
check_reg <- stations_filt2 %>%
  filter(REGULATED != FALSE) %>%
  select(STATION_NUMBER, STATION_NAME, "Year_from", "Year_to")
#write.csv(check_reg %>%  select(STATION_NUMBER, STATION_NAME), "station_selection/regulated_stations.csv")

# Manual checks on regulated stations for degree of regulation (open to interpretation)
# STN <- "08NM232"
# stns_reg_sf <-  stns_reg %>%
#   filter(STATION_NUMBER == STN) %>%
#   mutate(STATION= paste0(STATION_NUMBER, " - ", STATION_NAME)) %>%
#   select(STATION, LONGITUDE, LATITUDE) %>%
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
#            crs = 4326)
# mapview::mapview(stns_reg_sf)
# plot_daily_stats(station_number = STN, complete_years = TRUE, add_year = 2000)
# SCREEN <- screen_flow_data(station_number = STN)
# for (stn in unique(stns_reg$STATION_NUMBER)) {
#   ggsave(filename = paste0("station_selection/regulation_checks/", stn, ".png"), height = 4, width = 10,
#          plot = plot_daily_stats(station_number = stn, complete_years = TRUE, add_year = 2000)[[1]]
#   )
# }

check_reg_results <- tibble::tribble(
  ~STATION_NUMBER, ~REG_KEEP, ~Year_from_REG,
  "07FD010", FALSE,          NA,
  "08EC004",  TRUE,          NA,
  "08EG016",  TRUE,          NA,
  "08FE002", FALSE,          NA,
  "08GA010",  TRUE,          NA,
  "08GA022",  TRUE,          NA,
  "08GA030",  TRUE,       1970,
  "08GA043",  TRUE,          NA,
  "08GA047",  TRUE,          NA,
  "08GA076",  TRUE,          NA,
  "08HA011",  TRUE,          NA,
  "08HB002",  TRUE,          NA,
  "08HB006", FALSE,          NA,
  "08HB011", FALSE,          NA,
  "08HB022", FALSE,          NA,
  "08HB023", FALSE,          NA,
  "08HB034",  TRUE,          NA,
  "08HB041", FALSE,          NA,
  "08HB092",  TRUE,          NA,
  "08HC001",  TRUE,          NA,
  "08HC005",  TRUE,          NA,
  "08HD003", FALSE,          NA,
  "08HD005",  TRUE,          NA,
  "08HD006",  TRUE,          NA,
  "08HD018",  TRUE,          NA,
  "08HD026", FALSE,          NA,
  "08HE006",  TRUE,          NA,
  "08JA013", FALSE,          NA,
  "08JB008", FALSE,          NA,
  "08JC002",  TRUE,       1978,
  "08KH020", FALSE,          NA,
  "08LB078",  TRUE,          NA,
  "08LC002",  TRUE,          NA,
  "08LC039",  TRUE,          NA,
  "08LE021",  TRUE,          NA,
  "08LE112",  TRUE,          NA,
  "08LF002",  TRUE,          NA,
  "08LF007",  TRUE,          NA,
  "08LF027",  TRUE,          NA,
  "08LG004", FALSE,          NA,
  "08LG010", FALSE,          NA,
  "08LG049",  TRUE,          NA,
  "08MC040",  TRUE,          NA,
  "08ME002",  TRUE,          NA,
  "08ME003", FALSE,          NA,
  "08MF005",  TRUE,          NA,
  "08MG025",  TRUE,          NA,
  "08MH005",  TRUE,          NA,
  "08MH090",  TRUE,          NA,
  "08MH098",  TRUE,          NA,
  "08MH153",  TRUE,          NA,
  "08NA011",  TRUE,          NA,
  "08ND025",  TRUE,          NA,
  "08NE049",  TRUE,          NA,
  "08NE123",  TRUE,          NA,
  "08NE126", FALSE,          NA,
  "08NG002",  TRUE,          NA,
  "08NH118", FALSE,          NA,
  "08NJ158", FALSE,          NA,
  "08NL039",  TRUE,          NA,
  "08NL045",  TRUE,          NA,
  "08NM037",  TRUE,          NA,
  "08NM065", FALSE,          NA,
  "08NM085", FALSE,          NA,
  "08NM200",  TRUE,          NA,
  "08NN003",  TRUE,          NA,
  "08NN026",  TRUE,          NA,
  "08HB008",  TRUE,          NA,
  "08HB029",  TRUE,          NA,
  "08NM116",  TRUE,          NA,
  "08NM232", FALSE,          NA
) %>%
  left_join(check_reg, by = "STATION_NUMBER") %>%
  mutate(Year_from = ifelse(is.na(Year_from), Year_from_REG, Year_from))


stn_reg_keep <- check_reg_results %>%
  filter(REG_KEEP) %>%
  pull(STATION_NUMBER)
stn_reg_remove <- check_reg_results %>%
  filter(!REG_KEEP) %>%
  pull(STATION_NUMBER)

# Filter and add years of regulated to filter
stations_filt3 <- stations_filt2 %>%
  select(-Year_to, -Year_from) %>%
  filter(!STATION_NUMBER %in% stn_reg_remove) %>%
  left_join(check_reg_results %>%
              select(-Year_from_REG, -STATION_NAME), by = "STATION_NUMBER")


###### Year filtering


# min_years_allowed <- 25
min_years_allowed <- 10

# Calculate annual summaries for:
# i. All data
stns_ann_data_all = hydat_daily_all |>
  mutate(Year = year(Date)) %>%
  group_by(STATION_NUMBER, Year) %>%
  calculate_metrics()

# ii. Just data from 1990+
stns_ann_data_1990 <- hydat_daily_all %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 1990) |>
  group_by(STATION_NUMBER, Year) %>%
  calculate_metrics()

# iii. Just data from 2010+
stns_ann_data_2010 <- hydat_daily_all %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2010) |>
  group_by(STATION_NUMBER, Year) %>%
  calculate_metrics()

stns_ann_data_qaqc_stations <- hydat_daily_all %>%
  filter(STATION_NUMBER %in% unique(stations_filt3$STATION_NUMBER)) %>%
  mutate(Year = year(Date)) %>%
  group_by(STATION_NUMBER, Year) %>%
  calculate_metrics() |>
  filter(sum(!is.na(Median)) >= min_years_allowed) |>
  ungroup()

all_stations_annual <- unique(stns_ann_data_all$STATION_NUMBER)
stations_annual_qaqc_stations = unique(stns_ann_data_qaqc_stations$STATION_NUMBER)

# ggplot(stns_ann_data_qaqc_stations, aes(Year,STATION_NUMBER, colour = Median))+
  # geom_point()
# plotly::ggplotly()

remove_custom <- tribble(
  ~STATION_NUMBER, ~Note,
  "08NP003", "large data gap 90s",
  "08NM146", "large gap 90s",
  "08NL039", "large data gap",
  "08HB029", "gap in 90s",
  "08EE005", "gap in 90s",
  "08OA005", "gap in 00s",
  "08OA004", "gap in 00s",
  "08MH156", "gap in 90s + 00s",
  "08HE006", "too short"
)


# DO THIS, THEN MAX OF THIS IS ANNUAL NA filt, NOT FINAL, JUST REMOVING OLD GAPPY DATA
dates_custom <- tribble(
  ~STATION_NUMBER, ~Year_from_OLD,
  "08NN003", 1987,
  "08NN002", 1966,
  "08NN026", 1989,
  "08NM037", 1964,
  "08NM200", 2006,
  "08NL004", 1947,
  "08NK002", 1970,
  "08NJ130", 1966,
  "08NJ026", 1995,
  "08NJ013", 1925,
  "08NH115", 1964,
  "08NH084", 1966,
  "08NH016", 1979,
  "08NH007", 1945,
  "08NH005", 1964,
  "08NG002", 1927,
  "08NE039", 1949,
  "08NE006", 1963,
  "08ND012", 1963,
  "08NA011", 1948,
  "08NA006", 1974,
  "08MH006", 1960,
  "08MH005", 1960,
  "08MH153", 1988,
  "08MH147", 1992,
  "08MH001", 1951,
  "08MG001", 1979,
  "08ME002", 1955,
  "08MC045", 1998,
  "08MA003", 1982,
  "08LG049", 1965,
  "08LG016", 1969,
  "08LG008", 1970,
  "08LF027", 1961,
  "08LF007", 1961,
  "08LF002", 1972,
  "08LE077", 1977,
  "08LE027", 1969,
  "08LE024", 1965,
  "08LE021", 1961,
  "08LC039", 1975,
  "08LC002", 1960,
  "08LB038", 1984,
  "08LB024", 1970,
  "08LB020", 1952,
  "08LA001", 1950,
  "08KA001", 1966,
  "08JB002", 1950,
  "08HB032", 1986,
  "08HB025", 1985,
  "08HB002", 1979,
  "08HA001", 1952,
  "08GA022", 1955,
  "08FA002", 1961,
  "08EF001", 1936,
  "08ED001", 1972,
  "08DB001", 1956,
  "07FD001", 1962,
  "10CD001", 1965,
  "10CB001", 1965,
  "08KG001", 1970,
  "08JE001", 1951,
  "08HA003", 1960,
  "08GD004", 1973,
  "08FB006", 1973,
  "08CG001", 1965,
  "08CE001", 1965,
  "07FB001", 1965,
  "08MA001", 1954
)

# Remove stations with large recent gaps
# add years to filter from old gappy data
stations_filt4 <- stations_filt3 %>%
  filter(!STATION_NUMBER %in% remove_custom$STATION_NUMBER) %>%
  left_join(dates_custom, by = "STATION_NUMBER") %>%
  mutate(Year_from = case_when(
    !is.na(Year_from_OLD) ~ Year_from_OLD,
    T ~ year_min)) %>%
  dplyr::select(-Year_from_OLD)
  # mutate(Year_from = ifelse(is.na(Year_from), Year_from_OLD, Year_from),
  #        Year_from = ifelse(is.na(Year_from), year_min, Year_from))# %>%
  # select(-Year_from_OLD)


# For each station, filter out big data gaps.
stns_ann_data_qaqc_stations <- stations_filt4$STATION_NUMBER %>%
    map( ~ {
      stns_ann_data_qaqc_stations %>%
        filter(STATION_NUMBER == .x) %>%
        filter(Year >= stations_filt4[stations_filt4$STATION_NUMBER == .x,]$Year_from) |>
        ungroup()
    }) %>%
  bind_rows()

stns_ann_data_qaqc_stations %>%
  count(STATION_NUMBER, sort = T)

# mapfilt4 <- sf::st_as_sf(stations_filt4 %>% select(STATION_NUMBER, LONGITUDE, LATITUDE), coords = c("LONGITUDE", "LATITUDE"),crs = 4326)
# mapview::mapview(mapfilt4)

final_stations_table <- stns_ann_data_qaqc_stations

final_stations_summary <- final_stations_table %>%
  filter(!is.na(Median)) %>%
  group_by(STATION_NUMBER) %>%
  summarise(N_Years = n(),
            Min_Year = min(Year),
            Max_Year = max(Year),
            Total_Years = Max_Year - Min_Year +1)

list_of_qaqc_stations = unique(final_stations_summary$STATION_NUMBER)

write.csv(final_stations_summary, "data/included_stations_and_years.csv", row.names = F)
write.csv(list_of_qaqc_stations, 'app/www/qaqc_stations.csv', row.names = F)

# Save the annually summarized datasets.
qs::qsave(stns_ann_data_all, 'app/www/ann_flow_summary.qs')
qs::qsave(stns_ann_data_1990, 'app/www/ann_flow_summary_1990.qs')
qs::qsave(stns_ann_data_2010, 'app/www/ann_flow_summary_2010.qs')

# Save dataset of ALL flow. We will use this in the app with
# spatial subsets of the province, filtering by station name.
all_bc_hydat_data = hydat_daily_all %>%
  filter(!is.na(Value),
         Parameter == 'Flow') %>%
  dplyr::select(-Symbol, -Parameter) %>%
  mutate(Month = lubridate::month(Date),
         Year = lubridate::year(Date))

qsave(all_bc_hydat_data, 'app/www/daily_flow_records.qs')

# Also curious if I could try one value per week for the records...
all_bc_hydat_data_week_av = all_bc_hydat_data |>
  mutate(day_of_year = lubridate::yday(Date)) |>
  mutate(Week = 1 + (day_of_year %/% 7)) |>
  group_by(STATION_NUMBER, Week, Year) |>
  mutate(week_value = mean(Value, na.rm=T)) |>
  slice(1) |>
  dplyr::select(-Week,Value = week_value, -week_value,-day_of_year) |>
  ungroup()

qs::qsave(all_bc_hydat_data_week_av, 'app/www/weekly_flow_records.qs')

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Get all stations, add column indicating if station
# meets filtering criteria.
all_stations = tidyhydat::hy_stations(prov_terr_state_loc = 'BC') |>
  filter(STATION_NUMBER %in% unique(all_bc_hydat_data$STATION_NUMBER)) |>
  mutate(meets_dat_qual_check = STATION_NUMBER %in% list_of_qaqc_stations) |>
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS,meets_dat_qual_check)


# Add in which of each spatial delineation of BC each station is in.
# Doing this here saves us some processing power in the shiny app.

ecoprov = read_sf('app/www/ecoprovinces.gpkg')
ecoreg = read_sf('app/www/ecoregions.gpkg')
ecosec = read_sf('app/www/ecosections.gpkg')
nr_dist = read_sf('app/www/nr_districts.gpkg')
nr_reg = read_sf('app/www/nr_regions.gpkg')
subw = read_sf('app/www/subw.gpkg')

all_stations = all_stations %>%
  st_join(ecoprov %>% select(ecoprov = shape_name), st_intersects) %>%
  st_join(ecoreg %>% select(ecoreg = shape_name), st_intersects) %>%
  st_join(ecosec %>% select(ecosec = shape_name), st_intersects) %>%
  st_join(nr_dist %>% select(nr_dist = shape_name), st_intersects) %>%
  st_join(nr_reg %>% select(nr_reg = shape_name), st_intersects) %>%
  st_join(subw %>% select(subw = shape_name), st_intersects)

write_sf(all_stations, 'app/www/stations.gpkg')
