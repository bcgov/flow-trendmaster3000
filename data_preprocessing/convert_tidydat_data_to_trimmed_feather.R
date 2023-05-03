library(tidyverse)
library(tidyhydat)

flow_dat_daily = feather::read_feather('app/www/daily_flow_records.feather')

bundle_flow_data = function(dat){
  dat %>%
    dplyr::select(STATION_NUMBER, Date, Value) %>%
    mutate(station_meets_reqs = 'meets_requirements') %>%
    mutate(date_chunk = 1) %>%
    mutate(the_row = row_number()) %>%
    mutate(new_chunk = ifelse(lag(Date) == Date - days(1) | is.na(lag(Date)), FALSE, TRUE)) %>%
    mutate(date_chunk = 1 + cumsum(new_chunk)) %>%
    group_by(STATION_NUMBER, date_chunk, station_meets_reqs) %>%
    mutate(chunk_start_date = as.character(min(Date)),
           chunk_end_date = as.character(max(Date))) %>%
    mutate(feather_bundle = as.character(round(Value,5))) %>%
    reframe(feather_bundle = c(unique(STATION_NUMBER),
                               unique(station_meets_reqs),
                               unique(chunk_start_date),
                               feather_bundle,
                               unique(chunk_end_date))) %>%
    dplyr::select(feather_bundle)
}

dat_p = flow_dat_daily %>%
  dplyr::select(STATION_NUMBER, Date, Value) %>%
  mutate(station_meets_reqs = 'meets_requirements') %>%
  mutate(date_chunk = 1) %>%
  mutate(the_row = row_number())

flow_dat_featherbundle = dat_p %>%
  mutate(new_chunk = ifelse(lag(Date) == Date - days(1) | is.na(lag(Date)), FALSE, TRUE)) %>%
  mutate(date_chunk = 1 + cumsum(new_chunk)) %>%
  group_by(STATION_NUMBER, date_chunk, station_meets_reqs) %>%
  mutate(chunk_start_date = as.character(min(Date)),
         chunk_end_date = as.character(max(Date))) %>%
  mutate(feather_bundle = as.character(round(Value,5))) %>%
  reframe(feather_bundle = c(unique(STATION_NUMBER),
                             unique(station_meets_reqs),
                               unique(chunk_start_date),
                               feather_bundle,
                               unique(chunk_end_date))) %>%
  dplyr::select(feather_bundle)

feather::write_feather(x = flow_dat_featherbundle,
                       'app/www/size_test.feather')

saveRDS(flow_dat_daily, 'app/www/rds_size_check.RDS')
qsave(flow_dat_daily, 'app/www/qsave_size.check.qs')
qread('app/www/qsave_size.check.qs')

open_bundle = function(dat_bundled){

}

# determine_column_type = function(value){
#   browser()
#   if(!grepl(pattern = '[A-Z]+', x = value)) 'Value'
#   if(grepl(pattern = '[0-9]{2}[A-Z]+', x = value)) 'STATION_NUMBER'
#   if(grepl(pattern = '[0-9]{4}-', x = value)) 'Date'
# }

# Unbundle a bundled dataset.
flow_dat_unbundled = flow_dat_featherbundle %>%
  mutate(column_type = data.table::fcase(
    !grepl(pattern = '[A-Z]+', x = feather_bundle), 'Value',
    grepl(pattern = '[0-9]{2}[A-Z]+', x = feather_bundle), 'STATION_NUMBER',
    grepl(pattern = '[0-9]{4}-', x = feather_bundle), 'Date'
  ))

flow_dat_unbundled %>%
  pivot_wider()




# Trying it with all the tidyhydat data... is it still small enough to import
# to our shiny app?

all_flows = tidyhydat::hy_daily_flows()

