library(tidyverse)
library(sf)
library(qs)
library(EnvStats)
library(data.table)
library(profvis)
library(bench)

data = qs::qread('app/www/daily_flow_records.qs') %>%
  rename(values = Value)

data = data %>%
  group_by(STATION_NUMBER, Year) %>%
  reframe(values = median(values,na.rm=T))

# stations = read_sf('app/www/stations.gpkg')


fcase_example = function(chosen_variable = 'average'){
  data |>
  add_count(STATION_NUMBER) %>%
  filter(n >= 3) %>%
  group_by(STATION_NUMBER) %>%
  reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) |>
  unnest(MK_results) |>
  unnest_longer(col = MK_results) |>
  group_by(STATION_NUMBER) |>
  mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) |>
  pivot_wider(names_from = MK_results_id, values_from = MK_results) |>
  ungroup() |>
  mutate(trend_sig = fcase(
    abs(Tau) <= 0.05 , "No Trend",
    Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Significant Trend Earlier",
    Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Non-Significant Trend Earlier",
    Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Non-Significant Trend Later",
    Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Significant Trend Later",
    Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Significant Trend Down",
    Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Non-Significant Trend Down",
    Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Non-Significant Trend Up",
    Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Significant Trend Up"
  ))
}

case_when_example = function(dataset, chosen_variable = 'average'){
  dataset |>
  add_count(STATION_NUMBER) %>%
  filter(n >= 3) %>%
  group_by(STATION_NUMBER) %>%
  reframe(MK_results = kendallTrendTest(values ~ Year, warn = FALSE)[c('statistic','p.value','estimate')]) |>
  unnest(MK_results) |>
  unnest_longer(col = MK_results) |>
  group_by(STATION_NUMBER) |>
  mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) |>
  pivot_wider(names_from = MK_results_id, values_from = MK_results) |>
  ungroup() |>
  mutate(trend_sig = case_when(
    abs(Tau) <= 0.05 ~ "No Trend",
    Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Significant Trend Earlier",
    Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Non-Significant Trend Earlier",
    Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Non-Significant Trend Later",
    Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Significant Trend Later",
    Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Significant Trend Down",
    Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Non-Significant Trend Down",
    Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Non-Significant Trend Up",
    Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Significant Trend Up"
  ))
}

case_when_split_example = function(dataset, chosen_variable = 'average'){
  data = dataset |>
    add_count(STATION_NUMBER) %>%
    filter(n >= 3)

  data %>%
    group_by(STATION_NUMBER) %>%
    reframe(MK_results = kendallTrendTest(values ~ Year, warn = FALSE)[c('statistic','p.value','estimate')]) |>
    unnest(MK_results) |>
    unnest_longer(col = MK_results) |>
    group_by(STATION_NUMBER) |>
    mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) |>
    pivot_wider(names_from = MK_results_id, values_from = MK_results) |>
    ungroup() |>
    mutate(trend_sig = case_when(
      abs(Tau) <= 0.05 ~ "No Trend",
      Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Significant Trend Earlier",
      Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Non-Significant Trend Earlier",
      Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Non-Significant Trend Later",
      Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY') ~ "Significant Trend Later",
      Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Significant Trend Down",
      Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Non-Significant Trend Down",
      Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Non-Significant Trend Up",
      Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')) ~ "Significant Trend Up"
    ))
}

profvis::profvis(case_when_example())

# bench::bench_memory(fcase_example())
bench::bench_memory({
  case_when_example(dataset = data)
  })

bench::system_time(case_when_example(dataset = data))
bench::system_time(case_when_split_example(dataset = data))

bench::bench_memory(case_when_example(dataset = data))
bench::bench_memory(case_when_split_example(dataset = data))


bench::workout({
  case_when_split_example(dataset = data)
  case_when_example(dataset = data)
})
