# Calculate Mann-Kendall trend test for data.
calculate_MK_results = function(data, user_var){

  data = data()
  chosen_variable = user_var

  data |>
    ungroup() |>
    group_by(STATION_NUMBER) |>
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
