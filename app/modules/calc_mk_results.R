
calculate_mk_mod <- function(id, data, processed_stations, number_stations_to_process = 400, user_var_choice) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get subset of 200 stations.
      stations_to_process = reactive({
        unique(data()$STATION_NUMBER)[c(1:number_stations_to_process)]
      })


      d_with_mk = reactive({
        # browser()
        chosen_variable = user_var_choice()
        unique_stations = unique(data()$STATION_NUMBER)

        unique_stations_as_list = data() |>
          dplyr::select(STATION_NUMBER) |>
          distinct() |>
          mutate(group_of_stations = (row_number()-1) %/% number_stations_to_process) |>
          group_by(group_of_stations) %>%
          nest() %>% pull(data)

        # browser()


        unique_stations_as_list |>
          map( ~ {
            # Filter the master data frame for stations of this iteration...
            data = data() |>
              filter(STATION_NUMBER %in% .x$STATION_NUMBER) |>
              add_count(STATION_NUMBER) |>
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
          }) |>
          bind_rows()
      })
      return(reactive(d_with_mk()))
    }
  )
}
