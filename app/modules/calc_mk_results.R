
calculate_mk_mod <- function(id, data, user_var_choice) {
  moduleServer(
    id,
    function(input, output, session) {

      d_with_mk = reactive({
        # chosen_variable = user_var_choice()
        chosen_variable = user_var_choice()
        unique_stations = unique(data()$STATION_NUMBER)

        # Filter for stations with 3+ records.
        data = data()[,group_count := .N, by = STATION_NUMBER
               ][group_count >= 3,]

        # This chunk does the following things:
        # 1. First, applies the Mann-Kendall trend test. Results are a list inside each row.
        # 2. Unnest the list results, putting each in its own row.
        # 3. Add a new column that indicates what each row is (e.g. p-value, or slope, or Tau, etc.)
        data = data[, .(MK_results = kendallTrendTest(values ~ Year, warn = FALSE)[c('statistic','p.value','estimate')]),
                   by = STATION_NUMBER
                   ][,
                     .(MK_results = unlist(MK_results)), by = .(STATION_NUMBER)
                     ][,
                       .(.SD[,], MK_key = c('Statistic','P_value','Tau','Slope','Intercept'))
                       ]

        # Lean on pivot_wider from tidyverse to easily pivot wider.
        # Then, add in what the trend significance is.
        as.data.frame(data) |>
          pivot_wider(id_cols = STATION_NUMBER, names_from = MK_key, values_from = MK_results) |>
              mutate(trend_sig = fcase(
                abs(Tau) <= 0.05 , "No Trend",
                Tau < -0.05 & P_value < 0.05 & grepl(pattern = '.*(TotalQ|DoY)', chosen_variable) , "Significant Trend Earlier",
                Tau < -0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$', chosen_variable) , "Non-Significant Trend Earlier",
                Tau > 0.05 & P_value >= 0.05 & grepl(pattern = '(TotalQ|DoY)$', chosen_variable) , "Non-Significant Trend Later",
                Tau > 0.05 & P_value < 0.05 & grepl(pattern = '(TotalQ|DoY)$', chosen_variable) , "Significant Trend Later",
                Tau < -0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$', chosen_variable)) , "Significant Trend Down",
                Tau < -0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$', chosen_variable)) , "Non-Significant Trend Down",
                Tau > 0.05 & P_value >= 0.05 & (!grepl(pattern = '(TotalQ|DoY)$', chosen_variable)) , "Non-Significant Trend Up",
                Tau > 0.05 & P_value < 0.05 & (!grepl(pattern = '(TotalQ|DoY)$', chosen_variable)) , "Significant Trend Up"
              ))
      })
      return(reactive(d_with_mk()))
    }
  )
}
