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

source('UI.R')
source('functions.R')

ui = shiny::fluidPage(
  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.5;}
         #trend_selector:hover{opacity:0.9;}'))),
  shinyFeedback::useShinyFeedback(),
  titlePanel("Flow Indicator"),
  map_abs_panel,
  card(trend_select_abs_panel)
)

server <- function(input, output) {

  source(file.path('Load_Filter_Data.R'), local = T)$value
  source(file.path('Render_UI_elements.R'), local = T)$value

  date_vars = c("Min_7_Day_DoY","DoY_50pct_TotalQ")

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$scale_selector_radio, {
    if(input$scale_selector_radio == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Median')
      )
    } else {
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Median',
                                       'Date of 50% Flow' = 'DoY_50pct_TotalQ',
                                       'Minimum Flow (7day)' = 'Min_7_Day',
                                       'Date of Minimum Flow (7day)' = 'Min_7_Day_DoY',
                                       'Minimum Flow (30day)' = 'Min_30_Day',
                                       'Date of Minimum Flow (30day)' = 'Min_30_Day_DoY')
      )
    }

  })

  mk_results = reactive({
    calculate_MK_results(data = dat_with_metric(),
                         chosen_variable = input$user_var_choice)
  })

  flow_dat_with_mk = reactive({
    dat_with_metric() %>%
      left_join(mk_results())
  })

  output$testytest = DT::renderDT({dat_with_metric()})

  stations_sf_with_trend = reactive({
    dat = stations_sf %>%
      left_join(mk_results())

    if(input$user_var_choice %in% date_vars){
      dat = dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Earlier",
                                                        'Non-Significant Trend Earlier',
                                                        'No Trend',
                                                        'Non-Significant Trend Later',
                                                        'Significant Trend Later')))
    } else {
      dat = dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Down",
                                                        'Non-Significant Trend Down',
                                                        'No Trend',
                                                        'Non-Significant Trend Up',
                                                        'Significant Trend Up')))
    }
    return(dat)
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    if(!input$tabset == 'Station Hydrograph'){
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Station Trend Plot')
    }
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  date_choice_label = reactive({
    switch(input$scale_selector_radio,
           Annual = 'Based on data from: entire year',
           Monthly = paste0('Based on data from: ',month.name[as.numeric(input$time_selector)]),
           Seasonal = paste0('Based on data from: ',str_to_title(input$season_selector)),
           `Select Dates` = paste0('Based on data from: ',month.abb[input$start_month],'-',input$start_day,' to ',month.abb[input$end_month],'-',input$end_day)
    )
  })

  output$myplot = renderPlot({
    station_flow_plot(data = dat_with_metric(),
                      variable_choice = input$user_var_choice,
                      clicked_station = click_station(),
                      stations_shapefile = stations_sf,
                      slopes = senslope_dat(),
                      caption_label = date_choice_label())
  })

  output$my_hydrograph = renderPlot({
    hydrograph_plot(dat = flow_dat_daily,
                    clicked_station = click_station(),
                    stations_shapefile = stations_sf)
  })

  output$test = DT::renderDT(dat_with_metric())
  output$test_text = renderText(input$filter_interval)

  mypal = reactive({
    if(input$user_var_choice %in% date_vars){
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Earlier",
                             'Non-Significant Trend Earlier',
                             'No Trend',
                             'Non-Significant Trend Later',
                             'Significant Trend Later'),
                  ordered = T)
    } else {
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Down",
                             'Non-Significant Trend Down',
                             'No Trend',
                             'Non-Significant Trend Up',
                             'Significant Trend Up'),
                  ordered = T)
    }
  })

  output$leafmap <- renderLeaflet({

    easyButton(icon = 'circle',
               onClick = )
    leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets","Terrain"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

  observe({
    leafletProxy("leafmap") %>%
      clearMarkers() %>%
      addCircleMarkers(layerId = ~STATION_NUMBER,
                       color = 'black',
                       fillColor = ~mypal()(trend_sig),
                       radius = 8,
                       weight = 1,
                       fillOpacity = 0.75,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                       data = stations_sf_with_trend()) %>%
      addCircleMarkers(layerId = 'selected_station',
                       color = 'orange',
                       weight = 7.5,
                       fillColor = 'transparent',
                       data = stations_sf_with_trend() %>%
                         filter(STATION_NUMBER == click_station())) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal(),
                values = ~trend_sig,
                title = 'Mann-Kendall Trend Result',
                data = stations_sf_with_trend(),
                layerId = 'legend')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
