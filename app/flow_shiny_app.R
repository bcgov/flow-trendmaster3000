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
  titlePanel("Flow Indicator"),
  map_abs_panel,
  card(trend_select_abs_panel)
)

server <- function(input, output) {

  source(file.path('Load_Filter_Data.R'), local = T)$value
  source(file.path('Render_UI_elements.R'), local = T)$value

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$time_scale, {
    if(input$time_scale == 'Monthly'){
      updateSelectizeInput(inputId = 'month_selector',
                           choices = month.abb,
                           selected = month.abb[1])
    }
    if(input$time_scale == 'Annual'){
      updateSelectizeInput(inputId = 'month_selector',
                           choices = 'All',
                           selected = 'All')
    }
  })
  mk_results = reactive({
    calculate_MK_results(data = flow_dat_focused(),
                         chosen_variable = input$user_var_choice)
  })

  flow_dat_with_mk = reactive({
    flow_dat() %>%
      left_join(mk_results())
  })

  output$testytest = DT::renderDT({flow_dat_focused()})

  stations_sf_with_trend = reactive({
    stations_sf %>%
      left_join(mk_results()) %>%
      mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Down",
                                                      'Non-Significant Trend Down',
                                                      'No Trend',
                                                      'Non-Significant Trend Up',
                                                      'Significant Trend Up')))
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year) %>%
      dplyr::select(STATION_NUMBER,
                    SlopePreds,
                    Slope,
                    trend_sig,
                    P_value,
                    Month,
                    Year)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Station Plot')
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  output$myplot = renderPlot({
    station_flow_plot(data = flow_dat_focused(),
                      variable_choice = input$user_var_choice,
                      clicked_station = click_station(),
                      stations_shapefile = stations_sf,
                      slopes = senslope_dat())
  })

  mypal = reactive({
    colorFactor(palette = 'RdBu',
                domain = mk_results()$trend_sig,
                levels = c("Significant Trend Down",
                           'Non-Significant Trend Down',
                           'No Trend',
                           'Non-Significant Trend Up',
                           'Significant Trend Up'),
                # reverse = F,
                ordered = T)
  })

  output$leafmap <- renderLeaflet({

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

  observeEvent(c(input$user_var_choice,
                 input$month_selector), {
                   leafletProxy("leafmap") %>%
                     clearMarkers() %>%
                     addCircleMarkers(layerId = ~STATION_NUMBER,
                                      color = ~mypal()(trend_sig),
                                      radius = 3,
                                      weight = 10,
                                      label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                                      data = stations_sf_with_trend()) %>%
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