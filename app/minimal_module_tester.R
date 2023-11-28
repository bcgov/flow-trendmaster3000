library(shiny)
# library(shinyFeedback)

source('modules/access_wsurvey_canada_data.R')

ui <- fluidPage(
  filter_data_Mod_UI("data"),
  textOutput('number_records')
)

server <- function(input, output, session) {


  if(!str_detect(getwd(),".*www$")){
    setwd(paste0(getwd(),"/www"))
  }

  all_flow_records_for_hydrograph = qs::qread("weekly_flow_records.qs")

  bc_boundary = read_sf('bc_bound.gpkg') %>%
    st_transform(crs = 4326) %>%
    mutate(shape_name = 'unselected area')

  ## Stations spatial file.
  stations_sf = read_sf('stations.gpkg')

  filtering_mod_output = filter_data_Mod_Server("data",
                                                reactive(TRUE),
                                                reactive(stations_sf[1]),
                                                number_station_cutoff = 50)

  output$number_records = renderText({
    filtering_mod_output$dat_filtered()
  })
}

shinyApp(ui, server)
