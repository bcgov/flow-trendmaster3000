library(shiny)

source('modules/ask_user_for_dir.R')

ui <- fluidPage(
  ask_user_for_dir_ui('dir')
)

server <- function(input, output, session) {
  ask_user_for_dir_server('dir')
}

shinyApp(ui, server)
