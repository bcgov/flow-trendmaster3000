shinyDirChoose(
  input,
  'dir',
  roots = c(home = 'C:/'),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)

dir_reactive <- reactive(input$dir)

output$ask_user_for_dir = renderUI({
  showModal(
    modalDialog(
      h5("Please locate HYDAT database on your machine. If not yet downloaded, this will perform a one-time download (~200MB)."),
      shinyDirButton("dir", "Select Folder","Select Folder"),
      h5("If this is the first time you've run this script, \nit will also be necessary to run a station-filtering script. This takes 2-5 minutes.")
      # footer = NULL
    )
  )
})

dir_reactive_formatted = reactive({
  req(is.list(input$dir))
  paste0('C:',paste0(dir_reactive()$path, collapse = '/'))
})
