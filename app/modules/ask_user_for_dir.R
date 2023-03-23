ask_user_for_dir_ui = function(id){
  ns = NS(id)

  uiOutput(ns('ask_user_for_dir'))

  # showModal(
  #   modalDialog(
  #     h3("Folder Selection"),
  #     h5("Please locate the HYDAT database on your machine.
  #          If not yet downloaded, this will perform a one-time download (1.1GB)."),
  #     h5("Please write (or copy + paste) the directory path in the following format:"),
  #     h6("e.g. C:/Users/EPRESLEY/Downloads"),
  #     textInput(ns('text_filepath'),
  #               label = 'Path to your chosen folder (I recommend C:/tmp or C:/Users/YOURNAME/Downloads)',
  #               value = 'C:/tmp',
  #               width = '500px'),
  #     h5(HTML("<b>Please note</b>: If this is the first time you've run this script, <br>it will also be necessary to run a station-filtering script. This takes 5-10 minutes.")),
  #     actionButton(ns('submit_filepath'), label = 'Submit Filepath')
  #   )
  # )

}


ask_user_for_dir_server = function(id){
  moduleServer(
    id,
    function(input,output,session){

      ns = NS(id)

      output$ask_user_for_dir = renderUI({

        showModal(
          modalDialog(
            h3("Folder Selection"),
            h5("Please locate the HYDAT database on your machine.
         If not yet downloaded, this will perform a one-time download (1.1GB)."),
            h5("Please write (or copy + paste) the directory path in the following format:"),
            h6("e.g. C:/Users/EPRESLEY/Downloads"),

            textInput(ns('text_filepath'),
                      label = 'Path to your chosen folder (I recommend C:/tmp)',
                      value = 'C:/tmp',
                      width = '500px'),

            h5(HTML("<b>Please note</b>: If this is the first time you've run this script, <br>it will also be necessary to run a station-filtering script. This takes 5-10 minutes.")),

            actionButton(ns('submit_filepath'), label = 'Submit Filepath')
          )
        )
      })

      tempfiles_folder = eventReactive(input$submit_filepath, {
        path = input$text_filepath
        # Make sure last character of path is a '/'
        if(str_detect(path, '.{1}$') != '/') path = paste0(path,'/')

        # Tack on name of folder for temporary files: "Trendmaster3000_tmpfiles"
        paste0(path,"Trendmaster3000_tmpfiles/")
      })

      observeEvent(input$submit_filepath, {

        # Drop the modal dialogue box.
        return(removeModal())

        #  Create the daily_flow_records.feather file, if not yet made.
        if(!file.exists(paste0(tempfiles_folder(),"daily_flow_records.feather"))){

          print("need to make feather file!")

          first_time_file_generator(temporary_folder = tempfiles_folder())

          print("Finished making temporary files!")
        }
      })

      # tempfile_dir_checked = reactiveVal(T)
      # return(reactive(tempfile_dir_checked()))
      return(reactive(tempfiles_folder()))
    }
  )
}
