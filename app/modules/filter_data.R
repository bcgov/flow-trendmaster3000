
# Module server filters data based on inputs from user.
filter_data_module = function(id){
  moduleServer(
    id,
    function(input, output, session){

      joined_text = reactive(paste0(input$text_left, input$join_type, input$text_right))

      output$joined_text = renderText({

        # Code in 'secret code'
        if(joined_text() == 'feral_pig'){
          return('**secret unlocked**')
        } else{
          return(joined_text())
        }

      })
    })
}
