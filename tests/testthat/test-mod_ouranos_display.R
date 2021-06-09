test_that("mod_ouranos_display generates an app", {
  skip_if_not(interactive())
  
  # testing function
  testapp_ouranos_display <- function(){
    ui <- fluidPage(
      mod_ouranos_display_ui("observation_display_ui_1")
    )
    
    server <-  function(input, output, session) {
      
      mod_ouranos_display_server("observation_display_ui_1", 
                                 region = reactive("Abitibi-Témiscamingue"))
    }
    shinyApp(ui, server)
  }
  
  testapp_ouranos_display()
})
