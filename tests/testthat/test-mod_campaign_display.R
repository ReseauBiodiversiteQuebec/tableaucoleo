test_that("multiplication works", {
  skip_if_not(interactive())
  
  testapp_campaign_display <- function(){
    
    ui <- fluidPage(
      mod_campaign_display_ui("ttt")
    )
    
    server <-  function(input, output, session) {
      
      mod_campaign_display_server("ttt", 
                                  region = reactive("148_101_H01"))
    }
    shinyApp(ui, server)
  }
  
  
  testapp_campaign_display()
  
})
