## explore CAFAS server.R ##

shinyServer(
  function(input, output) {
    
    cafasInput <- reactive({  
      scrub_cafas %<>% 
        filter(as.Date(assess_date) >= input$dateRange[1]
               & as.Date(assess_date) <= input$dateRange[2])
    })
    
  }
)
