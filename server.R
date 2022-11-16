# Server
function(input, output) {
  
  output$graphTitle <- renderText({
    paste(input$state, "Takings (1990 - 2016)")
    
    
  })
  output$distPlot <- renderPlot({
    plotData <- aus_accommodation %>%
      filter(State == input$state)

    ggplot(data = plotData, aes(x = Date, y = Takings)) +
      geom_point(colour = "navyblue") +
      geom_smooth()
  })
}
