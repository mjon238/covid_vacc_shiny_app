# Server
function(input, output) {
  output$distPlot <- renderPlot({
    plotData <- aus_accommodation %>%
      filter(State == input$state)

    ggplot(data = plotData, aes(x = Date, y = Takings)) +
      geom_point(colour = "navyblue") +
      geom_smooth()
  })
}
