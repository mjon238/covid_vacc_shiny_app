# Server
function(input, output) {
  # output$out <- renderUI({
  #
  #
  # })
  
  observeEvent(input$items, {
    if (input$items == "vaccDHB") {
      show("DHB")
    } else {
      hide("DHB")
    }
  })

  output$graphTitle <- renderText({
    title <- switch(input$items,
      "vacc" = "Fully Vaccinated NZ",
      "vaccDHB" = "Fully Vaccinated by DHB",
      "ratio" = "Rate Ratios",
      "ratio2" = "Rate Ratios Comparison"
    )
    paste(title)
  })

  output$plots1 <- renderPlot({
    
    ##### Total Vaccinated Plots
    
    # Change Data based on selection of Total/Maori
    data <- switch(input$ethnicity,
      "total" = subset(HSUvsERP_TFVacc_DHB.df),
      "maori" = subset(HSUvsERP_MFVacc_DHB.df)
    )

    # Change Title based on Selection
    titlePlot <- switch(input$ethnicity,
      "total" = "Total Fully Vaccinated Rates by Age Groups",
      "maori" = "Maori Fully Vaccinated Rates by Age Groups"
    )

    figTotal <- ggplot(data, aes(x = factor(AgeGroup, level = level_order), y = RateMult, fill = population, group = population)) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = Rate_Gamma1Lwr, ymax = Rate_Gamma1Upr), position = position_dodge(width = 0.85), width = 0.5) +
      plot_annotation(title = titlePlot) +
      labs(
        y = "Rate per 100,000",
        x = "Age Groups"
      ) +
      scale_fill_discrete(name = "Population") &
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
        panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
        legend.background = element_rect(fill = "#ECF0F5")
      )
    
    ##### Ratio Plots
    dataRatio <- switch(input$ethnicity,
                        "total" = subset(TFVacc_DHBpopulation.df, DHB == "Total"),
                        "maori" = subset(MFVacc_DHBpopulation.df, DHB == "Total"),
                        "nmaori" = subset(NMFVacc_DHBpopulation.df, DHB == "Total")
    )
    
    titlePlot <- switch(input$ethnicity,
                        "total" = "Total Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
                        "maori" = "Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
                        "nmaori" = "Non-Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)")
    
    
    figRatio <- ggplot(dataRatio, aes(x=factor(AgeGroup, level= level_order), y = RelativeRisk, fill=RelativeRisk))+
      geom_col(fill = "Steel Blue 2")+
      geom_errorbar(aes(min=RelativeRiskLwr, ymax=RelativeRiskUpr), width=0.5) +
      plot_annotation(title = titlePlot) +
      labs(y= "Rate Ratio",
           x= "Age Groups") &
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
        panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
        legend.background = element_rect(fill = "#ECF0F5")
      )
    
    
    switch(input$items,
           "vacc" = figTotal,
           "ratio" = figRatio)
    
  })
  
output$ratioComparsion <- renderPlot({
  
  datafilter <- AllFVacc_DHBpopulation.df%>%
    filter(population == input$eth2)

  ggplot(subset(datafilter), aes(x=factor(AgeGroup, level= level_order), y = RelativeRisk , fill = population, group=population)) + 
    geom_col(position = "dodge") +
    plot_annotation(title = "Fully Vaccinated Rate Ratio by Age Group & Ethnicity (HSU as baseline)") +
    labs(y= "Rate Ratio",
         x= "Age Groups")+
    scale_fill_discrete(name = "Population")+ 
    scale_fill_manual(values = c("#06b73c","#639bfb", "#fb746c")) &
    
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
      panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
      panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
      legend.background = element_rect(fill = "#ECF0F5")
    )
})
}
