# Server
function(input, output) {

  output$graphTitle <- renderText({
    title <- switch(input$items,
      "vacc" = "Fully Vaccinated",
      "ratio" = "Rate Ratios",
      "ratio2" = "Rate Ratios Comparison"
    )
    paste(title)
  })
  
  #Create Plots

  output$fullyVaccPlot <- renderPlotly({
    ##### Total Vaccinated Plots

    # Change Data based on selection of Total/Maori
    data <- switch(input$ethFull,
      "total" = subset(HSUvsERP_TFVacc_DHB.df),
      "maori" = subset(HSUvsERP_MFVacc_DHB.df)
    )

    # Change Title based on Selection
    titlePlot <- switch(input$ethFull,
      "total" = "Total Fully Vaccinated Rates by Age Groups",
      "maori" = "Maori Fully Vaccinated Rates by Age Groups"
    )
    figTotal <- plot_ly(subset(data),
                       x = ~factor(AgeGroup, level = level_order),
                       y = ~RateMult,
                       type = "bar",
                       color = ~population,
                       error_y = ~list(type = "data", 
                                      array = Rate_Gamma1Upr - RateMult,
                                      arrayminus = RateMult - Rate_Gamma1Lwr,
                                      color = 'black',
                                      width = 6,
                                      thickness = 1),
                       text = paste0("Age Group: ", data$AgeGroup,
                                    "<br>Rate: ", round(data$RateMult),
                                    "<br>Gamma Interval: +",
                                    round(data$Rate_Gamma1Upr - data$RateMult), "/ -",
                                    round(data$RateMult - data$Rate_Gamma1Lwr),
                                    "<br>Population: ", data$population),
                       hoverinfo = 'text')
    
    figTotal <- figTotal%>%
      layout(xaxis = list(title = list(text = "Age Groups",
                                       size = 2),
                          tickangle = 45),
             yaxis = list(title = list(text = "Rate per 100,000",
                                       size = 2),
                          zerolinecolor = 'black',
                          zerolinewidth = 2,
                          gridcolor = 'black',
                          hoverformat = '.0f'),
             title = list(text = titlePlot,
                          font=list(size = 15)),
             plot_bgcolor = 'transparent',
             paper_bgcolor = 'transparent',
             legend = list(bgcolor = 'transparent',
                           title = list(text = "Population"))
      )
      

    figTotal
  })

  output$plotDHB <- renderPlotly({
    ##### Total Vaccinated Plots

    # Change Data based on selection of Total/Maori
    dataHSU <- switch(input$ethDHB,
      "total" = DHBData$HSU$Total %>% filter(DHB %in% input$DHB),
      "maori" = DHBData$HSU$Maori %>% filter(DHB %in% input$DHB),
      "nmaori" = DHBData$HSU$NMaori %>% filter(DHB %in% input$DHB)
    )
    dataERP <- switch(input$ethDHB,
      "total" = DHBData$ERP$Total %>% filter(DHB %in% input$DHB),
      "maori" = DHBData$ERP$Maori %>% filter(DHB %in% input$DHB),
      "nmaori" = DHBData$ERP$NMaori %>% filter(DHB %in% input$DHB)
    )


    data <- rbind(
      data.frame(dataHSU, population = "HSU"),
      data.frame(dataERP, population = "ERP")
    )

    # Change Title based on Selection
    titlePlot <- switch(input$ethDHB,
      "total" = "Total Fully Vaccinated by Age Group & DHB",
      "maori" = "Maori Fully Vaccinated by Age Group & DHB",
      "nmaori" = "Non-Maori Fully Vaccinated by Age Group & DHB"
    )

    
      

    p <- ggplot(subset(data), aes(x = factor(AgeGroup, level = level_order), 
                                  y = RateMult, fill = population, 
                                  group = population,
                                  text = paste("Age Group:", AgeGroup,
                                               "<br>Rate:", round(RateMult),
                                               "<br>Population:", population))) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = Rate_Gamma1Lwr, 
                        ymax = Rate_Gamma1Upr, size = 0.1),
                    position = position_dodge(width = 0.85),
                    color = 'black') +
      facet_wrap(~DHB) +
      scale_fill_discrete(name = "Population")+
      plot_annotation(title = titlePlot)+
      labs(
        y = "Rate per 100,000",
        x = "Age Groups"
      ) &
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin=margin(t=20)),
        axis.title.y = element_text(margin=margin(r=5)),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
        panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
        legend.background = element_rect(fill = "#ECF0F5")
      )
    
    figDHB <- ggplotly(p, tooltip = 'text')
    
    figDHB

     
  })

  output$rateRatioPlot <- renderPlotly({
    ##### Ratio Plots
    dataRatio <- switch(input$ethRatio,
      "total" = subset(TFVacc_DHBpopulation.df, DHB == "Total"),
      "maori" = subset(MFVacc_DHBpopulation.df, DHB == "Total"),
      "nmaori" = subset(NMFVacc_DHBpopulation.df, DHB == "Total")
    )

    titlePlot <- switch(input$ethRatio,
      "total" = "Total Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
      "maori" = "Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
      "nmaori" = "Non-Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)"
    )

    ratioColour <- switch(input$ethRatio,
      "nmaori" = "#06b73c",
      "total" = "#639bfb",
      "maori" = "#fb746c"
    )
    figRatio <- ggplotly(ggplot(dataRatio, aes(x = factor(AgeGroup, level = level_order), y = RelativeRisk, fill = RelativeRisk)) +
      geom_col(fill = ratioColour) +
      geom_errorbar(aes(min = RelativeRiskLwr, ymax = RelativeRiskUpr), width = 0.5) +
      plot_annotation(title = titlePlot) +
      labs(
        y = "Rate Ratio",
        x = "Age Groups"
      ) &
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
        panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
        legend.background = element_rect(fill = "#ECF0F5")
      ))


    figRatio
  })

  output$ratioComparsion <- renderPlotly({
    datafilter <- AllFVacc_DHBpopulation.df %>%
      filter(population == input$eth2)

    ggplotly(ggplot(subset(datafilter), aes(x = factor(AgeGroup, level = level_order), y = RelativeRisk, fill = population, group = population)) +
      geom_col(position = "dodge") +
      plot_annotation(title = "Fully Vaccinated Rate Ratio by Age Group & Ethnicity (HSU as baseline)") +
      labs(
        y = "Rate Ratio",
        x = "Age Groups"
      ) +
      scale_fill_manual(
        values = c("Non-Maori" = "#06b73c", "Total" = "#639bfb", "Maori" = "#fb746c"),
        name = "Population"
      ) &

      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
        panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
        legend.background = element_rect(fill = "#ECF0F5")
      ))
  })
}
