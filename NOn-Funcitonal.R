#NON F
output$rateRatioPlot <- renderPlotly({
  
  data <- Data_All[[input$genderRatio]]$RatioInfo%>%
    filter(population %in% input$ethRatio,
           DHB == "Nationwide")
  
  
  genderTitle <- switch(input$genderRatio,
                        "Male" = "Male ",
                        "Female" = "Female ")
  
  # Change Title based on Selection
  titlePlot <- switch(input$ethRatio,
                      "Total" = paste0("Total ", 
                                       genderTitle,
                                       "Fully Vaccinated Rate Ratio by Age Groups"),
                      "Maori" = paste0("Maori ", 
                                       genderTitle, 
                                       "Fully Vaccinated Rate Ratio by Age Groups"),
                      "Non-Maori" = paste0("Non-Maori ", 
                                           genderTitle ,
                                           "Fully Vaccinated Rate Ratio by Age Groups")
                      
  )
  
  #FIXX
  ratioColour <- switch(input$ethRatio,
                        "Non-Maori" = "#06b73c",
                        "Total" = "#639bfb",
                        "Maori" = "#fb746c"
  )
  
  dataInterest <- switch(input$ratioTabs,
                         "Rate Ratio" = data$RelativeRisk,
                         "Rate Difference" = data$AttributableRisk,
                         "Count" = data$Count)
  
  dataInterestUpr <- switch(input$ratioTabs,
                            "Rate Ratio" = data$RelativeRiskUpr,
                            "Rate Difference" = data$AttributableRiskUpr,
                            "Count" = NULL)
  
  dataInterestLwr <- switch(input$ratioTabs,
                            "Rate Ratio" = data$RelativeRiskLwr,
                            "Rate Difference" = data$AttributableRiskLwr,
                            "Count" = NULL)
  
  
  figRatio <- data%>%
    plot_ly(x = ~factor(AgeGroup, level = level_order))%>%
    add_trace(type = "bar",
              y = ~dataInterest,
              error_y = ~list(type = "x",
                              array = dataInterestUpr - dataInterest,
                              arrayminus = dataInterest - dataInterestLwr,
                              color = 'black',
                              thickness = 1,
                              width = 14),
              text = paste0("Age Group: ", data$AgeGroup,
                            "<br>Relative Risk: ", round(dataInterest, 2),
                            "<br>Gamma Interval: +",
                            round(dataInterestUpr - dataInterest, 4), 
                            "/ -",
                            round(dataInterest - dataInterestLwr, 4)),
              hoverinfo = 'text',
              showlegend = F,
              color = ratioColour)
  
  figRatio <- figRatio%>%
    layout(xaxis = list(title = list(text = "Age Groups",
                                     size = 2),
                        tickangle = 45),
           yaxis = list(title = list(text = "Rate Ratio",
                                     size = 2),
                        zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'black',
                        hoverformat = '.0f'),
           title = list(text = titlePlot,
                        font=list(size = 15)),
           plot_bgcolor = 'transparent',
           paper_bgcolor = 'transparent'
    )
  
  suppressWarnings(figRatio)
})



output$fullyVaccPlot <- renderPlotly({
  ##### Total Vaccinated Plots
  
  # Change Data based on selection of Total/Maori
  
  data <- Data_All[[input$genderVacc]]$Nationwide%>%
    filter(ethnicity == input$ethFull)
  
  genderTitle <- switch(input$genderVacc,
                        "Male" = "Male ",
                        "Female" = "Female ")
  
  # Change Title based on Selection
  titlePlot <- switch(input$ethFull,
                      "Total" = paste0("Total ", genderTitle,"Fully Vaccinated Rates by Age Groups"),
                      "Maori" = paste0("Maori ", genderTitle, "Fully Vaccinated Rates by Age Groups"),
                      "Non-Maori" = paste0("Non-Maori ", genderTitle ,"Fully Vaccinated Rates by Age Groups")
                      
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
                                      width = 10,
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
           title = list(text = paste0(titlePlot, "<br> <sup> Green: ERP, Blue: HSU </sup>"),
                        font=list(size = 15)),
           plot_bgcolor = 'transparent',
           paper_bgcolor = 'transparent',
           showlegend = F,
           legend = list(bgcolor = 'transparent',
                         title = list(text = "Population"))
    )
  
  
  figTotal
})

#OLD Rate Ratio Stuff
output$ratioComparison <- renderPlotly({
  
  
  data <- Data_All[[input$genderRatio]]$RatioInfo%>%
    arrange(`population`)%>%
    filter(population %in% input$ethRatio2,
           DHB == "Nationwide")
  
  mainNames <- colnames(data)
  
  data <- data%>%
    pivot_wider(names_from = population,
                values_from = c(mainNames[-c(1,2,23)]))
  
  
  widthInfo2 <- c(20,9,6)
  
  
  titlePlotGender <- switch(input$genderRatio,
                            "Male" = "Male ",
                            "Female" = "Female ",
                            "Total" = NULL)
  
  removeName <- case_when(input$ratioTabs == "Count" ~ "",
                          TRUE ~ "(HSU as baseline)")
  
  
  figPlot <- data%>%
    plot_ly()
  
  #Create Maori
  if("Maori" %in% input$ethRatio2){
    
    dataInterest_Maori <- switch(input$ratioTabs,
                                 "Rate Ratio" = data$RelativeRisk_Maori,
                                 "Rate Difference" = data$AttributableRisk_Maori,
                                 "Count" = data$Count_Maori)
    
    dataInterestUpr_Maori <- switch(input$ratioTabs,
                                    "Rate Ratio" = data$RelativeRiskUpr_Maori,
                                    "Rate Difference" = data$AttributableRiskUpr_Maori,
                                    "Count" = NULL)
    
    dataInterestLwr_Maori <- switch(input$ratioTabs,
                                    "Rate Ratio" = data$RelativeRiskLwr_Maori,
                                    "Rate Difference" = data$AttributableRiskLwr_Maori,
                                    "Count" = NULL)
    
    figPlot <- figPlot%>%
      add_trace(x = ~factor(AgeGroup, level = level_order),
                y = ~dataInterest_Maori,
                name = "Maori",
                marker = list(color = 'rgba(102,194,165,255)'),
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(dataInterest_Maori, 2),
                              "<br>Ethnicity: Maori ",
                              "<br>Gamma Interval: +",
                              round(dataInterestUpr_Maori - dataInterest_Maori, 4), "/ -",
                              round(dataInterest_Maori - dataInterestLwr_Maori, 4)),
                textposition = "none",
                hoverinfo = 'text',
                error_y = ~list(type = "x",
                                array = dataInterestUpr_Maori - dataInterest_Maori,
                                arrayminus = dataInterest_Maori - dataInterestLwr_Maori,
                                color = 'black',
                                thickness = 1,
                                width = widthInfo2[[length(input$ethRatio2)]])) }
  
  #Create for Non-Maori
  if("Non-Maori" %in% input$ethRatio2){
    
    dataInterest_Non_Maori <- switch(input$ratioTabs,
                                     "Rate Ratio" = data$`RelativeRisk_Non-Maori`,
                                     "Rate Difference" = data$`AttributableRisk_Non-Maori`,
                                     "Count" = data$`Count_Non-Maori`)
    
    dataInterestUpr_Non_Maori <- switch(input$ratioTabs,
                                        "Rate Ratio" = data$`RelativeRiskUpr_Non-Maori`,
                                        "Rate Difference" = data$`AttributableRiskUpr_Non-Maori`,
                                        "Count" = NULL)
    
    dataInterestLwr_Non_Maori <- switch(input$ratioTabs,
                                        "Rate Ratio" = data$`RelativeRiskLwr_Non-Maori`,
                                        "Rate Difference" = data$`AttributableRiskLwr_Non-Maori`,
                                        "Count" = NULL)
    
    figPlot <- figPlot%>%
      add_trace(x = ~factor(AgeGroup, level = level_order),
                y = ~dataInterest_Non_Maori,
                name = "Non-Maori",
                marker = list(color = 'rgba(252,141,98,255)'),
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(dataInterest_Non_Maori, 2),
                              "<br>Ethnicity: Maori ",
                              "<br>Gamma Interval: +",
                              round(dataInterestUpr_Non_Maori - dataInterest_Non_Maori, 4), "/ -",
                              round(dataInterest_Non_Maori - dataInterestLwr_Non_Maori, 4)),
                textposition = "none",
                hoverinfo = 'text',
                error_y = ~list(type = "x",
                                array = dataInterestUpr_Non_Maori - dataInterest_Non_Maori,
                                arrayminus = dataInterest_Non_Maori - dataInterestLwr_Non_Maori,
                                color = 'black',
                                thickness = 1,
                                width = widthInfo2[[length(input$ethRatio2)]]))}
  
  
  if("Total" %in% input$ethRatio2){
    
    dataInterest_Total <- switch(input$ratioTabs,
                                 "Rate Ratio" = data$RelativeRisk_Total,
                                 "Rate Difference" = data$AttributableRisk_Total,
                                 "Count" = data$Count_Total)
    
    dataInterestUpr_Total <- switch(input$ratioTabs,
                                    "Rate Ratio" = data$RelativeRiskUpr_Total,
                                    "Rate Difference" = data$AttributableRiskUpr_Total,
                                    "Count" = NULL)
    
    dataInterestLwr_Total <- switch(input$ratioTabs,
                                    "Rate Ratio" = data$RelativeRiskLwr_Total,
                                    "Rate Difference" = data$AttributableRiskLwr_Total,
                                    "Count" = NULL)
    
    figPlot <- figPlot%>%
      add_trace(x = ~factor(AgeGroup, level = level_order),
                y = ~dataInterest_Total,
                name = "Total",
                marker = list(color = 'rgba(141,160,203,255)'),
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(dataInterest_Total, 2),
                              "<br>Ethnicity: Maori ",
                              "<br>Gamma Interval: +",
                              round(dataInterestUpr_Total - dataInterest_Total, 4), "/ -",
                              round(dataInterest_Total - dataInterestLwr_Total, 4)),
                textposition = "none",
                hoverinfo = 'text',
                error_y = ~list(type = "x",
                                array = dataInterestUpr_Total - dataInterest_Total,
                                arrayminus = dataInterest_Total - dataInterestLwr_Total,
                                color = 'black',
                                thickness = 1,
                                width = widthInfo2[[length(input$ethRatio2)]]))}
  
  
  
  
  figPlot <- figPlot%>%
    layout(xaxis = list(title = list(text = "Age Groups",
                                     size = 2),
                        tickangle = 45),
           yaxis = list(title = list(text = paste0(input$ratioTabs),
                                     size = 2),
                        zerolinecolor = 'black',
                        zerolinewidth = 2,
                        gridcolor = 'black',
                        hoverformat = '.0f'),
           title = list(text = paste0("Fully ", 
                                      titlePlotGender,
                                      "Vaccinated ", 
                                      input$ratioTabs,
                                      " by Age Group & Ethnicity ",
                                      removeName),
                        font=list(size = 15)),
           plot_bgcolor = 'transparent',
           paper_bgcolor = 'transparent',
           legend = list(bgcolor = 'transparent',
                         title = list(text = "Ethnicity")
           ))
  
  figPlot
  
  suppressWarnings(figPlot)
})
