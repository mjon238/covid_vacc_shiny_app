# Server
function(input, output) {
  
  #Title -----
  output$graphTitle <- renderText({
    title <- switch(input$items,
                    "vacc" = "Fully Vaccinated",
                    "ratio" = "Rate Ratios",
                    "ratio2" = "Rate Ratios Comparison"
    )
    paste(title)
  })
  
  #Creating reactive values -----
  DHBSelected <- reactiveValues()
  
  observe({
    maxDHB <- rbind(DHBData$HSU[[input$ethDHB]], 
                    DHBData$ERP[[input$ethDHB]])%>%
      arrange(desc(Rate_Gamma1Upr))
    
    maxDHB <- maxDHB[[1,1]]
    
    minDHB <- rbind(DHBData$HSU[[input$ethDHB]], 
                    DHBData$ERP[[input$ethDHB]])%>%
      arrange((Rate_Gamma1Lwr))%>%
      filter(DHB != "Overseas / Unknown")
    
    minDHB <- minDHB[[1,1]]
    
    DHBSelected$DHB = switch(input$groupDHB,
                             "All" = listOfDHB,
                            "Select DHB\'s" = input$DHB,
                            "Auckland Region" = c("Auckland", "Waitemata",
                                         "Counties Manukau"),
                            "Wellington Region" = c("Capital and Coast", "Hutt Valley"),
                            "North Island" = c(
                              "Auckland", "Bay of Plenty", "Capital and Coast",
                              "Counties Manukau", "Hawkes Bay", "Hutt Valley", "Lakes",
                              "MidCentral", "Northland", "Tairawhiti", "Taranaki", "Waikato",
                              "Wairarapa", "Waitemata", "Whanganui"),
                            "South Island" = c("Canterbury", "Nelson Marlborough", "Southern",         
                                                "South Canterbury", "West Coast" ),
                            "High vs. Low" = c(maxDHB, minDHB)
    )
  })

  observe({
    
    # toggleState(id = "DHB",
    #             condition = input$groupDHB == "Custom")
    
    if(input$groupDHB == 'Select DHB\'s') {
      show("DHB")
    } else {
      hide("DHB")
    }
    # 
  })
  
  #Create Plots ----
  
  #ERP vs. HSU Plots ----

  output$fullyVaccPlot <- renderPlotly({
    ##### Total Vaccinated Plots

    # Change Data based on selection of Total/Maori
    data <- switch(input$ethFull,
      "Total" = subset(HSUvsERP_TFVacc_DHB.df),
      "Maori" = subset(HSUvsERP_MFVacc_DHB.df)
    )

    # Change Title based on Selection
    titlePlot <- switch(input$ethFull,
      "Total" = "Total Fully Vaccinated Rates by Age Groups",
      "Maori" = "Maori Fully Vaccinated Rates by Age Groups"
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
      "Total" = DHBData$HSU$Total %>% filter(DHB %in% DHBSelected$DHB),
      "Maori" = DHBData$HSU$Maori %>% filter(DHB %in% DHBSelected$DHB),
      "Non-Maori" = DHBData$HSU$`Non-Maori` %>% filter(DHB %in% DHBSelected$DHB)
    )
    dataERP <- switch(input$ethDHB,
      "Total" = DHBData$ERP$Total %>% filter(DHB %in% DHBSelected$DHB),
      "Maori" = DHBData$ERP$Maori %>% filter(DHB %in% DHBSelected$DHB),
      "Non-Maori" = DHBData$ERP$`Non-Maori` %>% filter(DHB %in% DHBSelected$DHB)
    )

    data <- rbind(
      data.frame(dataHSU, population = "HSU"),
      data.frame(dataERP, population = "ERP")
    )

    # Change Title based on Selection
    titlePlot <- switch(input$ethDHB,
      "Total" = "Total Fully Vaccinated by Age Group & DHB",
      "Maori" = "Maori Fully Vaccinated by Age Group & DHB",
      "Non-Maori" = "Non-Maori Fully Vaccinated by Age Group & DHB"
    )

    
      
    dataMain <- lapply(DHBSelected$DHB, function(x) {
      data%>%filter(DHB %in% x)
    })
    
    #Use this to set all the y-axis the same (across row subplots)
    topRange <- data%>%arrange(desc(Rate_Gamma1Upr))
    topRange <- topRange[[1,'Rate_Gamma1Upr']]
    
    #Dynamically set widths.
    widthInfo = c(6,4,3,rep(2,17))
    
    plotList <- lapply(dataMain, function (x) {
      fig <- x%>%
        group_by(population)%>%
        plot_ly(x = ~factor(AgeGroup, level = level_order),
                color = ~population)%>%
        add_trace(type = 'bar', y = ~RateMult,
                  error_y = ~list(type = "x",
                                  array = Rate_Gamma1Upr - RateMult,
                                  arrayminus = RateMult - Rate_Gamma1Lwr,
                                  color = 'black',
                                  thickness = 1,
                                  width = widthInfo[length(DHBSelected$DHB)]),
                  text = paste0("Age Group: ", x$AgeGroup,
                                "<br>Rate: ", round(x$RateMult),
                                "<br>Gamma Interval: +",
                                round(x$Rate_Gamma1Upr - x$RateMult), "/ -",
                                round(x$RateMult - x$Rate_Gamma1Lwr),
                                "<br>Population: ", x$population),
                  hoverinfo = 'text',
                  showlegend = F)%>%
        layout(annotations = list(list(x = 0.5,  
                                  y = 0.95,  
                                  text = paste(unique(x$DHB)),  
                                  xref = "paper",  
                                  yref = "paper",  
                                  xanchor = "center",  
                                  yanchor = "bottom",  
                                  showarrow = FALSE)
                                  ),
               xaxis = list(title = list(text = " "),
                            tickangle = 45),
               yaxis = list(title = list(text = " "),
                            range = c(0,1.1*topRange),
                            # range=c(0, ceiling(max(
                            # aggregate(displ~cyl+class, mpg, sum)$displ)/10)*10)),
                            zerolinecolor = 'black',
                            zerolinewidth = 2,
                            gridcolor = 'black',
                            hoverformat = '.0f'),
               title = list(text = titlePlot,
                            font=list(size = 15)),
               plot_bgcolor = 'transparent',
               paper_bgcolor = 'transparent')
    })
    
    subplot(plotList, nrows = ceiling(length(DHBSelected$DHB)/4),
            shareX = T, shareY = T, margin = c(0.005,0.005, 0.035, 0.035))%>%
      layout(annotations = list(list(
        x = -0.058 , y = 0.3, text = "Rate per 100,000",
        font = list(color = "black",size = 15),
        textangle = 270,xanchor = "center",  
        yanchor = "bottom", 
        showarrow = F, xref='paper', yref='paper', size=48),
        list(
          x = 0.5 , y = -0.18, text = "Age Groups",
          font = list(color = "black",size = 15),xanchor = "center",  
          yanchor = "bottom", 
          showarrow = F, xref='paper', yref='paper', size=48)),
        margin = list(l = 50, r = 50,
                      b = 50, t = 50,
                      pad = 20))
  })
  
  #Rate Ratio Plots ----

  output$rateRatioPlot <- renderPlotly({

    data <- AllFVacc_DHBpopulation.df%>%
      filter(population %in% input$ethRatio,
             DHB == "Total")
    
    titlePlot <- switch(input$ethRatio,
      "Total" = "Total Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
      "Maori" = "Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)",
      "Non-Maori" = "Non-Maori Fully Vaccinated Rate Ratio by Age Group (HSU as baseline)"
    )

    ratioColour <- switch(input$ethRatio,
      "Non-Maori" = "#06b73c",
      "Total" = "#639bfb",
      "Maori" = "#fb746c"
    )
    
    figRatio <- data%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order))%>%
      add_trace(type = "bar",
                y = ~RelativeRisk,
                error_y = ~list(type = "x",
                                array = RelativeRiskUpr - RelativeRisk,
                                arrayminus = RelativeRisk - RelativeRiskLwr,
                                color = 'black',
                                thickness = 1,
                                width = 14),
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(data$RelativeRisk, 2),
                              "<br>Gamma Interval: +",
                              round(data$RelativeRiskUpr - data$RelativeRisk, 4), "/ -",
                              round(data$RelativeRisk - data$RelativeRiskLwr, 4)),
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

  output$ratioComparsion <- renderPlotly({
    
    data <- AllFVacc_DHBpopulation.df %>%
      filter(population %in% input$ethRatio2,
             DHB == "Total")
    widthInfo2 <- c(14,6,4)
    
    figPlot <- data%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order),
              color = ~population)%>%
      add_trace(type = 'bar',
                y = ~RelativeRisk,
                # color = ~population,
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(data$RelativeRisk, 2),
                              "<br>Ethnicity: ", data$population,
                              "<br>Gamma Interval: +",
                              round(data$RelativeRiskUpr - data$RelativeRisk, 4), "/ -",
                              round(data$RelativeRisk - data$RelativeRiskLwr, 4)),
                hoverinfo = 'text')%>%
                # error_y = ~list(type = "x",
                #                 array = data$RelativeRiskUpr - data$RelativeRisk,
                #                 arrayminus = data$RelativeRisk - data$RelativeRiskLwr,
                #                 color = 'black',
                #                 thickness = 1,
                #                 width = 4))%>%
      layout(xaxis = list(title = list(text = "Age Groups",
                                           size = 2),
                              tickangle = 45),
                 yaxis = list(title = list(text = "Rate Ratio",
                                           size = 2),
                              zerolinecolor = 'black',
                              zerolinewidth = 2,
                              gridcolor = 'black',
                              hoverformat = '.0f'),
                 title = list(text = "Fully Vaccinated Rate Ratio by Age Group & Ethnicity (HSU as baseline)",
                              font=list(size = 15)),
                 plot_bgcolor = 'transparent',
                 paper_bgcolor = 'transparent',
             legend = list(bgcolor = 'transparent',
                           title = list(text = "Population")
      ))
    
    suppressWarnings(figPlot)
  })
  
  
  #Count Plots ----
  
  output$ratioCountPlot <- renderPlotly({
    
    data <- AllFVacc_DHBpopulation.df%>%
      filter(population %in% input$ethCount,
             DHB == "Total")
    
    
    titlePlot <- switch(input$ethCount,
                        "Total" = "Total Fully Vaccinated Count by Age Group (HSU as baseline)",
                        "Maori" = "Maori Fully Vaccinated Count by Age Group (HSU as baseline)",
                        "Non-Maori" = "Non-Maori Fully Vaccinated Count by Age Group (HSU as baseline)"
    )
    
    ratioColour <- switch(input$ethCount,
                          "Non-Maori" = "#06b73c",
                          "Total" = "#639bfb",
                          "Maori" = "#fb746c"
    )
    
    figRatio <- data%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order))%>%
      add_trace(type = "bar",
                y = ~Count,
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Count: ", data$Count),
                hoverinfo = 'text',
                showlegend = F,
                color = ratioColour)
    
    figRatio <- figRatio%>%
      layout(xaxis = list(title = list(text = "Age Groups",
                                       size = 2),
                          tickangle = 45),
             yaxis = list(title = list(text = "Count",
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
    
    figRatio
    
    
  })
  
  output$countCompare <- renderPlotly({
    data <- AllFVacc_DHBpopulation.df %>%
      filter(population %in% input$ethCount2,
             DHB == "Total")
    widthInfo2 <- c(14,6,4)
    
    figPlot <- data%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order),
              color = ~population)%>%
      add_trace(type = 'bar',
                y = ~Count,
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Count: ", round(data$Count),
                              "<br>Ethnicity: ", data$population),
                hoverinfo = 'text')%>%
      layout(xaxis = list(title = list(text = "Age Groups",
                                       size = 2),
                          tickangle = 45),
             yaxis = list(title = list(text = "Count",
                                       size = 2),
                          zerolinecolor = 'black',
                          zerolinewidth = 2,
                          gridcolor = 'black',
                          hoverformat = '.0f'),
             title = list(text = "Fully Vaccinated Count by Age Group & Ethnicity (HSU as baseline)",
                          font=list(size = 15)),
             plot_bgcolor = 'transparent',
             paper_bgcolor = 'transparent',
             legend = list(bgcolor = 'transparent',
                           title = list(text = "Population")
             ))
    
    
  })
  
}
