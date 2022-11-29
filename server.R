# Server
function(input, output) {

  # largestDHB <- reactiveVal()
    
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
                            "High vs. Low" = c(maxDHB, minDHB),
                            "Custom" = input$DHB,
                            "Auckland Region" = c("Auckland", "Waitemata",
                                         "Counties Manukau"),
                            "North Island" = c(
                              "Auckland", "Bay of Plenty", "Capital and Coast",
                              "Counties Manukau", "Hawkes Bay", "Hutt Valley", "Lakes",
                              "MidCentral", "Northland", "Tairawhiti", "Taranaki", "Waikato",
                              "Wairarapa", "Waitemata", "Whanganui"),
                            "South Island" = c("Canterbury", "Nelson Marlborough", "Southern",         
                                                "South Canterbury", "West Coast" ) 
    )
    # if(input$groupDHB == "High vs. Low"){
    #   DHBSelected$DHB <- c(minDHB, maxDHB)
    #   # updatePickerInput(session = getDefaultReactiveDomain(),
    #   #                   inputId = "DHB",
    #   #                   selected = DHBSelected$DHB)
    #   }
    # else if(input$groupDHB == "Custom"){
    #   DHBSelected$DHB <- input$DHB
    # 
    # } else i
    
  })

  observe({
    
    # toggleState(id = "DHB",
    #             condition = input$groupDHB == "Custom")
    
    if(input$groupDHB == 'Custom') {
      show("DHB")
    } else {
      hide("DHB")
    }
    # 
  })
  
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

    # if(input$groupDHB == "High vs. Low") {
    #   
    #   req()
    # }

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
    widthInfo = c(8,5,3,rep(2,17))
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
                            range = c(0,1.1*max(data$Rate_Gamma1Upr)),
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
            shareX = T, shareY = T, margin = c(0.005,0.005, 0.05, 0.05))  %>%
      layout(annotations = list(list(
        x = -0.08 , y = 0.3, text = "Rate per 100,000",
        font = list(color = "black",size = 15),
        textangle = 270,xanchor = "center",  
        yanchor = "bottom", 
        showarrow = F, xref='paper', yref='paper', size=48),
        list(
          x = 0.5 , y = -0.35, text = "Age Groups",
          font = list(color = "black",size = 15),xanchor = "center",  
          yanchor = "bottom", 
          showarrow = F, xref='paper', yref='paper', size=48)),
        margin = list(l = 50, r = 50,
                      b = 50, t = 50,
                      pad = 20))
  })

  output$rateRatioPlot <- renderPlotly({
    ##### Ratio Plots
    dataRatio <- switch(input$ethRatio,
      "Total" = subset(TFVacc_DHBpopulation.df, DHB == "Total"),
      "Maori" = subset(MFVacc_DHBpopulation.df, DHB == "Total"),
      "Non-Maori" = subset(NMFVacc_DHBpopulation.df, DHB == "Total")
    )

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
    
    figRatio <- dataRatio%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order))%>%
      add_trace(type = "bar",
                y = ~RelativeRisk,
                error_y = ~list(type = "x",
                                array = RelativeRiskUpr - RelativeRisk,
                                arrayminus = RelativeRisk - RelativeRiskLwr,
                                color = 'black',
                                thickness = 1,
                                width = 14),
                text = paste0("Age Group: ", dataRatio$AgeGroup,
                              "<br>Relative Risk: ", round(dataRatio$RelativeRisk, 2),
                              "<br>Gamma Interval: +",
                              round(dataRatio$RelativeRiskUpr - dataRatio$RelativeRisk, 4), "/ -",
                              round(dataRatio$RelativeRisk - dataRatio$RelativeRiskLwr, 4)),
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
    
    figRatio
  })

  output$ratioComparsion <- renderPlotly({
    dataFilter <- AllFVacc_DHBpopulation.df %>%
      filter(population == input$eth2,
             DHB == "Total")
    
    figPlot <- dataFilter%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order),
              color = ~population)%>%
      add_trace(type = 'bar',
                y = ~RelativeRisk,
                color = ~population,
                text = paste0("Age Group: ", dataFilter$AgeGroup,
                              "<br>Relative Risk: ", round(dataFilter$RelativeRisk, 2),
                              "<br>Ethnicity: ", dataFilter$population),
                              # "<br>Gamma Interval: +",
                              # round(dataFilter$RelativeRiskUpr - dataFilter$RelativeRisk, 2), "/ -",
                              # round(dataFilter$RelativeRisk - dataFilter$RelativeRiskLwr, 2)),
                hoverinfo = 'text')%>%
                # error_y = ~list(type = "x",
                #                 array = RelativeRiskUpr - RelativeRisk,
                #                 arrayminus = RelativeRisk - RelativeRiskLwr,
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

    # ggplotly(ggplot(subset(datafilter), aes(x = factor(AgeGroup, level = level_order), y = RelativeRisk, fill = population, group = population)) +
      # geom_col(position = "dodge") +
      # plot_annotation(title = "Fully Vaccinated Rate Ratio by Age Group & Ethnicity (HSU as baseline)") +
      # labs(
      #   y = "Rate Ratio",
      #   x = "Age Groups"
      # ) +
      # scale_fill_manual(
      #   values = c("Non-Maori" = "#06b73c", "Total" = "#639bfb", "Maori" = "#fb746c"),
      #   name = "Population"
      # ) &
      # 
      # theme(
      #   plot.title = element_text(hjust = 0.5),
      #   plot.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5"),
      #   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      #   panel.grid.minor = element_line(size = 0.25, linetype = "solid", colour = "white"),
      #   panel.grid.major = element_line(size = 0.5, linetype = "solid", colour = "white"),
      #   panel.background = element_rect(fill = "#ECF0F5", colour = "#ECF0F5", size = 2, linetype = "solid"),
      #   legend.background = element_rect(fill = "#ECF0F5")
      # ))
  })
}
