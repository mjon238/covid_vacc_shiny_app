# Server
function(input, output) {
  
  #Titles -----
  output$graphTitle <- renderUI({
    title <- switch(input$items,
                    "vacc" = "Fully Vaccinated",
                    "ratio" = "Rate Ratios",
                    "ratio2" = "Rate Ratios Comparison"
    )
    HTML("<b>",title,"</b>")
  })
  
  #Chart Section Title
  output$chartTitle1 <- output$chartTitle2 <- output$chartTitle3 <- output$chartTitle4 <- output$chartTitle5 <- renderUI({
    
    HTML("<font size='+1'><b> Charts </b></font>")
  })
  
  #"Brief Info " Title
  output$textTitle <- renderUI({
    HTML("<font size='+1'><b> Brief Information </b></font>")
  })
  
  #Raw Data Section Title
  output$rawDataTitle1 <- output$rawDataTitle2 <- renderUI({
    
    HTML("<font size='+1'><b> Raw Data </b></font>")
  })
  
  
  
  #Reactive Text -----
  output$textMain <- renderUI({
    menuName <- switch(input$items,
                       "vacc" = HTML("<b> HSU vs. ERP Data</b> compares NZ Vaccination Rates based on a unique population denominator, ERP and HSU."),
                       "ratio" = HTML("<b>Rate Ratios</b> acts as a means to compare vaccination rates between Maori and the General population."))
menuName
  })
  output$textERP <- renderUI({
    HTML("<b> ERP </b>",
         "<br><ul><li> \'Estimated Resident Population\'</li>",
         "<li>Measured by Stats NZ</li>",
         "<li>Updated regularly</li>",
         "<li>Data is captured using Census</li>",
         "<li></li>",
         "</ul>")
  })
  output$textHSU <- renderUI({
    HTML("<b> HSU </b>",
         "<br><ul><li> \'Health Service User (Population)\'</li>",
         "<li>Measured by the Ministry of Helath</li>",
         "<li>Measure of the population that interacts with the healthcare system</li>",
         "<li>GP appointments, ED visits etc.</li>",
         "<li>Gives location specific data</li>",
         "<li>Misses a large proportion of the \'at-risk\' population</li>",
         "</ul>")
  })
  
  output$textRateRatios <- renderUI({
    
    ethnicity <- c("Total", "Maori")

    HTML("<ul> <li> <b>Rate Ratio</b> measures the selected ethnicity divided by unselected ethnicity </li>",
         "<li> For example you have selected <em>", input$ethRatio, "</em>divided by <em>", 
         ethnicity[which(ethnicity != input$ethRatio)]," </em> </li> </ul>" )
    
  })
  #Creating reactive values -----
  DHBSelected <- reactiveValues()
  
  observe({
    
    maxDHB <- Data_All[[input$genderVaccDHB]]$DHB%>%
      filter(ethnicity == input$ethDHB)%>%
      arrange(desc(Rate_Gamma1Upr))

    maxDHB <- maxDHB[[1,1]]
    
    minDHB <- Data_All[[input$genderVaccDHB]]$DHB%>%
      filter(DHB != "Overseas / Unknown", 
             ethnicity == input$ethDHB)%>%
      arrange((Rate_Gamma1Lwr))
    
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
    
    data <- Data_All[[input$genderVacc]]$Nationwide%>%
      filter(ethnicity == input$ethFull)

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
    
    data <- Data_All[[input$genderVaccDHB]]$DHB%>%
      filter(ethnicity == input$ethDHB,
             DHB %in% DHBSelected$DHB)

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
    
    data <- Data_All[[input$genderRatio]]$RatioInfo%>%
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
    
    data <- Data_All[[input$genderRatio]]$RatioInfo%>%
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
    
    data <- Data_All[[input$genderCount]]$RatioInfo%>%
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
    data <- Data_All[[input$genderCount]]$RatioInfo %>%
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
  
  
  
  
  #Data Tables ----
  
  output$tableFull <- renderDataTable({
    

      data <- switch(input$vaccType,
                     "Nationwide" = Data_All[[input$genderVacc]]$Nationwide%>%
                       filter(ethnicity == input$ethFull),
                     "By DHB" = Data_All[[input$genderVaccDHB]]$DHB%>%
                       filter(ethnicity == input$ethDHB)
                     )
      
      
      
      datatable(data%>%
                  select("DHB", "population", "ethnicity",
                         "AgeGroup", "Count", "Total",
                         "Rate", "Rate_Gamma1Lwr", "Rate_Gamma1Upr", 
                         "RateMult", "Variance", 
                         "Weights"),
                colnames = c("DHB", "Pop.Measure", "Ethnicity",
                             "Age Group", "Count", "Total",
                             "Rate", "Gamma Lwr", "Gamma Upr", 
                             "Rate Multiplier", "Variance", 
                             "Weights"),
                options = list(scrollX = TRUE)
                )%>%
        formatRound(columns = c("Rate", "RateMult", "Variance",
                                "Rate_Gamma1Lwr", "Rate_Gamma1Upr",
                                "Weights"), 
                    digits = c(4, 0, 7, 
                               0, 0, 0),
                    mark = "")

  })
  
  
  output$tableRate <- renderDataTable({
    gender <- switch(input$ratioTabs,
                     "Rate Ratio" = input$genderRatio,
                     "Rate Difference" = input$genderDifference,
                     "Count" = input$genderCount)
    
    data <- Data_All[[gender]]$RatioInfo
    
    datatable(data%>%
                select("DHB", "AgeGroup", "population",
                       "Count",
                       # "Total", "Weights", 
                       # "Rate","RateMult", "Variance", 
                       # "Rate_KeyfitzLwr", "Rate_KeyfitzUpr", 
                       # "Rate_Gamma1Lwr", "Rate_Gamma1Upr", 
                       # "RateBaseline", "VarianceBaseline", 
                       # "RateBaselineLwr", "RateBaselineUpr",
                       "RelativeRisk", 
                       "RelativeRiskLwr", "RelativeRiskUpr"
                       # "AttributableRisk",
                       # "AttributableRiskLwr", "AttributableRiskUpr"
                       ),
              colnames = c("DHB", "Age Group", 
                           "Ethnicity", "Count", "Relative Risk",
                           "Relative Risk Lwr", "Relative Risk Upr"),
              options = list(scrollX = TRUE))%>%
      formatRound(columns = c(
                              # "Weights", "Rate", 
                              # "RateMult", "Variance",
                              # "Rate_KeyfitzLwr", "Rate_KeyfitzUpr",
                              # "Rate_Gamma1Lwr", "Rate_Gamma1Upr",
                              # "RateBaseline", "VarianceBaseline",
                              # "RateBaselineLwr", "RateBaselineUpr",
                              "RelativeRisk", 
                              "RelativeRiskLwr", "RelativeRiskUpr"
                              # "AttributableRisk",
                              # "AttributableRiskLwr", "AttributableRiskUpr"
                              ),
                  digits = c(2,2,2),
                  mark = "")
    
  })
  
}
