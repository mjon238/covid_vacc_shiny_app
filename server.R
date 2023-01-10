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

    HTML("<ul> <li> <b>Rate Ratio</b> measures the ERP vaccination rate divided by the HSU vaccination rate. </li>",
         "<li> A Rate Ratio greater than 1 inidicates that the ERP vaccination rate is higher than the HSU. </li>",
         "<li> <b>Rate Difference</b> measures the ERP vaccination rate minus the HSU vaccination rate.</li>",
         "<li> <b>Count</b> measures the actual count of inidividuals vaccinated.</li></ul>")

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

  output$plotDHB <- renderPlotly({
    
    data <- Data_All[[input$genderVaccDHB]]$DHB%>%
      filter(ethnicity == input$ethDHB,
             DHB %in% DHBSelected$DHB)

    genderTitle <- switch(input$genderVaccDHB,
                          "Male" = "Male ",
                          "Female" = "Female ")
    
    # Change Title based on Selection
    titlePlot <- switch(input$ethDHB,
                        "Total" = paste0("Total ", genderTitle,"Fully Vaccinated Rates by Age Groups"),
                        "Maori" = paste0("Maori ", genderTitle, "Fully Vaccinated Rates by Age Groups"),
                        "Non-Maori" = paste0("Non-Maori ", genderTitle ,"Fully Vaccinated Rates by Age Groups")
                        
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
               title = list(text = paste0(titlePlot,"<br> <sup> Green: ERP, Blue: HSU </sup>"),
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

  
  #UNUSED
  output$rateRatioPlot <- renderPlotly({
    
    data <- Data_All[[input$genderRatio]]$RatioInfo%>%
      filter(population %in% input$ethRatio,
             DHB == "Total")
    

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

  output$ratioComparsion <- renderPlotly({
    
    
    data <- Data_All[[input$genderRatio]]$RatioInfo%>%
      arrange(`population`)%>%
      filter(population %in% input$ethRatio2,
             DHB == "Total")
    
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
    
   
    
    
    widthInfo2 <- c(14,6,4)
    
    
    titlePlotGender <- switch(input$genderRatio,
                        "Male" = "Male ",
                        "Female" = "Female ",
                        "Total" = NULL)
    
    removeName <- case_when(input$ratioTabs == "Count" ~ "",
                            TRUE ~ "(HSU as baseline)")
    
    
    #COLOURS
    #Non-Maori = #fc8d62
    #Total = #8da0cb
    #Maori = #66c2a5
    
    figPlot <- data%>%
      plot_ly(x = ~factor(AgeGroup, level = level_order),
              color = ~population)%>%
      add_trace(type = 'bar',
                y = ~dataInterest,
                text = paste0("Age Group: ", data$AgeGroup,
                              "<br>Relative Risk: ", round(dataInterest, 2),
                              "<br>Ethnicity: ", data$population,
                              "<br>Gamma Interval: +",
                              round(dataInterestUpr - dataInterest, 4), "/ -",
                              round(dataInterest - dataInterestLwr, 4)),
                hoverinfo = 'text',
                error_y = ~list(type = "x",
                                array = dataInterestUpr - dataInterest,
                                arrayminus = dataInterest - dataInterestLwr,
                                color = 'black',
                                thickness = 1,
                                width = widthInfo2[[length(input$ethRatio2)]]))%>%
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
    
    suppressWarnings(figPlot)
  })
  

  
  
  #Data Tables ----
  
  output$tableFull <- renderDataTable({
    

      data <- switch(input$vaccType,
                     "Nationwide" = Data_All[[input$genderVacc]]$Nationwide%>%
                       filter(ethnicity == input$ethFull),
                     "By DHB" = Data_All[[input$genderVaccDHB]]$DHB%>%
                       filter(ethnicity == input$ethDHB,
                              DHB %in% DHBSelected$DHB)
                     )
      
      
      
      datatable(data%>%
                  select("DHB", "ethnicity", "population",
                         "AgeGroup",
                         "Rate", "RateMult", 
                         "Rate_Gamma1Lwr", "Rate_Gamma1Upr", "Variance", 
                         "Weights"),
                colnames = c("DHB",  "Ethnicity","Pop.Measure",
                             "Age Group",
                             "Rate", "Rate per 100k",
                             "Gamma Lwr", "Gamma Upr", "Variance", 
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

    data <- Data_All[[input$genderRatio]]$RatioInfo%>%
      arrange(`population`)%>%
      filter(population %in% input$ethRatio2,
             DHB == "Total")
    
    
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
                       "RelativeRiskLwr", "RelativeRiskUpr",
                       "AttributableRisk",
                       "AttributableRiskLwr", "AttributableRiskUpr"
                       ),
              colnames = c("DHB", "Age Group", 
                           "Ethnicity", "Count", "Relative Risk",
                           "Relative Risk Lwr", "Relative Risk Upr",
                           "AttributableRisk",
                           "AttributableRiskLwr", "AttributableRiskUpr"
              ),
              options = list(scrollX = TRUE))%>%
      formatRound(columns = c(
                              # "Weights", "Rate", 
                              # "RateMult", "Variance",
                              # "Rate_KeyfitzLwr", "Rate_KeyfitzUpr",
                              # "Rate_Gamma1Lwr", "Rate_Gamma1Upr",
                              # "RateBaseline", "VarianceBaseline",
                              # "RateBaselineLwr", "RateBaselineUpr",
                              "RelativeRisk", 
                              "RelativeRiskLwr", "RelativeRiskUpr",
                              "AttributableRisk",
                              "AttributableRiskLwr", "AttributableRiskUpr"
                              ),
                  digits = c(2,2,2, 
                             3,3,3),
                  mark = "")
    
  })
  
}
