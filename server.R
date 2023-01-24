# Server
function(input, output) {
  
  #Titles -----
  output$graphTitle <- renderUI({
    title <- switch(input$items,
                    "vacc" = "HSU vs. ERP Data",
                    "ratio" = "Comparisons")
    
    HTML("<b>",title,"</b> <br>")
  })
  
  #Chart Section Title
  output$chartTitle2 <- output$chartTitle3 <-  renderUI({
    
    HTML("<font size='+1'><b> Charts </b></font>")
  })
  
  #Raw Data Section Title
  output$rawDataTitle1 <- output$rawDataTitle2 <- renderUI({
    
    HTML("<font size='+1'><b> Raw Data </b></font>")
  })
  
  
  
  #Reactive Text -----
  output$textHSUvsERP <- renderUI({
    HTML("<font size='+1'><b> Brief Information </b></font>",
         "<br> <b> HSU vs. ERP Data</b> compares NZ Vaccination Rates based on a unique population denominator, ERP and HSU.",
         "<b> ERP </b>",
         "<br><ul><li> \'Estimated Resident Population\'</li>",
         "<li>Measured by Stats NZ</li>",
         "<li>Updated regularly</li>",
         "<li>Data is captured using Census</li>",
         "<li></li>",
         "</ul>",
         "<b> HSU </b>",
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

    HTML("<font size='+1'><b> Brief Information </b></font>",
         "<br> <b>Rate Ratios</b> acts as a means to compare vaccination rates between Maori and the General population.",
         "<ul> <li> <b>Rate Ratio</b> measures the ERP vaccination rate divided by the HSU vaccination rate. </li>",
         "<li> A Rate Ratio greater than 1 inidicates that the ERP vaccination rate is higher than the HSU. </li>",
         "<li> <b>Rate Difference</b> measures the ERP vaccination rate minus the HSU vaccination rate.</li>",
         "<li> <b>Count</b> measures the actual count of inidividuals vaccinated.</li></ul>")

  })
  #Creating reactive values -----
  DHBSelected <- reactiveValues()
  
  observe({
    
    #First HSUvsERP
    
    maxDHB <- Data_All[[input$dateFull]][[input$genderVacc]]$HSUvsERP%>%
      filter(DHB != "Nationwide",
             ethnicity == input$ethFull)%>%
      arrange(desc(Rate_Gamma1Upr))

    maxDHB <- maxDHB[[1,1]]
    
    minDHB <- Data_All[[input$dateFull]][[input$genderVacc]]$HSUvsERP%>%
      filter(DHB != "Overseas / Unknown", 
             DHB != "Nationwide",
             ethnicity == input$ethFull)%>%
      arrange((Rate_Gamma1Lwr))
    
    minDHB <- minDHB[[1,1]]
    
    if(input$region == "By DHB"){
    
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
    } else {
      
      DHBSelected$DHB = "Nationwide"
      
    }
    
    #Second Ratio
    if(input$region2 == "By DHB"){
      
      DHBSelected$Ratio = switch(input$groupDHB2,
                                "All" = listOfDHB,
                                "Select DHB\'s" = input$DHB2,
                                "Auckland Region" = c("Auckland", "Waitemata",
                                                      "Counties Manukau"),
                                "Wellington Region" = c("Capital and Coast", "Hutt Valley"),
                                "North Island" = c(
                                  "Auckland", "Bay of Plenty", "Capital and Coast",
                                  "Counties Manukau", "Hawkes Bay", "Hutt Valley", "Lakes",
                                  "MidCentral", "Northland", "Tairawhiti", "Taranaki", "Waikato",
                                  "Wairarapa", "Waitemata", "Whanganui"),
                                "South Island" = c("Canterbury", "Nelson Marlborough", "Southern",         
                                                   "South Canterbury", "West Coast" )
      )
    } else {
      
      DHBSelected$Ratio = "Nationwide"
      
    }
    
  })

  
  #Show/Hide Group DHB Inputs
  observe({
    
    if(input$groupDHB == 'Select DHB\'s') {
      show("DHB")
    } else {
      hide("DHB")
    }
    
    
    if(input$groupDHB2 == 'Select DHB\'s'){
      show("DHB2")
      
    } else{
      hide("DHB2")
      
    }
  })
  
  #Create Plots ----
  #ERP vs. HSU Plots ----
  plotFull <- reactive({
    data <- Data_All[[input$dateFull]][[input$genderVacc]]$HSUvsERP%>%
      filter(ethnicity == input$ethFull,
             DHB %in% DHBSelected$DHB)
    
    genderTitle <- switch(input$genderVacc,
                          "Male" = "Male ",
                          "Female" = "Female ")
    
    # Change Title based on Selection
    titlePlot <- switch(input$ethFull,
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
    widthInfo = c(8,4,3,rep(2,17))
    
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
                  textposition = 'none',
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
                     zerolinecolor = 'black',
                     zerolinewidth = 2,
                     gridcolor = 'black',
                     hoverformat = '.0f'),
        title = list(text = paste0(titlePlot,"<br> <sup> Green: ERP, Blue: HSU </sup>"),
                     font=list(size = 15)),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent')
    })
    
    figPlot <- subplot(plotList, nrows = ceiling(length(DHBSelected$DHB)/4),
                                  shareX = T, shareY = T, margin = c(0.005,0.005, 0.035, 0.035))%>%
      layout(annotations = list(list(
        x = -0.058 , y = 0.3, text = "Rate per 100,000",
        font = list(color = "black",size = 15),
        textangle = 270,xanchor = "center",  
        yanchor = "bottom", 
        showarrow = F, xref='paper', yref='paper', size=48),
        list(
          x = 0.5 , y = -0.14, text = "Age Groups",
          font = list(color = "black",size = 15),xanchor = "center",  
          yanchor = "bottom", 
          showarrow = F, xref='paper', yref='paper', size=48)),
        margin = list(l = 50, r = 50,
                      b = 50, t = 50,
                      pad = 20))
    
    figPlot
  })
  output$plotDHB <- renderPlotly({
    plotFull()
   
  })
  

  #Rate Ratio Plots ----
  plotCompare <- reactive({
    #Step 2 Create Main Data
    
    data <- Data_All[[input$dateRatio]][[input$genderRatio]]$RatioInfo%>%
      arrange(`population`)%>%
      filter(population %in% input$ethRatio2,
             DHB %in%  DHBSelected$Ratio)
    
    mainNames <- colnames(data)
    
    data <- data%>%
      pivot_wider(names_from = population,
                  values_from = c(mainNames[-c(1,2,3)]))
    
    dataMain <- lapply(DHBSelected$Ratio, function(x) {
      data%>%filter(DHB %in% x)
    })
    
    
    #Step 3 Create Titles
    genderTitle <- switch(input$genderRatio,
                          "Male" = "Male ",
                          "Female" = "Female ",
                          "Total" = NULL)
    removeName <- case_when(input$ratioTabs == "Count" | 
                              input$ratioTabs == "ERP Rate" |
                              input$ratioTabs == "HSU Rate" ~ "",
                            TRUE ~ "(HSU as baseline)")
    
    titlePlot <- paste0("Fully ", 
                        genderTitle,
                        "Vaccinated ", 
                        input$ratioTabs,
                        " by Age Group & Ethnicity ",
                        removeName)
    
    
    #Step 4: Create Top Range and Eror bar widths
    
    dataInterestMain <- switch(input$ratioTabs,
                               "ERP Rate" = c("ERP_Rate_Gamma1Upr", "HSU_Rate_Gamma1Upr"),
                               "HSU Rate" = c("ERP_Rate_Gamma1Upr", "HSU_Rate_Gamma1Upr"),
                               "Rate Ratio" = "RelativeRiskUpr",
                               "Rate Difference" = c("AttributableRiskUpr", 
                                                     "AttributableRiskLwr"),
                               "Count" = "Count")
    
    rangeMain <- data%>%
      select(contains(c(dataInterestMain)))
    
    rangeMain <- rangeMain%>%
      pivot_longer(cols = colnames(rangeMain),
                   names_to = "Desc",
                   values_to = "values")%>%
      na.omit()
    
    topRange <- max(rangeMain$values)
    bottomRange <- min(0, rangeMain$values)
    
    #Dynamically set widths.
    
    widthInfo = list("1" = c(15,7,5,rep(4,17)),
                     "2" = c(9,4,3,rep(2,17)),
                     "3" = c(7,3,2,rep(2,17)))
    
    widthInfoMain = widthInfo[[paste0(length(input$ethRatio2))]]
    
    dataTitle <- switch(input$ratioTabs,
                        "ERP Rate" = "ERP Rate",
                        "HSU Rate" = "HSU Rate",
                        "Rate Ratio" = "Relative Risk",
                        "Rate Difference" = "Attributable Risk",
                        "Count" = "Count")
    
    #Step 5: Create Plots
    plotList <- lapply(dataMain, function (x) {
      
      figPlot <- x%>%
        plot_ly()
      
      #Create Maori
      if("Maori" %in% input$ethRatio2){
        
        dataInterest_Maori <- switch(input$ratioTabs,
                                     "ERP Rate" = x$ERP_RateMult_Maori,
                                     "HSU Rate" = x$HSU_RateMult_Maori,
                                     "Rate Ratio" = x$RelativeRisk_Maori,
                                     "Rate Difference" = x$AttributableRisk_Maori,
                                     "Count" = x$Count_Maori)
        
        dataInterestUpr_Maori <- switch(input$ratioTabs,
                                        "ERP Rate" = x$ERP_Rate_Gamma1Upr_Maori,
                                        "HSU Rate" = x$HSU_Rate_Gamma1Upr_Maori,
                                        "Rate Ratio" = x$RelativeRiskUpr_Maori,
                                        "Rate Difference" = x$AttributableRiskUpr_Maori,
                                        "Count" = NULL)
        
        dataInterestLwr_Maori <- switch(input$ratioTabs,
                                        "ERP Rate" = x$ERP_Rate_Gamma1Lwr_Maori,
                                        "HSU Rate" = x$HSU_Rate_Gamma1Lwr_Maori,
                                        "Rate Ratio" = x$RelativeRiskLwr_Maori,
                                        "Rate Difference" = x$AttributableRiskLwr_Maori,
                                        "Count" = NULL)
        
        figPlot <- figPlot%>%
          add_trace(x = ~factor(AgeGroup, level = level_order),
                    type = "bar",
                    y = ~dataInterest_Maori,
                    name = "Maori",
                    marker = list(color = 'rgba(102,194,165,255)'),
                    text = paste0("Age Group: ", x$AgeGroup,
                                  "<br>", dataTitle, ": ", round(dataInterest_Maori, 2),
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
                                    width =  widthInfoMain[length(DHBSelected$Ratio)])) }
      
      #Create for Non-Maori
      if("Non-Maori" %in% input$ethRatio2){
        
        dataInterest_Non_Maori <- switch(input$ratioTabs,
                                         "ERP Rate" = x$`ERP_RateMult_Non-Maori`,
                                         "HSU Rate" = x$`HSU_RateMult_Non-Maori`,
                                         "Rate Ratio" = x$`RelativeRisk_Non-Maori`,
                                         "Rate Difference" = x$`AttributableRisk_Non-Maori`,
                                         "Count" = x$`Count_Non-Maori`)
        
        dataInterestUpr_Non_Maori <- switch(input$ratioTabs,
                                            "ERP Rate" = x$`ERP_Rate_Gamma1Upr_Non-Maori`,
                                            "HSU Rate" = x$`HSU_Rate_Gamma1Upr_Non-Maori`,
                                            "Rate Ratio" = x$`RelativeRiskUpr_Non-Maori`,
                                            "Rate Difference" = x$`AttributableRiskUpr_Non-Maori`,
                                            "Count" = NULL)
        
        dataInterestLwr_Non_Maori <- switch(input$ratioTabs,
                                            "ERP Rate" = x$`ERP_Rate_Gamma1Lwr_Non-Maori`,
                                            "HSU Rate" = x$`HSU_Rate_Gamma1Lwr_Non-Maori`,
                                            "Rate Ratio" = x$`RelativeRiskLwr_Non-Maori`,
                                            "Rate Difference" = x$`AttributableRiskLwr_Non-Maori`,
                                            "Count" = NULL)
        
        figPlot <- figPlot%>%
          add_trace(x = ~factor(AgeGroup, level = level_order),
                    type = "bar",
                    y = ~dataInterest_Non_Maori,
                    name = "Non-Maori",
                    marker = list(color = 'rgba(252,141,98,255)'),
                    text = paste0("Age Group: ", x$AgeGroup,
                                  "<br>", dataTitle, ": ", round(dataInterest_Non_Maori, 2),
                                  "<br>Ethnicity: Non-Maori ",
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
                                    width =  widthInfoMain[length(DHBSelected$Ratio)])
                    )}
      
      
      if("Total" %in% input$ethRatio2){
        
        dataInterest_Total <- switch(input$ratioTabs,
                                     "ERP Rate" = x$ERP_RateMult_Total,
                                     "HSU Rate" = x$HSU_RateMult_Total,
                                     "Rate Ratio" = x$RelativeRisk_Total,
                                     "Rate Difference" = x$AttributableRisk_Total,
                                     "Count" = x$Count_Total)
        
        dataInterestUpr_Total <- switch(input$ratioTabs,
                                        "ERP Rate" = x$ERP_Rate_Gamma1Upr_Total,
                                        "HSU Rate" = x$HSU_Rate_Gamma1Upr_Total,
                                        "Rate Ratio" = x$RelativeRiskUpr_Total,
                                        "Rate Difference" = x$AttributableRiskUpr_Total,
                                        "Count" = NULL)
        
        dataInterestLwr_Total <- switch(input$ratioTabs,
                                        "ERP Rate" = x$ERP_Rate_Gamma1Lwr_Total,
                                        "HSU Rate" = x$HSU_Rate_Gamma1Lwr_Total,
                                        "Rate Ratio" = x$RelativeRiskLwr_Total,
                                        "Rate Difference" = x$AttributableRiskLwr_Total,
                                        "Count" = NULL)
        
        figPlot <- figPlot%>%
          add_trace(x = ~factor(AgeGroup, level = level_order),
                    type = "bar",
                    y = ~dataInterest_Total,
                    name = "Total",
                    marker = list(color = 'rgba(141,160,203,255)'),
                    text = paste0("Age Group: ", x$AgeGroup,
                                  "<br>", dataTitle, ": ", round(dataInterest_Total, 2),
                                  "<br>Ethnicity: Total ",
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
                                    width =  widthInfoMain[length( DHBSelected$Ratio)]))}
      
      
      
      
      figPlot <- figPlot%>%
        layout(annotations = list(list(x = 0.5,  
                                       y = 1,
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
                     range = 1.1*c(bottomRange, topRange),
                     zerolinecolor = 'black',
                     zerolinewidth = 2,
                     gridcolor = 'black',
                     hoverformat = '.0f'),
        title = list(text = paste0(titlePlot,"<br> <sup> Green: Maori, Orange: Non-Maori, Blue: Total </sup> <br>  "),
                     font=list(size = 15)),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent',
        showlegend = F)
    })
    
    #Step 6: Format Sub Plotss
    figPlot <- subplot(plotList, nrows = ceiling(length( DHBSelected$Ratio)/4),
                         shareX = T, shareY = T, margin = c(0.005,0.005, 0.035, 0.035))%>%
      layout(annotations = list(list(
        x = -0.058 , y = 0.4, text = case_when(input$ratioTabs == "ERP Rate" |
                                                 input$ratioTabs == "HSU Rate" ~ 
                                                 paste0(input$ratioTabs, " per 100,000"),
                                               TRUE ~ paste0(input$ratioTabs)
        ),
        font = list(color = "black",size = 15),
        textangle = 270,xanchor = "center",  
        yanchor = "bottom", 
        showarrow = F, xref='paper', yref='paper', size=48),
        list(
          x = 0.5 , y = -0.15, text = "Age Groups",
          font = list(color = "black",size = 15),xanchor = "center",  
          yanchor = "bottom", 
          showarrow = F, xref='paper', yref='paper', size=48)),
        margin = list(l = 75, r = 50,
                      b = 50, t = 80,
                      pad = 20))
    
    figPlot
  })
  output$ratioComparison <- renderPlotly({
    
    plotCompare()
   
  })

  
  #Data Tables ----
  
  dataFull <- reactive({
    
    data <- Data_All[[input$dateFull]][[input$genderVacc]]$HSUvsERP%>%
      filter(ethnicity == input$ethFull,
             DHB %in% DHBSelected$DHB)%>%
      select("DHB", "ethnicity", "population",
             "AgeGroup",
             "Rate", "RateMult", 
             "Rate_Gamma1Lwr", "Rate_Gamma1Upr", "Variance")%>%
      mutate(AgeGroup = factor(AgeGroup, levels = level_order))
    
    colnames(data) <- c("DHB",  "Ethnicity","Pop.Measure",
                 "Age Group",
                 "Rate", "Rate per 100k",
                 "Gamma Lwr", "Gamma Upr", "Variance")
    
    data
                 
                 
  })
  
  output$tableFull <- renderDataTable({
    
      datatable(dataFull(),
                options = list(scrollX = TRUE,
                               pageLength = 25)
                )%>%
        formatRound(columns = c("Rate", "Rate per 100k", "Variance",
                                "Gamma Lwr", "Gamma Upr"), 
                    digits = c(4, 0, 7, 
                               0, 0, 0),
                    mark = "")

  })
  
  
  
  comparisonData <- reactive({
    
    rowSelect <- switch(input$ratioTabs,
                        "ERP Rate" = c("ERP_RateMult", "ERP_Rate_Gamma1Lwr",
                                       "ERP_Rate_Gamma1Upr", "ERP_Variance"),
                        "HSU Rate" = c("HSU_RateMult", "HSU_Rate_Gamma1Lwr",
                                       "HSU_Rate_Gamma1Upr", "HSU_Variance"),
                        "Rate Ratio" = c("RelativeRisk", 
                                         "RelativeRiskLwr", "RelativeRiskUpr"),
                        "Rate Difference" = c("AttributableRisk",
                                              "AttributableRiskLwr", "AttributableRiskUpr"),
                        "Count" = "Count")
    
    rowSelectedNames <- switch(input$ratioTabs,
                               "ERP Rate" = c("ERP Rate Per 100k", "ERP Gamma Lwr",
                                              "ERP Gamma Upr", "ERP Variance"),
                               "HSU Rate" = c("HSU Rate Per 100k", "HSU Gamma Lwr",
                                              "HSU Gamma Upr", "HSU Variance"),
                               "Rate Ratio" = c("Relative Risk", 
                                                "Relative Risk Lwr", "Relative Risk Upr"),
                               "Rate Difference" = c("Attributable Risk",
                                                     "Attributable Risk Lwr", "Attributable Risk Upr"),
                               "Count" = "Count" )
    
    data <- Data_All[[input$dateRatio]][[input$genderRatio]]$RatioInfo%>%
      arrange(`population`)%>%
      filter(population %in% input$ethRatio2,
             DHB %in% DHBSelected$Ratio)%>%
      select("DHB", "population", "AgeGroup",
             rowSelect
      )%>%
      mutate(AgeGroup = factor(AgeGroup, levels = level_order))%>%
      arrange(DHB, population, AgeGroup)
    
    
    colnames(data) <- c("DHB", "Ethnicity", "Age Group",
                 rowSelectedNames)
    
    data
  })

  output$tableRate <- renderDataTable({

    rowSelectedNames <- switch(input$ratioTabs,
                               "ERP Rate" = c("ERP Rate Per 100k", "ERP Gamma Lwr",
                                              "ERP Gamma Upr", "ERP Variance"),
                               "HSU Rate" = c("HSU Rate Per 100k", "HSU Gamma Lwr",
                                              "HSU Gamma Upr", "HSU Variance"),
                               "Rate Ratio" = c("Relative Risk", 
                                                "Relative Risk Lwr", "Relative Risk Upr"),
                               "Rate Difference" = c("Attributable Risk",
                                                     "Attributable Risk Lwr", "Attributable Risk Upr"),
                               "Count" = "Count" )
    table <- datatable(comparisonData(),
              options = list(scrollX = TRUE,
                             pageLength = 25))
    
    if(input$ratioTabs == "Count"){
      table
      
    } else if (input$ratioTabs %in% c("ERP Rate", "HSU Rate")){
      
      table%>%
        formatRound(columns = c(rowSelectedNames),
                    digits = c(0,0,0,7),
                    mark = "")
      
    } else {
      
      table%>%
        formatRound(columns = c(rowSelectedNames),
                    digits = c(3,3,3),
                    mark = "")
      
      
    }
           

  })
  
  
  # Download Buttons -----
  
  # Text
  
  output$downloadTextMain1 <- output$downloadTextMain2 <- renderUI({
    
    HTML("<b> Select from the below options to Download.</b>",
         "<br> Graphs and Data downloaded are filtered by sidebar inputs.")
  })
  
  
  #HSU VS. ERP Download Data -----
  output$fullDataDownloadTitle <- renderUI({
    HTML("<b> Download Filtered HSU vs. ERP Data")
  })
  output$downloadFullData <- downloadHandler(
    filename = function() {
      paste0(gsub("\\-", "_", input$dateRatio), 
             "_HSUvsERP_Data", ".csv")
    },
    content = function(file) {
      write.csv(dataFull(), file)
    }
  )
  
  output$fullChartDownloadTitle <- renderUI({
    HTML("<b> Download Full HSU vs. ERP Chart")
  })
  output$downloadFullPlot <- downloadHandler(
    filename = function(){
      paste0("HSUvsERP_Chart_", 
             gsub("\\-", "_", input$dateFull),
             ".png")
    },
    content = function(file) {
      
      export(plotFull(), file)
      
    }
  )
  
  
  output$fullRawDataDownload <- renderUI({
    HTML("<b> Download Complete Raw Data")
  })
  output$downloadFullRawData <- downloadHandler(
    filename = function() {
      paste0(gsub("\\-", "_", input$dateRatio), 
             "_Complete_HSUvsERP_Raw_Data", ".csv")
    },
    content = function(file) {
      write.csv(Data_All[[input$dateFull]][[input$genderVacc]]$HSUvsERP, file)
    }
  )
  
  
  #Comparison Download Data -----
  output$compareDataDownloadTitle <- renderUI({
    HTML("<b> Download", input$ratioTabs, "Filtered Data")
  })
  output$downloadRatioData <- downloadHandler(
    filename = function() {
      paste0(gsub("\\-", "_", input$dateRatio),
             "_", sub(" ", "_", input$ratioTabs),
             "_Comparison_Data", ".csv")
    },
    content = function(file) {
      write.csv(comparisonData(), file)
    }
  )
  
  output$compareChartDownloadTitle <- renderUI({
    HTML("<b> Download", input$ratioTabs, "Comparison Chart")
  })
  output$downloadRatioPlot <- downloadHandler(
    filename = function(){
      paste0(sub(" ", "_", input$ratioTabs),
             "_Chart_", 
             gsub("\\-", "_", input$dateRatio),
              ".png")
    },
    content = function(file) {
      
      export(plotCompare(), file)

          }
  )

  output$compareAllDownloadTitle <- renderUI({
    HTML("<b> Download All Comparison Data </b>")
  })
  output$downloadAllDataRatio <- downloadHandler(
    filename = function(){
      paste0(gsub("\\-", "_", input$dateRatio),
             "_All_Comparison_Data.csv")
    },
    content = function(file) {
      
      write.csv(Data_All[[input$dateRatio]][[input$genderRatio]]$RatioInfo, file)
      
    }
  )
  
  
}
