# UI
 # fluidPage(
# shinyUI(

#Start UI ----
  dashboardPage(
    title = "",
    dashboardHeader(
      title = "COVID-19 Vaccination"
    ),
    
    #Menu Items (Inputs) ----
    dashboardSidebar(
      tags$head(tags$style(".wrapper {overflow: visible !important;}")),
      sidebarMenu(
        id = "items",
        menuItem(tabName = "vacc", "Comparing HSU & ERP"),
        menuItem(tabName = "ratio", "Comparing Ethnicities"),
        menuItem(tabName = "about", "About")
        
      ),
      div(style = "margin-top: -10px;"),
      hr(),
      div(style = "margin-bottom: -20px;"),
      
      #HSU vs. ERP Inputs ------
      #Need this to activate hide/show input functions
      useShinyjs(),
      
      conditionalPanel(condition = 
                         "input.items == 'vacc'",


        #Date
        selectInput(
          inputId = "dateFull",
          label = "Select Date",
          choices = c(
                      "2023-01-17"),
          selected = "2021-11-01",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        #Gender
        selectInput(
          inputId = "genderVacc",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        #Ethincity
        selectInput(
          inputId = "ethFull",
          label = "Select Ethnicity",
          choices = c("Total", "Maori", "Non-Maori"),
          selected = "Total",
          selectize = F
        ),
        
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        
        
        selectInput(inputId = 'region',
                    label = "Select Region",
                    choices = c("Nationwide", "By DHB"),
                    selected = "Nationwide",
                    selectize = F)
      ),

      conditionalPanel(
        condition = "input.items == 'vacc'
        && input.region == 'By DHB'",
        
        div(style = "margin-bottom: -10px;"),
        
        
        #DHB Group
        selectInput(inputId = "groupDHB",
                    label = "Select DHB Groups",
                    choices = c("All", "High vs. Low", 
                                "Auckland Region", "Wellington Region", "North Island",
                                "South Island", "Select DHB\'s"),
                    selected = "All",
                    selectize = F),
        
        div(style = "margin-bottom: -10px;"),
        
        
        #Select DHB
        pickerInput(
          inputId = "DHB",
          label = "Select DHB",
          choices = listOfDHB,
          selected = "Auckland",
          width = "200px",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
        
      ),
      

      ## Ratio Rates Inputs ----
      conditionalPanel(condition = "input.items == 'ratio'",
                       
                       

                       selectInput(inputId = "ratioTabs",
                                   label = "Select Comparison",
                                   choices = c("HSU Rate",
                                               "ERP Rate",
                                               "Rate Ratio",
                                               "Rate Difference",
                                               "Count"),
                                   selected = "Rate Ratio",
                                   selectize = F),
                       hr(),
                       div(style = "margin-bottom: -20px;"),
                       
        #Date
        selectInput(
          inputId = "dateRatio",
          label = "Select Date",
          choices = c(
                      "2023-01-17"),
          selected = "2021-11-01",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        selectInput(
          inputId = "genderRatio",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
      
    
        #Ethnicity
      pickerInput(
        inputId = "ethRatio2",
        label = "Select Ethnicities To Compare",
        choices = c("Total", "Maori"
                    , "Non-Maori"
                    ),
        selected = c("Total", "Maori"
                     , "Non-Maori"
                     ),
        multiple = T
      ),
      hr(),
      div(style = "margin-bottom: -20px;"),
      
      
      
      selectInput(inputId = 'region2',
                  label = "Select Region",
                  choices = c("Nationwide", "By DHB"),
                  selected = "Nationwide",
                  selectize = F)      
      
      ),
      
      conditionalPanel(
        condition = "input.items == 'ratio'
        && input.region2 == 'By DHB'",
        
        div(style = "margin-bottom: -10px;"),
        
        
        #DHB Group
        selectInput(inputId = "groupDHB2",
                    label = "Select DHB Groups",
                    choices = c("All", 
                                "Auckland Region", "Wellington Region", "North Island",
                                "South Island", "Select DHB\'s"),
                    selected = "All",
                    selectize = F),
        
        div(style = "margin-bottom: -10px;"),
        
        
        #Select DHB
        pickerInput(
          inputId = "DHB2",
          label = "Select DHB",
          choices = listOfDHB,
          selected = "Auckland",
          width = "200px",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
        
      )
   
  ),





  
  
    ## Body Section (Outputs) -----
    dashboardBody(
      ###  Tags ----
      tags$head(tags$style(HTML(
        '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow-x: auto;
        color: white;
      }
    '
      ))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">  </span>\');
      })
     ')),
      tags$head(
        tags$style(HTML("
                      .body { height: 90vh; overflow-y: auto;}
                      " )
        )
      ),
      tags$head(
        tags$style(
          type = 'text/css',  'plotDHB { overflow-x: scroll; }')),
      
      # Add and edit text output
      tags$head(
      tags$style("#graphTitle{color: black;
                                 font-size: 20px;
                                 }")),
      
      tags$head(tags$style("#point1Text, #point2Text, 
      #point3Text, #point4Text, #andText, #whereText, #whereText2
      {margin-left:20px;")),
      
      tags$head(
      tags$style("MathJax.Hub.Config({
      jax: ['input/TeX","output/HTML-CSS'],
      displayAlign: 'left'
    })"
      )),
      
#       HTML("div.MathJax_Display{
#    text-align: left !important;
# }")
      
      
      #Title ----
      uiOutput(outputId = "graphTitle"),
      div(style = "margin-top: 10px;"),
      
      
      conditionalPanel(
        condition = "input.items == 'vacc'",
        
        
        
        tabsetPanel(id = "tabVacc",
                    
                    tabPanel("Charts",
                             div(style = "margin-top: 10px;"),
                             
                             uiOutput("chartTitle2"),
                             br(),
                             plotlyOutput("plotDHB", width = "1200px", height = "700px")
                    ),
                    tabPanel("Raw Data",
                             div(style = "margin-top: 10px;"),
                             
                             uiOutput("rawDataTitle1"),
                             DTOutput("tableFull")
                    ),
                    # tabPanel("Brief Information",
                    #          div(style = "margin-top: 10px;"),
                    #          
                    #          fluidRow(column(12, 
                    #                          uiOutput("textHSUvsERP")
                    #                  
                    # ))),
                    tabPanel("Download",
                             div(style = "margin-top: 10px;"),
                             uiOutput("downloadTextMain2"),
                             br(),
                             
                             uiOutput("fullDataDownloadTitle"),
                             downloadButton("downloadFullData", "Download Raw Data"),
                             
                             br(),
                             div(style = "margin-top: 20px;"),
                             uiOutput("fullChartDownloadTitle"),
                             downloadButton("downloadFullPlot", "Download Plot"),
                             
                             br(),
                             div(style = "margin-top: 20px;"),
                             uiOutput("fullRawDataDownload"),
                             downloadButton("downloadFullRawData", "Download Complete Raw Data")
                             
                             
                    )
                    )
            
        ),

      
      #Ratio Item Outputs ----

          #Ratio Rates Tab ----
        conditionalPanel(condition = "input.items == 'ratio'",

            tabsetPanel(id = "tabRatio",
                        tabPanel("Charts",
                                 div(style = "margin-top: 10px;"),
                                 
                                 uiOutput("chartTitle3"),
                                 br(),
                                 plotlyOutput("ratioComparison", 
                                              width = "1200px", height = "700px")
                        ),
                        tabPanel("Raw Data",
                                 div(style = "margin-top: 10px;"),
                                 
                                 uiOutput("rawDataTitle2"),
                                 DTOutput("tableRate")
                        ),
                        # tabPanel("Brief Information",
                        #          div(style = "margin-top: 10px;"),
                        #          
                        #          fluidRow(column(12,
                        #          uiOutput("textRateRatios")))
                        # ),
                        tabPanel("Download",
                                 div(style = "margin-top: 10px;"),
                                 uiOutput("downloadTextMain1"),
                                 br(),
                                 uiOutput("compareDataDownloadTitle"),
                                 downloadButton("downloadRatioData", "Download Raw Data"),
                                 
                                 br(),
                                 div(style = "margin-top: 20px;"),
                                 uiOutput("compareChartDownloadTitle"),
                                 downloadButton("downloadRatioPlot", "Download Plot"),
                                 
                                 br(),
                                 div(style = "margin-top: 20px;"),
                                 uiOutput("compareAllDownloadTitle"),
                                 downloadButton("downloadAllDataRatio", "Download All Comparison Data")
                                 
                        )
            )

        ),
      
      #About Tab -----
      conditionalPanel(condition = "input.items == 'about'",
                       uiOutput("point1Title"),
                       uiOutput("point1Text"),
                       br(),
                       uiOutput("point2Title"),
                       uiOutput("point2Text"),
                       br(),
                       uiOutput("point3Title"),
                       uiOutput("point3Text"),
                       uiOutput("rateFormula"),
                       uiOutput("whereText2"),
                       uiOutput("rateFormula2"),
                       br(),
                       uiOutput("point4Title"),
                       uiOutput("point4Text"),
                       uiOutput("formula"),
                       uiOutput("andText"),
                       uiOutput("formula2"),
                       uiOutput("whereText"),
                       uiOutput("formula3"),
                       br(),
                       uiOutput("point5Title")
      )


    )
  )
