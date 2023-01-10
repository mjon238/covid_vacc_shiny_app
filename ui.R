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
        menuItem(tabName = "vacc", "HSU vs. ERP Data"),
        menuItem(tabName = "ratio", "Compare")
      ),
      div(style = "margin-top: -10px;"),
      hr(),
      div(style = "margin-bottom: -20px;"),
      
      #HSU vs. ERP Inputs ------
      #Need this to activate hide/show input functions
      useShinyjs(),
      
        #Nationwide Tab ----
      conditionalPanel(
        condition = "input.items == 'vacc' 
        && input.vaccType == 'Nationwide'",
        
        #Date
        selectInput(
          inputId = "dateFull",
          label = "Select Date",
          choices = c("2021-11-01"),
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
        )
        ),

        #DHB Only Tab ----
      conditionalPanel(
        condition = "input.items == 'vacc' && input.vaccType == 'By DHB'",
        
        #Date
        selectInput(
          inputId = "dateDHB",
          label = "Select Date",
          choices = c("2021-11-01"),
          selected = "2021-11-01",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        #Gender
        selectInput(
          inputId = "genderVaccDHB",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        #Ethnicity
        selectInput(
          inputId = "ethDHB",
          label = "Select Ethnicity",
          choices = c("Total", "Maori"
                      ,"Non-Maori"
                      ),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
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
                                   choices = c("Rate Ratio",
                                               "Rate Difference",
                                               "Count"),
                                   selected = "Rate Ratio",
                                   selectize = F),
                       hr(),
                       div(style = "margin-bottom: -20px;")
                       
                       
      ),
      
      conditionalPanel(
        condition = "input.items == 'ratio'",
        
        #Date
        selectInput(
          inputId = "dateRatio",
          label = "Select Date",
          choices = c("2021-11-01"),
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
        div(style = "margin-bottom: -20px;")),
      
      
      
        #Ratio Rates Single Inputs -----
      # conditionalPanel(
      #   condition = "input.items == 'ratio'
      #   && input.ratioCompare1 == 'Single'",
      #   
      #   #Ethinicty
      #   selectInput(
      #     inputId = "ethRatio",
      #     label = "Select Ethnicity",
      #     choices = c("Total", "Maori"
      #                 , "Non-Maori"
      #                 ),
      #     selected = "Total",
      #     selectize = F
      #   )),
      
      
        #Ratio Rates Compare Inputs -----
      conditionalPanel(
        condition = "input.items == 'ratio'",
        # && input.ratioCompare1 == 'Compare'",
        
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
      tags$head(tags$style( type = 'text/css',  'plotDHB { overflow-x: scroll; }')),
      
      # Add and edit text output
      tags$head(tags$style("#graphTitle{color: black;
                                 font-size: 20px;
                                 }")),
      
      
      #Title ----
      uiOutput(outputId = "graphTitle"),
      
      #HSU vs. ERP Output -----
      conditionalPanel(
        condition = "input.items == 'vacc'",
        tabsetPanel(
          id = "vaccType",
          
          #Nationwide Tab ----
          tabPanel(
            "Nationwide",
            uiOutput("chartTitle1"),
            br(),
            plotlyOutput("fullyVaccPlot", width = "1200px", height = "600px")
            
          ),
          
          #DHB Tab ----
          tabPanel(
            "By DHB",
            uiOutput("chartTitle2"),
            br(),
            plotlyOutput("plotDHB", width = "1200px", height = "600px")
            
          )
        )
      ),
      
      
      #Ratio Item Outputs ----

          #Ratio Rates Tab ----
        conditionalPanel(condition = "input.items == 'ratio'",
            uiOutput("chartTitle3"),
            
            br(),
            
          #   #Single ----
          # conditionalPanel(condition = "input.ratioCompare1 == 'Single'",
          #   plotlyOutput("rateRatioPlot", width = "800px"),
          #   ),
          
            #Compare ----
          # conditionalPanel(condition = "input.ratioCompare1 == 'Compare'",
                           plotlyOutput("ratioComparsion", width = "800px")
          
          
          ),
      
    
      # conditionalPanel(condition = "input.items == 'ratio'",
      #                  radioGroupButtons(inputId = "ratioCompare1",
      #                                    label = "Plot View",
      #                                    choices = c("Single", "Compare"),
      #                                    selected = "Compare")),
      hr(),
      
      #Text Information ----
      fluidRow(column(12, 
      uiOutput("textTitle"),
       uiOutput("textMain"),
      conditionalPanel(condition = "input.items == 'vacc'",
       uiOutput("textERP"),
       br(),
        uiOutput("textHSU"),
       br(),
       
       
       
       #Raw Data Tables ----
       
       uiOutput("rawDataTitle1"),
       DTOutput("tableFull")
      ),
      conditionalPanel(condition = "input.items == 'ratio'",
                       uiOutput("textRateRatios"),
                       br(),
                       uiOutput("rawDataTitle2"),
                       DTOutput("tableRate")
                       
      )
      
      ))

      
    )
  )
