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
        menuItem(tabName = "ratio", "Rate Ratios")
      ),
      div(style = "margin-top: -10px;"),
      hr(),
      div(style = "margin-bottom: -20px;"),
      
      #HSU vs. ERP Inputs ------
      #Need this to activate hide/show input functions
      useShinyjs(),
      
      # conditionalPanel(
      #   condition = "input.items == 'vacc'",
      #   
      # ),
      # 
        #Nationwide Tab ----
      conditionalPanel(
        condition = "input.items == 'vacc' && input.vaccType == 'Nationwide'",
        selectInput(
          inputId = "genderVacc",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;"),
        
        selectInput(
          inputId = "ethFull",
          label = "Select Ethnicity",
          choices = c("Total", "Maori"),
          selected = "Total",
          selectize = F
        )),

        #DHB Only Tab ----
      conditionalPanel(
        condition = "input.items == 'vacc' && input.vaccType == 'By DHB'",
        
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
          choices = c("Total", "Maori", "Non-Maori"),
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
      
      #Gender Inputs
      conditionalPanel(
        condition = "input.items == 'ratio' 
        && input.ratioTabs == 'Ratio Rates'",
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
      conditionalPanel(
        condition = "input.items == 'ratio'
        && input.ratioTabs == 'Ratio Rates'
        && input.ratioCompare1 == 'Single'",
        
        #Ethinicty
        selectInput(
          inputId = "ethRatio",
          label = "Select Ethnicity",
          choices = c("Total", "Maori", "Non-Maori"),
          selected = "Total",
          selectize = F
        )),
      
      
        #Ratio Rates Compare Inputs -----
      conditionalPanel(
        condition = "input.items == 'ratio'
        && input.ratioTabs == 'Ratio Rates'
        && input.ratioCompare1 == 'Compare'",
        
        #Ethnicity
      pickerInput(
        inputId = "ethRatio2",
        label = "Select Ethnicity's To Compare",
        choices = c("Total", "Maori", "Non-Maori"),
        selected = c("Total", "Maori", "Non-Maori"),
        multiple = T
      )
      ),
      
      
      #Count Inputs ----
      
      #Gender
      conditionalPanel(
        condition = "input.items == 'ratio' 
        && input.ratioTabs == 'Count'",
        selectInput(
          inputId = "genderCount",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        ),
        hr(),
        div(style = "margin-bottom: -20px;")),
      
      
        #Count Single Inputs ----
      conditionalPanel(
        condition = "input.items == 'ratio'
        && input.ratioTabs == 'Count'
        && input.ratioCompare2 == 'Single'",
        
        #Ethnicity
        selectInput(
          inputId = "ethCount",
          label = "Select Ethnicity",
          choices = c("Total", "Maori", "Non-Maori"),
          selected = "Total",
          selectize = F
        )),
      
        #Count Compare Inputs ----
      conditionalPanel(
        condition = "input.items == 'ratio'
        && input.ratioTabs == 'Count'
        && input.ratioCompare2 == 'Compare'",
        
        #Ethnicity
        pickerInput(
          inputId = "ethCount2",
          label = "Select Ethnicity's To Compare",
          choices = c("Total", "Maori", "Non-Maori"),
          selected = c("Total", "Maori", "Non-Maori"),
          multiple = T
        )),

    
    #Difference Inputs ----
    
    #Gender
    conditionalPanel(
      condition = "input.items == 'ratio' 
        && input.ratioTabs == 'Ratio Difference'",
      selectInput(
        inputId = "genderDifference",
        label = "Select Sex",
        choices = c("Total", "Male", "Female"),
        selected = "Total",
        selectize = F
      ),
      hr(),
      div(style = "margin-bottom: -20px;")),
    
    
       #Difference Single Inputs ----
    conditionalPanel(
      condition = "input.items == 'ratio'
        && input.ratioTabs == 'Ratio Difference'
        && input.ratioCompare3 == 'Single'",
      
      #Ethnicity
      selectInput(
        inputId = "ethDifference",
        label = "Select Ethnicity",
        choices = c("Total", "Maori", "Non-Maori"),
        selected = "Total",
        selectize = F
      )),
    
       #Difference Compare Inputs ----
    conditionalPanel(
      condition = "input.items == 'ratio'
        && input.ratioTabs == 'Ratio Difference'
        && input.ratioCompare3 == 'Compare'",
      
      #Ethnicity
      pickerInput(
        inputId = "ethDifference2",
        label = "Select Ethnicity's To Compare",
        choices = c("Total", "Maori", "Non-Maori"),
        selected = c("Total", "Maori", "Non-Maori"),
        multiple = T
      ))
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
      textOutput(outputId = "graphTitle"),
      
      #HSU vs. ERP Output -----
      conditionalPanel(
        condition = "input.items == 'vacc'",
        tabsetPanel(
          id = "vaccType",
          
          #Nationwide Tab ----
          tabPanel(
            "Nationwide",
            br(),
            plotlyOutput("fullyVaccPlot", width = "800px", height = "400px")
            
          ),
          
          #DHB Tab ----
          tabPanel(
            "By DHB",
            br(),
            # box(width = 12, height = 12, style = "overflow-y: auto; overflow-x: auto;",
            plotlyOutput("plotDHB", width = "1200px", height = "600px")
            
          )
        )
      ),
      
      
      #Ratio Item Outputs ----
      conditionalPanel(
        condition = "input.items == 'ratio'",
        tabsetPanel(
          id = "ratioTabs",
          
          
          #Ratio Rates Tab ----
          tabPanel(
            "Ratio Rates",
            br(),
            
            #Single ----
          conditionalPanel(condition = "input.ratioCompare1 == 'Single'",
            plotlyOutput("rateRatioPlot", width = "800px")),
          
            #Compare ----
          conditionalPanel(condition = "input.ratioCompare1 == 'Compare'",
                           plotlyOutput("ratioComparsion", width = "800px")),
          
          radioGroupButtons(inputId = "ratioCompare1",
                            label = "Plot View",
                            choices = c("Single", "Compare"),
                            selected = "Single")
          ),
          
          #Count Outputs ----
          tabPanel(
            "Count",
            br(),
            
            #Single ----
            conditionalPanel(condition = "input.ratioCompare2 == 'Single'",
                             plotlyOutput("ratioCountPlot", width = "800px")),
            
            #Compare ----
            conditionalPanel(condition = "input.ratioCompare2 == 'Compare'",
                             plotlyOutput("countCompare", width = "800px")),
            radioGroupButtons(inputId = "ratioCompare2",
                              label = "Plot View",
                              choices = c("Single", "Compare"),
                              selected = "Single")
          ),
          
          #Ratio Difference Tab ----
          tabPanel("Ratio Difference",
          radioGroupButtons(inputId = "ratioCompare3",
                            label = "Plot View",
                            choices = c("Single", "Compare"),
                            selected = "Single"))
        )
      )
    )
  )
# )
# )
