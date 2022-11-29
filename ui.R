# UI
 fillPage(
shinyUI(
  dashboardPage(
    title = "",
    
    ## Sidebar Section
    dashboardHeader(
      title = "COVID-19 Vaccination"
    ),
    dashboardSidebar(
      tags$head(tags$style(".wrapper {overflow: visible !important;}")),
      sidebarMenu(
        id = "items",
        menuItem(tabName = "vacc", "Fully Vaccinated"),
        menuItem(tabName = "ratio", "Rate Ratios")
      ),
      div(style = "margin-top: -10px;"),
      hr(),
      div(style = "margin-bottom: -20px;"),
      conditionalPanel(
        condition = "input.items == 'vacc'",
      selectInput(
        inputId = "genderVacc",
        label = "Select Sex",
        choices = c("Total", "Male", "Female"),
        selected = "Total",
        selectize = F
      )),
      conditionalPanel(
        condition = "input.items == 'ratio'",
        selectInput(
          inputId = "genderRatio",
          label = "Select Sex",
          choices = c("Total", "Male", "Female"),
          selected = "Total",
          selectize = F
        )),
      # useShinyjs(), # Set up shinyjs
      # hidden(
      conditionalPanel(
        condition = "input.items == 'vacc' &&
                                      input.vaccType == 'By DHB'",
        hr(),
        div(style = "margin-bottom: -20px;"),
        useShinyjs(),
        # hidden(
        
        selectInput(inputId = "groupDHB",
                          label = "Select DHB Groups",
                          choices = c("Custom", "High vs. Low", 
                                      "Auckland Region", "North Island",
                                      "South Island"),
                          selected = "Custom",
                    selectize = F),
        pickerInput(
          inputId = "DHB",
          label = "Select DHB",
          choices = listOfDHB,
          selected = "Auckland",
          width = "200px",
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      )
    ),

    ## Body Section
    dashboardBody(
      ### Create the title text
      tags$head(tags$style(HTML(
        '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '
      ))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">  </span>\');
      })
     ')),

      ## End create title text
      textOutput(outputId = "graphTitle"),
      conditionalPanel(
        condition = "input.items == 'vacc'",
        tabsetPanel(
          id = "vaccType",
          tabPanel(
            "Nationwide",
            br(),
            plotlyOutput("fullyVaccPlot", width = "800px", height = "400px"),
            radioGroupButtons(
              inputId = "ethFull",
              label = "Select Ethnicity",
              choices = c("Total", "Maori"),
              selected = "Total"
            )
          ),
          tabPanel(
            "By DHB",
            br(),
            plotlyOutput("plotDHB", width = "800px", height = "400px"),
            radioGroupButtons(
              inputId = "ethDHB",
              label = "Select Ethnicity",
              choices = c("Total", "Maori", "Non-Maori"),
              selected = "Total"
            )
          )
        )
      ),
      # Add and edit text output
      tags$head(tags$style("#graphTitle{color: black;
                                 font-size: 20px;
                                 }")),

      # Add Mainpanel body text
      conditionalPanel(
        condition = "input.items == 'ratio'",
        tabsetPanel(
          id = "ratioTabs",
          tabPanel(
            "Single",
            br(),
            plotlyOutput("rateRatioPlot", width = "800px"),
            radioGroupButtons(
              inputId = "ethRatio",
              label = "Select Ethnicity",
              choices = c("Total", "Maori", "Non-Maori"),
              selected = "Total"
            )
          ),
          tabPanel(
            "Comparison",
            br(),
            plotlyOutput("ratioComparsion", width = "800px"),
            checkboxGroupButtons(
              inputId = "eth2",
              label = "Select Ethnicity's To Comparse",
              choices = c("Total", "Maori", "Non-Maori"),
              selected = c("Total", "Maori", "Non-Maori"),
            )
          )
        )
      )
    )
  )
)
)
