# UI
shinyUI(
  dashboardPage(
    title = "",

    ## Sidebar Section
    dashboardHeader(
      title = "COVID-19 Vaccination"
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "items",
        menuItem(tabName = "vacc", "Fully Vaccinated NZ"),
        menuItem(tabName = "vaccDHB", "Fully Vaccinated by DHB"),
        menuItem(tabName = "ratio", "Rate Ratios"),
        menuItem(tabName = "ratio2", "Rate Ratios Comparison")
      ),
      useShinyjs(), # Set up shinyjs
      hidden(
        # conditionalPanel(condition = "input.items == 'vaccDHB'",
        pickerInput(
          inputId = "DHB",
          label = "Select DHB",
          choices = listOfDHB,
          selected = listOfDHB,
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
      tabsetPanel(
        type = "tabs",

        ## Tab Panel 1
        tabPanel(
          "Total",
          # Add and edit text output
          textOutput(outputId = "graphTitle"),
          tags$head(tags$style("#graphTitle{color: black;
                                 font-size: 20px;
                                 }")),

          # Add Mainpanel body text
          mainPanel(conditionalPanel(condition = "input.items != 'ratio2'",
              plotOutput("plots1", width = "800px"),
            radioGroupButtons(
              inputId = "ethnicity",
              label = "Select Ethnicity",
              choiceNames = c("Total", "Maori", "Non-Maori"),
              choiceValues = c("total", "maori", "nmaori"),
              selected = "total"
            )),
            conditionalPanel(condition = "input.items == 'ratio2'",
                             plotOutput("ratioComparsion", width = "800px"),
                             checkboxGroupButtons(
                               inputId = "eth2",
                               label = "Select Ethnicity's To Comparse",
                               choices = c("Total", "Maori", "Non-Maori"),
                               selected = c("Total", "Maori", "Non-Maori"),
                             )
                             ))
          
        ),

        ## Tab Panel 2
        tabPanel("Male"),
        tabPanel("Female")
      )
    )
  )
)
