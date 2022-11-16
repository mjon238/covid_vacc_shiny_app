# UI
shinyUI(
  dashboardPage(
    title = "Test Shiny",

    ## Sidebar Section
    dashboardHeader(
      title = "Test App"
    ),
    dashboardSidebar(
      selectInput(
        inputId = "state",
        label = "Select State",
        choices = c(unique(aus_accommodation$State)),
        selected = "New South Wales",
        selectize = F,
        width = "300px"
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
        $("header").find("nav").append(\'<span class="myClass"> Testing a Shiny App </span>\');
      })
     ')),

      ## End create title text
      tabsetPanel(
        type = "tabs",
        
        ##Tab Panel 1
        tabPanel("Takings",
          #Add and edit text output
          textOutput(outputId = "graphTitle"),
          tags$head(tags$style("#graphTitle{color: black;
                                 font-size: 20px;
                                 }"
          )
        ),
        
        #Add Mainpanel body text
          mainPanel(
            plotOutput(outputId = "distPlot")
          )
        ),
        
        ## Tab Panel 2
        tabPanel("CPI", )
      )
    ),
    # tabPanel("CPI")
  )
)
