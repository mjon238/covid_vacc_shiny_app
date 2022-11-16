source(global.R)

ui <- source("ui.R")
server <- source("server.R")

shinyApp(ui = ui, server = server)

runApp()
