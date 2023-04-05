library(shiny)
ui <- fluidPage("Hello World!")
server <- function(input,output,session){}
shinyApp(ui,server)

ui <- fluidPage(selectInput("input",
label = "Dataset", 
choices = ls("package:datasets")),
verbatimTextOutput("summary"),
tableOutput("table"))

shinyApp(ui,server)

server <- function(input,output,session){
    output$summary <- renderPrint({dataset <- get(input$dataset, "package:datasets")})
}