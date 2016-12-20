server <- function(input, output) {

    # You can access the values of the widget (as a vector)
    # with input$checkGroup, e.g.
    output$value <- renderPrint({ input$checkGroup })

}

ui <- fluidPage(

    # Copy the chunk below to make a group of checkboxes
    checkboxGroupInput("checkGroup", label = h3("Checkbox group"),
                       choices = list("Choice 1" = TRUE, "Choice 2" = TRUE, "Choice 3" = TRUE)),


    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))

)

shinyApp(ui = ui, server = server)
