server1 <- function(input, output) {
    output$distPlot <- renderPlot({
        hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    })
}

ui1 <- fluidPage(
    titlePanel("Hello World!"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("obs", "Number of observations:", min = 5, max = 500, value = 100)
        ),
        mainPanel(plotOutput("distPlot"))
    )
)

shinyApp(ui = ui1, server = server1)
