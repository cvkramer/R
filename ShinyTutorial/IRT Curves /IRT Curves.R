data = data.frame(a = 1:22, b=22:1)
rows = rep(TRUE, 22)
server <- function(input, output) {
    getData <- reactive({
        rows = c(input$I1, input$I2, input$I3, input$I4, input$I5,
                 input$I6, input$I7, input$I8, input$I9, input$I10,
                 input$I11, input$I12, input$I13, input$I14, input$I15,
                 input$I16, input$I17, input$I18, input$I19, input$I20,
                 TRUE, TRUE)
        useData = data[rows, ]
        useData
    })

    output$sPlot <- renderPlot({
        plot(1:10, 10:1)
    })
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                checkboxInput("I1", "Item 1", value = TRUE, width = NULL),
                checkboxInput("I2", "Item 2", value = FALSE, width = NULL),
                checkboxInput("I3", "Item 3", value = FALSE, width = NULL),
                checkboxInput("I4", "Item 4", value = FALSE, width = NULL),
                checkboxInput("I5", "Item 5", value = FALSE, width = NULL),
                checkboxInput("I6", "Item 6", value = FALSE, width = NULL),
                checkboxInput("I7", "Item 7", value = FALSE, width = NULL),
                checkboxInput("I8", "Item 8", value = FALSE, width = NULL),
                checkboxInput("I9", "Item 9", value = FALSE, width = NULL),
                checkboxInput("I10", "Item 10", value = FALSE, width = NULL),
                checkboxInput("I11", "Item 11", value = FALSE, width = NULL),
                checkboxInput("I12", "Item 12", value = FALSE, width = NULL),
                checkboxInput("I13", "Item 13", value = FALSE, width = NULL),
                checkboxInput("I14", "Item 14", value = FALSE, width = NULL),
                checkboxInput("I15", "Item 15", value = FALSE, width = NULL),
                checkboxInput("I16", "Item 16", value = FALSE, width = NULL),
                checkboxInput("I17", "Item 17", value = FALSE, width = NULL),
                checkboxInput("I18", "Item 18", value = FALSE, width = NULL),
                checkboxInput("I19", "Item 19", value = FALSE, width = NULL),
                checkboxInput("I20", "Item 20", value = FALSE, width = NULL)
            )
        ), mainPanel()
    ),
    mainPanel(
        plot(1:10, 10:1),
        plotOutput("sPlot")
    )
)

shinyApp(ui = ui, server = server)
