server <- function(input, output) {

}

ui <- fluidpage(
    sidebarLayout(
        sidebarPanel(
            checkboxInput("I1", "Item 1", value = FALSE, width = NULL)
            checkboxInput("I2", "Item 2", value = FALSE, width = NULL)
            checkboxInput("I3", "Item 3", value = FALSE, width = NULL)
            checkboxInput("I4", "Item 4", value = FALSE, width = NULL)
            checkboxInput("I5", "Item 5", value = FALSE, width = NULL)
            checkboxInput("I6", "Item 6", value = FALSE, width = NULL)
            checkboxInput("I7", "Item 7", value = FALSE, width = NULL)
            checkboxInput("I8", "Item 8", value = FALSE, width = NULL)
            checkboxInput("I9", "Item 9", value = FALSE, width = NULL)
            checkboxInput("I10", "Item 10", value = FALSE, width = NULL)
            checkboxInput("I11", "Item 11", value = FALSE, width = NULL)
            checkboxInput("I12", "Item 12", value = FALSE, width = NULL)
            checkboxInput("I13", "Item 13", value = FALSE, width = NULL)
            checkboxInput("I14", "Item 14", value = FALSE, width = NULL)
            checkboxInput("I15", "Item 15", value = FALSE, width = NULL)
            checkboxInput("I16", "Item 16", value = FALSE, width = NULL)
            checkboxInput("I17", "Item 17", value = FALSE, width = NULL)
            checkboxInput("I18", "Item 18", value = FALSE, width = NULL)
            checkboxInput("I19", "Item 19", value = FALSE, width = NULL)
            checkboxInput("I20", "Item 20", value = FALSE, width = NULL)
            )
    )
)

shinyApp(ui = ui, server = server)
