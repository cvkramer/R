# data = data.frame(a = 1:5, b=5:1)
#data = readxl::read_excel("/home/cvkramer/Desktop/Catherine Class/Sample6257_Unit7_Weekly Assignment 10_Item Statistics-1.xlsx")
data = readxl::read_excel(file.choose())

server <- function(input, output) {
    iRows = rep(FALSE, 39)
    iCols = sprintf("%s%i",rep("IIF",40),1:40)
    clr = c("red","blue","green","orange")

    genPlot <- function(){
        iRows = c(input$Itm1,input$Itm2,input$Itm3,input$Itm4,input$Itm5,
                  input$Itm6,input$Itm7,input$Itm8,input$Itm9,input$Itm10,
                  input$Itm11,input$Itm12,input$Itm13,input$Itm14,input$Itm15,
                  input$Itm16,input$Itm17,input$Itm18,input$Itm19,input$Itm20,
                  input$Itm21,input$Itm22,input$Itm23,input$Itm24,input$Itm25,
                  input$Itm26,input$Itm27,input$Itm28,input$Itm29,input$Itm30,
                  input$Itm31,input$Itm32,input$Itm33,input$Itm34,input$Itm35,
                  input$Itm36,input$Itm37,input$Itm38,input$Itm39)
        tmp.dat = data[iRows, ]

        plot(NA, xlim=c(0,40), ylim=c(0,1))
        #for(sbj in 1:unique(tmp.dat[["Content Area"]])){
            for(itm in 1:sum(iRows)){
                ncolor = grep(tmp.dat[["Content Area"]][itm], unique(data[["Content Area"]]))
                points(1:40, tmp.dat[itm, iCols], type="l", col = clr[ncolor])
            } # End row loop
        #} # End sbj loop
    }
    genPlot2 <- function(){
        tmp = list()

        iRows = c(input$Itm1,input$Itm2,input$Itm3,input$Itm4,input$Itm5,
                  input$Itm6,input$Itm7,input$Itm8,input$Itm9,input$Itm10,
                  input$Itm11,input$Itm12,input$Itm13,input$Itm14,input$Itm15,
                  input$Itm16,input$Itm17,input$Itm18,input$Itm19,input$Itm20,
                  input$Itm21,input$Itm22,input$Itm23,input$Itm24,input$Itm25,
                  input$Itm26,input$Itm27,input$Itm28,input$Itm29,input$Itm30,
                  input$Itm31,input$Itm32,input$Itm33,input$Itm34,input$Itm35,
                  input$Itm36,input$Itm37,input$Itm38,input$Itm39)
        max = 0
        plot(1:40, apply(data[iRows, iCols], 2, sum))
        #for(sbj in 1:unique(data[iRows, "Content Area"])){
            # Save the Content area
        #    sbj.chr = tmp.dat[itm, "Content Area"]

            # Get the sum of observations in a Content Area
        #    iSbj = data[iRows, "Content Area"] == sbj.chr

        #    tmp[sbj] = apply(data[iRows & iSbj, iCols], 2, sum)
         #   if(tmp[sbj] > max){max = tmp[sbj]}
        #}
        #plot(NA, xlim=c(0,40), ylim=c(0,max))
        #for(sbj in 1:unique(data[iRows, "Content Area"])){
        #    points(1:40, tmp[sbj], col = clr[sbj])
        #}
    }
    output$sPlot <- renderPlot({
        genPlot()
    })
    output$xPlot <- renderPlot({
        genPlot2()
    })
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                    "Algebraic Concepts",
                    # Alg Concepts
                    checkboxInput("Itm1", "Item 1", value = TRUE, width = NULL),
                    checkboxInput("Itm2", "Item 2", value = FALSE, width = NULL),
                    checkboxInput("Itm3", "Item 3", value = FALSE, width = NULL),
                    checkboxInput("Itm4", "Item 4", value = FALSE, width = NULL),
                    checkboxInput("Itm5", "Item 5", value = FALSE, width = NULL),
                    checkboxInput("Itm6", "Item 6", value = FALSE, width = NULL)
                ),
                column(6,
                    "Data Analysis",
                    # Data Analysis
                    checkboxInput("Itm7", "Item 7", value = FALSE, width = NULL),
                    checkboxInput("Itm8", "Item 8", value = FALSE, width = NULL),
                    checkboxInput("Itm9", "Item 9", value = FALSE, width = NULL),
                    checkboxInput("Itm10", "Item 10", value = FALSE, width = NULL),
                    checkboxInput("Itm11", "Item 11", value = FALSE, width = NULL),
                    checkboxInput("Itm12", "Item 12", value = FALSE, width = NULL),
                    checkboxInput("Itm13", "Item 13", value = FALSE, width = NULL),
                    checkboxInput("Itm14", "Item 14", value = FALSE, width = NULL),
                    checkboxInput("Itm15", "Item 15", value = FALSE, width = NULL),
                    checkboxInput("Itm16", "Item 16", value = FALSE, width = NULL),
                    checkboxInput("Itm17", "Item 17", value = FALSE, width = NULL)
                )
            ), # End Fluid Row (6)
            fluidRow(
                column(6,
                    "Geometry",
                    # Geometry
                    checkboxInput("Itm18", "Item 18", value = FALSE, width = NULL),
                    checkboxInput("Itm19", "Item 19", value = FALSE, width = NULL),
                    checkboxInput("Itm20", "Item 20", value = FALSE, width = NULL),
                    checkboxInput("Itm21", "Item 21", value = FALSE, width = NULL),
                    checkboxInput("Itm22", "Item 22", value = FALSE, width = NULL),
                    checkboxInput("Itm23", "Item 23", value = FALSE, width = NULL),
                    checkboxInput("Itm24", "Item 24", value = FALSE, width = NULL),
                    checkboxInput("Itm25", "Item 25", value = FALSE, width = NULL),
                    checkboxInput("Itm26", "Item 26", value = FALSE, width = NULL),
                    checkboxInput("Itm27", "Item 27", value = FALSE, width = NULL),
                    checkboxInput("Itm28", "Item 28", value = FALSE, width = NULL)
                ),
                column(6,
                    "Numbers and Operation",
                    # Numbers and Ops
                    checkboxInput("Itm29", "Item 29", value = FALSE, width = NULL),
                    checkboxInput("Itm30", "Item 30", value = FALSE, width = NULL),
                    checkboxInput("Itm31", "Item 31", value = FALSE, width = NULL),
                    checkboxInput("Itm32", "Item 32", value = FALSE, width = NULL),
                    checkboxInput("Itm33", "Item 33", value = FALSE, width = NULL),
                    checkboxInput("Itm34", "Item 34", value = FALSE, width = NULL),
                    checkboxInput("Itm35", "Item 35", value = FALSE, width = NULL),
                    checkboxInput("Itm36", "Item 36", value = FALSE, width = NULL),
                    checkboxInput("Itm37", "Item 37", value = FALSE, width = NULL),
                    checkboxInput("Itm38", "Item 38", value = FALSE, width = NULL),
                    checkboxInput("Itm39", "Item 39", value = FALSE, width = NULL)
                ) # End Columns
            ) # End Fluidrow (6)
        ), # End sidebarPanel
        mainPanel(
            plotOutput("sPlot"),
            plotOutput("xPlot")
        )
    )
)

shinyApp(ui = ui, server = server)
