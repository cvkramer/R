library(shiny)

iif = function(a,b,c,theta){
    c + (1 - c)/((1+exp(-1.7*a*(theta-b))))
}
iif(1.5864, 0.7885, 0.13342,0)


# data = data.frame(a = 1:5, b=5:1)
data = readxl::read_excel("/home/cvkramer/Desktop/Catherine Class/Sample6257_Unit7_Weekly Assignment 10_Item Statistics-1.xlsx")
#data = readxl::read_excel(file.choose())

server <- function(input, output) {
    iCols = sprintf("%s%i",rep("IIF",40),1:40)

    genICC <- function(){
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        tmp.dat = data[iRows, ]
        # Find Maximum y-value
        max = .001
        if(!is.null(input$AC)){
            if(max(data[input$AC, iCols]>max)){
                max=max(data[input$AC, iCols])
            }
        }
        if(!is.null(input$DA)){if(max(data[input$DA, iCols]>max)){max=max(data[input$DA, iCols])}}
        if(!is.null(input$G)){if(max(data[input$G, iCols]>max)){max=max(data[input$G, iCols])}}
        if(!is.null(input$NaO)){if(max(data[input$NaO, iCols]>max)){max=max(data[input$NaO, iCols])}}

        plot(NA,
             xlim=c(-4,4), ylim=c(0,max),
             xlab="Ability", ylab="Information")

        fcol = list()
        fcol[["Algebraic Concepts"]] = "Red"
        fcol[["Data Analysis"]] = "Green"
        fcol[["Geometry"]] = "Blue"
        fcol[["Numbers and Operations"]] = "Orange"
        if(!is.null(iRows)){
            for(itm in 1:length(iRows)){
                tmp.dat[itm, "Content Area"]
                points(1:40, tmp.dat[itm, iCols], type="l", col = fcol[[tmp.dat[itm, "Content Area"]]])
            } # End row loop
        }
    }

    genTIF <- function(){
        tmp = list()

        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))

        # Find Maximum y-value
        max = .001
        if(!is.null(input$AC )){
            tmp.max = max(apply(data[input$AC , iCols], 2, sum))
            if(tmp.max>max){max=tmp.max
        }}
        if(!is.null(input$DA )){
            tmp.max = max(apply(data[input$DA , iCols], 2, sum))
            if(tmp.max>max){max=tmp.max
            }}
        if(!is.null(input$G )){
            tmp.max = max(apply(data[input$G , iCols], 2, sum))
            if(tmp.max>max){max=tmp.max
            }}
        if(!is.null(input$NaO )){
            tmp.max = max(apply(data[input$NaO , iCols], 2, sum))
            if(tmp.max>max){max=tmp.max
            }}

        plot(NA,
             xlim=c(0,40), ylim=c(0,max),
             xlab="Ability", ylab="Information")

        if(!is.null(input$AC )){
            points(1:40, apply(data[input$AC , iCols], 2, sum), type="l", col = "Red")
        }
        if(!is.null(input$DA )){
            points(1:40, apply(data[input$DA , iCols], 2, sum), type="l", col = "Green")
        }
        if(!is.null(input$G )){
            points(1:40, apply(data[input$G , iCols], 2, sum), type="l", col = "Blue")
        }
        if(!is.null(input$NaO )){
            points(1:40, apply(data[input$NaO , iCols], 2, sum), type="l", col = "Orange")
        }
    }

    output$ICC <- renderPlot({
        genICC()
    })
    output$TIF <- renderPlot({
        genTIF()
    })

}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(4,
                       numericInput(inputId = "AbMin",
                                    label = "T Min",
                                    value = -4,
                                    step = .5
                       )
                ),
                column(4,
                       numericInput(inputId = "AbMax",
                                    label = "T Max",
                                    value = -4,
                                    step = .5
                       )
                ),
                column(4,
                       numericInput(inputId = "QPts",
                                    label = "Q Points",
                                    value = 30,
                                    step = 1
                       )
                )
            ),
            fluidRow(
                column(6,
                    # Alg Concepts
                    checkboxGroupInput("AC",
                                       label = "Algebraic Concepts",
                                       choices = list(
                                           "Item 1" = 1,
                                           "Item 2" = 2,
                                           "Item 3" = 3,
                                           "Item 4" = 4,
                                           "Item 5" = 5,
                                           "Item 6" = 6
                                       )
                    )
                ),
                column(6,
                    # Data Analysis
                    checkboxGroupInput("DA",
                                       label = "Data Analysis\n",
                                       choices = list(
                                           "Item 7" = 7,
                                           "Item 8" = 8,
                                           "Item 9" = 9,
                                           "Item 10" = 10,
                                           "Item 11" = 11,
                                           "Item 12" = 12,
                                           "Item 13" = 13,
                                           "Item 14" = 14,
                                           "Item 15" = 15,
                                           "Item 16" = 16,
                                           "Item 17" = 17
                                       )
                    )
                )
            ), # End Fluid Row (6)
            fluidRow(
                column(6,
                    # Geometry
                    checkboxGroupInput("G",
                                       label = "Geometry\n",
                                       choices = list(
                                           "Item 18" = 18,
                                           "Item 19" = 19,
                                           "Item 20" = 20,
                                           "Item 21" = 21,
                                           "Item 22" = 22,
                                           "Item 23" = 23,
                                           "Item 24" = 24,
                                           "Item 25" = 25,
                                           "Item 26" = 26,
                                           "Item 27" = 27,
                                           "Item 28" = 28,
                                           "Item 29" = 29
                                       )
                    )
                ),
                column(6,
                    # Numbers and Ops
                    checkboxGroupInput("NaO",
                                       label = "Numbers & Operation",
                                       choices = list(
                                           "Item 30" = 30,
                                           "Item 31" = 31,
                                           "Item 32" = 32,
                                           "Item 33" = 33,
                                           "Item 34" = 34,
                                           "Item 35" = 35,
                                           "Item 36" = 36,
                                           "Item 37" = 37,
                                           "Item 38" = 38,
                                           "Item 39" = 39
                                       )
                    )
                ) # End Columns
            ) # End Fluidrow (6)
        ), # End sidebarPanel
        mainPanel(
            plotOutput("ICC"),
            plotOutput("TIF")
        )
    )
)

shinyApp(ui = ui, server = server)
