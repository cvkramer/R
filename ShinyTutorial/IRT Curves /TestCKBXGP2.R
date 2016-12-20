library(shiny)

icc = function(a,b,c,theta){
    c + (1 - c)/((1+exp(-1.7*a*(theta-b))))
}

iif = function(a,b,c,theta){
    P = icc(a,b,c,theta)
    Q = 1-P
    a^2 * Q/P * ((P-c)/(1-c))^2
}

iif(1.5864, 0.7885, 0.13342,0)


# data = data.frame(a = 1:5, b=5:1)
indata = readxl::read_excel("/home/cvkramer/Desktop/Catherine Class/Sample6257_Unit7_Weekly Assignment 10_Item Statistics-1.xlsx")
#data = readxl::read_excel(file.choose())

server <- function(input, output) {
    data.o <- reactive({
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)

        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF
        params = indata[, c("A","B","C")]
        ticcs = as.data.frame(t(apply(params, 1, function(x) icc(x[1], x[2], x[3], theta=xLim))))

        names(ticcs) = iCols
        iccs = cbind(indata[, c("Content Area", "A", "B", "C")], ticcs)
        iccs
    })

    genICC <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF

        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) iif(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols
        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

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
             xlim=c( input$sl1[1], input$sl1[2]), ylim=c(0,max),
             xlab="Ability", ylab="Information",
             main = "Item Characteristic Curves")

        fcol = list()
            fcol[["Algebraic Concepts"]] = "Red"
            fcol[["Data Analysis"]] = "Green"
            fcol[["Geometry"]] = "Blue"
            fcol[["Numbers and Operations"]] = "Orange"
        if(!is.null(iRows)){
            for(itm in iRows){
                points(xLim, data[itm, iCols], type="l", col = fcol[[data[itm, "Content Area"]]])
            } # End row loop
        }
    }

    genTCC <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF

        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) iif(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols
        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

        tmp = list()


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
             xlim=c( input$sl1[1], input$sl1[2]), ylim=c(0,max),
             xlab="Ability", ylab="Information",
             main = "Test Characteristic Curve")

        if(!is.null(input$AC )){
            points(xLim, apply(data[input$AC , iCols], 2, sum), type="l", col = "Red")
        }
        if(!is.null(input$DA )){
            points(xLim, apply(data[input$DA , iCols], 2, sum), type="l", col = "Green")
        }
        if(!is.null(input$G )){
            points(xLim, apply(data[input$G , iCols], 2, sum), type="l", col = "Blue")
        }
        if(!is.null(input$NaO )){
            points(xLim, apply(data[input$NaO , iCols], 2, sum), type="l", col = "Orange")
        }
    }
    genOTCC <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF

        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) iif(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols
        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

        yax = apply(data[c(input$AC, input$DA, input$G, input$NaO), iCols], 2, sum)

        plot(x = xLim,
             y = yax,
             xlab="Ability", ylab="Information",
             main = "Test Characteristic Curve",
             type = "l")
    }

    genIIF <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF
        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) icc(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols
        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

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
             xlim=c( input$sl1[1], input$sl1[2]), ylim=c(0,max),
             xlab="Ability", ylab="Probability Correctly Answered",
             main = "Item Information Functions")

        fcol = list()
        fcol[["Algebraic Concepts"]] = "Red"
        fcol[["Data Analysis"]] = "Green"
        fcol[["Geometry"]] = "Blue"
        fcol[["Numbers and Operations"]] = "Orange"
        if(!is.null(iRows)){
            for(itm in iRows){
                points(xLim, data[itm, iCols], type="l", col = fcol[[data[itm, "Content Area"]]])
            } # End row loop
        }
    }

    genTIF <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF

        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) icc(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols
        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

        tmp = list()


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
             xlim=c( input$sl1[1], input$sl1[2]), ylim=c(0,max),
             xlab="Ability", ylab="Probable Test Score",
             main = "Test Information Function")

        if(!is.null(input$AC )){
            points(xLim, apply(data[input$AC , iCols], 2, sum), type="l", col = "Red")
        }
        if(!is.null(input$DA )){
            points(xLim, apply(data[input$DA , iCols], 2, sum), type="l", col = "Green")
        }
        if(!is.null(input$G )){
            points(xLim, apply(data[input$G , iCols], 2, sum), type="l", col = "Blue")
        }
        if(!is.null(input$NaO )){
            points(xLim, apply(data[input$NaO , iCols], 2, sum), type="l", col = "Orange")
        }
    }
    genOTIF <- function(){
        data = data.o()
        #iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        iRows = as.numeric(c(input$AC, input$DA, input$G, input$NaO))
        iCols = sprintf("%s%i",rep("IIF", input$QPts), 1:input$QPts)
        # input = data.frame(AbMax = 3, AbMin = -3, QPts = 40)
        qChg = ( input$sl1[2]- input$sl1[1])/input$QPts
        xLim = seq( input$sl1[1],  input$sl1[2], length.out=input$QPts)

        # Generate Item IIF
        params = data[, c("A","B","C")]
        tiif = as.data.frame(t(apply(params, 1, function(x) icc(x[1], x[2], x[3], theta=xLim))))

        names(tiif) = iCols

        data = cbind(data[, c("Content Area", "A", "B", "C")], tiif)

        yax = apply(data[c(input$AC, input$DA, input$G, input$NaO), iCols], 2, sum)

        plot(x = xLim,
             y = yax,
             xlab="Ability", ylab="Probable Test Score",
             main = "Test Information Function",
             type = "l")

    }
    output$ICC <- renderPlot({
        genICC()
    })
    output$TCC <- renderPlot({
        genTCC()
    })
    output$OTCC <- renderPlot({
        genOTCC()
    })
    output$IIF <- renderPlot({
        genIIF()
    })
    output$TIF <- renderPlot({
        genTIF()
    })
    output$OTIF <- renderPlot({
        genOTIF()
    })
    output$paramT <- renderTable({
        indata
    })
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
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
                                       ),
                                       selected = 1
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
                                           "Item 39" = 39,
                                           "Item 40" = 40,
                                           "Item 41" = 41,
                                           "Item 42" = 42,
                                           "Item 43" = 43,
                                           "Item 44" = 44,
                                           "Item 45" = 45,
                                           "Item 46" = 46,
                                           "Item 47" = 47,
                                           "Item 48" = 48,
                                           "Item 49" = 49,
                                           "Item 50" = 50
                                       )
                    )
                ) # End Columns
            ), # End Fluidrow (6)
        width = 3
        ), # End sidebarPanel
        mainPanel(
            tabsetPanel(
                tabPanel("Plots",
                    column(6,
                        plotOutput("ICC"),
                        plotOutput("TCC"),
                        plotOutput("OTCC")
                    ),
                    column(6,
                        plotOutput("IIF"),
                        plotOutput("TIF"),
                        plotOutput("OTIF")
                    )
                ),
                tabPanel("Item Parameters",
                         renderTable("paramT")
                ),
                tabPanel("Settings",
                         fluidRow(
                                    sliderInput("sl1",
                                                label = "Ability Range",
                                                min = -9,
                                                max = 9,
                                                step = .5,
                                                value = c(-3,3)
                                    ),
                                    numericInput(inputId = "QPts",
                                                 label = "Q Points",
                                                 value = 300,
                                                 step = 1
                                    )
                         )
                )
            )
        )
    )
)

shinyApp(ui = ui, server = server)




# mainPanel(
#     tabsetPanel(
#         tabPanel("One",
#                  plotOutput("ICC"),
#                  plotOutput("TIF")
#         ),
#         tabPanel("Two",
#                  verbatimTextOutput("summary")
#         )
#
#     )
# )
#                        numericInput(inputId = "AbMin",
# label = "T Min",
# value = -4,
# step = .5
# )
# ),
# column(3.5,
#        numericInput(inputId = "AbMax",
#                     label = "T Max",
#                     value = 4,
#                     step = .5
#        )
# ),
