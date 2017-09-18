#
library(shiny)
options(rgl.useNULL=TRUE)
library(shinyRGL)
library(rgl)
library(devtools)

shinyUI(navbarPage("Cerberos",
                   tabPanel("Selection",
                            titlePanel("Selection"),
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("sampleFile",
                                             label = h3("Data:"),
                                             choices = list("Longley’s Economic Data" = 1,
                                                            "Red Wine Data" = 2,
                                                            "Exam Data" = 3,
                                                            "Air Quality Data" = 4,
                                                            "2010 Violent Crime Data" = 5,
                                                            "Other Dataset" = 6),
                                             selected = 6),
                                fileInput("datafile",
                                          'Choose data File',
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                uiOutput("y"),
                                uiOutput("x1"),
                                uiOutput("x2")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  # Will contain just a 3dplot or the dataframe or the corelation
                                  tabPanel("Plot",webGLOutput("selectionPlot")),
                                  tabPanel("File Opened",dataTableOutput("filetable")),
                                  tabPanel("Correlation",dataTableOutput("corMat"))
                                )
                              ))),
                   tabPanel("Regression",
                            titlePanel("Regression Models"),
                            sidebarLayout(
                              sidebarPanel(
                                #Buttons
                                sliderInput("alpha",
                                            "Learning Rate",
                                            min = 0,
                                            max = 1,
                                            value = 0),
                                sliderInput("num_iter",
                                            "Number of Iterations",
                                            min = 100,
                                            max = 2000,
                                            value = 100),
                                radioButtons("regressionType",
                                             label = h3("Approach"),
                                             choices = list("Linear" = 1,
                                                            "Polynomial" = 2),
                                             selected = 1),
                                h3("Prediction"),
                                numericInput("predict1",
                                             label=h5("Feature 1"),
                                             value = NULL),
                                numericInput("predict2",
                                             label = h5("Feature 2"),
                                             value = NULL),
                                uiOutput("oracle")
                                # value returns form server
                                #tags$head(tags$style("regressionPlot{height:100vh !important;}"))
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Theta Values",tableOutput("stats")),
                                  tabPanel("Plot",webGLOutput("regressionPlot"))
                                ),width = 8
                              )
                            )),
                   tabPanel("Comparative Analysis",
                            titlePanel("Comparative Analysis"),
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("comp_linear_alpha",
                                            "Learning rate for Linear Regression",
                                            min = 0,
                                            max = 1,
                                            value = 0),
                                sliderInput("comp_linear_num_iter",
                                            "Number of Iterations for Linear Regression",
                                            min = 100,
                                            max = 2000,
                                            value =100),
                                sliderInput("comp_poly_alpha",
                                            "Learning rate for Polynomial Regression",
                                            min = 0,
                                            max = 1,
                                            value = 0),
                                sliderInput("comp_poly_num_iter",
                                            "Number of Iterations for Polynomial Regression",
                                            min = 100,
                                            max = 2000,
                                            value = 100)
                              ),
                              mainPanel(
                                webGLOutput("compPlot")
                            )
                            )),
                   tabPanel("Help & FAQ",
                            sidebarLayout(
                              mainPanel("",
                                        titlePanel("Known Issues"),
                                        column(12,
                                               h3("Issue 1: I cannot view plots / Javascript bug"),
                                               helpText("Press F11 and wait for a few seconds and click in the area of the plot, Webgl will kick in action."),
                                               helpText("The problem is caused by a glitch in Javascript and or WebGL which arises to the dimensions of the screen")),
                                        column(12,
                                               h3("Issue 2: NAs in foreign function call (arg 2)"),
                                               helpText("Error due to no file being selected"),
                                               helpText("Select a file to remove the error")),
                                        column(12,
                                               h3("Issue 3: 'from' cannot be NA, NaN or infinite"),
                                               helpText("Same as Issue 2")),
                                        column(12,
                                               h3("Issue 4: 'x' must be numeric"),
                                               helpText("Issue created due to some elements of the datafile being String or containing words"),
                                               helpText("Convert it to numeric by categorizing and or assigning Unique IDs")),
                                        column(12,
                                               h3("Issue 5: only defined on a data frame with all numeric variables"),
                                               helpText("Same as issue 4"))
                                        ),
                              sidebarPanel("",
                                           titlePanel("Contact"),
                                           helpText("Pratik Kubal"),
                                           helpText("Email: pratik.p.kubal@gmail.com"))
                            ))
                   )
        
        )
