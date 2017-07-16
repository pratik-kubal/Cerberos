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
                                  tabPanel("Demo Plot",webGLOutput("selectionPlot")),
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
                            )))
        
        )
