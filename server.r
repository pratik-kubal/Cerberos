#each time server is run

#ploting and UI libraryies
library(shiny)
options(rgl.useNULL=TRUE)
library(shinyRGL)
library(rgl)
library(devtools)
library(data.table)
# importing
source("core/featScaling.r")
source("core/oracle.R")
source("core/eq.r")
source("core/gd.r")
source("core/rgl_init.r")
source("core/costFunc.r")
grid.lines = 26
shinyServer(
  function(input,output){
    filedata <- reactive({
      if (input$sampleFile == 1)
        datavar <- read.csv("sample_files/longley.csv")
      if (input$sampleFile == 2)
        datavar <- read.csv("sample_files/winequality-red_edit.csv")
      if (input$sampleFile == 3)
        datavar <- read.csv("sample_files/mlr03_psy.csv")
      if (input$sampleFile == 4)
        datavar <- read.csv("sample_files/AirQualityUCI.csv")
      if (input$sampleFile == 5)
        datavar <- read.csv("sample_files/2010.csv")
      if (input$sampleFile == 6)
      {
        inFile <- input$datafile
        if (is.null(inFile)) return(NULL)
        {
          datavar <- read.csv(inFile$datapath,header = TRUE)
        }
      }
      datavar
    })
    output$filetable <- renderDataTable({
      filedata = filedata()
      filedata
    })
    output$y <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("predict","Predict:",items)
    })
    output$x1 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("feature1","First Feature(Square Root):",items)
    })
    output$x2 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("feature2","Second Feature(Square):",items)
    })
    feature1 <- reactive({
      #if (input$regression==0) return(NULL)
      df = filedata()
      if (is.null(df)) return(NULL)
      iter=input$feature1
      df[iter]
    })
    feature2 <- reactive({
      #if (input$regression==0) return(NULL)
      df = filedata()
      if (is.null(df)) return(NULL)
      iter=input$feature2
      df[iter]
    })
    y <- reactive({
      #if (input$regression==0) return(NULL)
      df = filedata()
      if (is.null(df)) return(NULL)
      iter=input$predict
      df[iter]
    })
    x_final <- reactive({
      x_plot = feature1()
      y_plot = y()
      z_plot = feature2()
      x_1 <- as.matrix(x_plot)
      x_2 <- as.matrix(z_plot)
      ones <- as.matrix(rep.int(1,nrow(x_1)))
      x_temp <- cbind(ones,x_1)
      x <- cbind(x_temp,x_2)
      y <- as.matrix(y_plot)
      x_final <- list(x=x,y=y)
    })
    gridexp <- reactive({
      x_plot = feature1()
      z_plot = feature2()
      grid.lines =26
      x.pred <- seq(min(x_plot), max(x_plot), length.out = grid.lines)
      z.pred <- seq(min(z_plot), max(z_plot), length.out = grid.lines)
      xz <- expand.grid( x_plot = x.pred, z_plot = z.pred)
      ones <- as.matrix(rep.int(1,nrow(xz)))
      xz_l <- as.matrix(cbind(ones,xz))
      xz_q <- polyMat(xz_l)
      gridexp <- list(x.pred=x.pred,z.pred=z.pred,xz=xz,xz_l=xz_l,xz_q=xz_q)
    })
    regression_l <- reactive({
      gridexp = gridexp()
      xz_l <- gridexp$xz_l
      #Gives theta for linear equations
      x_final = x_final()
      x <- x_final$x
      y <- x_final$y
      x.scaled_l <- FScale(x)
      conv_temp <- gd(x.scaled_l,y,input$alpha,input$num_iter)
      theta_temp <- as.matrix(conv_temp[1]) #conv is the object made from the gd function
      #J_hist_temp <- as.matrix(conv_temp[2]) #conv is the object made from the gd function
      theta_l <- unlist(theta_temp)
      y.pred_l <- matrix(profX(xz_l,theta_l,x))
      error_p <- matrix(profX(x,theta_l,x))
      error_l <- sqrt(sum((y-error_p)^2)/nrow(y))
      regression_l <- list(y.pred_l=y.pred_l,theta_l=theta_l,error_l=error_l)
      #J_hist_l <- unlist(J_hist_temp)
    })
    regression_q <- reactive({
      gridexp = gridexp()
      xz_q <- gridexp$xz_q
      
      x_final = x_final()
      x <- x_final$x
      y <- x_final$y
      x_q <- polyMat(x)
      x.scaled_q <- FScale(x_q)
      conv_temp <- gd(x.scaled_q,y,input$alpha,input$num_iter)
      theta_temp <- as.matrix(conv_temp[1]) #conv is the object made from the gd function
      #J_hist_temp <- as.matrix(conv_temp[2]) #conv is the object made from the gd function
      theta_q <- unlist(theta_temp)
      y.pred_q <- matrix(profX(xz_q,theta_q,x_q))
      error_p <- matrix(profX(x_q,theta_q,x_q))
      error_q <- sqrt(sum((y-error_p)^2)/nrow(y))
      #J_hist_q <- unlist(J_hist_temp)
      regression_q <- list(y.pred_q=y.pred_q,theta_q=theta_q,error_q=error_q)
    })
    regression_c <- reactive({
      gridexp = gridexp()
      xz_l <- gridexp$xz_l
      xz_q <- gridexp$xz_q
      
      x_final = x_final()
      x <- x_final$x
      y <- x_final$y
      x_q <- polyMat(x)
      x.scaled_l <- FScale(x)
      x.scaled_q <- FScale(x_q)
      
      conv_l <- gd(x.scaled_l,y,input$comp_linear_alpha,input$comp_linear_num_iter)
      theta_temp_l <- as.matrix(conv_l[1])
      theta_l <- unlist(theta_temp_l)
      y.pred_l <- matrix(profX(xz_l,theta_l,x),nrow = grid.lines,ncol = grid.lines)
      
      conv_q <- gd(x.scaled_q,y,input$comp_poly_alpha,input$comp_poly_num_iter)
      theta_temp_q <- as.matrix(conv_q[1])
      theta_q <- unlist(theta_temp_q)
      y.pred_q <- matrix(profX(xz_q,theta_q,x_q),nrow = grid.lines,ncol = grid.lines)
      
      regression_c <- list(y.pred_q=y.pred_q,y.pred_l=y.pred_l)
    })
    plotdata <- reactive({
      regression_l = regression_l()
      y.pred_l <- regression_l$y.pred_l
      
      regression_q = regression_q()
      y.pred_q <- regression_q$y.pred_q
      
      if (input$regressionType==1){
        y.pred <- y.pred_l
      }
      if(input$regressionType==2){
        y.pred <- y.pred_q
      }
      plotdata <- list(y.pred=y.pred)
    })
    output$selectionPlot <- renderWebGL({
      x_plot = as.numeric(unlist(feature1()))
      y_plot = as.numeric(unlist(y()))
      z_plot = as.numeric(unlist(feature2()))

      rgl_init()
      aspect3d(1,1,1)
      plot3d(x_plot,y_plot,z_plot,size=0.5,type='s',xlab = "Feature 1", ylab = "Prediction", zlab = "Feature 2",main="Plot")
    })
    output$regressionPlot <- renderWebGL({
      df = filedata()
      if(is.null(df)) return(NULL)
      {
        
        x_plot = as.numeric(unlist(feature1()))
        y_plot = as.numeric(unlist(y()))
        z_plot = as.numeric(unlist(feature2()))
        
        gridexp = gridexp()
        x.pred <- gridexp$x.pred
        z.pred <- gridexp$z.pred
        rgl_init()
        aspect3d(1,1,1)
        plot3d(x_plot,y_plot,z_plot,size=0.5,type='s',xlab = "Feature 1", ylab = "Prediction", zlab = "Feature 2",main="Regression")
        if(input$regressionType == 1 | input$regressionType==2){
          plotdata = plotdata()
          y.pred <- plotdata$y.pred
          rgl.surface(x.pred, z.pred, y.pred, color = "green", 
                      alpha = 0.5, lit = FALSE)
          rgl.surface(x.pred, z.pred, y.pred, color = "black",
                      alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
        }
      }})
    output$compPlot <- renderWebGL({
      x_plot = as.numeric(unlist(feature1()))
      y_plot = as.numeric(unlist(y()))
      z_plot = as.numeric(unlist(feature2()))
      
      gridexp = gridexp()
      x.pred <- gridexp$x.pred
      z.pred <- gridexp$z.pred
      
      rgl_init()
      aspect3d(1,1,1)
      plot3d(x_plot,y_plot,z_plot,size=0.5,type='s',xlab = "Feature 1", ylab = "Prediction", zlab = "Feature 2",main="Comparison")
      
      regression_c = regression_c()
      y.pred_l <- regression_c$y.pred_l
      y.pred_q <- regression_c$y.pred_q
      
      rgl.surface(x.pred, z.pred, y.pred_l, color = "green", 
                  alpha = 0.5, lit = FALSE)
      rgl.surface(x.pred, z.pred, y.pred_l, color = "black",
                  alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
      rgl.surface(x.pred, z.pred, y.pred_q, color = "steelblue", 
                  alpha = 0.5, lit = FALSE)
      rgl.surface(x.pred, z.pred, y.pred_q, color = "black",
                  alpha = 0.5, lit = FALSE, front = "lines", back = "lines")
    })
    accur <- reactive({
      df = filedata()
      if(is.null(df)) return(NULL)
      if(input$regressionType == 1){
        regression_l=regression_l()
        theta <- regression_l$theta_l
        error <- regression_l$error_l
        theta <- as.matrix(t(theta))
        theta <- cbind(theta,as.matrix(error))
        colnames(theta) <- c("Intercept","Theta 1","Theta 2","Error")
      }
      if(input$regressionType == 2){
        regression_q=regression_q()
        theta <- regression_q$theta_q
        error <- regression_q$error_q
        theta <- as.matrix(t(theta))
        theta <- cbind(theta, as.matrix(error))
        colnames(theta) <- c("Intercept","Theta 1","Theta 2","Theta 3","Error")
      }
      theta
    })
    output$stats <- renderTable({
      theta <- accur()
    })
    predictValue <- reactive({
      x_final = x_final()
      x <- x_final$x
      
      x1 <- input$predict1
      x2 <- input$predict2
      x_temp <- as.matrix(cbind(x1,x2))
      ones <- as.matrix(rep.int(1,nrow(x_temp)))
      features <- as.matrix(cbind(ones,x_temp))
      if(input$regressionType == 1){
        regression_l=regression_l()
        theta_l <- regression_l$theta_l
        prediction <- profX(features,theta_l,x)
      }
      if(input$regressionType == 2){
        regression_q = regression_q()
        theta_q <- regression_q$theta_q
        theta_q <- as.matrix(theta_q)
        x <- polyMat(x)
        features <- polyMat(features)
        prediction <- profX(features,theta_q,x)
      }
      predictValue <- list(prediction=prediction)
    })
    output$oracle <- renderUI({
      predictValue = predictValue()
      if(is.null(predictValue)) return(NULL)
      prediction <- predictValue$prediction
      numericInput("prediction","Prediction",prediction)
    })
    corMat <- reactive({
      filedata = filedata()
      filedata <- as.matrix(filedata)
      cordata <- cor(filedata)
      cordata <- as.matrix(cordata)
      cordata <- round(cordata,digits = 3)
      rowname <- colnames(filedata)
      output <- as.matrix(cbind(rowname,cordata))
      output
    })
    output$corMat <- renderDataTable({
      data.table(corMat())
    })
  })
