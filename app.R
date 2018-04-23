#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(fBasics)
library(ROI)
library(dygraphs)
library(ROI.plugin.quadprog)
library(forecast)
library(urca)
library(gsubfn)
library(DT)
library(LPTime)
library(MTS)
library(stochvol)
library(PortfolioAnalytics)

# Define UI for application that draws a histogram
ui <- fluidPage(
  img(src='data:image/png;base64,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', align = "Right"),
  
  # Application title
  titlePanel("Application for Financial Econometrics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("symbol","Enter the stock symbol",value = toupper("FB SQ")),
      dateInput("startDate","Enter the starting Date",value = "2016-01-01",format = "yyyy-mm-dd"),
      dateInput("lastDate","Enter the starting Date",value = "2018-01-01",format = "yyyy-mm-dd"),
      checkboxInput("Arima","ARIMA Analysis",value = FALSE),
      checkboxInput("Garch","GARCH Analysis",value = FALSE),
      checkboxInput("var","VAR(Vectorized Auto Regression Model) Analysis",value = FALSE),
      submitButton("Submit"),
      p("NOTE: Only stock symbols separated by space are allowed ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel( #poistion="right",
      tabsetPanel(
        type="tab",
        
        tabPanel(
          "Baisc Information",
          h3("Price Plot"),
          dygraphOutput("priceplot"),
          h3("Discrete Returns Plot"),
          dygraphOutput("returnsplot"),
          h3("Log Returns Plot"),
          dygraphOutput("logreturnsplot"),
          h3("Basic Log Return Statistical Results"),
          verbatimTextOutput("bstat_log"),
          h3("Basic Discrete Return Statistical Results"),
          verbatimTextOutput("bstat"),
          h6("References"),
          p("The application has solely been created for academic purposes only.")
        ),
        tabPanel(
          "Prelimenery Tests",
          h3("Autocorrelation Function Results"),
          plotOutput("autocorr"),
          verbatimTextOutput("sum_autocorr"),
          h3("Partial Autocorrelation Function Results"),
          plotOutput("pacf"),
          verbatimTextOutput("sum_pacf"),
          h3("TEST RESULTS"),
          h4("Ljung Box Test Results"),
          verbatimTextOutput("boxtest"),
          h4("Annual Seasonality Test Results"),
          verbatimTextOutput("annualseasonal"),
          h4("ADF Test Results"),
          verbatimTextOutput("adftest"),
          h4("Cointegration Test"),
          verbatimTextOutput("coit"),
          h6("References"),
          p("The application has solely been created for academic purposes only.")
          
        ),
        tabPanel(
          "ARIMA & GARCH Analysis",
          h3("Arima Models"),
          verbatimTextOutput("arima"),
          h4("Arima Forecast for next 10 Periods"),
          verbatimTextOutput("arima_forecast"),
          h3("GARCH Models (1,1)"),
          verbatimTextOutput("Garch"),
          h6("References"),
          p("The application has solely been created for academic purposes only.")
        ),
        tabPanel(
          "VAR Analysis",
          h3("VAR Model"),
          verbatimTextOutput("var"),
          h3("VAR Model Predictions"),
          verbatimTextOutput("var_pred"),
           # h3("Random Stuff"),
           # verbatimTextOutput("as"),
          h6("References"),
          p("The application has solely been created for academic purposes only.")
        ),
        tabPanel(
            "Portfolio Analytics",
            h2("Value at Risk"),
            verbatimTextOutput("Value"),
            h2("Minimimum variance potfolio"),         
            plotOutput("plot2"), 
            h4("Optimised weights for the given stocks in percenatges are"),
            verbatimTextOutput("wt"),
            verbatimTextOutput("tab"),
            plotOutput("plot4"),
            h6("References"),
            p("The application has solely been created for academic purposes only.")
        )
        
        
      )
      
    )
  ),
  hr(),
  print("~~~Created By: Nikhil Lohiya~~~~"),
  print("~~~Portfolio Analytics Created By: Taranpreet Singh~~~~")
)



# Define server logic required to draw a histogram
server <- function(input, output) ({
  df <- reactive({
    #splitting the input
    syms <- unlist(strsplit(input$symbol, " "))
    #getting stocks with 10 year data and subsetting for close prices 
    Stocks = lapply(syms, function(sym) {
      na.omit(getSymbols(sym,auto.assign=FALSE,src="yahoo", from = input$startDate, to = input$lastDate)[,4])
    })
    
    #removing na's for stocks which dont have 10 yr data
    x<-do.call(cbind, Stocks)
    df<-x[complete.cases(x), ]
    
    #changing colnames of df
    for(name in names(df)){
      colnames(df)[colnames(df)==name] <- strsplit(name,"\\.")[[1]][1]}
    
    return(df)
    #returning df withclosing prices of stocks
    
  })
  
  returns_log<-reactive({
    return(Return.calculate(df(),method = c("log"))[-1])
  })
  returns<-reactive({
    return(Return.calculate(df(),method = c("discrete"))[-1])
  })
  
  
  bstats <- function(input, output)({
    return(basicStats(input))
  })
  
  alpha <- function(input, out)({
    x = vector("list", n())
    for (i in 1:n())({
      if (input == "boxtest")
      {
        ac <- Box.test(returns_log()[,i], lag = 10, type = "Ljung")
        x[[i]] <- ac
      }
      if (input == "annualseasonal")
      {
        a <- ts(returns_log()[,i], frequency=365/365)
        fit <- tbats(a)
        x[[i]] <- !is.null(fit$seasonal)
      }
      if (input == "adftest")
      {
        adf <- adf.test(returns_log()[,i])
        x[[i]] <- adf
      }
    })
    setNames(x, paste0(colnames(df)[,1:n()] , 1:n()))
    return(x)
  })
  
  # arima_model = list(2)
  arima_model <- function(input, output)
    ({
      x = vector("list", n())
      
      if (input == TRUE)
      {
        for (i in 1:n())({
          final_arima <- auto.arima(returns_log()[,i], max.p = 7, trace = TRUE, seasonal = TRUE, parallel = TRUE, stepwise = FALSE)
          x[[i]] <- summary(final_arima)
        })
        setNames(x, paste0(colnames(df)[,1:n()] , 1:n()))
        return(x)
      }
      if (input == FALSE)
      {
        return(NULL)
      }
    })
  arima_forecast_model <- function(input, output)
    ({
      x = vector("list", n())
      
      if (input == TRUE)
      {
        for (i in 1:n())({
          final_arima <- auto.arima(returns_log()[,i], max.p = 7, trace = TRUE, seasonal = TRUE, parallel = TRUE, stepwise = FALSE)
          x[[i]] <- predict(final_arima, n.ahead = 10)
        })
        setNames(x, paste0(colnames(df)[,1:n()] , 1:n()))
        return(x)
      }
      if (input == FALSE)
      {
        return(NULL)
      }
    })
  
  # garch_family <- reactive{
  #   spec1=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
  #   spec2=ugarchspec(variance.model=list(model="iGARCH"),mean.model=list(armaOrder=c(0,0)))
  #   spec3=ugarchspec(variance.model=list(model="eGARCH"),mean.model=list(armaOrder=c(0,0)))
  #   spec4=ugarchspec(variance.model=list(model="gjrGARCH"),mean.model=list(armaOrder=c(0,0)))
  #   mm=ugarchfit(spec=spec1,data=returns_log()[-1])
  #   
  # }
  garch_model <- function(input, output)
    ({
      x = vector("list", n())
      if (input == TRUE)
        ({
          for (i in 1:n()) {
            x[[i]] <- fGarch::garchFit(~garch(1,1),data=returns_log()[,i], trace = F)
            
          }
          setNames(x, paste0(colnames(df)[,1:n()], 1:n()))
          return(x)
        })
      if (input == FALSE)
        ({
          return(NULL)
        })
    })
  
  
  var_model <- function(input, output)
    ({
      if (input ==  TRUE){
        return(VAR(returns_log(),1))
      }
      if (input == FALSE)
        ({
          return(NULL)
        })
    })
  
  var_pred_model <- function(input, output)
    ({
      if (input ==  TRUE){
        return(VARpred(VAR(returns_log(),1),4))
      }
      if (input == FALSE)
        ({
          return(NULL)
        })
    })
  # rm_fit <- function(input,output)
  # {
  #   x = vector("list", n())
  #   source("RMfit.R")
  #   for (i in 1:n()) {
  #     x[[i]] <- RMfit(returns_log()[,i])
  # 
  #   }
  #   setNames(x, paste0(colnames(df)[,1:n()], 1:n()))
  # 
  #   x <- quantile(returns_log()[,1:n()],c(0.95,0.99,0.999))
  #   return(x)
  # }
  
  n <- reactive({
    return(ncol(df()))
  })
  
  opt<-reactive({
    
    # Create the portfolio specification
    port_spec <- portfolio.spec(colnames(returns()))
    
    # Add a full investment constraint such that the weights sum to 1
    port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
    
    # Add a long only constraint such that the weight of an asset is between 0 and 1
    port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
    
    # Add an objective to minimize portfolio standard deviation
    port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
    
    # Solve the optimization problem
    opt <- optimize.portfolio(returns(), portfolio = port_spec, optimize_method = "ROI")
    
    return(opt)
  })
  
  wts<-reactive({
    
    return(extractWeights(opt()))
    
  })

                           
  #First Tab Outputs
  output$priceplot <- renderDygraph(dygraph(df()[,1:n()]))
  output$logreturnsplot <- renderDygraph(dygraph(returns_log()[,1:n()]))
  output$bstat_log <- renderPrint(bstats(returns_log()[,1:n()]))
  output$bstat <- renderPrint(bstats(returns()[,1:n()]))  
  output$returnsplot <- renderDygraph(dygraph(returns()[,1:n()]))
  
  #Second Tab Outputs
  output$autocorr <- renderPlot(acf(returns_log()[,1:n()], lag.max = 10))
  output$sum_autocorr <- renderPrint(summary(acf(returns_log()[,1:n()], lag.max = 10)))
  output$pacf <- renderPlot(summary(pacf(returns_log()[,1:n()], lag.max = 10)))
  output$sum_pacf <- renderPrint(summary(pacf(returns_log()[,1:n()], lag.max = 10)))
  output$adftest <- renderPrint(alpha("adftest"))
  output$boxtest <- renderPrint(alpha("boxtest"))
  output$annualseasonal <- renderPrint(alpha("annualseasonal"))
  output$coit <- renderPrint(summary(ca.jo(returns_log(), type = c("eigen", "trace"), ecdet = c("none", "const", "trend"), K = 2, spec=c("longrun", "transitory"), season = NULL, dumvar = NULL)))
  
  #Third Tab Outputs
  output$arima <- renderPrint(arima_model(input$Arima))
  output$arima_forecast <- renderPrint(arima_forecast_model(input$Arima))
  output$Garch <- renderPrint(garch_model(input$Garch))
  
  #Fourth Tab Outputs
  output$var <- renderPrint(var_model(input$var))
  output$var_pred <- renderPrint(var_pred_model(input$var))
  output$mcs <- renderPrint(svsample(returns_log()[1:n()]))
  # output$as <- renderPrint(SharpeRatio(R = returns_log(), Rf = .029, p = .95, annualize = TRUE, weights = c(wts())))
  
  #Fifth Tab Outputs
  output$Value <- renderPrint(round(VaR(returns()),2))
  output$wt<-renderPrint(round(wts()*100,2))
  output$plot2<-renderPlot(barplot(wts(),
                                   col="wheat",ylab="Weights", main ="Optimised weights" ))
  output$tab<-renderPrint(table.AnnualizedReturns(returns()))
  output$plot4<-renderPlot(chart.TimeSeries(df(),main="Prices",legend.loc="topleft"))
  
})

# Run the application 
shinyApp(ui = ui, server = server)
