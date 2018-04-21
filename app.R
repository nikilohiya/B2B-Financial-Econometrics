#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
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
# library(gsubfn)
library(DT)
library(fGarch)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Application for Financial Econometrics"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("symbol","Enter the stock symbol",value = toupper("MSFT FB")),
      dateInput("startDate","Enter the starting Date",value = "2012-01-01",format = "yyyy-mm-dd"),
      dateInput("lastDate","Enter the starting Date",value = "2018-01-01",format = "yyyy-mm-dd"),
      checkboxInput("Arima","ARIMA Analysis",value = FALSE),
      checkboxInput("Garch","GARCH Analysiss",value = FALSE),
      textInput("rfr","Enter the risk free rate for Analysis",value = toupper("3")),
      # checkboxInput("VAR","VAR(Vectorized Auto Regression Model) Analysis",value = FALSE),
      submitButton("Submit"),
      p("NOTE: Only stock symbols separated by space are allowed ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(#poistion="right",
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
          verbatimTextOutput("bstat")
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
          h5("Annual Seasonality Test Results"),
          verbatimTextOutput("annualseasonal"),
          h5("ADF Test Results"),
          verbatimTextOutput("adftest")
        ),
        tabPanel(
          "ARIMA & GARCH Analysis",
          h3("Arima Models"),
          # DT::dataTableOutput("mytable"),
          verbatimTextOutput("Arima"),
          h3("Summary of Arima Models"),
          h3("CAPM Model Against S&P500"),
          verbatimTextOutput("CAPM"),
          # h3("GARCH Model Fit"),
          verbatimTextOutput("garch"),
          h3("Correlations among the Securities"),
          verbatimTextOutput("crosscorr")),
        tabPanel(
          "Risk performace",
          h3("Down Side Risk Analysis"),
          verbatimTextOutput("downrisk"),
          h3("Summary of Arima Models"),
          h3("Correlations among the Securities"),
          verbatimTextOutput("crosscorr")
        )
    )
)
)
)






# Define server logic required to draw a histogram
server <- function(input, output) ({
  df <- reactive
  ({
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
  
  
  bstats <- function(input, output)
    ({
    return(basicStats(input))
  })

  alpha <- function(input, output)
    ({
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

  garch_model <- function(input, output)
  ({
    x = vector("list", n())
    if (input == TRUE)
      ({
        for (i in 1:n()) ({
        x[[i]] <- summary(garchFit(~garch(1,1),data=returns_log()[,i], trace = FALSE, cond.dist = "std"))
        
        })
        setNames(x, paste0(colnames(df)[,1:n()], 1:n()))
        return(x)
      })
    if (input == FALSE)
      ({
        return(NULL)
      })
  })
  
  n <- reactive
  ({
    return(ncol(df()))
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
  #  output$annualseasonal <- renderPrint(annualseasonal(returns_log()[,1:n()]))
  output$adftest <- renderPrint(alpha("adftest"))
  output$boxtest <- renderPrint(alpha("boxtest"))
  output$annualseasonal <- renderPrint(alpha("annualseasonal"))
  
  #Third Tab Outputs
  output$Arima <- renderPrint(arima_model(input$Arima))
  output$garch <- renderPrint(garch_model(input$Garch))
#  output$mytable <- DT::renderDataTable(arima_model(input$Arima))
#  output$Garch <- renderPrint(list1(input$Garch))
  output$downrisk <- renderPrint(table.DownsideRisk(R = returns_log()[,1:n()], ci = 0.95, digits = 6, Rf = input$rfr))
  output$CAPM <- renderPrint(table.CAPM(Ra = returns_log()[,1:n()], Rb = (return_log(getSymbols(Symbols = "^GSPC",src="yahoo", from = input$startDate, to = input$lastDate)[,4])),scale = "daily scale", digits = 6, Rf = input$rfr))
  # output$crosscorr <- renderPrint(table.DownsideRisk(R = returns_log()[,1:n()], ci = 0.95, digits = 6, Rf = input$rfr))
  # output$crosscorr <- renderPrint(table.DownsideRisk(R = returns_log()[,1:n()], ci = 0.95, digits = 6, Rf = input$rfr))
  
})

# Run the application 
shinyApp(ui = ui, server = server)