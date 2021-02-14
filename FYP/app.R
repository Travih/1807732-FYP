## ui.R ##
library(shinydashboard)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "purple"

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)


header <- dashboardHeader(
    title = "Stocks Simulation")

sidebar <- dashboardSidebar(
    ## Sidebar content
    sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Stocks", tabName = "stocks", icon = icon("bar-chart-o")),
        menuItem("Help", tabName = "help", icon = icon("book-open"),  badgeLabel = "new",
                 badgeColor = "green"))
                 
        )
 
    

    

body <- dashboardBody(
    ## Body content
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(
                        title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                        textInput("StockCode", "StockCode", value = "AAPL"),
                        actionButton(inputId = "click", label = "Predict")
                    )
                    
                ),
                
                
                fluidRow(
                    
                    box(
                        title = "Auto Arima - Custom",
                        status = "primary",
                        plotOutput("auto.arima", height = 350),
                        height = 425
                    ),
                    box(
                        title = "Auto Arima - Non Seasonal",
                        
                        width = 6,
                        tableOutput("auto.arima1"),
                        height = 425
                    )
                    
                ),
                
                fluidRow(
                    
                    box(
                        title = "Auto Arima - Industry standard",
                        status = "primary",
                        plotOutput("arima.seasonal", height = 350),
                        height = 425
                    ),
                    box(
                        title = "Auto Arima Seasonal",
                        
                        width = 6,
                        tableOutput("arima.seasonal1"),
                        height = 425
                    )
                    
                )
        ),
        
        
        # second tab content
        tabItem(tabName = "stocks",
                h2("Yahoo Stocks"),
                
                fluidRow(
                    
                    box(
                        title = "Yahoo Stocks",
                        status = "primary",
                        height = 800
                    )
                )
        ),
        
        # third tab content
        tabItem(tabName = "help",
                h2("Help"),
                
        )
        
        
    )
)


## app.R ##
ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) { 
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        if (is.null(input$count) || is.null(input$fill))
            return()
        
        data <- histdata[seq(1, input$count)]
        color <- input$fill
        if (color == "none")
            color <- NULL
        hist(data, col = color, main = NULL)
    })
    
    #Auto Arima Plot data
    output$auto.arima <- renderPlot({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        library('timeSeries')
        library('xts')
        
        data <- eventReactive(input$click, {
            (input$StockCode)  
        })

        Stock <- as.character(data())
        print(Stock)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock, 
                                           from = "2017-01-01", 
                                           auto.assign = FALSE))
        
        Stock_data_Close = Stock_df[,4]
        #plot(Stock_data_Close)
        Acf(Stock_data_Close, main='ACF for Differnced Series')
        Pacf(Stock_data_Close, main='PACF for Differnced Series')
        
        fitA = auto.arima(Stock_data_Close, seasonal = FALSE)
        #fitA = arima(Stock_data_Close, order = c(1,1,3))
        tsdisplay(residuals(fitA), lag.max=40, main='(0,1,1) Model Residuals')
        auto.arima(Stock_data_Close, seasonal = FALSE)
        
        #term = days
        term <- 45
        fcast1 <-  forecast(fitA, h=term)
        #fcast2 <-  forecast(Stock_data_Close, h=term)
        plot(fcast1, col="blue", lty=1, ylab = "Price", xlab = "Days")
        #lines(Stock_data_Close, col="red", lty=2)
    
        
    })
    
    
    #Auto.Arima1 - Forecast table
    output$auto.arima1 <- renderTable({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)

        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", 
                                           from = "2017-01-01", 
                                           auto.assign = FALSE))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        #plot(as.ts(Stock_df$Close))
        
        Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        #Seasonal Trends
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=12)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        fit <- auto.arima(Stock_df$Close,ic="bic")
        fit.forecast <- forecast(fit)
        #plot(fit.forecast,   col = "red")
        (fit.forecast)
    })
    
    #Industry standard Auto.Arima 
    output$arima.seasonal <- renderPlot({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        library('timeSeries')
        library(xts)
        
        
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)

        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           from = "2017-01-01", 
                                           auto.assign = FALSE))

        Stock_data_Close = Stock_df[,4]
        #plot(Stock_data_Close)

        fitB = arima(Stock_data_Close, order = c(1,1,1))
        tsdisplay(residuals(fitB), lag.max=40, main='(1,1,1) Model Residuals')
        
        
        term <- 10
        fcast2 <- forecast(fitB, h=term)
        plot(fcast2, col="blue", lty=1, ylab = "Price", xlab = "Days")
        
       
    })
    
    #Auto.Arima Seasonal Forecast table
    output$arima.seasonal1 <- renderTable({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", 
                                           from = "2017-01-01", 
                                           auto.assign = FALSE))
        Stock_df$Open = Stock_df[,1]
        Stock_df$High = Stock_df[,2]
        Stock_df$Low = Stock_df[,3]
        Stock_df$Close = Stock_df[,4]
        Stock_df$Volume = Stock_df[,5]
        Stock_df$Adj = Stock_df[,6]
        Stock_df <- Stock_df[,c(7,8,9,10,11,12)]
        
        #plot(as.ts(Stock_df$Close))
        
        Stock_df$v7_MA = ma(Stock_df$Close, order=7)
        Stock_df$v30_MA <- ma(Stock_df$Close, order=30)
        
        #STL
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=12)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        #fit <- auto.arima(Stock_df$Close,ic="bic")
        #fit.forecast <- forecast.Arima(fit)
        #plot(fit.forecast,   col = "red")
        #(fit.forecast)
        fit_s<-auto.arima(adj_rental, seasonal=TRUE)
        term <- 10
        fcast_s <- forecast(fit_s, h=term)
        fcast_s
    })
    
    
    
    output$scatter1 <- renderPlot({
        spread <- as.numeric(input$spread) / 100
        x <- rnorm(1000)
        y <- x + rnorm(1000) * spread
        plot(x, y, pch = ".", col = "blue")
    })
    
    output$scatter2 <- renderPlot({
        spread <- as.numeric(input$spread) / 100
        x <- rnorm(1000)
        y <- x + rnorm(1000) * spread
        plot(x, y, pch = ".", col = "red")
    })
    
    #library(rvest)
    # ftse100URL <- "https://www.londonstockexchange.com/indices/ftse-100/constituents/table"
    # ftse100URL %>%
    # html %>% 
    #   html_nodes(xpath = '//*[@id="ftse-index-table"]/table[2]') %>% 
    #   html_table
}

shinyApp(ui, server)
