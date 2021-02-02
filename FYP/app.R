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
        menuItem("Widgets", tabName = "widgets", icon = icon("th"),  badgeLabel = "new",
                 badgeColor = "green")
    ),
    menuItem("Stocks", icon = icon("bar-chart-o"),
             menuSubItem("FTSE 100", tabName = "subitem1"),
             menuSubItem("FTSE 250", tabName = "subitem2"),
             menuSubItem("FTSE 350", tabName = "subitem3")
    )
    
)

body <- dashboardBody(
    ## Body content
    tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(
                        title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
                        textInput("StockCode", "StockCode", value = "FRES.L"),
                        radioButtons("seasonal", "Select", c(NonSeasonal = "NonSeasonal", "Seasonal")),
                        actionButton(inputId = "click", label = "Predict")
                    )
                    
                ),
                
                
                fluidRow(
                    
                    box(
                        title = "Auto Arima - Non Seasonal",
                        status = "primary",
                        plotOutput("auto.arima", height = 350),
                        height = 400
                    ),
                    box(
                        title = "Auto Arima - Non Seasonal",
                        
                        width = 6,
                        tableOutput("auto.arima1"),
                        height = 380
                    )
                    
                ),
                
                fluidRow(
                    
                    box(
                        title = "Auto Arima Seasonal",
                        status = "primary",
                        plotOutput("arima.seasonal", height = 350),
                        height = 400
                    ),
                    box(
                        title = "Auto Arima Seasonal",
                        
                        width = 6,
                        tableOutput("arima.seasonal1"),
                        height = 380
                    )
                    
                )
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
                h2("Widgets tab content")
        ),
        
        # third tab content
        tabItem(tabName = "subitem1",
                h2("FTSE 100"),
                
                fluidRow(
                    
                    box(
                        title = "FTSE 100",
                        status = "primary",
                        height = 800
                    )
                )
        ),
        
        # forth tab content
        tabItem(tabName = "subitem2",
                h2("FTSE 250")
        ),
        
        # fifth tab content
        tabItem(tabName = "subitem3",
                h2("FTSE 350")
                
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
        
        
        # if (is.null(input$StockCode))
        #   return()
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        #Stock <- as.character(input$StockCode)
        
        data <- eventReactive(input$click, {
            (input$StockCode) 
        })
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)  
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock, 
                                           src = "yahoo", from = "2018-01-01", env = NULL))
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
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        fit <- auto.arima(Stock_df$Close,ic="bic")
        fit.forecast <- forecast(fit)
        plot(fit.forecast,  main= Stock)
        fit.forecast
        
    })
    
    #Auto.Arima1 - plot here  Tile#5
    output$auto.arima1 <- renderTable({
        #if (is.null(input$StockCode))
        #  return()
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", from = "2018-01-01", env = NULL))
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
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
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
    
    #Auto.Arima Seasonal 
    output$arima.seasonal <- renderPlot({
        if (input$seasonal == "NonSeasonal")
            return()
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", from = "2018-01-01", env = NULL))
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
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
        decomp_rental <- stl(rental_ma, s.window="periodic")
        #plot(decomp_rental)
        adj_rental <- seasadj(decomp_rental)
        #plot(adj_rental)
        
        
        #arima
        #fit <- auto.arima(Stock_df$Close,ic="bic")
        #fit.forecast <- forecast(fit)
        #plot(fit.forecast,   col = "red")
        #(fit.forecast)
        fit_s<-auto.arima(adj_rental, seasonal=TRUE)
        fcast_s <- forecast(fit_s, h=10)
        plot(fcast_s)
    })
    
    #Auto.Arima Seasonal 
    output$arima.seasonal1 <- renderTable({
        #if (is.null(input$StockCode))
        #  return()
        if (input$seasonal == "NonSeasonal")
            return()
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        
        #Stock <- as.character(input$StockCode)
        data <- eventReactive(input$click, {
            (input$StockCode)
        })
        Stock <- as.character(data())
        print(Stock)
        #getSymbols("AAPL", src = "yahoo",from="2017-07-01")
        # plot(AAPL$AAPL.Close)
        Stock_df<-as.data.frame(getSymbols(Symbols = Stock,
                                           src = "yahoo", from = "2018-01-01", env = NULL))
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
        rental_ma <- ts(na.omit(Stock_df$v7_MA), frequency=30)
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
        fcast_s <- forecast(fit_s, h=10)
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
