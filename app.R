
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(shinythemes)
library(rugarch)
library(forecast)
library(lubridate)
library(gridExtra)
library(shinydashboard)

#reading data
exchange_rates <- read.csv("exchange_rate_dataset.csv",fileEncoding = "ISO-8859-1")

# Converting the Date column to a Date type
exchange_rates$Date <- as.Date(exchange_rates$Date, format="%d-%m-%y")

#Date
start_date <- min(exchange_rates$Date, na.rm = TRUE) #Determine the minimum date for start_date
end_date <- max(exchange_rates$Date, na.rm = TRUE) #Determine the maximum date for end_date

#Define UI
ui <- dashboardPage(
  skin = "purple",  #To set the theme color of the dashboard
  dashboardHeader(title = "Exchange Rates Analysis"), #Setting title
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "timeseries"), #Adding time series tab in the sidebar
      menuItem("Decomposition", tabName = "decomposition"), #Adding decomposition tab in sidebar
      menuItem("GARCH", tabName = "garch"), #Adding garch tab in sidebar
      selectInput("currency", "Select Currency:", choices = names(exchange_rates)[-1]),  #for currency selection
      dateRangeInput("dateRange", "Select Date Range:",  #for Date range selection
                     start = start_date, end = end_date, 
                     min = start_date, max = end_date)
    )
  ),
  dashboardBody(
    tabItems(
      #Time Series Tab
      tabItem(tabName = "timeseries",
              fluidRow(
                box(plotOutput("timeSeriesPlot"), width = 12)  #Container for Time Series plot
              ),
              fluidRow(
                box(title = "Exchange Rate Summary", verbatimTextOutput("statsSummary"), width = 12)  #Container for statistics summary
              )
      ),
      #Decomposition Tab
      tabItem(tabName = "decomposition",
              fluidRow(
                box(plotOutput("decompPlot"), width = 12)  #Container for Decomposition plot
              ),
              fluidRow(
                box(title = "Seasonality Summary", verbatimTextOutput("seasonalitySummary"), width = 4), #Container for seasonality summary
                box(title = "Trend Summary", verbatimTextOutput("trendSummary"), width = 4), #Container for trend summary
                box(title = "Remainder Summary", verbatimTextOutput("remainderSummary"), width = 4) #Container for remainder summary
                )
      ),
      #GARCH Volatility Tab
      tabItem(tabName = "garch", 
              fluidRow(
                box(plotOutput("garchVolatilityPlot"), width = 12)  #Container garch volatility plot
              ),
              fluidRow(
                box(title = "Volatility Summary", verbatimTextOutput("volatilitySummary"), width = 12)  #Container for garch summary
              )
      )
    )
  )
)

#Define Server 
server <- function(input, output) {
  #Define "filtered_data" as a reactive expression to return the filtered dataset
  filtered_data <- reactive({
    req(input$currency, input$dateRange[1], input$dateRange[2])  #ensure currency and date range are selected
    
    exchange_rates %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>% #filter data by date range
      select(Date, Rate = .data[[input$currency]]) %>% #select date and the chosen currency column
      na.omit() #remove rows with NA values
  })
  
  #Time Series Plot
  output$timeSeriesPlot <- renderPlot({
    data <- filtered_data()  #access the reactive filtered data
    req(nrow(data) > 0)  #ensure there is enough filtered data
    
    ggplot(data, aes(x = Date, y = Rate)) + #Plot Date vs Rate
      geom_line(color = "darkblue") + #Use a blue line for the plot
      labs(x = "Date", y = "Exchange Rate", title = paste("Exchange Rate of", input$currency, "Over Time")) + #set plot labels
      theme_minimal() #a minimal theme for the plot
  })
  
  #Summary for the Time Series Plot
  output$statsSummary <- renderText({
    data <- filtered_data()  #access the reactive filtered data
    req(nrow(data) > 0)  #ensure there is enough filtered data
    
    paste("Mean: ", mean(data$Rate), "\n",  #Calculate and display mean
          "Median: ", median(data$Rate), "\n", # Calculate and display median
          "Standard Deviation: ", sd(data$Rate), sep = "") #Calculate and display standard deviation
  })
  
  #Time Series Decomposition Plot
  output$decompPlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 1)
    
    result <- tryCatch({  #trycatch to catch any errors
      exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25) #convert to time series object
      stl(exchange_rate_ts, s.window = "periodic", robust = TRUE) #conduct decomposition
    }, error = function(e) {
      return(NULL)  #return NULL in case of error
    })
    
    if (is.null(result)) {   #check if the STL decomposition was unsuccessful
      plot.new() #create a new blank plot
      title(main = "Not enough data points or periods for STL decomposition.", cex.main = 0.8) #display this message if decomposition fails
    } else {
      #proceed with plotting if decomposition was successful
      trend_df <- data.frame(Time = time(result$time.series), 
                             Trend = result$time.series[, "trend"]) #data frame for trend component
      seasonal_df <- data.frame(Time = time(result$time.series), 
                                Seasonal = result$time.series[, "seasonal"]) #data frame for seasonal component
      remainder_df <- data.frame(Time = time(result$time.series),
                                 Remainder = result$time.series[, "remainder"]) #data frame for remainder component
      
      #create plots for each component with ggplot
      trend_plot <- ggplot(trend_df, aes(x = Time, y = Trend)) + 
        geom_line(color = "blue") + #blue color line
        labs(title = paste("Trend -", input$currency)) #add title for the plot
      seasonal_plot <- ggplot(seasonal_df, 
                              aes(x = Time, y = Seasonal)) + 
        geom_line(color = "green") + #green color line
        labs(title = paste("Seasonality -", input$currency)) # add title for the plot
      remainder_plot <- ggplot(remainder_df, aes(x = Time, y = Remainder)) + 
        geom_area(fill = "darkorange") + #dark orange color line
        labs(title = paste("Remainder -", input$currency)) # add title for the plot
      
      # Arrange the three component plots vertically
      grid.arrange(trend_plot, seasonal_plot, remainder_plot, ncol = 1)
    }
  })
  
 #Trend Summary
 output$trendSummary <- renderText({
   data <- filtered_data() #access the filtered data
   req(nrow(data) > 1) #ensure at least two data points for decomposition
   
   result <- tryCatch({  #trycatch to catch any errors
     exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25) #convert to time series object
     stl(exchange_rate_ts, s.window = "periodic", robust = TRUE) #conduct decomposition
   }, error = function(e) {
     return(NULL)  #return NULL in case of error
   })
   
   if (is.null(result)) {  
     "Not enough data points or periods for STL decomposition." #display this message if decomposition fails
   } 
   else {    #if decomposition successful then display summary stats
     trend_component <- result$time.series[, "trend"] #extract trend component
     paste("Long-term Trend Mean:", mean(trend_component, na.rm = TRUE), #display mean of trend component
           "\nOverall Trend Change:", diff(range(trend_component, na.rm = TRUE))) #display overall trend change
   }
 })
 
 #Seasonality Summary
 output$seasonalitySummary <- renderText({
   data <- filtered_data() #access the filtered data
   req(nrow(data) > 1) #ensure at least two data points for decomposition
   
   result <- tryCatch({  #trycatch to catch any errors
     exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25) #convert to time series object
     stl(exchange_rate_ts, s.window = "periodic", robust = TRUE) #conduct decomposition
   }, error = function(e) {
     return(NULL)  #return NULL in case of error
   })
   
   if (is.null(result)) {
     "Not enough data points or periods for STL decomposition." #display this message if decomposition fails
   } 
   else {   #if decomposition successful then display summary stats
     seasonal_component <- result$time.series[, "seasonal"]  #extract seasonal component
     paste("Max Seasonal Effect:", max(seasonal_component),  #display maximum seasonal effect
           "\nMin Seasonal Effect:", min(seasonal_component)) #display minimum seasonal effect
   }
 })
 
 #Remainder Summary
 output$remainderSummary <- renderText({ 
   data <- filtered_data()  #access the filtered data
   req(nrow(data) > 1)  #ensure at least two data points for decomposition
   
   result <- tryCatch({  #trycatch to catch any errors
     exchange_rate_ts <- ts(data$Rate, start = c(year(min(data$Date)), 1), frequency = 365.25) #convert to time series object
     stl(exchange_rate_ts, s.window = "periodic", robust = TRUE)  #conduct decomposition
   }, error = function(e) {
     return(NULL)  #return NULL in case of error
   })
   
   if (is.null(result)) {
     "Not enough data points or periods for STL decomposition to analyze the remainder component."  #display this message if decomposition fails
   } 
   else {  #if decomposition successful then display summary stats
     remainder_component <- result$time.series[, "remainder"] #extract remainder component
     paste("Remainder Standard Deviation:", sd(remainder_component, na.rm = TRUE),  #display standard deviation of remainder
           "\nNumber of Outliers:", sum(abs(remainder_component) > 2 * sd(remainder_component, na.rm = TRUE))) #display number of outliers in remainder
   }
 })
  
  #GARCH Volatility Plot
  output$garchVolatilityPlot <- renderPlot({
    data <- filtered_data()  #access the reactive filtered data
    req(nrow(data) > 0)  #ensure there is enough filtered data
    
    #Define the GARCH(1,1) model specification
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
    #fit the GARCH model to the "Rate" data
    garch_fit <- ugarchfit(spec = spec, data = data$Rate)
    #extract the conditional volatility from the fitted GARCH model
    volatility <- sigma(garch_fit)
    #create a data frame for plotting with Date and the calculated Volatility
    volatility_data <- data.frame(Date = data$Date, Volatility = volatility)
    #Plot the conditional volatility
    ggplot(volatility_data, aes(x = Date, y = Volatility)) + 
      geom_line(color = "darkred") + #red line for the plot
      labs(x = "Date", y = "Volatility", title = paste("Conditional Volatility of", input$currency)) + #Add labels and title
      theme_minimal() #using a minimal theme for the plot
  })
  
  # Volatility Summary for the GARCH Volatility Plot
  output$volatilitySummary <- renderText({
    data <- filtered_data()  #access the reactive filtered data
    req(nrow(data) > 0)  #ensure there is enough filtered data
    
    #Define the GARCH(1,1) model specification
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
    #fit the GARCH model to the "Rate" data
    garch_fit <- ugarchfit(spec = spec, data = data$Rate)
    
    #extract the conditional volatility from the fitted GARCH model
    volatility <- sigma(garch_fit)
    #compile and paste the summary statistics for the extracted volatility
    paste("Average Volatility: ", mean(volatility), "\n", 
          "Max Volatility: ", max(volatility), "\n", 
          "Min Volatility: ", min(volatility), sep = "")
  })
}
#run the app
shinyApp(ui = ui, server = server)