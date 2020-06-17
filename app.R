#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(usethis)
library(riingo)
library(tidyquant)
library(ggthemes)
library(data.table)
library(tidyverse)
library(lubridate)
library(ftplottools)

# Get riingo token
riingo_set_token(Sys.getenv("RIINGO_TOKEN"))
riingo_get_token()

# Set necessary variables
tickers = as_tibble(c("batusd", "ethusd", "btcusd", "ltcusd", "trxusd", "xrpusd", "eosusd", "xlmusd", "zrxusd")) %>%
  setNames(., "ticker")

intervals = as_tibble(c("daily", "intraday")) %>%
  setNames(., "interval")

ma_functions = as_tibble(c("SMA", "EMA", "WMA", "DEMA", "ZLEMA", "VWMA", "EVWMA"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cryptocurrency Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tickers",
                  "Select ticker:",
                  choices = tickers,
                  selected = tickers[3,1],
                  multiple = TRUE
      ),
      dateInput("start_date",
                "Start date (Only for Daily interval):",
                "2020-03-01"),
      selectInput("interval",
                  "Select interval: Daily Data or Today's Intraday Data?",
                  choices = intervals,
                  selected = intervals[1,1],
                  multiple = FALSE),
      numericInput("n",
                   "Desired Period",
                   value = 12,
                   min = 5,
                   max = 100,
                   step = 1)
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("Table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    symbols = input$tickers
    start_date = input$start_date
    n = input$n
    
    # Get Daily Data
    crypto_data <- riingo_crypto_prices(symbols,
                                        start_date = start_date,
                                        end_date = today(),
                                        resample_frequency = "1day") %>%
      group_by(ticker)%>%
      select(ticker, date, open, high, low, close, volume) %>%
      mutate(date = as.Date.POSIXct(date))  
    
    # Get Intraday Data
    crypto_intraday_data <- riingo_crypto_latest(symbols,
                                        resample_frequency = "5min") %>%
      group_by(ticker) %>%
      convert_to_local_time(tz = "EST")
    
    crypto_graphs <- crypto_data %>%
      ggplot(aes(x = date, y = close)) +
      geom_line() +
      geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = n, show.legend = TRUE) +
      ft_theme() +
      scale_color_hc()+facet_wrap(~ ticker, ncol = 2, scales = "free") +
      labs(title = "SMA + BBands", y = "Price in USD", x = "Date")
    
    # Intraday Graphs
    crypto_intraday_graphs = crypto_intraday_data %>%
      ggplot(aes(x = date, y = close)) +
      geom_line() +
      geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = n, show.legend = TRUE) +
      ft_theme() +
      scale_color_hc()+facet_wrap(~ ticker, ncol = 2, scales = "free") +
      labs(title = "SMA + BBands (Intraday)", y = "Price in USD", x = "Time")
    
    if (input$interval == "daily") {
      print(crypto_graphs)
    }
    else {
      print(crypto_intraday_graphs)
    }
  })
 ## Add table 
  output$Table = renderTable({
    symbols = input$tickers
    start_date = input$start_date
    
    crypto_data <- riingo_crypto_prices(symbols,
                                        start_date = start_date,
                                        end_date = today(),
                                        resample_frequency = "1day") %>%
      group_by(ticker) %>%
      convert_to_local_time(tz = "EST") %>%
      select(ticker, date, open, high, low, close, volume)
    
    # Get Intraday Data
    crypto_intraday_data <- riingo_crypto_latest(symbols,
                                                 resample_frequency = "5min") %>%
      group_by(ticker) %>%
      convert_to_local_time(tz = "EST") %>%
      select(ticker, date, open, high, low, close, volume)
    
    if (input$interval == "daily") {
      crypto_data
    }
    else {
      crypto_intraday_data
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
