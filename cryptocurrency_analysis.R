library(usethis)
library(tidyverse)
library(riingo)
library(tidyquant)
library(ggthemes)
library(lubridate)
library(ftplottools)

# Useful libraries for further analysis:
# library(PerformanceAnalytics)
# library(quantmod)
# library(PortfolioAnalytics)

# Set riingo token
riingo_set_token(Sys.getenv("RIINGO_TOKEN"))
riingo_get_token()

# Set the start date and symbols of interest
from <- "2020-04-15"
symbols <- c("batusd", "ethusd", "btcusd", "trxusd")

# Get the data from Tiingo
crypto_data <- riingo_crypto_prices(symbols,
                                    start_date = from,
                                    end_date = today(),
                                    resample_frequency = "1day") %>%
  group_by(ticker)

# Graph the data (SMA + BBands)
crypto_graphs <- crypto_data %>%
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 12, show.legend = TRUE) +
  ft_theme() +
  scale_color_hc()+facet_wrap(~ ticker, ncol = 2, scales = "free") +
  labs(title = "SMA + BBands", y = "Price in USD", x = "Date")
crypto_graphs

# Create data frame of daily returns. Spread by ticker for quick viewing
crypto_returns_df <- crypto_data %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "Returns")  %>%
  select(ticker, date, Returns) %>%
  spread(key = ticker, value = Returns)

# Create data frame (no spread like in crypto_returns_df above)
crypto_returns2 <- crypto_data %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "Returns")  %>%
  select(ticker, date, Returns)

# Omit NA values
crypto_returns2 <- na.omit(crypto_returns2)

# Graph returns
crypto_return_graph <- ggplot(data = crypto_returns2, aes(x = date, y = Returns)) +
  geom_line() +
  geom_hline(yintercept = 0, colour = "red") +
  ft_theme() +
  facet_wrap(~ ticker, ncol = 2, scales = "free") +
  labs(title = "Daily Returns", x = "Date")
crypto_return_graph

# Calculate portfolio returns
crypto_returns <- na.omit(crypto_returns)
crypto_returns_total <- crypto_returns %>%
  transmute(total_returns = rowSums(crypto_returns[,2:length(crypto_returns)]))

# Histogram of the returns
crypto_histogram <- ggplot(data = crypto_returns_total, aes(x = total_returns)) +
  geom_histogram(bins = 45) +
  ft_theme() +
  geom_vline(xintercept = mean(crypto_returns_total$total_returns))+
  labs(title = "Daily Returns of Equal Weight Portfolio From (start_date) - Present", x = "Daily Return")
crypto_histogram
