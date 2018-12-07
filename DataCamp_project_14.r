#This script is for a project titled "PROJECT: FUNCTIONS FOR FOOD PRICE FORECASTS" on DataCamp.

# Load the readr and dplyr packages
library(readr)
library(dplyr)

# Import the potatoes dataset
potato_prices <- read_csv("datasets/Potatoes (Irish).csv")

# Take a glimpse at the contents
glimpse(potato_prices)

# Import again, only reading specific columns
potato_prices <- read_csv("datasets/Potatoes (Irish).csv", 
                          col_types=cols_only(adm1_name = 'c', 
                                              mkt_name = 'c', 
                                              cm_name = 'c', 
                                              mp_month = 'i', 
                                              mp_year = 'i', 
                                              mp_price = 'd'))

# Rename the columns to be more informative
potato_prices_renamed <- potato_prices %>%
    transmute(region = adm1_name, market = mkt_name, commodity_kg = cm_name,
             month = mp_month, year = mp_year, price_rwf = mp_price)

# Check the result
glimpse(potato_prices_renamed)

# Load lubridate
library(lubridate)

# Convert year and month to Date
potato_prices_cleaned <- potato_prices_renamed %>%
    mutate(date = ymd(paste(year, month, "01"))) %>%
    select(-year, -month)

# See the result
glimpse(potato_prices_cleaned)

# Wrap this code into a function
read_price_data <- function(commodity){
    file <- paste0("datasets/", commodity, ".csv")
    
    commodity <- read_csv(file,
                        col_types = cols_only(
                            adm1_name = col_character(),
                            mkt_name = col_character(),
                            cm_name = col_character(),
                            mp_month = col_integer(),
                            mp_year = col_integer(),
                            mp_price = col_double()
                        )
                       )
    
    commodity_renamed <-commodity %>%
        rename(
            region = adm1_name, 
            market = mkt_name,
            commodity_kg = cm_name,
            month = mp_month,
            year = mp_year,
            price_rwf = mp_price
        )
    
    commodity_cleaned <- commodity_renamed %>%
        mutate(date = ymd(paste(year, month, "01"))) %>% 
    select(-month, -year)
}

# Test it
pea_prices <- read_price_data("Peas (fresh)")
glimpse(pea_prices)

# Load ggplot2
library(ggplot2)

# Draw a line plot of price vs. date grouped by market 
ggplot(potato_prices_cleaned, aes(x = date, y = price_rwf, group = market)) +
    geom_line(alpha = 0.2) +
    labs(title = "Potato price over time")
    
# Wrap this code into a function
plot_price_vs_time <- function(prices, commodity){
    prices %>%
        ggplot(aes(date, price_rwf, group = market)) +
        geom_line(alpha = 0.2) +
        ggtitle(paste(commodity, "price over time"))
}

# Try the function on the pea data
plot_price_vs_time(pea_prices, "Pea")

# Group by date, and calculate the median price
potato_prices_summarized <- potato_prices_cleaned %>%
    group_by(date) %>%
    summarize(median_price_rwf = median(price_rwf))

# See the result
potato_prices_summarized

# Load magrittr
library(magrittr)

max_date = max(potato_prices_summarized$date)
min_date = min(potato_prices_summarized$date)

# Extract a time series
potato_time_series <- ts(potato_prices_summarized$median_price_rwf, 
                         end=c(year(max_date), month(max_date)),
                         start=c(year(min_date), month(min_date)),
                         frequency=12)

# See the result
potato_time_series

# Wrap this code into a function
create_price_time_series <- function(price){
    price_summarized <- price %>%
    group_by(date) %>% 
    summarize(median_price_rwf = median(price_rwf))
    
    price_series <- price_summarized %$%
      ts(
        median_price_rwf, 
        start = c(year(min(date)), month(min(date))), 
        end   = c(year(max(date)), month(max(date))), 
        frequency = 12
      )    
}
# Try the function on the pea data
pea_time_series <- create_price_time_series(pea_prices)
pea_time_series

# Load forecast
library(forecast)

# Forecast the potato time series
potato_price_forecast <- forecast(potato_time_series)

# View it
potato_price_forecast

# Plot the forecast
autoplot(potato_price_forecast, main = "Potato price forecast")

# Wrap the code into a function
plot_price_forecast <- function(time_series, commodity){
    time_forecast <- forecast(time_series)
    autoplot(time_forecast, main = paste(commodity, "price forecast"))
}

# Try the function on the pea data
plot_price_forecast(pea_time_series, "Pea")

# Choose dry beans as the commodity
commodity <- "Beans (dry)"

# Read the price data
bean_prices <- read_price_data(commodity)

# Plot price vs. time
plot_price_vs_time(bean_prices, "Bean")

# Create a price time series
bean_time_series <- create_price_time_series(bean_prices)

# Plot the price forecast
plot_price_forecast(bean_time_series, "Bean")
