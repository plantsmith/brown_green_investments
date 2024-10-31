# Load necessary libraries
library(tidyverse)
library(here)
library(quantmod) # Tools for modeling financial data

# Define the date range
start_date <- "2014-01-01"
end_date <- "2019-12-31"

# Define tickers for the brown (fossil fuels) and green (renewable energy) portfolios
brown_tickers <- c("XOM", "CVX", "BP", "COP", "SHEL")  # Fossil fuel companies
green_tickers <- c("NEE", "TSLA", "ENPH", "BEP", "FSLR")  # Renewable/green tech companies

# Fetch data for the Brown Portfolio
for (ticker in brown_tickers) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
}

# Fetch data for the Green Portfolio
for (ticker in green_tickers) {
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
}

# Combine Adjusted Close Prices for each portfolio
brown_prices <- do.call(merge, lapply(brown_tickers, function(ticker) Ad(get(ticker))))
green_prices <- do.call(merge, lapply(green_tickers, function(ticker) Ad(get(ticker))))

# Calculate daily returns for each portfolio
brown_returns <- na.omit(ROC(brown_prices, type = "discrete"))
green_returns <- na.omit(ROC(green_prices, type = "discrete"))

# Calculate annualized mean returns for each portfolio
brown_avg_return <- mean(brown_returns) * 252  # 252 trading days
green_avg_return <- mean(green_returns) * 252

# Set up initial variables
set.seed(123)  # For reproducibility
initial_investment <- 10000
n_scenarios <- 100  # Number of scenarios to simulate
n_months <- 60  # Simulation for 5 years (60 months)

# Adjusted standard deviations for monthly returns
brown_sd <- 0.02 / sqrt(12)      # Monthly standard deviation for brown stocks
green_sd <- 0.015 / sqrt(12)     # Monthly standard deviation for green stocks

# Initialize an empty data frame to store results
results <- data.frame(
  scenario = integer(n_scenarios),
  brown_allocation = numeric(n_scenarios),
  green_allocation = numeric(n_scenarios),
  final_value = numeric(n_scenarios),
  total_return = numeric(n_scenarios),
  environmental_cost = numeric(n_scenarios)
)

# Run scenarios with a loop
for (i in 1:n_scenarios) {
  
  # Randomly allocate a portion to brown and the rest to green
  brown_allocation <- runif(1, 0, 1)    # Random allocation between 0 and 1
  green_allocation <- 1 - brown_allocation
  
  # Generate monthly returns for each portfolio type
  brown_monthly_returns <- rnorm(n_months, brown_avg_return / 12, brown_sd)
  green_monthly_returns <- rnorm(n_months, green_avg_return / 12, green_sd)
  
  # Calculate portfolio growth over time
  portfolio_value <- initial_investment
  for (j in 1:n_months) {
    # Update portfolio value with weighted returns for the month
    monthly_return <- (brown_monthly_returns[j] * brown_allocation) + 
      (green_monthly_returns[j] * green_allocation)
    portfolio_value <- portfolio_value * (1 + monthly_return)
  }
  
  # Calculate cumulative return and environmental impact
  total_return <- (portfolio_value - initial_investment) / initial_investment
  environmental_cost <- portfolio_value * brown_allocation * 0.1  # Adjusted emissions proxy based on final value
  
  # Store results
  results[i, ] <- c(
    scenario = i,
    brown_allocation = brown_allocation,
    green_allocation = green_allocation,
    final_value = portfolio_value,
    total_return = total_return,
    environmental_cost = environmental_cost
  )
}

# Review results
summary(results)

# Plotting results
plot(results$brown_allocation, results$total_return, 
     xlab = "Brown Portfolio Allocation", 
     ylab = "Total Return", 
     main = "Impact of Brown vs. Green Allocation on Returns")

plot(results$green_allocation, results$total_return, 
     xlab = "Green Portfolio Allocation", 
     ylab = "Total Return", 
     main = "Impact of Green vs. Brown Allocation on Returns")

# Plotting environmental cost vs. brown portfolio allocation
plot(results$brown_allocation, results$environmental_cost, 
     xlab = "Brown Portfolio Allocation", 
     ylab = "Environmental Cost", 
     main = "Impact of Brown Allocation on Environmental Cost",
     col = "darkred", pch = 16)
