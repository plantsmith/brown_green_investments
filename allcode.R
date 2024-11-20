#Specify parameters for the model:
  
#set up initial variables
set.seed(123)  #for reproducibility
initial_investment <- 10000 # starting investment is 10k
n_scenarios <- 100  #number of scenarios to simulate
n_months <- 60  #simulation for 5 years (60 months)

# Run scenarios to simulate random allocation
for (i in 1:n_scenarios) {
  # Random allocation for each company
  allocation <- runif(length(tickers))
  allocation <- allocation / sum(allocation)  # Normalize to sum to 1
  
  # Generate monthly returns for each company
  company_monthly_returns <- matrix(rnorm(n_months * length(tickers), avg_return / 12, sd / sqrt(12)),
                                    nrow = n_months, ncol = length(tickers))
  
  # Initialize portfolio value and returns
  portfolio_value <- initial_investment
  monthly_portfolio_returns <- numeric(n_months)
  
  # Initialize vectors to store individual company data
  company_volatilities <- numeric(length(tickers))
  company_final_returns <- numeric(length(tickers))
  
  # Calculate emissions and environmental costs
  emissions <- initial_investment * allocation * emissions_intensity
  environmental_cost_allocation <- emissions * SCC
  
  # Initialize a matrix to store individual company portfolio values for each month
  company_portfolio_values <- matrix(0, nrow = n_months, ncol = length(tickers))
  
  # Update portfolio value based on monthly returns and allocation
  for (j in 1:n_months) {
    # For each company, calculate individual company returns
    company_returns_at_j <- company_monthly_returns[j, ]  # Returns for all companies at month j
    company_final_returns <- company_final_returns + company_returns_at_j * allocation  # Accumulated return by company
    
    # Calculate portfolio monthly return
    portfolio_monthly_return <- sum(company_returns_at_j * allocation)
    portfolio_value <- portfolio_value * (1 + portfolio_monthly_return)
    monthly_portfolio_returns[j] <- portfolio_monthly_return
    
    # Calculate individual company portfolio values assuming 100% allocation
    for (k in 1:length(tickers)) {
      if (j == 1) {
        company_portfolio_values[j, k] <- initial_investment * (1 + company_returns_at_j[k])
      } else {
        company_portfolio_values[j, k] <- company_portfolio_values[j - 1, k] * (1 + company_returns_at_j[k])
      }
    }
  }
  
  # Calculate cumulative return for individual companies
  total_return <- (portfolio_value - initial_investment) / initial_investment
  total_enviro_cost_scenario <- sum(environmental_cost_allocation)  # Total environmental cost
  
  # Store individual company portfolio values at the end of the simulation
  company_final_portfolio_values <- company_portfolio_values[n_months, ]
  
  # Calculate volatility for each company (standard deviation of returns over time)
  for (k in 1:length(tickers)) {
    company_returns <- company_monthly_returns[, k]
    company_volatility <- sd(company_returns) * sqrt(12)  # Annualize volatility
    company_volatilities[k] <- company_volatility
  }
  
  # Calculate portfolio (scenario) volatility based on weighted returns
  portfolio_volatility <- sd(monthly_portfolio_returns) * sqrt(12)  # Annualize portfolio volatility
  
  # Store results for individual company returns and portfolio return
  results_list[[i]] <- data.frame(
    scenario = i,
    company = tickers,
    allocation = allocation,
    final_portfolio_value = portfolio_value,
    total_return = total_return,
    company_final_return = company_final_returns,
    company_final_portfolio_value = company_final_portfolio_values,  # New addition
    emissions = emissions,
    environmental_cost_allocation = environmental_cost_allocation,
    total_enviro_cost_scenario = total_enviro_cost_scenario,
    company_volatility = company_volatilities,
    scenario_volatility = portfolio_volatility
  )
}

# Combine all results from all scenarios
results <- do.call(rbind, results_list)
