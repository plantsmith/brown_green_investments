#Specify parameters for the model:
  
#set up initial variables
set.seed(123)  #for reproducibility
initial_investment <- 10000 # starting investment is 10k
n_scenarios <- 100  #number of scenarios to simulate
n_months <- 60  #simulation for 5 years (60 months)

# Initialize results list to store each scenario's data before combining
results_list <- vector("list", n_scenarios)

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
  
  # Initialize matrix and vectors
  company_final_returns <- numeric(length(tickers))
  company_portfolio_values <- matrix(0, nrow = n_months, ncol = length(tickers))
  emissions <- initial_investment * allocation * emissions_intensity
  environmental_cost_allocation <- emissions * SCC
  
  # Update portfolio value based on monthly returns and allocation
  for (j in 1:n_months) {
    company_returns_at_j <- company_monthly_returns[j, ]  # Returns for all companies at month j
    
    # Update portfolio and monthly return
    portfolio_monthly_return <- sum(company_returns_at_j * allocation)
    portfolio_value <- portfolio_value * (1 + portfolio_monthly_return)
    monthly_portfolio_returns[j] <- portfolio_monthly_return
    
    # Calculate individual company portfolio values
    for (k in 1:length(tickers)) {
      if (j == 1) {
        company_portfolio_values[j, k] <- initial_investment * allocation[k] * (1 + company_returns_at_j[k])
      } else {
        company_portfolio_values[j, k] <- company_portfolio_values[j - 1, k] * (1 + company_returns_at_j[k])
      }
    }
    
    # Accumulate final returns
    company_final_returns <- company_final_returns + company_returns_at_j * allocation
  }
  
  # Final calculations outside the monthly loop
  total_return <- (portfolio_value - initial_investment) / initial_investment
  total_enviro_cost_scenario <- sum(environmental_cost_allocation)
  company_final_portfolio_values <- company_portfolio_values[n_months, ]
  
  # Calculate volatilities
  company_volatilities <- apply(company_monthly_returns, 2, sd) * sqrt(12)  # Annualize
  portfolio_volatility <- sd(monthly_portfolio_returns) * sqrt(12)
  
  # Store results
  results_list[[i]] <- data.frame(
    scenario = i,
    company = tickers,
    allocation = allocation,
    final_portfolio_value = portfolio_value,
    total_return = total_return,
    company_final_return = company_final_returns,
    company_final_portfolio_value = company_final_portfolio_values,
    emissions = emissions,
    environmental_cost_allocation = environmental_cost_allocation,
    total_enviro_cost_scenario = total_enviro_cost_scenario,
    company_volatility = company_volatilities,
    scenario_volatility = portfolio_volatility
  )
}

# Combine all results
results <- do.call(rbind, results_list)