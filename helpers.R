

library(shiny)
library(dplyr)
library(reshape2)

set.seed(1234)
# Set up sample data. wIll fill this in later
data <- data.frame(date = rnorm(20), 
                   stock1 = rnorm(20)/100, 
                   stock2 = rnorm(20)/100,
                   stock3 = rnorm(20)/100)

weights <- data.frame(asset = c("stock2", "stock1", "stock3"), 
                      port1 = c(0.40, 0.3, 0.3),
                      port2 = c(0.5, 0.2, 0.3),
                      port3 = c(0.1,0.1,0.8),
                      port4 = c(0.2,0.5,0.3))

# Store the max and min dates for reference, as well as the stock names
max_date <- max(data$date)
min_date <- min(data$date)
asset_names <- names(data)[-1]

# get the returns into matrix form
m_returns <- data %>%
  select(-date) %>%
  as.matrix


# Before you make calcs, make sure the weights are in the same
# order as returns. 
weights$asset <- factor(weights$asset, levels = asset_names)
weights <- arrange(weights, asset)
m_weights <- as.matrix(select(weights, -asset))

# Calc the covariance matrix
covariance <- cov(m_returns)

# Calc the overall risk
port_vol <- t(m_weights) %*% covariance %*% m_weights # The diag is the vol?
marginal_vols <- covariance %*% m_weights
vol_contribution <- marginal_vols * m_weights

