library(shiny)
library(dplyr)
library(reshape2)

# With Returns
calcPortfolioVols <- function(dReturns,weights) {
  #
  # Inputs
  #
  # dReturns: daily return dataframe. A 'date' header in column 1. Asset class
  #           names in the remainder
  # weights:  weight dataframe. Asset classes in column 1. Portfolio names 
  #           in columns. Weights underneath the portfolio names
  
  
  
  # TODO, endure the names match up for column headings, or don't use names at all
  # Also, ensure that the structure is correct for both inputs, otherwise boot
  
  
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
  
  # Calc the overall risk stats
  port_vol <- sqrt(diag(t(m_weights) %*% covariance %*% m_weights))
  
  # Return annualized port_vol
  port_vol * sqrt(252)
  
}


# With a covariance matrix
calcPortfolioVols2 <- function(covMat,weights) {
  #
  # Inputs
  #
  # covMat:   Cov Matrix. class MAtrix. Should be labeled for integrity check
  # weights:  weight dataframe. Asset classes are rownames. Portfolio names 
  #           in columns. Weights underneath the portfolio names
  
  
  
  # TODO, endure the names match up for column headings, or don't use names at all
  # Also, ensure that the structure is correct for both inputs, otherwise boot
  
  asset_names <- colnames(covMat)

  # Before you make calcs, make sure the weights are in the same
  # order as the covariance matrix, and turn into a matrix
 # weights <- weights[order(rownames(covMat)),]
  m_weights <- as.matrix(weights)
  
  # Calc portfolio annualized vols
  port_vol <- sqrt(diag(t(m_weights) %*% covMat %*% m_weights))*sqrt(252)
  
  port_vol
}

# set.seed(1234)
# # Set up sample data. wIll fill this in later
# data <- data.frame(date = rnorm(20), 
#                    stock1 = rnorm(20)/100, 
#                    stock2 = rnorm(20)/100,
#                    stock3 = rnorm(20)/100)
# # PORTFOLIOS ARE COLUMNS
# weights <- data.frame(asset = c("stock2", "stock1", "stock3"), 
#                       port1 = c(0.40, 0.3, 0.3),
#                       port2 = c(0.5, 0.2, 0.3),
#                       port3 = c(0.1,0.1,0.8),
#                       port4 = c(0.2,0.5,0.3))
# 
# # Store the max and min dates for reference, as well as the stock names
# max_date <- max(data$date)
# min_date <- min(data$date)
# asset_names <- names(data)[-1]
# 
# # get the returns into matrix form
# m_returns <- data %>%
#   select(-date) %>%
#   as.matrix
# 
# 
# # Before you make calcs, make sure the weights are in the same
# # order as returns. 
# weights$asset <- factor(weights$asset, levels = asset_names)
# weights <- arrange(weights, asset)
# m_weights <- as.matrix(select(weights, -asset))
# 
# # Calc the covariance matrix
# covariance <- cov(m_returns)
# 
# # Calc the overall risk stats
# port_vol <- sqrt(diag(t(m_weights) %*% covariance %*% m_weights))# The diag is the vol?
# # Need to use 'sweep here'. Simply divinding by port_vols makes the operation go row-wise first
# marginal_vols <- sweep(covariance %*% m_weights, MARGIN = 2, port_vol, '/') 
# vol_contribution <- marginal_vols * m_weights
# 
