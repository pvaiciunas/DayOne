seed(1234)


# Dummy names for the assets and portfolios
column_names <- c(paste("D1", seq(2060, 2010, -5)), "D1 Income")
row_names <- paste("Asset", seq(1, 10, 1))



# Weight Tables -----------------------------------------------------------

# Setup input table (weight deltas)
wgt_deltas <- as.data.frame(matrix(data = 0, nrow = 10, ncol = 12))
rownames(wgt_deltas) <- row_names
colnames(wgt_deltas) <- column_names
  

# Setup the intial weights for all assets
# Eventually this will change to an upload from excel
starting_wgts <- as.data.frame(matrix(data = abs(rnorm(10*12)), nrow = 10, ncol = 12))
rownames(starting_wgts) <- row_names
colnames(starting_wgts) <- column_names



# Covariance Matrices -----------------------------------------------------

# CMA Covariance MAtrix
# LAter this will be uploaded

cma_covmat <- matrix(rnorm(100)/1000, nrow = 10)

# This is the historical data covmat
# This will also be uploaded once new data arrives
hist_covmat <- matrix(rnorm(100)/1000, nrow = 10)


