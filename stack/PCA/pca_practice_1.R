# doc ---------------------------------------------------------------------
# Author: CN
# Purpose: Learn Principle Component Analysis with R
# Deps:
install.packages("RCurl")
require (RCurl)
require (quantmod)

# load Michael Kapler's source code
sit <- getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con <- gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)

# load data from yahoo finance --------------------------------------------
data <- new.env()

# Set tickers:
tickers<-spl("VBMFX,VTSMX,VGTSX,VGSIX")

# IMPORTANT: remember this: get symbol from yahoo into 4 dataframes
quantmod::getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)

# clean up 4 data frames for OHLC
for(i in ls(data)) data[[i]] = quantmod::adjustOHLC(data[[i]], use.Adjusted=T)

# remove NA and trimp the data, then create the prices df
bt.prep(data, align='remove.na', dates='1990::2013')
# bring prices and returns to the global environment:
prices <- data$prices
returns <- na.omit(prices/mlag(prices) - 1)

# Now, to calculate the port's return assuming equal weights:
weights <- matrix (1/ncol(returns), nrow = 1 , ncol = ncol (returns), byrow = T)
port_returns <- returns %*% t(weights)  

# Explicit: PCA using linear algebra ------------------------------------------------
# The problem: the given series of returns are "naive" basis, and the whole purpuse of our PCA is to rotate it to a better basis
# In other words, each basis is a transformation. Naive basis is the Identity matrix.
# Better basis linearly transforms the returns series


# demean and then calculate the covariance of the data (not really neccessary)
demean <- scale(coredata(returns), center=TRUE, scale=FALSE)

# Note: you do not need to demean it, because cov (returns) and cov (demean) are the same

covm <- cov(demean)
eval <- eigen(x = covm, symmetric = TRUE)$values
evec <- eigen(x = covm, symmetric = TRUE)$vectors

# Mathematical explanation:
# Each evec of the cov matrix is the principle component == loading == linear transformation to the original series that makes the covm of the transformed series diagonal
# The first evec associates with the biggest eigen value and thus is the most important component
# The magnitude of the eigen values are the variance of the transformed series, so each eval tells us how much that PC explains the total variation.

# Implicit: PCA using princomp() ----------------------------------------------------
returns.mat <- as.matrix (returns)

# poke the structures of returns vector and returns matrix
str (returns)
str (returns.mat)

# generate the PCA object:
pc.fit <- princomp (returns.mat, scores = T)
summary (pc.fit)
names (pc.fit)

# the eval vector in the algebra method is the squares of "sdev"
# confirm by checking the difference:
pc.fit$sdev^2 - eval

# The relative size of evals / variances of the PCs are the percentage of explanation:
pc.fit$sdev^2 / (sum(pc.fit$sdev^2))

# similarly:

eval / sum (eval)


# the evec could be calculated from "loadings" 
pc.fit$loadings


# Q: Should I use correlation or covariance in PCA? --------------------------
# ref: http://stats.stackexchange.com/questions/53/pca-on-correlation-or-covariance
# A: Depend, but correlation is more appropriate with multi-assets series. 
# When the covariances of the original series widely vary, the series with biggest variance will dominate the (hypothetically real) first PC, so the results will not be meaningful

# When we standadize the data; howeverk, we lose information on the variation of each series.



