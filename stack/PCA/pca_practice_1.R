# doc ---------------------------------------------------------------------
# Author: CN
# Purpose: Learn Principle Component Analysis with R
# Deps:
install.packages("RCurl")
require (RCurl)
load.packages('quantmod')

# load Michael Kapler's source code
sit <- getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
con <- gzcon(rawConnection(sit, 'rb'))
source(con)
close(con)

# load data from yahoo finance --------------------------------------------
data <- new.env()

# Set tickers:
tickers<-spl("VBMFX,VTSMX,VGTSX,VGSIX")

# get symbol from yahoo into 4 dataframes
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
# clean up 4 data frames for OHLC
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

# remove NA and trimp the data, then create the prices df
bt.prep(data, align='remove.na', dates='1990::2013')
# bring prices and returns to the global environment:
prices <- data$prices
returns <- na.omit(prices/mlag(prices) - 1)

# Now, to calculate the port's return assuming equal weights:
weights <- matrix (1/ncol(returns), nrow = 1 , ncol = ncol (returns), byrow = T)
port_returns <- returns %*% t(weights)  


# PCA using princomp() ----------------------------------------------------
str (returns)
str (returns.mat)

returns.mat <- as.matrix (returns)
pc.fit <- princomp (returns.mat, scores = T)

# the eval is the squares of "sdev"
pc.fit$sdev^2

# the evec could be calculated from "loadings" 
# loadings also show how many percents of the variation explained by each components
pc.fit$loadings

summary (pc.fit)
names (pc.fit)


# PCA using linear algebra ------------------------------------------------

# demean and then calculate the covariance of the data
demean = scale(coredata(returns), center=TRUE, scale=FALSE)
covm <- cov(demean)
eval <- eigen(x = covm, symmetric = TRUE)$values
evec <- eigen(x = covm, symmetric = TRUE)$vectors
