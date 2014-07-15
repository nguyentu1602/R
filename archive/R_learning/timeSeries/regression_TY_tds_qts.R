## Place:   ECN395 Research - timesSeries project 
## Purpose: 1) Match quotes and trades in the aggregate TY_qtsagg and TY_tdsagg objects
##          2) Get the orderflows using the highfrequency package. Experiment with different correction
##           
## Produce: xts objects to be ready to work with highfrequency package. .R data files of TY trades & quotes

# These two lines are to get the data from my MAC and sould be ignore

# install.packages('highfrequency') 
# install.packages('biglm') 
# install.packages('dynlm') 
# install.packages('RcppArmadillo') 
# install.packages('lubridate')
# install.packages('rugarch')
# install.packages('forecast')
# install.packages('TTR')

load("~/Desktop/ecn395data/TYeverything.R")

load("~/Desktop/ecn395data/ES_qts_aggregate.R")
load("~/Desktop/ecn395data/ES_qts_xts_full.R")
#load("~/Desktop/ecn395data/ES_Quotes_raw.R")
load("~/Desktop/ecn395data/ES_tds_aggregate.R")
load("~/Desktop/ecn395data/ES_tds_xts_full.R")
#load("~/Desktop/ecn395data/ES_Trades_raw.R")

require (fasttime)
require (zoo); require (xts); require (highfrequency)
require (quantmod)
require (dynlm)
require (RcppArmadillo)
require (lubridate)
require (rugarch)
require (forecast)
require (TTR)

# Set the Sys.time variable to have 6 digits after seconds 
options("digits.secs"=6)
Sys.time()

# Matching the quotes and trades at miliseconds level ---------------------

# Now match the ES trades and the quotes data, no adjustment
TYmatch <- matchTradesQuotes (TYtds,TYqts, adjustment=0.000)
TYmatch <- merge (TYmatch, TYtds$tds_volume)

length(TYmatch$PRICE)


TYmatch[1]
TYmatch[908494]

# Testing data
TYmatch[1:10]
TYqts[1:10]
TYtds[1:10]
orderflow[1:10]


# Get the trade directions of the data using Lee and Ready algo
# 1 is buy and -1 is sell
direction <- getTradeDirection (TYmatch)
orderflow <- direction * TYmatch$tds_volume
TYmatch <- merge (TYmatch, direction, orderflow) 

# Trick to eliminate a column in an object very quickly
# TYmatch$orderflow = NULL


# Matching quotes and trades at the 0.5 and 1 SECONDS level -------------------------
# data to store in TYmatch.second

# Create a vector of average price per second. So price at 17:00:00 means
# the average price from 17:00:00.000 to 17:00:00.999
timestamp_halfsecond <- align.time(index(TYmatch), n= 0.5)
timestamp_quartersecond <- align.time(index(TYmatch), n= 0.25)

TYmatch.quartersecond <- aggregate (x= TYmatch$PRICE, 
                                    by= timestamp_quartersecond, 
                                    FUN= mean)
TYmatch.halfsecond <- aggregate (x= TYmatch$PRICE, 
                                 by= timestamp_halfsecond, 
                                 FUN= mean)
TYmatch.second <- aggregate (x= TYmatch$PRICE, 
                             by= fastPOSIXct(trunc(index(TYmatch), units= "secs"),tz= "Chicago"), 
                             FUN= mean)

# Change the vector to xts object and name the first column
TYmatch.quartersecond <- as.xts(x= TYmatch.quartersecond)
dimnames(TYmatch.quartersecond) <- list(list(), c("PRICE"))

TYmatch.halfsecond <- as.xts(x= TYmatch.halfsecond)
dimnames(TYmatch.halfsecond) <- list(list(), c("PRICE"))

TYmatch.second <- as.xts(x= TYmatch.second)
dimnames(TYmatch.second) <- list(list(), c("PRICE"))




# Now create several vectors of BID ASk etc
tempquart <- as.xts (aggregate (x= TYmatch[,2:5], 
                                by= timestamp_quartersecond,
                                FUN= median)) 
temphalf <- as.xts (aggregate (x= TYmatch[,2:5], 
                               by= timestamp_halfsecond,
                               FUN= median)) 
temp <- as.xts (aggregate (x= TYmatch[,2:5], 
                           by= fastPOSIXct(trunc(index(TYmatch), units= "secs"),tz= "Chicago"), 
                           FUN= median)) 

# merge. After this TYmatch.second has 5 columns
TYmatch.quartersecond <- merge(TYmatch.quartersecond, tempquart)
TYmatch.quartersecond$direction <- getTradeDirection (TYmatch.quartersecond)

TYmatch.halfsecond <- merge(TYmatch.halfsecond, temphalf)
TYmatch.halfsecond$direction <- getTradeDirection (TYmatch.halfsecond)

TYmatch.second <- merge(TYmatch.second, temp)
TYmatch.second$direction <- getTradeDirection (TYmatch.second)


# get the order flow by summing up directions from higher frequency:
TYmatch.quartersecond$orderflow <- as.xts (
  aggregate (x= TYmatch$direction, 
             by= timestamp_quartersecond, 
             FUN= sum))
TYmatch.halfsecond$orderflow <- as.xts (
  aggregate (x= TYmatch$direction, 
             by= timestamp_halfsecond, 
             FUN= sum))
TYmatch.second$orderflow <- as.xts (
  aggregate (x= TYmatch$direction, 
             by= fastPOSIXct (trunc (index (TYmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))

# get weighted_orderflow by multiplying direction with tds_volume before summing up:
TYmatch.quartersecond$weighted_orderflow <- as.xts (
  aggregate (x= (TYmatch$direction * TYmatch$tds_volume), 
             by= timestamp_quartersecond,
             FUN= sum))

TYmatch.halfsecond$weighted_orderflow <- as.xts (
  aggregate (x= (TYmatch$direction * TYmatch$tds_volume), 
             by= timestamp_halfsecond,
             FUN= sum))

TYmatch.second$weighted_orderflow <- as.xts (
  aggregate (x= (TYmatch$direction * TYmatch$tds_volume), 
             by= fastPOSIXct (trunc (index (TYmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))

# get trade volume by summing up tds_volume
TYmatch.quartersecond$volume <- as.xts (
  aggregate (x= TYmatch$tds_volume, 
             by= timestamp_quartersecond,
             FUN= sum))
TYmatch.halfsecond$volume <- as.xts (
  aggregate (x= TYmatch$tds_volume, 
             by= timestamp_halfsecond,
             FUN= sum))

TYmatch.second$volume <- as.xts (
  aggregate (x= TYmatch$tds_volume, 
             by= fastPOSIXct (trunc (index (TYmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))


# get the returns and standardize it
TYmatch.quartersecond$returns <- log (TYmatch.quartersecond$PRICE / lag(TYmatch.quartersecond$PRICE))
TYmatch.quartersecond$normalizedreturns <- scale (x= TYmatch.quartersecond$returns)

TYmatch.second$returns <- log (TYmatch.second$PRICE / lag(TYmatch.second$PRICE))
TYmatch.second$normalizedreturns <- scale (x= TYmatch.second$returns)

TYmatch.halfsecond$returns <- log (TYmatch.halfsecond$PRICE / lag(TYmatch.halfsecond$PRICE))
TYmatch.halfsecond$normalizedreturns <- scale (x= TYmatch.halfsecond$returns)


# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
TYmatch.quartersecond$lunchtime <-  as.numeric((hour (index (TYmatch.quartersecond)) == 12))
TYmatch.quartersecond$tradinghour <- as.numeric(((hour (index (TYmatch.quartersecond)) >= 9) 
                                                 & (hour (index (TYmatch.quartersecond)) < 16)
                                                 & !(TYmatch.quartersecond$lunchtime)))
TYmatch.quartersecond$hour <-  as.numeric(hour (index (TYmatch.quartersecond)))
TYmatch.quartersecond$wday <-  as.numeric(wday (index (TYmatch.quartersecond)))

TYmatch.halfsecond$lunchtime <-  as.numeric((hour (index (TYmatch.halfsecond)) == 12))
TYmatch.halfsecond$tradinghour <- as.numeric(((hour (index (TYmatch.halfsecond)) >= 9) 
                                              & (hour (index (TYmatch.halfsecond)) < 16)
                                              & !(TYmatch.halfsecond$lunchtime)))
TYmatch.halfsecond$hour <-  as.numeric(hour (index (TYmatch.halfsecond)))
TYmatch.halfsecond$wday <-  as.numeric(wday (index (TYmatch.halfsecond)))

TYmatch.second$lunchtime <-  as.numeric((hour (index (TYmatch.second)) == 12))
TYmatch.second$tradinghour <- as.numeric(((hour (index (TYmatch.second)) >= 9) 
                                          & (hour (index (TYmatch.second)) < 16)
                                          & !(TYmatch.second$lunchtime)))
TYmatch.second$hour <-  as.numeric(hour (index (TYmatch.second)))
TYmatch.second$wday <-  as.numeric(wday (index (TYmatch.second)))

length(TYmatch.quartersecond) / length(TYmatch.second$PRICE)

# estimate moving average 
TYmatch.quartersecond$movingaverage <- WMA (x= TYmatch.quartersecond$PRICE, n= 20, wts=1:20)
TYmatch.quartersecond$deltamovingaverage <- diff (TYmatch.quartersecond$movingaverage)
TYmatch.quartersecond$averageorderflow <- WMA (x= TYmatch.quartersecond$orderflow, n= 20, wts=1:20)


TYmatch.halfsecond$movingaverage <- WMA (x= TYmatch.halfsecond$PRICE, n= 20, wts=1:20)
TYmatch.halfsecond$deltamovingaverage <- diff (TYmatch.halfsecond$movingaverage)
TYmatch.halfsecond$averageorderflow <- WMA (x= TYmatch.halfsecond$orderflow, n= 20, wts=1:20)


TYmatch.second$movingaverage <- WMA (x= TYmatch.second$PRICE, n= 20, wts=1:20)
TYmatch.second$deltamovingaverage <- diff (TYmatch.second$movingaverage)
TYmatch.second$averageorderflow <- WMA (x= TYmatch.second$orderflow, n= 20, wts=1:20)

# estimate volatility
TYmatch.quartersecond$volatility <- volatility(OHLC=TYmatch.quartersecond$PRICE, n=20, calc="close")
TYmatch.halfsecond$volatility <- volatility(OHLC=TYmatch.halfsecond$PRICE, n=20, calc="close")
TYmatch.second$volatility <- volatility(OHLC=TYmatch.second$PRICE, n=20, calc="close")

# Matching quotes and trades at the MINUTTY level -------------------------
# data to store in TYmatch.minute

# Create a vector of average price per minute. So price at 17:00:00 means
# the average price from 17:00:00.000 to 17:00:59.999 - 1 min gap

TYmatch.minute <- aggregate (x= TYmatch.second$PRICE, 
                             by= fastPOSIXct(trunc(index(TYmatch.second), units= "mins"),tz= "Chicago"), 
                             FUN= mean)
# Change the vector to xts object and name the first column
TYmatch.minute <- as.xts(x= TYmatch.minute)
dimnames(TYmatch.minute) <- list(list(), c("PRICE"))

# Create another vector of median price named medPRICE, data used from the original timseries
TYmatch.minute$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= fastPOSIXct (trunc (index (TYmatch), units= "mins"), tz= "Chicago"), 
             FUN= median))

# Now create several vectors of BID ASk etc
temp <- as.xts (aggregate (x= TYmatch.second[,2:5], 
                           by= fastPOSIXct(trunc(index(TYmatch.second), units= "mins"),tz= "Chicago"), 
                           FUN= median)) 
# merge. After this TYmatch.second has 5 columns
TYmatch.minute <- merge(TYmatch.minute, temp)

# get orderlow
TYmatch.minute$orderflow <- as.xts (
  aggregate (x= TYmatch.second$orderflow, 
             by= fastPOSIXct (trunc (index (TYmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))

# get weighted_orderflow by multiplying direction with tds_volume before summing up:
TYmatch.minute$weighted_orderflow <- as.xts (
  aggregate (x= TYmatch.second$weighted_orderflow, 
             by= fastPOSIXct (trunc (index (TYmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))


# get trade volume
TYmatch.minute$volume <- as.xts (
  aggregate (x= TYmatch.second$volume, 
             by= fastPOSIXct (trunc (index (TYmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))

# get the returns - both mean and median - and standardized it
TYmatch.minute$returns <- log (TYmatch.minute$PRICE / lag(TYmatch.minute$PRICE))
TYmatch.minute$normalizedreturns <- scale (TYmatch.minute$returns)

TYmatch.minute$returnsmed <- log (TYmatch.minute$PRICEmed / lag(TYmatch.minute$PRICEmed))
TYmatch.minute$normalizedreturnsmed <- scale (TYmatch.minute$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
TYmatch.minute$lunchtime <-  (hour (index (TYmatch.minute)) == 12)
TYmatch.minute$tradinghour <- ((hour (index (TYmatch.minute)) >= 9) 
                               & (hour (index (TYmatch.minute)) < 16)
                               & !(TYmatch.minute$lunchtime))
TYmatch.minute$hour <-  as.numeric(hour (index (TYmatch.minute)))
TYmatch.minute$wday <-  as.numeric(wday (index (TYmatch.minute)))

# estimate moving average 
TYmatch.minute$movingaverage <- WMA (x= TYmatch.minute$PRICE, n= 20, wts=1:20)
TYmatch.minute$deltamovingaverage <- diff (TYmatch.minute$movingaverage)

# estimate volatility
TYmatch.minute$volatility <- volatility(OHLC=TYmatch.minute$PRICE, n=20, calc="close")

# Matching quotes and trades at the 2, 5, 10, 20 and 30-seconds level ---------------------
# Create a timestamp index that has 5-second intervals, using align.time in the xts package:
timestamp_2second <- align.time (index (TYmatch.second), n= 2)
timestamp_5second <- align.time (index (TYmatch.second), n= 5)
timestamp_10second <- align.time (index (TYmatch.second), n= 10)
timestamp_20second <- align.time (index (TYmatch.second), n= 20)
timestamp_30second <- align.time (index (TYmatch.second), n= 30)

TYmatch.2second <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                      by= timestamp_2second, 
                                      FUN= mean))
TYmatch.5second <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                      by= timestamp_5second, 
                                      FUN= mean))
TYmatch.10second <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                       by= timestamp_10second, 
                                       FUN= mean))
TYmatch.20second <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                       by= timestamp_20second, 
                                       FUN= mean))
TYmatch.30second <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                       by= timestamp_30second, 
                                       FUN= mean))

# Create another vector of median price named medPRICE, data used from the original timseries
TYmatch.2second$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 2), 
             FUN= median))

TYmatch.5second$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 5), 
             FUN= median))
TYmatch.10second$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 10), 
             FUN= median))

TYmatch.20second$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 20), 
             FUN= median))

TYmatch.30second$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 30), 
             FUN= median))


# get the orderflows 
TYmatch.2second$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                by= timestamp_2second, 
                                                FUN= sum)) 

TYmatch.5second$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                by= timestamp_5second, 
                                                FUN= sum)) 
TYmatch.10second$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                 by= timestamp_10second, 
                                                 FUN= sum)) 
TYmatch.20second$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                 by= timestamp_20second, 
                                                 FUN= sum)) 

TYmatch.30second$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                 by= timestamp_30second, 
                                                 FUN= sum)) 
# get the weighted orderflow
TYmatch.2second$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                         by= timestamp_2second, 
                                                         FUN= sum))

TYmatch.5second$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                         by= timestamp_5second, 
                                                         FUN= sum))
TYmatch.10second$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                          by= timestamp_10second, 
                                                          FUN= sum))
TYmatch.20second$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                          by= timestamp_20second, 
                                                          FUN= sum))
TYmatch.30second$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                          by= timestamp_30second, 
                                                          FUN= sum))

# get the volume
TYmatch.2second$volume <- as.xts (aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_2second, 
                                             FUN= sum))

TYmatch.5second$volume <- as.xts (aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_5second, 
                                             FUN= sum))
TYmatch.10second$volume <- as.xts (aggregate (x= TYmatch.second$volume, 
                                              by= timestamp_10second, 
                                              FUN= sum))
TYmatch.20second$volume <- as.xts (aggregate (x= TYmatch.second$volume, 
                                              by= timestamp_20second, 
                                              FUN= sum))
TYmatch.30second$volume <- as.xts(aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_30second, 
                                             FUN= sum))

# get the returns 
TYmatch.2second$returns <- log(TYmatch.2second$PRICE / lag(TYmatch.2second$PRICE))
TYmatch.2second$normalizedreturns <- scale (TYmatch.2second$returns)
TYmatch.2second$returnsmed <- log (TYmatch.2second$PRICEmed / lag(TYmatch.2second$PRICEmed))
TYmatch.2second$normalizedreturnsmed <- scale (TYmatch.2second$returnsmed)

TYmatch.5second$returns <- log(TYmatch.5second$PRICE / lag(TYmatch.5second$PRICE))
TYmatch.5second$normalizedreturns <- scale (TYmatch.5second$returns)
TYmatch.5second$returnsmed <- log (TYmatch.5second$PRICEmed / lag(TYmatch.5second$PRICEmed))
TYmatch.5second$normalizedreturnsmed <- scale (TYmatch.5second$returnsmed)

TYmatch.10second$returns <- log(TYmatch.10second$PRICE / lag(TYmatch.10second$PRICE))
TYmatch.10second$normalizedreturns <- scale (TYmatch.10second$returns)
TYmatch.10second$returnsmed <- log (TYmatch.10second$PRICEmed / lag(TYmatch.10second$PRICEmed))
TYmatch.10second$normalizedreturnsmed <- scale (TYmatch.10second$returnsmed)

TYmatch.20second$returns <- log(TYmatch.20second$PRICE / lag(TYmatch.20second$PRICE))
TYmatch.20second$normalizedreturns <- scale (TYmatch.20second$returns)
TYmatch.20second$returnsmed <- log (TYmatch.20second$PRICEmed / lag(TYmatch.20second$PRICEmed))
TYmatch.20second$normalizedreturnsmed <- scale (TYmatch.20second$returnsmed)

TYmatch.30second$returns <- log(TYmatch.30second$PRICE / lag(TYmatch.30second$PRICE))
TYmatch.30second$normalizedreturns <- scale (TYmatch.30second$returns)
TYmatch.30second$returnsmed <- log (TYmatch.30second$PRICEmed / lag(TYmatch.30second$PRICEmed))
TYmatch.30second$normalizedreturnsmed <- scale (TYmatch.30second$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
TYmatch.2second$lunchtime <-  as.numeric((hour (index (TYmatch.2second)) == 12))
TYmatch.2second$tradinghour <- as.numeric((hour (index (TYmatch.2second)) >= 9) 
                                          & (hour (index (TYmatch.2second)) < 16)
                                          & !(TYmatch.2second$lunchtime))
TYmatch.2second$hour <-  as.factor(hour (index (TYmatch.2second)))
TYmatch.2second$wday <-  as.factor(wday (index (TYmatch.2second)))

TYmatch.5second$lunchtime <-  as.numeric((hour (index (TYmatch.5second)) == 12))
TYmatch.5second$tradinghour <- as.numeric((hour (index (TYmatch.5second)) >= 9) 
                                          & (hour (index (TYmatch.5second)) < 16)
                                          & !(TYmatch.5second$lunchtime))
TYmatch.5second$hour <-  as.factor(hour (index (TYmatch.5second)))
TYmatch.5second$wday <-  as.factor(wday (index (TYmatch.5second)))

TYmatch.10second$lunchtime <-  as.numeric((hour (index (TYmatch.10second)) == 12))
TYmatch.10second$tradinghour <- as.numeric((hour (index (TYmatch.10second)) >= 9) 
                                           & (hour (index (TYmatch.10second)) < 16)
                                           & !(TYmatch.10second$lunchtime))
TYmatch.10second$hour <-  as.factor(hour (index (TYmatch.10second)))
TYmatch.10second$wday <-  as.factor(wday (index (TYmatch.10second)))

TYmatch.20second$lunchtime <-  as.numeric((hour (index (TYmatch.20second)) == 12))
TYmatch.20second$tradinghour <- as.numeric((hour (index (TYmatch.20second)) >= 9) 
                                           & (hour (index (TYmatch.20second)) < 16)
                                           & !(TYmatch.20second$lunchtime))
TYmatch.20second$hour <-  as.factor(hour (index (TYmatch.20second)))
TYmatch.20second$wday <-  as.factor(wday (index (TYmatch.20second)))

TYmatch.30second$lunchtime <-  as.numeric(hour (index (TYmatch.30second)) == 12)
TYmatch.30second$tradinghour <- as.numeric((hour (index (TYmatch.30second)) >= 9) 
                                           & (hour (index (TYmatch.30second)) < 16)
                                           & !(TYmatch.30second$lunchtime))
TYmatch.30second$hour <-  as.factor(hour (index (TYmatch.30second)))
TYmatch.30second$wday <-  as.factor(wday (index (TYmatch.30second)))

# estimate moving average 

TYmatch.2second$movingaverage <- WMA (x= TYmatch.2second$PRICE, n= 20, wts=1:20)
TYmatch.2second$deltamovingaverage <- diff (TYmatch.2second$movingaverage)
TYmatch.2second$averageorderflow <- WMA (x= TYmatch.2second$orderflow, n= 11, wts=1:11)

TYmatch.5second$movingaverage <- WMA (x= TYmatch.5second$PRICE, n= 20, wts=1:20)
TYmatch.5second$deltamovingaverage <- diff (TYmatch.5second$movingaverage)
TYmatch.5second$averageorderflow <- WMA (x= TYmatch.5second$orderflow, n= 10, wts=1:10)

TYmatch.10second$movingaverage <- WMA (x= TYmatch.10second$PRICE, n= 20, wts=1:20)
TYmatch.10second$deltamovingaverage <- diff (TYmatch.10second$movingaverage)
TYmatch.10second$averageorderflow <- WMA (x= TYmatch.10second$orderflow, n= 10, wts=1:10)

TYmatch.20second$movingaverage <- WMA (x= TYmatch.20second$PRICE, n= 20, wts=1:20)
TYmatch.20second$deltamovingaverage <- diff (TYmatch.20second$movingaverage)
TYmatch.20second$averageorderflow <- WMA (x= TYmatch.20second$orderflow, n= 10, wts=1:10)

TYmatch.30second$movingaverage <- WMA (x= TYmatch.30second$PRICE, n= 20, wts=1:20)
TYmatch.30second$deltamovingaverage <- diff (TYmatch.30second$movingaverage)
TYmatch.30second$averageorderflow <- WMA (x= TYmatch.30second$orderflow, n= 10, wts=1:10)
# estimate volatility
TYmatch.2second$volatility <- volatility(OHLC=TYmatch.2second$PRICE, n=20, calc="close")
TYmatch.5second$volatility <- volatility(OHLC=TYmatch.5second$PRICE, n=20, calc="close")
TYmatch.10second$volatility <- volatility(OHLC=TYmatch.10second$PRICE, n=20, calc="close")
TYmatch.20second$volatility <- volatility(OHLC=TYmatch.20second$PRICE, n=20, calc="close")
TYmatch.30second$volatility <- volatility(OHLC=TYmatch.30second$PRICE, n=20, calc="close")



# Matching quotes and trades at the 5-MINUTES, 15-MINUTES and 30-MINUTES level --------
# Create a timestamp index that has 5-minute and 15-minute intervals, using align.time in the xts package:
timestamp_5minute <- align.time (index (TYmatch.second), n= 5*60)
timestamp_15minute <- align.time (index (TYmatch.second), n= 15*60)
timestamp_30minute <- align.time (index (TYmatch.second), n= 30*60)


TYmatch.5minute <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                      by= timestamp_5minute, 
                                      FUN= mean))
TYmatch.15minute <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                       by= timestamp_15minute, 
                                       FUN= mean))
TYmatch.30minute <- as.xts (aggregate (x= TYmatch.second[, 1:5], 
                                       by= timestamp_30minute, 
                                       FUN= mean))


# Create another vector of median price named medPRICE, data used from the original timseries
TYmatch.5minute$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 5*60), 
             FUN= median))

TYmatch.15minute$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 15*60), 
             FUN= median))

TYmatch.30minute$PRICEmed <- as.xts (
  aggregate (x= TYmatch$PRICE, 
             by= align.time (index (TYmatch), n= 30*60), 
             FUN= median))


# get the orderflows 
TYmatch.5minute$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                by= timestamp_5minute, 
                                                FUN= sum)) 

TYmatch.15minute$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                 by= timestamp_15minute, 
                                                 FUN= sum)) 
TYmatch.30minute$orderflow <- as.xts (aggregate (x= TYmatch.second$orderflow, 
                                                 by= timestamp_30minute, 
                                                 FUN= sum)) 

# get the weighted orderflow
TYmatch.5minute$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                         by= timestamp_5minute, 
                                                         FUN= sum))

TYmatch.15minute$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                          by= timestamp_15minute, 
                                                          FUN= sum))

TYmatch.30minute$weighted_orderflow <- as.xts (aggregate (x= TYmatch.second$weighted_orderflow, 
                                                          by= timestamp_30minute, 
                                                          FUN= sum))
# get the volume
TYmatch.5minute$volume <- as.xts (aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_5minute, 
                                             FUN= sum))

TYmatch.15minute$volume <- as.xts(aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_15minute, 
                                             FUN= sum))
TYmatch.30minute$volume <- as.xts(aggregate (x= TYmatch.second$volume, 
                                             by= timestamp_30minute, 
                                             FUN= sum))


# get the returns 
TYmatch.5minute$returns <- log(TYmatch.5minute$PRICE / lag(TYmatch.5minute$PRICE))
TYmatch.5minute$normalizedreturns <- scale (TYmatch.5minute$returns)

TYmatch.5minute$returnsmed <- log (TYmatch.5minute$PRICEmed / lag(TYmatch.5minute$PRICEmed))
TYmatch.5minute$normalizedreturnsmed <- scale (TYmatch.5minute$returnsmed)


TYmatch.15minute$returns <- log(TYmatch.15minute$PRICE / lag(TYmatch.15minute$PRICE))
TYmatch.15minute$normalizedreturns <- scale (TYmatch.15minute$returns)

TYmatch.15minute$returnsmed <- log (TYmatch.15minute$PRICEmed / lag(TYmatch.15minute$PRICEmed))
TYmatch.15minute$normalizedreturnsmed <- scale (TYmatch.15minute$returnsmed)


TYmatch.30minute$returns <- log(TYmatch.30minute$PRICE / lag(TYmatch.30minute$PRICE))
TYmatch.30minute$normalizedreturns <- scale (TYmatch.30minute$returns)

TYmatch.30minute$returnsmed <- log (TYmatch.30minute$PRICEmed / lag(TYmatch.30minute$PRICEmed))
TYmatch.30minute$normalizedreturnsmed <- scale (TYmatch.30minute$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
TYmatch.5minute$lunchtime <-  as.numeric (hour (index (TYmatch.5minute)) == 12)
TYmatch.5minute$tradinghour <- as.numeric ((hour (index (TYmatch.5minute)) >= 9) 
                                           & (hour (index (TYmatch.5minute)) < 16)
                                           & !(TYmatch.5minute$lunchtime))
TYmatch.5minute$hour <-  as.numeric(hour (index (TYmatch.5minute)))
TYmatch.5minute$wday <-  as.numeric(wday (index (TYmatch.5minute)))

TYmatch.15minute$lunchtime <-  as.numeric (hour (index (TYmatch.15minute)) == 12)
TYmatch.15minute$tradinghour <- as.numeric ((hour (index (TYmatch.15minute)) >= 9) 
                                            & (hour (index (TYmatch.15minute)) < 16)
                                            & !(TYmatch.15minute$lunchtime))
TYmatch.15minute$hour <-  as.numeric(hour (index (TYmatch.15minute)))
TYmatch.15minute$wday <-  as.numeric(wday (index (TYmatch.15minute)))
TYmatch.30minute$lunchtime <-  as.numeric (hour (index (TYmatch.30minute)) == 12)
TYmatch.30minute$tradinghour <- as.numeric ((hour (index (TYmatch.30minute)) >= 9) 
                                            & (hour (index (TYmatch.30minute)) < 16)
                                            & !(TYmatch.30minute$lunchtime))
TYmatch.30minute$hour <-  as.numeric(hour (index (TYmatch.30minute)))
TYmatch.30minute$wday <-  as.numeric(wday (index (TYmatch.30minute)))

# estimate moving average 
TYmatch.5minute$movingaverage <- WMA (x= TYmatch.5minute$PRICE, n= 6, wts=1:6)
TYmatch.5minute$deltamovingaverage <- diff (TYmatch.5minute$movingaverage)
TYmatch.5minute$averageorderflow <- WMA (x= TYmatch.5minute$orderflow, n= 6, wts=1:6)


TYmatch.15minute$movingaverage <- WMA (x= TYmatch.15minute$PRICE, n= 4, wts=1:4)
TYmatch.15minute$deltamovingaverage <- diff (TYmatch.15minute$movingaverage)
TYmatch.15minute$averageorderflow <- WMA (x= TYmatch.15minute$orderflow, n= 4, wts=1:4)

TYmatch.30minute$movingaverage <- WMA (x= TYmatch.30minute$PRICE, n= 2, wts=1:2)
TYmatch.30minute$deltamovingaverage <- diff (TYmatch.30minute$movingaverage)
TYmatch.30minute$averageorderflow <- WMA (x= TYmatch.30minute$orderflow, n= 2, wts=1:2)


# estimate volatility
TYmatch.5minute$volatility <- volatility(OHLC=TYmatch.5minute$PRICE, n=6, calc="close")
TYmatch.15minute$volatility <- volatility(OHLC=TYmatch.15minute$PRICE, n=4, calc="close")

save.image()

# Fitting linear models! --------------------------------------------------


dim (TYmatch.5second)
dim (TYmatch.quartersecond)

TYmodel.full <- list (length=12)
TYmodel.full.summary <- list (length=12)
TYdatasets <- list (TYmatch.quartersecond, TYmatch.halfsecond, TYmatch.second, TYmatch.2second,
                TYmatch.5second, TYmatch.10second, TYmatch.20second, TYmatch.30second,
                TYmatch.minute, TYmatch.5minute, TYmatch.15minute, TYmatch.30minute)


TYmatch.2second[100:110]

for (i in 5:12) {
  TYmodel.full[[i]] <- dynlm (normalizedreturns ~ orderflow
                              + L (deltamovingaverage, 1)  
                              + volatility
                              + volume
                              + factor(hour) + factor(wday),
                              data= TYdatasets[[i]] )
}

for (i in 5:12) {
  TYmodel.full.summary[[i]] <- summary (TYmodel.full[[i]])
}

summary(TYmodel.full[[1]])









save(list= ls(), file="/home/nguyentu/Desktop/ecn395data/TYeverything.R")

















# Test liquidity during different time of the day:


TYliquidity <- aggregate (x= TYmatch.second$volume, 
                        by= fastPOSIXct(trunc(index(TYmatch.second), units= "hours"),tz= "Chicago"), 
                        FUN= sum)
length(liquidity)

a <- liquidity["2013-09-10 15:00:00"]
b <- TYmatch.30minute["2013-09-01 00:00:00/2013-09-10 15:00:00"]
length(b$lunchtime)/2/22


plot(liquidity)


