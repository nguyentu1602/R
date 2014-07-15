## Place:   ECN395 Research - timesSeries project 
## Purpose: 1) Match quotes and trades in the aggregate ES_qtsagg and ES_tdsagg objects
##          2) Get the orderflows using the highfrequency package. Experiment with different correction
##          3) Regress the 
## Produce: xts objects to be ready to work with highfrequency package. .R data files of ES trades & quotes

## Learned: cat("\014") to clear the console; head (vector, -1) to get rid of the last column

cat("\014") 

# These two lines are to get the data from my MAC and sould be ignore
# save(list=c("ESmatch", "ESmatch.minute", "ESmatch.second", "ESqts", "EStds", "orderflow"), file="ESeverything.R")
# load("/Users/cuongnguyen/Dropbox/class/01.s14/ECN395/ESeverything.R")

# install.packages('highfrequency') 
# install.packages('biglm') 
# install.packages('dynlm') 
# install.packages('RcppArmadillo') 
# install.packages('lubridate')
# install.packages('rugarch')
# install.packages('forecast')
# install.packages('TTR')

<<<<<<< HEAD
load("~/Desktop/ecn395data/ESeverything.R",)
=======
>>>>>>> 1fcf8d375b3a14267e59f133753ff614e27bfc76
load("~/Desktop/ecn395data/ES_qts_aggregate.R")
load("~/Desktop/ecn395data/ES_qts_xts_full.R")
#load("~/Desktop/ecn395data/ES_Quotes_raw.R")
load("~/Desktop/ecn395data/ES_tds_aggregate.R")
load("~/Desktop/ecn395data/ES_tds_xts_full.R")
#load("~/Desktop/ecn395data/ES_Trades_raw.R")

require(fasttime)
require(zoo); require(xts); require(highfrequency)
require(quantmod)
require(dynlm)
require(RcppArmadillo)
require (lubridate)
require (rugarch)
require (forecast)
require (TTR)

rm(list=ls(pattern=("model.")))
# Set the Sys.time variable to have 6 digits after seconds 
options("digits.secs"=6)
Sys.time()

# Rename the object, we make use of lazy evaluation here
# ESqts <- qtsagg      # This is the aggregated ES qts xts
# ESqts.full <- qts    # This is full ES qts xts
# EStds <- tdsagg
# EStds.full <- tds

# Matching the quotes and trades at miliseconds level ---------------------

# Now match the ES trades and the quotes data, no adjustment
ESmatch <- matchTradesQuotes (EStds,ESqts, adjustment=0.000)
ESmatch <- merge (ESmatch, EStds$tds_volume)

# Get the trade directions of the data using Lee and Ready algo
# 1 is buy and -1 is sell
direction <- getTradeDirection (ESmatch)
orderflow <- direction * ESmatch$tds_volume
ESmatch <- merge (ESmatch, direction, orderflow) 

# Trick to eliminate a column in an object very quickly
# ESmatch$orderflow = NULL

# Matching quotes and trades at the 0.5 and 1 SECONDS level -------------------------
# data to store in ESmatch.second

# Create a vector of average price per second. So price at 17:00:00 means
# the average price from 17:00:00.000 to 17:00:00.999
timestamp_halfsecond <- align.time(index(ESmatch), n= 0.5)
timestamp_quartersecond <- align.time(index(ESmatch), n= 0.25)

ESmatch.quartersecond <- aggregate (x= ESmatch$PRICE, 
                                 by= timestamp_quartersecond, 
                                 FUN= mean)
ESmatch.halfsecond <- aggregate (x= ESmatch$PRICE, 
                             by= timestamp_halfsecond, 
                             FUN= mean)
ESmatch.second <- aggregate (x= ESmatch$PRICE, 
                             by= fastPOSIXct(trunc(index(ESmatch), units= "secs"),tz= "Chicago"), 
                             FUN= mean)

# Change the vector to xts object and name the first column
ESmatch.quartersecond <- as.xts(x= ESmatch.quartersecond)
dimnames(ESmatch.quartersecond) <- list(list(), c("PRICE"))

ESmatch.halfsecond <- as.xts(x= ESmatch.halfsecond)
dimnames(ESmatch.halfsecond) <- list(list(), c("PRICE"))

ESmatch.second <- as.xts(x= ESmatch.second)
dimnames(ESmatch.second) <- list(list(), c("PRICE"))

getwd()

#Now create several vectors of BID ASk etc
tempquart <- as.xts (aggregate (x= ESmatch[,2:5], 
                               by= timestamp_quartersecond,
                               FUN= median)) 
temphalf <- as.xts (aggregate (x= ESmatch[,2:5], 
                           by= timestamp_halfsecond,
                           FUN= median)) 
temp <- as.xts (aggregate (x= ESmatch[,2:5], 
                           by= fastPOSIXct(trunc(index(ESmatch), units= "secs"),tz= "Chicago"), 
                           FUN= median)) 

# merge. After this ESmatch.second has 5 columns
ESmatch.quartersecond <- merge(ESmatch.quartersecond, tempquart)
ESmatch.quartersecond$direction <- getTradeDirection (ESmatch.quartersecond)

ESmatch.halfsecond <- merge(ESmatch.halfsecond, temphalf)
ESmatch.halfsecond$direction <- getTradeDirection (ESmatch.halfsecond)

ESmatch.second <- merge(ESmatch.second, temp)
ESmatch.second$direction <- getTradeDirection (ESmatch.second)


# get the order flow by summing up directions from higher frequency:
ESmatch.quartersecond$orderflow <- as.xts (
  aggregate (x= ESmatch$direction, 
             by= timestamp_quartersecond, 
             FUN= sum))
ESmatch.halfsecond$orderflow <- as.xts (
  aggregate (x= ESmatch$direction, 
             by= timestamp_halfsecond, 
             FUN= sum))
ESmatch.second$orderflow <- as.xts (
  aggregate (x= ESmatch$direction, 
             by= fastPOSIXct (trunc (index (ESmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))

# get weighted_orderflow by multiplying direction with tds_volume before summing up:
ESmatch.quartersecond$weighted_orderflow <- as.xts (
  aggregate (x= (ESmatch$direction * ESmatch$tds_volume), 
             by= timestamp_quartersecond,
             FUN= sum))

ESmatch.halfsecond$weighted_orderflow <- as.xts (
  aggregate (x= (ESmatch$direction * ESmatch$tds_volume), 
             by= timestamp_halfsecond,
             FUN= sum))

ESmatch.second$weighted_orderflow <- as.xts (
  aggregate (x= (ESmatch$direction * ESmatch$tds_volume), 
             by= fastPOSIXct (trunc (index (ESmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))

# get trade volume by summing up tds_volume
ESmatch.quartersecond$volume <- as.xts (
  aggregate (x= ESmatch$tds_volume, 
             by= timestamp_quartersecond,
             FUN= sum))
ESmatch.halfsecond$volume <- as.xts (
  aggregate (x= ESmatch$tds_volume, 
             by= timestamp_halfsecond,
             FUN= sum))

ESmatch.second$volume <- as.xts (
  aggregate (x= ESmatch$tds_volume, 
             by= fastPOSIXct (trunc (index (ESmatch), units= "secs"), tz= "Chicago"), 
             FUN= sum))


# get the returns and standardize it
ESmatch.quartersecond$returns <- log (ESmatch.quartersecond$PRICE / lag(ESmatch.quartersecond$PRICE))
ESmatch.quartersecond$normalizedreturns <- scale (x= ESmatch.quartersecond$returns)

ESmatch.second$returns <- log (ESmatch.second$PRICE / lag(ESmatch.second$PRICE))
ESmatch.second$normalizedreturns <- scale (x= ESmatch.second$returns)

ESmatch.halfsecond$returns <- log (ESmatch.halfsecond$PRICE / lag(ESmatch.halfsecond$PRICE))
ESmatch.halfsecond$normalizedreturns <- scale (x= ESmatch.halfsecond$returns)


# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
ESmatch.quartersecond$lunchtime <-  as.numeric((hour (index (ESmatch.quartersecond)) == 12))
ESmatch.quartersecond$tradinghour <- as.numeric(((hour (index (ESmatch.quartersecond)) >= 9) 
                                              & (hour (index (ESmatch.quartersecond)) < 16)
                                              & !(ESmatch.quartersecond$lunchtime)))
ESmatch.quartersecond$hour <-  as.factor(hour (index (ESmatch.quartersecond)))
ESmatch.quartersecond$wday <-  as.factor(wday (index (ESmatch.quartersecond)))


ESmatch.halfsecond$lunchtime <-  as.numeric((hour (index (ESmatch.halfsecond)) == 12))
ESmatch.halfsecond$tradinghour <- as.numeric(((hour (index (ESmatch.halfsecond)) >= 9) 
                                              & (hour (index (ESmatch.halfsecond)) < 16)
                                              & !(ESmatch.halfsecond$lunchtime)))
ESmatch.halfsecond$hour <-  as.factor(hour (index (ESmatch.halfsecond)))
ESmatch.halfsecond$wday <-  as.factor(wday (index (ESmatch.halfsecond)))


ESmatch.second$lunchtime <-  as.numeric((hour (index (ESmatch.second)) == 12))
ESmatch.second$tradinghour <- as.numeric(((hour (index (ESmatch.second)) >= 9) 
                                          & (hour (index (ESmatch.second)) < 16)
                                          & !(ESmatch.second$lunchtime)))
ESmatch.second$hour <-  as.factor(hour (index (ESmatch.second)))
ESmatch.second$wday <-  as.factor(wday (index (ESmatch.second)))


length(ESmatch.quartersecond) / length(ESmatch.second$PRICE)

# estimate moving average of price and orderflow
ESmatch.quartersecond$movingaverage <- WMA (x= ESmatch.quartersecond$PRICE, n= 20, wts=1:20)
ESmatch.quartersecond$deltamovingaverage <- diff (ESmatch.quartersecond$movingaverage)

ESmatch.halfsecond$movingaverage <- WMA (x= ESmatch.halfsecond$PRICE, n= 20, wts=1:20)
ESmatch.halfsecond$deltamovingaverage <- diff (ESmatch.halfsecond$movingaverage)

ESmatch.second$movingaverage <- WMA (x= ESmatch.second$PRICE, n= 20, wts=1:20)
ESmatch.second$deltamovingaverage <- diff (ESmatch.second$movingaverage)
ESmatch.second$averageorderflow <- WMA (x= ESmatch.second$orderflow, n= 10, wts=1:10)


# estimate volatility
ESmatch.quartersecond$volatility <- volatility(OHLC=ESmatch.quartersecond$PRICE, n=20, calc="close")
ESmatch.halfsecond$volatility <- volatility(OHLC=ESmatch.halfsecond$PRICE, n=20, calc="close")
ESmatch.second$volatility <- volatility(OHLC=ESmatch.second$PRICE, n=20, calc="close")

# Matching quotes and trades at the MINUTES level -------------------------
# data to store in ESmatch.minute

# Create a vector of average price per minute. So price at 17:00:00 means
# the average price from 17:00:00.000 to 17:00:59.999 - 1 min gap

ESmatch.minute <- aggregate (x= ESmatch.second$PRICE, 
                             by= fastPOSIXct(trunc(index(ESmatch.second), units= "mins"),tz= "Chicago"), 
                             FUN= mean)
# Change the vector to xts object and name the first column
ESmatch.minute <- as.xts(x= ESmatch.minute)
dimnames(ESmatch.minute) <- list(list(), c("PRICE"))

# Create another vector of median price named medPRICE, data used from the original timseries
ESmatch.minute$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= fastPOSIXct (trunc (index (ESmatch), units= "mins"), tz= "Chicago"), 
             FUN= median))

# Now create several vectors of BID ASk etc
temp <- as.xts (aggregate (x= ESmatch.second[,2:5], 
                           by= fastPOSIXct(trunc(index(ESmatch.second), units= "mins"),tz= "Chicago"), 
                           FUN= median)) 
# merge. After this ESmatch.second has 5 columns
ESmatch.minute <- merge(ESmatch.minute, temp)

# get orderlow
ESmatch.minute$orderflow <- as.xts (
  aggregate (x= ESmatch.second$orderflow, 
             by= fastPOSIXct (trunc (index (ESmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))

# get weighted_orderflow by multiplying direction with tds_volume before summing up:
ESmatch.minute$weighted_orderflow <- as.xts (
  aggregate (x= ESmatch.second$weighted_orderflow, 
             by= fastPOSIXct (trunc (index (ESmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))


# get trade volume
ESmatch.minute$volume <- as.xts (
  aggregate (x= ESmatch.second$volume, 
             by= fastPOSIXct (trunc (index (ESmatch.second), units= "mins"), tz= "Chicago"), 
             FUN= sum))

# get the returns - both mean and median - and standardized it
ESmatch.minute$returns <- log (ESmatch.minute$PRICE / lag(ESmatch.minute$PRICE))
ESmatch.minute$normalizedreturns <- scale (ESmatch.minute$returns)

ESmatch.minute$returnsmed <- log (ESmatch.minute$PRICEmed / lag(ESmatch.minute$PRICEmed))
ESmatch.minute$normalizedreturnsmed <- scale (ESmatch.minute$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
ESmatch.minute$lunchtime <-  (hour (index (ESmatch.minute)) == 12)
ESmatch.minute$tradinghour <- ((hour (index (ESmatch.minute)) >= 9) 
                               & (hour (index (ESmatch.minute)) < 16)
                               & !(ESmatch.minute$lunchtime))
ESmatch.minute$hour <-  as.factor(hour (index (ESmatch.minute)))
ESmatch.minute$wday <-  as.factor(wday (index (ESmatch.minute)))



# estimate moving average 
ESmatch.minute$movingaverage <- WMA (x= ESmatch.minute$PRICE, n= 20, wts=1:20)
ESmatch.minute$deltamovingaverage <- diff (ESmatch.minute$movingaverage)

# estimate volatility
ESmatch.minute$volatility <- volatility(OHLC=ESmatch.minute$PRICE, n=20, calc="close")

# Matching quotes and trades at the 2, 5, 10, 20 and 30-seconds level ---------------------
# Create a timestamp index that has 5-second intervals, using align.time in the xts package:
timestamp_2second <- align.time (index (ESmatch.second), n= 2)
timestamp_5second <- align.time (index (ESmatch.second), n= 5)
timestamp_10second <- align.time (index (ESmatch.second), n= 10)
timestamp_20second <- align.time (index (ESmatch.second), n= 20)
timestamp_30second <- align.time (index (ESmatch.second), n= 30)

ESmatch.2second <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                      by= timestamp_2second, 
                                      FUN= mean))
ESmatch.5second <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                      by= timestamp_5second, 
                                      FUN= mean))
ESmatch.10second <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                      by= timestamp_10second, 
                                      FUN= mean))
ESmatch.20second <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                      by= timestamp_20second, 
                                      FUN= mean))
ESmatch.30second <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                       by= timestamp_30second, 
                                       FUN= mean))

# Create another vector of median price named medPRICE, data used from the original timseries
ESmatch.2second$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 2), 
             FUN= median))

ESmatch.5second$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 5), 
             FUN= median))
ESmatch.10second$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 10), 
             FUN= median))

ESmatch.20second$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 20), 
             FUN= median))

ESmatch.30second$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 30), 
             FUN= median))


# get the orderflows 
ESmatch.2second$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                by= timestamp_2second, 
                                                FUN= sum)) 

ESmatch.5second$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                by= timestamp_5second, 
                                                FUN= sum)) 
ESmatch.10second$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                by= timestamp_10second, 
                                                FUN= sum)) 
ESmatch.20second$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                 by= timestamp_20second, 
                                                 FUN= sum)) 

ESmatch.30second$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                 by= timestamp_30second, 
                                                 FUN= sum)) 
# get the weighted orderflow
ESmatch.2second$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                         by= timestamp_2second, 
                                                         FUN= sum))

ESmatch.5second$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                         by= timestamp_5second, 
                                                         FUN= sum))
ESmatch.10second$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                         by= timestamp_10second, 
                                                         FUN= sum))
ESmatch.20second$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                          by= timestamp_20second, 
                                                          FUN= sum))
ESmatch.30second$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                          by= timestamp_30second, 
                                                          FUN= sum))

# get the volume
ESmatch.2second$volume <- as.xts (aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_2second, 
                                             FUN= sum))

ESmatch.5second$volume <- as.xts (aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_5second, 
                                             FUN= sum))
ESmatch.10second$volume <- as.xts (aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_10second, 
                                             FUN= sum))
ESmatch.20second$volume <- as.xts (aggregate (x= ESmatch.second$volume, 
                                              by= timestamp_20second, 
                                              FUN= sum))
ESmatch.30second$volume <- as.xts(aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_30second, 
                                             FUN= sum))

# get the returns 
ESmatch.2second$returns <- log(ESmatch.2second$PRICE / lag(ESmatch.2second$PRICE))
ESmatch.2second$normalizedreturns <- scale (ESmatch.2second$returns)
ESmatch.2second$returnsmed <- log (ESmatch.2second$PRICEmed / lag(ESmatch.2second$PRICEmed))
ESmatch.2second$normalizedreturnsmed <- scale (ESmatch.2second$returnsmed)

ESmatch.5second$returns <- log(ESmatch.5second$PRICE / lag(ESmatch.5second$PRICE))
ESmatch.5second$normalizedreturns <- scale (ESmatch.5second$returns)
ESmatch.5second$returnsmed <- log (ESmatch.5second$PRICEmed / lag(ESmatch.5second$PRICEmed))
ESmatch.5second$normalizedreturnsmed <- scale (ESmatch.5second$returnsmed)

ESmatch.10second$returns <- log(ESmatch.10second$PRICE / lag(ESmatch.10second$PRICE))
ESmatch.10second$normalizedreturns <- scale (ESmatch.10second$returns)
ESmatch.10second$returnsmed <- log (ESmatch.10second$PRICEmed / lag(ESmatch.10second$PRICEmed))
ESmatch.10second$normalizedreturnsmed <- scale (ESmatch.10second$returnsmed)

ESmatch.20second$returns <- log(ESmatch.20second$PRICE / lag(ESmatch.20second$PRICE))
ESmatch.20second$normalizedreturns <- scale (ESmatch.20second$returns)
ESmatch.20second$returnsmed <- log (ESmatch.20second$PRICEmed / lag(ESmatch.20second$PRICEmed))
ESmatch.20second$normalizedreturnsmed <- scale (ESmatch.20second$returnsmed)

ESmatch.30second$returns <- log(ESmatch.30second$PRICE / lag(ESmatch.30second$PRICE))
ESmatch.30second$normalizedreturns <- scale (ESmatch.30second$returns)
ESmatch.30second$returnsmed <- log (ESmatch.30second$PRICEmed / lag(ESmatch.30second$PRICEmed))
ESmatch.30second$normalizedreturnsmed <- scale (ESmatch.30second$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
ESmatch.2second$lunchtime <-  as.numeric((hour (index (ESmatch.2second)) == 12))
ESmatch.2second$tradinghour <- as.numeric((hour (index (ESmatch.2second)) >= 9) 
                                          & (hour (index (ESmatch.2second)) < 16)
                                          & !(ESmatch.2second$lunchtime))
ESmatch.2second$hour <-  as.factor(hour (index (ESmatch.2second)))
ESmatch.2second$wday <-  as.factor(wday (index (ESmatch.2second)))

ESmatch.5second$lunchtime <-  as.numeric((hour (index (ESmatch.5second)) == 12))
ESmatch.5second$tradinghour <- as.numeric((hour (index (ESmatch.5second)) >= 9) 
                                          & (hour (index (ESmatch.5second)) < 16)
                                          & !(ESmatch.5second$lunchtime))
ESmatch.5second$hour <-  as.factor(hour (index (ESmatch.5second)))
ESmatch.5second$wday <-  as.factor(wday (index (ESmatch.5second)))


ESmatch.10second$lunchtime <-  as.numeric((hour (index (ESmatch.10second)) == 12))
ESmatch.10second$tradinghour <- as.numeric((hour (index (ESmatch.10second)) >= 9) 
                                          & (hour (index (ESmatch.10second)) < 16)
                                          & !(ESmatch.10second$lunchtime))
ESmatch.10second$hour <-  as.factor(hour (index (ESmatch.10second)))
ESmatch.10second$wday <-  as.factor(wday (index (ESmatch.10second)))

ESmatch.20second$lunchtime <-  as.numeric((hour (index (ESmatch.20second)) == 12))
ESmatch.20second$tradinghour <- as.numeric((hour (index (ESmatch.20second)) >= 9) 
                                          & (hour (index (ESmatch.20second)) < 16)
                                          & !(ESmatch.20second$lunchtime))
ESmatch.20second$hour <-  as.factor(hour (index (ESmatch.20second)))
ESmatch.20second$wday <-  as.factor(wday (index (ESmatch.20second)))

ESmatch.30second$lunchtime <-  as.numeric(hour (index (ESmatch.30second)) == 12)
ESmatch.30second$tradinghour <- as.numeric((hour (index (ESmatch.30second)) >= 9) 
                                           & (hour (index (ESmatch.30second)) < 16)
                                           & !(ESmatch.30second$lunchtime))
ESmatch.30second$hour <-  as.factor(hour (index (ESmatch.30second)))
ESmatch.30second$wday <-  as.factor(wday (index (ESmatch.30second)))


# estimate moving average 

ESmatch.2second$movingaverage <- WMA (x= ESmatch.2second$PRICE, n= 20, wts=1:20)
ESmatch.2second$deltamovingaverage <- diff (ESmatch.2second$movingaverage)
ESmatch.2second$averageorderflow <- WMA (x= ESmatch.2second$orderflow, n= 10, wts=1:10)

ESmatch.5second$movingaverage <- WMA (x= ESmatch.5second$PRICE, n= 20, wts=1:20)
ESmatch.5second$deltamovingaverage <- diff (ESmatch.5second$movingaverage)
ESmatch.5second$averageorderflow <- WMA (x= ESmatch.5second$orderflow, n= 5, wts=1:5)


ESmatch.10second$movingaverage <- WMA (x= ESmatch.10second$PRICE, n= 20, wts=1:20)
ESmatch.10second$deltamovingaverage <- diff (ESmatch.10second$movingaverage)
ESmatch.10second$averageorderflow <- WMA (x= ESmatch.10second$orderflow, n= 5, wts=1:5)


ESmatch.20second$movingaverage <- WMA (x= ESmatch.20second$PRICE, n= 20, wts=1:20)
ESmatch.20second$deltamovingaverage <- diff (ESmatch.20second$movingaverage)
ESmatch.20second$averageorderflow <- WMA (x= ESmatch.20second$orderflow, n= 5, wts=1:5)

ESmatch.30second$movingaverage <- WMA (x= ESmatch.30second$PRICE, n= 20, wts=1:20)
ESmatch.30second$deltamovingaverage <- diff (ESmatch.30second$movingaverage)

# estimate volatility
ESmatch.2second$volatility <- volatility(OHLC=ESmatch.2second$PRICE, n=20, calc="close")
ESmatch.5second$volatility <- volatility(OHLC=ESmatch.5second$PRICE, n=20, calc="close")
ESmatch.10second$volatility <- volatility(OHLC=ESmatch.10second$PRICE, n=20, calc="close")
ESmatch.20second$volatility <- volatility(OHLC=ESmatch.20second$PRICE, n=20, calc="close")
ESmatch.30second$volatility <- volatility(OHLC=ESmatch.30second$PRICE, n=20, calc="close")



# Matching quotes and trades at the 5-MINUTES, 15-MINUTES and 30-MINUTES level --------
# Create a timestamp index that has 5-minute and 15-minute intervals, using align.time in the xts package:
timestamp_5minute <- align.time (index (ESmatch.second), n= 5*60)
timestamp_15minute <- align.time (index (ESmatch.second), n= 15*60)
timestamp_30minute <- align.time (index (ESmatch.second), n= 30*60)


ESmatch.5minute <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                      by= timestamp_5minute, 
                                      FUN= mean))
ESmatch.15minute <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                       by= timestamp_15minute, 
                                       FUN= mean))
ESmatch.30minute <- as.xts (aggregate (x= ESmatch.second[, 1:5], 
                                       by= timestamp_30minute, 
                                       FUN= mean))


# Create another vector of median price named medPRICE, data used from the original timseries
ESmatch.5minute$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 5*60), 
             FUN= median))

ESmatch.15minute$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 15*60), 
             FUN= median))

ESmatch.30minute$PRICEmed <- as.xts (
  aggregate (x= ESmatch$PRICE, 
             by= align.time (index (ESmatch), n= 30*60), 
             FUN= median))


# get the orderflows 
ESmatch.5minute$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                by= timestamp_5minute, 
                                                FUN= sum)) 

ESmatch.15minute$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                 by= timestamp_15minute, 
                                                 FUN= sum)) 
ESmatch.30minute$orderflow <- as.xts (aggregate (x= ESmatch.second$orderflow, 
                                                 by= timestamp_30minute, 
                                                 FUN= sum)) 

# get the weighted orderflow
ESmatch.5minute$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                         by= timestamp_5minute, 
                                                         FUN= sum))

ESmatch.15minute$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                          by= timestamp_15minute, 
                                                          FUN= sum))

ESmatch.30minute$weighted_orderflow <- as.xts (aggregate (x= ESmatch.second$weighted_orderflow, 
                                                          by= timestamp_30minute, 
                                                          FUN= sum))
# get the volume
ESmatch.5minute$volume <- as.xts (aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_5minute, 
                                             FUN= sum))

ESmatch.15minute$volume <- as.xts(aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_15minute, 
                                             FUN= sum))
ESmatch.30minute$volume <- as.xts(aggregate (x= ESmatch.second$volume, 
                                             by= timestamp_30minute, 
                                             FUN= sum))


# get the returns 
ESmatch.5minute$returns <- log(ESmatch.5minute$PRICE / lag(ESmatch.5minute$PRICE))
ESmatch.5minute$normalizedreturns <- scale (ESmatch.5minute$returns)

ESmatch.5minute$returnsmed <- log (ESmatch.5minute$PRICEmed / lag(ESmatch.5minute$PRICEmed))
ESmatch.5minute$normalizedreturnsmed <- scale (ESmatch.5minute$returnsmed)


ESmatch.15minute$returns <- log(ESmatch.15minute$PRICE / lag(ESmatch.15minute$PRICE))
ESmatch.15minute$normalizedreturns <- scale (ESmatch.15minute$returns)

ESmatch.15minute$returnsmed <- log (ESmatch.15minute$PRICEmed / lag(ESmatch.15minute$PRICEmed))
ESmatch.15minute$normalizedreturnsmed <- scale (ESmatch.15minute$returnsmed)


ESmatch.30minute$returns <- log(ESmatch.30minute$PRICE / lag(ESmatch.30minute$PRICE))
ESmatch.30minute$normalizedreturns <- scale (ESmatch.30minute$returns)

ESmatch.30minute$returnsmed <- log (ESmatch.30minute$PRICEmed / lag(ESmatch.30minute$PRICEmed))
ESmatch.30minute$normalizedreturnsmed <- scale (ESmatch.30minute$returnsmed)

# get the dummy variables for lunchtime and trading hours
# Lunchtime is between 12:00 and 12:59:59.999 each day
# Trading hour is between 9:00 and 16:00 each day
# Non-lunch and non-trading hour are low-liquidity
ESmatch.5minute$lunchtime <-  as.numeric (hour (index (ESmatch.5minute)) == 12)
ESmatch.5minute$tradinghour <- as.numeric ((hour (index (ESmatch.5minute)) >= 9) 
                                           & (hour (index (ESmatch.5minute)) < 16)
                                           & !(ESmatch.5minute$lunchtime))
ESmatch.5minute$hour <-  as.factor(hour (index (ESmatch.5minute)))
ESmatch.5minute$wday <-  as.factor(wday (index (ESmatch.5minute)))

ESmatch.15minute$lunchtime <-  as.numeric (hour (index (ESmatch.15minute)) == 12)
ESmatch.15minute$tradinghour <- as.numeric ((hour (index (ESmatch.15minute)) >= 9) 
                                            & (hour (index (ESmatch.15minute)) < 16)
                                            & !(ESmatch.15minute$lunchtime))
ESmatch.15minute$hour <-  as.factor(hour (index (ESmatch.15minute)))
ESmatch.15minute$wday <-  as.factor(wday (index (ESmatch.15minute)))

ESmatch.30minute$lunchtime <-  as.numeric (hour (index (ESmatch.30minute)) == 12)
ESmatch.30minute$tradinghour <- as.numeric ((hour (index (ESmatch.30minute)) >= 9) 
                                            & (hour (index (ESmatch.30minute)) < 16)
                                            & !(ESmatch.30minute$lunchtime))
ESmatch.30minute$hour <-  as.factor(hour (index (ESmatch.30minute)))
ESmatch.30minute$wday <-  as.factor(wday (index (ESmatch.30minute)))
# estimate moving average 
ESmatch.5minute$movingaverage <- WMA (x= ESmatch.5minute$PRICE, n= 6, wts=1:6)
ESmatch.5minute$deltamovingaverage <- diff (ESmatch.5minute$movingaverage)

ESmatch.15minute$movingaverage <- WMA (x= ESmatch.15minute$PRICE, n= 4, wts=1:4)
ESmatch.15minute$deltamovingaverage <- diff (ESmatch.15minute$movingaverage)

ESmatch.30minute$movingaverage <- WMA (x= ESmatch.30minute$PRICE, n= 2, wts=1:2)
ESmatch.30minute$deltamovingaverage <- diff (ESmatch.30minute$movingaverage)


# estimate volatility
ESmatch.5minute$volatility <- volatility(OHLC=ESmatch.5minute$PRICE, n=6, calc="close")
ESmatch.15minute$volatility <- volatility(OHLC=ESmatch.15minute$PRICE, n=4, calc="close")


# Fitting linear models! --------------------------------------------------

dim (ESmatch.5second)
dim (ESmatch.quartersecond)

model.quartersecond.full <- dynlm (returns ~ orderflow
                                   + L (normalizedreturns, 1)
                                   + volume + factor(hour) + factor(wday) 
                                   + volatility 
                                   + L (deltamovingaverage, 1)
                                   , data=  ESmatch.quartersecond)

model.halfsecond.full <- dynlm (normalizedreturns ~ orderflow
                            + volume + factor(hour) + factor(wday) 
                           + volatility 
                           + L (deltamovingaverage, 1)
                           , data=  ESmatch.halfsecond)

model.second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                        + L (normalizedreturns, -1)
                        + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                        + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                        + L (normalizedreturns, -6) + L (normalizedreturns, -7)                        
                        + L (volatility, -1) + L (deltamovingaverage, -1)
                        , data=  ESmatch.second)


model.2second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                       + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                       + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                       + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                       + L (volume, -1) + L (volume, -2)
                       + tradinghour + lunchtime
                       + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                       + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                       + L (normalizedreturns, -1)
                       + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                       + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                       + L (normalizedreturns, -6) + L (normalizedreturns, -7)
                       + L (volatility, -1) + L (deltamovingaverage, -1)
                       , data=  ESmatch.2second)

model.5second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                        + L (normalizedreturns, -1)
                        + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                        + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                        + L (normalizedreturns, -6) + L (normalizedreturns, -7)
                        + L (volatility, -1) + L (deltamovingaverage, -1)
                        , data=  ESmatch.5second)

model.10second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                        + L (normalizedreturns, -1)
                        + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                        + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                        + L (normalizedreturns, -6) + L (normalizedreturns, -7)
                        + L (volatility, -1) + L (deltamovingaverage, -1)
                        , data=  ESmatch.10second)

model.20second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                         + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                         + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                         + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                         + L (volume, -1) + L (volume, -2)
                         + tradinghour + lunchtime
                         + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                         + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                         + L (normalizedreturns, -1)
                         + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                         + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                         + L (normalizedreturns, -6) + L (normalizedreturns, -7)
                         + L (volatility, -1) + L (deltamovingaverage, -1)
                         , data=  ESmatch.20second)


model.30second <- dynlm (normalizedreturns ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                         + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                         + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                         + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                         + L (volume, -1) + L (volume, -2)
                         + tradinghour + lunchtime
                         + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                         + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                         + L (weighted_orderflow, -5) + L (weighted_orderflow, -6) 
                         + L (normalizedreturns, -1)
                         + L (normalizedreturns, -2) + L (normalizedreturns, -3)
                         + L (normalizedreturns, -4) + L (normalizedreturns, -5)
                         + L (normalizedreturns, -6)                     
                         + L (volatility, -1) + L (deltamovingaverage, -1)
                         , data=  ESmatch.30second)

model.minute <- dynlm (normalizedreturnsmed ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                        + L (normalizedreturnsmed, -1)
                        + L (normalizedreturnsmed, -2) + L (normalizedreturnsmed, -3)
                        + L (normalizedreturnsmed, -4) + L (normalizedreturnsmed, -5)                       
                        + L (volatility, -1) + L (deltamovingaverage, -1)
                        , data=  ESmatch.minute)

model.5minute <- dynlm (normalizedreturnsmed ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3) + L (weighted_orderflow, -4) 
                        + L (normalizedreturnsmed, -1)
                        + L (normalizedreturnsmed, -2) + L (normalizedreturnsmed, -3)
                        + L (normalizedreturnsmed, -4) + L (normalizedreturnsmed, -5)                       
                        + L (volatility, -1) + L (deltamovingaverage, -1)
                        , data=  ESmatch.5minute)

model.15minute <- dynlm (normalizedreturnsmed ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                        + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                        + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                        + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                        + L (volume, -1) + L (volume, -2)
                        + tradinghour + lunchtime
                        + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                        + L (weighted_orderflow, -3)
                        + L (normalizedreturnsmed, -1)
                        + L (normalizedreturnsmed, -2) + L (normalizedreturnsmed, -3)
                        + L (volatility, -1) + L (deltamovingaverage, -1)                                             
                        , data=  ESmatch.15minute)

model.30minute <- dynlm (normalizedreturnsmed ~ L(orderflow, -1) + L(weighted_orderflow, -1)
                         + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                         + L(BID, -2) + L(OFR, -2) + L (BIDSIZ, -2) + L (OFRSIZ, -2)
                         + L(BID, -3) + L(OFR, -3) + L (BIDSIZ, -3) + L (OFRSIZ, -3)
                         + L (volume, -1) + L (volume, -2)
                         + tradinghour + lunchtime
                         + L (weighted_orderflow, -1) + L (weighted_orderflow, -2)
                         + L (weighted_orderflow, -3)
                         + L (normalizedreturnsmed, -1)
                         + L (normalizedreturnsmed, -2) + L (normalizedreturnsmed, -3)
                         + L (deltamovingaverage, -1)
                         , data=  ESmatch.30minute)

summary(model.quartersecond.full)
summary(model.halfsecond.full)
summary(model.second.full)
summary(model.2second.full)
summary(model.5second.full)
summary(model.10second.full)
summary(model.20second.full)
summary(model.30second.full)
summary(model.minute)
summary(model.5minute)
summary(model.15minute)
summary(model.30minute)

# Simple, contemporary models, no lags

model.quartersecond.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.quartersecond)
model.halfsecond.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.halfsecond)
model.second.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.second)
model.2second.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.2second)
model.5second.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.5second)
model.10second.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.10second)
model.minute.nolag <- lm(normalizedreturns ~ orderflow, data= ESmatch.minute)

model.halfsecond.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.halfsecond)
model.2second.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.2second)
model.second.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.second)
model.5second.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.5second)
model.10second.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.10second)
model.20second.lag <- dynlm(normalizedreturns ~ L(orderflow, -1), data= ESmatch.20second)

model.halfsecond.lag1 <- dynlm(normalizedreturns ~ L(weighted_orderflow, -1), data= ESmatch.halfsecond)

model.second.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.second)
model.2second.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.2second)
model.5second.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.5second)
model.10second.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.10second)
model.20second.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.20second)
model.5minute.lag1 <- dynlm(normalizedreturns ~ L(averageorderflow, -1), data= ESmatch.5minute)




summary(model.quartersecond.nolag)
summary(model.halfsecond.nolag)
summary(model.second.nolag)
summary(model.2second.nolag)
summary(model.5second.nolag)
summary(model.10second.nolag)
summary(model.minute.nolag)

summary(model.halfsecond.lag)
summary(model.second.lag)
summary(model.2second.lag)
summary(model.5second.lag)
summary(model.10second.lag)
summary(model.20second.lag)
summary(model.5minute.lag)

summary(model.halfsecond.lag1)
summary(model.second.lag1)
summary(model.2second.lag1)
summary(model.5second.lag1)
summary(model.10second.lag1)
summary(model.20second.lag1)
summary(model.5minute.lag1)

# can we predict orderflow with lag returns and lags flows, then use it to predict price?

model.futureflow.2second <- dynlm(orderflow ~ L(averageorderflow, -1) + L(movingaverage, -1)
                                  + L (normalizedreturns, -1) + L (normalizedreturns, -2)
                                  + L (orderflow, -1) + L (orderflow, -2)
                                  + L (volatility, -1) + L (deltamovingaverage, -1)
                                  + L(weighted_orderflow, -1)
                                  + L(BID, -1) + L(OFR, -1) + L (BIDSIZ, -1) + L (OFRSIZ, -1)
                                  ,data= ESmatch.2second )

rm(list=ls())



# Test liquidity during different time of the day:


ESliquidity <- aggregate (x= ESmatch.second$volume, 
                        by= fastPOSIXct(trunc(index(ESmatch.second), units= "hours"),tz= "Chicago"), 
                        FUN= sum)
plot(liquidity)
rm(direction)
