## Place:   ECN395 Research - timesSeries project 
## Purpose: 1) Process the  ES_trades.csv and ES_quotes.csv into xts objects
##          2) Aggregate the Quotes and Trades data to make sure each timestamp is unique
##          3) Save the data as .R file in the same folders for future
## Produce: xts objects to be ready to work with highfrequency package. .R data files of ES trades & quotes

#install.packages("quantmod")
#install.packages("zoo")

require(biglm)
require(fasttime); require(xts); require(zoo); 
require(highfrequency); require(quantmod)

# Set the Sys.time variable to have 6 digits after seconds 
options("digits.secs"=6)
Sys.time()

# TRADE DATA --------------------------------------------------------------

# Read the raw trade data onto R:
ES_trades <- read.csv(file="/Users/cuongnguyen/Downloads/ES_Sample/ES_Trades.csv", header = TRUE)[2:5]

# Turn the Date column into a vector
ES_trades$Date <- as.vector(ES_trades$Date)

# Manipulate the string to change the format of the Date
ES_trades$Date <- paste('2013-09-', substr(ES_trades$Date,4,5), sep="") 

# Join the two column Date and Time into Join
ES_trades$Join <- do.call(paste, c(ES_trades[c("Date", "Time")], sep = " ")) # Really fast!

# Now convert to POSXITx using fastPOSIXct
ES_trades$Join <- fastPOSIXct(ES_trades$Join)

# Now create xts object! This is the most important part!
tds <- xts(ES_trades[c("Price", "Volume")], order.by=make.time.unique(ES_trades$Join))   

# Save the file. Surprisingly the file is so small! Don't know why?
save(ES_trades,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_Trades_raw.R")
save(tds,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_tds_xts_full.R")


# QUOTES DATA -------------------------------------------------------------

# Read the raw quotes data onto R:
ES_quotes <- read.csv(file="/Users/cuongnguyen/Downloads/ES_Sample/ES_Quotes.csv", header = TRUE)[2:7]

# Turn the Date column into a vector
ES_quotes$Date <- as.vector(ES_quotes$Date)

# Manipulate the string to change the format of the Date
ES_quotes$Date <- paste('2013-09-', substr(ES_quotes$Date,4,5), sep="") 

# Join the two column Date and Time into Join
ES_quotes$Join <- do.call(paste, c(ES_quotes[c("Date", "Time")], sep = " ")) # Really fast!

# Now convert to POSXITx using fastPOSIXct
ES_quotes$Join <- fastPOSIXct(ES_quotes$Join)
attr(ES_quotes$Join, "tzone") <- "Chicago"  #Change the timezone

# Now create xts object! This is the most important part!
qts <- xts(ES_quotes[c("Bid.Price", "Bid.Size","Ask.Price", "Ask.Size")], order.by=make.time.unique(ES_quotes$Join))   

# Save the file. Surprisingly the file is so small! Don't know why?
save(qts,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_qts_xts_full.R")
save(ES_quotes,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_Quotes_raw.R")


### TESTING the saved files:
# Load tds and qts:
load(file="/Users/cuongnguyen/Downloads/ES_Sample/ES_trades.R")
load(file="/Users/cuongnguyen/Downloads/ES_Sample/ES_quotes.R")

# Change time zone for a xts object
xts::indexTZ(tds) <- "Chicago"

# Aggregate the trade data so that 
tds_volume <- aggregate(tds$Volume, ES_trades$Join, FUN = sum)
tds_price <- aggregate(tds$Price, ES_trades$Join, FUN = median)

tdsagg <- merge(tds_volume, tds_price)
tdsagg <- as.xts(tdsagg)
indexTZ(tdsagg) <- "Chicago"  # 

tdsagg[1:10]

qtsagg <- aggregate(qts,  ES_quotes$Join, FUN = median) # This line of code takes more than 15 mins
qtsagg <- as.xts(qtsagg)
indexTZ(qtsagg) <- "Chicago"  # 


save(qtsagg,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_qts_aggregate.R")
save(tdsagg,file="/Users/cuongnguyen/Downloads/ES_Sample/ES_tds_aggregate.R")

# Now use matchquotetrade from high frequency
## NOTE: In matching the trade and quote data, using several level of lag adjustment 
## to account for lag between quotes and trades
## This could be a form of robustness test
tdsagg[1:10]
length(tdsagg$tds_volume)

matchTQ <- matchTradesQuotes(tdsagg[10000:200000], qtsagg[10000:200000], adjustment=0.001)
matchTQ <- merge(matchTQ, tdsagg$tds_volume[10000:200000])
matchTQ[500:680]

# Get the trade directions of the data using Lee and Ready algo
# 1 is buy and -1 is sell
direction <- getTradeDirection (matchTQ)

flow <- direction * matchTQ$tds_volume
matchTQ$spread <- (matchTQ$OFR - matchTQ$BID)

diff(c(-1,1,2,3))

matchTQ[1:10]
lag(matchTQ$BIDSIZ[1:10],-1)
diff(matchTQ$PRICE[1:10])


matchTQ[1:10]
matchTQ$price.change <- diff(matchTQ$PRICE)

mean(matchTQ$price.change,na.rm=TRUE)
table(matchTQ$price.change)

lag.flow <- lag(flow, -1)
lag.direction <- lag(direction, -1)


matchTQ <- merge (matchTQ, direction, flow) 
matchTQ$lag.flow <- lag.flow 
matchTQ$lag.direction <- lag.direction 



length(flow)
length(lag.flow)

matchTQ[1:2]
model1 <- biglm(PRICE ~ lag.flow, data= matchTQ)

x <- sample (c(-1,1), 50000, replace = TRUE)

mean(direction)

model2 <- lm(price.change ~ , data= matchTQ)

model3 <- lm(PRICE ~ x + lag(tds_volume, -1) , data= matchTQ)


summary(model2)
summary(model3)

model2
ES_trades[1:10,]

table(diff(tdsagg$tds_price))
table(tdsagg$tds_price)

tdsagg[1:10]

