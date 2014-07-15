## Place:   ECN395 Research - timesSeries project 
## Purpose: 1) Process the  TY_trades.csv and TY_quotes.csv into xts objects
##          2) Aggregate the Quotes and Trades data to make sure each timestamp is unique
##          3) Save the data as .R file in the same folders for future
## Produce: xts objects to be ready to work with highfrequency package. .R data files of TY trades & quotes

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
TYtds.full <- read.csv(file="/Users/cuongnguyen/Downloads/TY_Sample/TY_Trades.csv", header = TRUE)[2:5]
TYtds.full[1:10,]
# Turn the Date column into a vector
TYtds.full$Date <- as.vector(TYtds.full$Date)

# Manipulate the string to change the format of the Date
TYtds.full$Date <- paste ('2013-', substr(TYtds.full$Date,1,2), '-', substr(TYtds.full$Date,4,5), sep="") 

# Join the two column Date and Time into Join
TYtds.full$Join <- do.call(paste, c(TYtds.full[c("Date", "Time")], sep = " ")) # Really fast!

# Now convert to POSXITx using fastPOSIXct
TYtds.full$Join <- fastPOSIXct(TYtds.full$Join)

# Now create xts object! This is the most important part!
TYtds.full <- xts(TYtds.full[c("Price", "Volume")], order.by=make.time.unique(TYtds.full$Join))   

# Save the file. Surprisingly the file is so small! Don't know why?
# save(TYtds.full.raw,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_Trades_raw.R")
save(TYtds.full,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_tds_xts_full.R")


# QUOTES DATA -------------------------------------------------------------

# Read the raw quotes data onto R:
TYqts.full <- read.csv(file="/Users/cuongnguyen/Downloads/TY_Sample/TY_Quotes.csv", header = TRUE)[3:7]
TYqts.full$Date <- read.csv(file="/Users/cuongnguyen/Downloads/TY_Sample/TY_Quotes.csv", header = TRUE)[2]

TYqts.full[1:10,]
# Turn the Date column into a vector
TYqts.full$Date <- as.vector(TYqts.full$Date)

# Manipulate the string to change the format of the Date
TYqts.full$Date <- paste('2013-', substr(TYqts.full$Date, 1, 2), '-', substr(TYqts.full$Date,4,5), sep="") 

# Join the two column Date and Time into Join
TYqts.full$Join <- do.call(paste, c(TYqts.full[c("Date", "Time")], sep = " ")) # Really fast!

# Now convert to POSXITx using fastPOSIXct
TYqts.full$Join <- fastPOSIXct(TYqts.full$Join)
attr(TYqts.full$Join, "tzone") <- "Chicago"  #Change the timezone

# Now create xts object! This is the most important part!
TYqts.full <- xts(TYqts.full[c("Bid.Price", "Bid.Size","Ask.Price", "Ask.Size")], order.by=make.time.unique(TYqts.full$Join))   

# Save the file. Surprisingly the file is so small! Don't know why?
# save(TYqts.full,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_Quotes_raw.R")
save(TYqts.full,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_qts_xts_full.R")

### TESTING the saved files:
# Load tds and qts:
load(file="/Users/cuongnguyen/Downloads/TY_Sample/TY_qts_xts_full.R")
load(file="/Users/cuongnguyen/Downloads/TY_Sample/TY_tds_xts_full.R")

# Change time zone for a xts object
xts::indexTZ(TYqts.full) <- "Chicago"

# Aggregate the trade data so that 
tds_volume <- aggregate(TYqts.full$Volume, TYqts.full$Join, FUN = sum)
tds_price <- aggregate(TYqts.full$Price, TYqts.full$Join, FUN = median)

TYtdsagg <- merge(tds_volume, tds_price)
TYtdsagg <- as.xts(TYtdsagg)
indexTZ(TYtdsagg) <- "Chicago"  # 

TYtdsagg[1:10]

TYqtsagg <- aggregate(TY_qts,  TY_quotes$Join, FUN = median) # This line of code takes more than 15 mins
TYqtsagg <- as.xts(TYqtsagg)
indexTZ(qtsagg) <- "Chicago"  # 


save(qtsagg,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_qts_aggregate.R")
save(tdsagg,file="/Users/cuongnguyen/Downloads/TY_Sample/TY_tds_aggregate.R")

# Now use matchquotetrade from high frequency
## NOTE: In matching the trade and quote data, using several level of lag adjustment 
## to account for lag between quotes and trades
## This could be a form of robustness test
tdsagg[1:10]
length(tdsagg$tds_volume)

matchTQ <- matchTradesQuotes(tdsagg[10000:200000], qtsagg[10000:200000], adjustment=0.001)
matchTQ <- merge(matchTQ, tdsagg$tds_volume[10000:200000])
matchTQ[500:680]


