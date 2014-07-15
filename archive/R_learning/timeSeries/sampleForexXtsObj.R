# Representing time series data:
install.packages("xts"); 
install.packages("highfrequency")
install.packages("fasttime")
require(fasttime)
require(xts);require(zoo); 
require(highfrequency)
?convert
??lubridate
# Getting the vignette for the insterested package:
vignette("zoo-faq", package = "zoo")
vignette( package = "zoo")

# read the data into a data frame:
quotes <- read.csv(file="/Users/cuongnguyen/Downloads/SampleForexData/Quotes/AUDUSD.csv", header=FALSE)[, 1:4]
trades <- read.csv(file="/Users/cuongnguyen/Downloads/SampleForexData/Trades/AUDUSD.csv", header=FALSE)[, 1:4]
colnames(quotes) <- c('date','time','bid','ask')
colnames(trades) <- c('date','time','price')

# pick only one day of trade
#quotes0630 <- quotes[quotes$date == '06/30/2013', ]
#trades0630 <- trades[trades$date == '06/30/2013', ]


# Change system option to print more digit of time stamp
Sys.time()
options("digits.secs"=6) 


strptime(paste(quotes$date, quotes$time)[1], "%m/%d/%Y %H:%M:%OS")


# Create xts objects:
# Convert the first two column into time series:
timestamp1 <- as.POSIXct(strptime(paste(quotes$date, quotes$time), "%m/%d/%Y %H:%M:%OS"))

timestamp1[1]
timestamp2 <- as.POSIXct(strptime(paste(trades$date, trades$time), "%m/%d/%Y %H:%M:%OS"))
timestamp2[1]


# make the duplicate time stamp unique
qts <- xts(quotes[, c("bid" ,"ask")], order.by=make.time.unique(timestamp1))   

tds <- xts(trades$price, order.by=make.time.unique(timestamp2))   

str(tds)
qts[1:10]
tds[1:10]


diff(log(tds))

vignette("xts",package="xts")
data(sample_matrix)
class(sample_matrix)
str(sample_matrix)
head(sample_matrix)
dim(sample_matrix)
matrix_xts <- as.xts(sample_matrix,dateFormat='Date')
str(matrix_xts)
df_xts <- as.xts(as.data.frame(sample_matrix),  important='very important info!')
str(df_xts)
methods(class="xts")

class(qts)
qts0630 <- qts['2013-07-01'] 
qts0701 <- last(qts, '2 ')
head(qts0701)

# PERIODICITY:
axTicksByTime(qts, ticks.on='minutes')
plot(qts,major.ticks='hours',minor.ticks=FALSE,main=NULL,col=3)

periodicity(qts)
# Find endpoints by time:
endpoints(qts,on='hours')

# Change periodicity: Problem: only work with OHLC data
head(qts)
foo <- to.minutes(qts)
head(foo)
foo