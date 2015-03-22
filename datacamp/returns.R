# Author:   Cuong Nguyen
# Date:     18072014
# Purpose:  1st problem, Zivot datacamp
# Deps:     

# Assign the url to the csv file
data_url = "http://faculty.washington.edu/ezivot/econ424/sbuxPrices.csv"
# Load the data frame using read.csv
sbux_df = read.csv (data_url, header=TRUE, stringsAsFactors=FALSE)   
# sbux_df should be a data frame object. Data frames are rectangular data objects typically with
# observations in rows and variables in columns
sbux_df[1,1]

str(sbux_df)
head(sbux_df)
tail(sbux_df)
class(sbux_df)
class(sbux_df$Date)

# Drop = false preserves the dimesion, so that closing price is still a matrix
closing_prices = sbux_df[,'Adj.Close', drop=FALSE]

dim (closing_prices)

# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 = which(sbux_df$Date == "3/1/1994")
index_2 = which(sbux_df$Date == "3/1/1995")
# Extract prices between 3/1/1994 and 3/1/1995
some_prices = sbux_df[index_1:index_2, 2]

# Create a new data frame containing the price data with the dates as the row names
sbux_prices_df = sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) = sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.
price_1 = sbux_prices_df['3/1/1994', ]
price_2 = sbux_prices_df['3/1/1995', ]

# Now add all relevant arguments to the plot function below to get a nicer
plot(sbux_df$Adj.Close, type="l", col= "blue", lwd= 2, 
     ylab="Adjusted close", main= "Monthly closing price of SBUX")

sbux_prices_df = sbux_df[, "Adj.Close", drop = FALSE]

# Denote n the number of time periods:
n = nrow(sbux_prices_df)
sbux_ret = ((sbux_prices_df[2:n, 1] - sbux_prices_df[1:(n - 1), 1])/sbux_prices_df[1:(n -1), 1])

# Notice that sbux_ret is not a data frame object
class(sbux_ret)

