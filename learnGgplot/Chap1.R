install.packages("ggplot2")
require(ggplot2)
install.packages("mgcv")
require(mgcv)
require(MASS)
require(splines)


head(diamonds)
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
head(dsmall)
is.data.frame(dsmall)

# Chapter 2 ---------------------------------------------------------------

# First function: qplot()
qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z , data = diamonds)
qplot(carat, price , data = dsmall, color = color)
qplot(carat, price , data = dsmall, shape = cut, color = color, size = I(4))
qplot(carat, price, data = diamonds, alpha = I(5/100))
      
# Play with Geom : point, smooth, boxplot, path, line, histogram, freqpoly, density, bar
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'))
qplot(carat, price , data = diamonds, geom = c('point', 'smooth'))
  # Use span to wiggle:
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (0.2))
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1))
  # Set method= to change the fitted line. "gam" = generalized additive model, "rlm" = robust linear model
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "gam")
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "gam", formula = y ~ s(x))
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "gam", formula = y ~ s(x, bs = "cs"))
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "lm")
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "lm", formula = y ~ ns(x,5))
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "rlm", formula = y ~ ns(x,5))
qplot(carat, price , data = dsmall, geom = c('point', 'smooth'), span = (1), method = "rlm", formula = y ~ ns(x,5))

  # Jitter and box plot geom
qplot(color,price/carat , data = diamonds, geom = "jitter", alpha = I(1/20))
qplot(color,price/carat , data = diamonds, geom = "boxplot", alpha = I(1/20))

# Histogram and density plot: VERY IMPORTANT to try varies bin width
qplot(carat, data = diamonds, geom = "histogram", fill = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color, binwidth = 1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", fill = color, binwidth = 0.1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", fill = color, binwidth = 0.01, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "density", colour = color)

# Barcharts  
qplot(color, data = diamonds, geom = "bar")
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(color, data = diamonds, geom = "bar", weight = carat)

# Line - time series
qplot(date, unemploy / pop , data = economics, geom = "line")
qplot(date, uempmed , data = economics, geom = "line")

# Path plot:




# Chapter 3 ---------------------------------------------------------------


