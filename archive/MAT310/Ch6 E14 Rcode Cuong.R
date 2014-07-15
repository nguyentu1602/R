#Randomly draw a vector of 10000 observations from a hypergeometric distribution
ranvec <- rhyper(10000, 13, 24, 21)

#Write a function to calculate the chisq statistics
chisq.stat <- function(obs) {
(X.sq = (13 - (21 - obs) - 5.62)^2/5.62 + (24 - obs - 10.38)^2/10.38 + (21 - obs - 7.38)^2/7.38 + (obs - 13.62)^2/13.62    )  
return (X.sq)}

#Calculate the chisq statistics for 10000 obs
chisq.vec <- chisq.stat(ranvec)

#Draw a histogram with specified breaks
hist(chisq.vec, breaks = seq(0, 90,by = 1))