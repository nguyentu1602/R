gdp <- read.csv("/Users/cuongnguyen/Desktop/book2.csv", header=TRUE) 
length(gdp)
vec <- rep(0,93*3)

for (i in 1:94) {
  vec[i*3] = gdp[i,1]/3
  vec[i*3-1] = gdp[i,1]/3
  vec[i*3-2] = gdp[i,1]/3
}

write.csv(vec, "/Users/cuongnguyen/Desktop/book3.csv")