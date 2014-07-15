# This file contains all regressions for my paper
install.packages("estout")
install.packages("stargazer")

require(estout); 
require(stargazer)

# Regression for the ES market only ---------------------------------------

ESmodel.full <- list (length=10)
ESmodel.uni <- list (length=10)
ESmodel.full.summary <- list (length=10)
ESmodel.uni.summary <- list (length=10)

ESdatasets <- list (ESmatch.quartersecond, ESmatch.halfsecond, ESmatch.second, ESmatch.2second,
                    ESmatch.5second, ESmatch.10second, ESmatch.20second, ESmatch.30second,
                    ESmatch.minute, ESmatch.5minute)


# Fit full models
for (i in 1:10) {
  # fit all big models
  ESmodel.full[[i]] <- dynlm (normalizedreturns ~ orderflow
                              + L (deltamovingaverage, 1)  
                              + volatility
                              + volume
                              + factor(hour) + factor(wday),
                              data= ESdatasets[[i]] )
  
  # fit all univariate models
  ESmodel.uni[[i]] <- lm (normalizedreturns ~ orderflow,
                             data= ESdatasets[[i]] )
  
}

# create the summaries:
for (i in 1:10) {
  ESmodel.uni.summary[[i]] <- summary (ESmodel.uni[[i]])
  ESmodel.full.summary[[i]] <- summary (ESmodel.full[[i]])
}


rm(ESmodel.full)
rm(ESmodel.uni)

ESmodel.full.summary[[10]]
ESmodel.reduce.summary[[1]]

