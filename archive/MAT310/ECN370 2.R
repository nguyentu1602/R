opec <- read.csv("/Users/cuongnguyen/Dropbox/00.F13/ECN370/CleanData.csv", header=TRUE) 
attach(opec)
opec.ols <- lm(Oil.price ~ OPEC.production + OPEC.Reserves + World.production + VIX + BDI + Dev.China.consumption + Seasonal + Oil.import + WorldGDP)
opec.ols3 <- lm(Oil.price ~ OPEC.production)

opec.ols1 <- lm(Oil.price ~ World.production + VIX + BDI + Dev.China.consumption + Seasonal)
opec.ols2 <- lm(Oil.price ~ World.production + VIX + BDI + Dev.China.consumption + Seasonal + Oil.import + WorldGDP)
#Need to do best subset here to pick the best models
#Need to compare the influence among time
