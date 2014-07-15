db = read.table("http://freakonometrics.free.fr/db.txt",header=TRUE,sep=";")
attach(db)
head(db)
factor(X3)
levels(X3)
# Get a model
X3bis=rep(NA,length(X3))
X3bis[X3%in%c("A","C","D")]="ACD"
X3bis[X3%in%c("B","E")]="BE"

db$X3bis=as.factor(X3bis)
detach(db)
threshold = 0.5

# Create a logistic regression:
reg=glm(Y~X1+X2+X3bis,family=binomial,data=db)

# Create a predicted probability vector
S = predict(reg,type="response")

summary (reg)



# Create a ROC curve function, that take a threshold and return a table 
roc.curve = function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
  if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
} # ROC curve

# Test ROC curve, TPR is also call sensitivity
roc.curve(threshold)
roc.curve(threshold, print= TRUE)

### Big trick: vectorize a function that take a value to take a whole vector
ROC.curve=Vectorize(roc.curve)
ROC.curve

## Now plotting
I=(((S>threshold)&(Y==0))|((S<=threshold)&(Y==1)))

plot(S,Y,col=c("red","blue")[I+1],pch=19,cex=.7,,xlab="",ylab="")

abline(v=,col="gray")
attach(mat)

M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l", main="LOGIT")



## Now into my data:

mat <- read.csv(file="/Users/cuongnguyen/Program/Git/mat499/fullData39Lag.csv", header = TRUE)
head(mat)
attach(mat)
# logitic model
logit1 <- glm(mat$Y ~ ., family=binomial, data=mat)
summary(logit1)

# ridge-regression model
??lm.ridge
require(MASS); require(ridge); require(glmnet);
install.packages("ridge")
install.packages("glmnet")



ridge1 <- lm.ridge(formula=mat$Y ~ ., data=mat,lambda=1 )
ridge2 <- lm.ridge(formula=mat$Y ~ ., data=mat,lambda=2 )
ridge1 <- linearRidge(formula=mat$Y ~ ., data=mat, lambda=1)
ridge2 <- linearRidge(formula=mat$Y ~ ., data=mat, lambda=25)

ridge200 <- linearRidge(formula=mat$Y ~ ., data=mat, lambda=200)
ridge200l <- logisticRidge(formula=mat$Y ~ ., data=mat, lambda="automatic")


S = predict(logit1,type="response")
S = predict(ridge1,type="response")
S = predict(ridge200,type="response")
S = predict(ridge200,type="response")

sum(predict(logit1,type="response") - predict(ridge200,type="response"))


head(S)
length(S)
length(Y)
summary(ridge1)
ridge1




# Create a ROC curve function, that take a threshold and return a table 
roc.curve = function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Y==0))/sum(Y==0)
  TP=sum((Ps==1)*(Y==1))/sum(Y==1)
  if(print==TRUE){
    print(table(Observed=Y,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
} # ROC curve

### Big trick: vectorize a function that take a value to take a whole vector
ROC.curve=Vectorize(roc.curve)

M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l", main="Ridge")
