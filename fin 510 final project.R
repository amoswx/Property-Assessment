library(glmnet)
library(dplyr)
library(caret)

# load the data
historic<-read.csv("historic_property_data.csv")
preddf=read.csv("predict_property_data.csv")
cdf=read.csv("codebook.csv")
# select variables for regression and omit NA
clean<-na.omit(historic[c(-7,-8,-11,-25,-26,-38,-39,-40,-41,-50,-57,-58)])
clean<-clean[c(-33,-42)]
clean2<-(preddf[c(-7,-8,-11,-25,-26,-38,-39,-40,-41,-50,-57,-58)])
clean2<-clean2[c(-33,-42)]
clean2[is.na(clean2)] = 0
x2=data.matrix(clean2)[,-1]

# convert a data frame of predictors to a matrix and create dummy variables for character variables 
x <- model.matrix(sale_price~.,clean)[,-1]
y<-clean$sale_price

# data partition 
set.seed(1)
train.index <- sample(c(1:dim(x)[1]), dim(x)[1]*0.6)
test.index <- (-train.index)
y.test <- y[test.index]

# fit a lasso regression model
cv.fit <- cv.glmnet(x[train.index,],y[train.index],alpha=1, type.measure="mse",nfold=5)
plot(cv.fit)
lambda.best <- cv.fit$lambda.min

# lasso regression coefficients
coef.lambda.best <- predict(cv.fit,s=lambda.best,type="coefficients")[1:49,]
coef.lambda.best[coef.lambda.best!=0]

# make predictions for records in the test set
pred.lambda.best <- predict(cv.fit,s=lambda.best,newx=x[test.index,])
pred.lambda.best[pred.lambda.best<=1000]=1000
summary(pred.lambda.best)
mean((y.test-pred.lambda.best)^2)


# make predictions for records in the predict set
pred.lambda.best <- predict(cv.fit,s=lambda.best,newx=x2)
pred.lambda.best[pred.lambda.best<=1000]=1000
summary(pred.lambda.best)
pred=as.data.frame(matrix(nrow=10000,ncol=2))
names(pred)=c("pid","sale_price")
pred$pid=1:10000
pred$sale_price=pred.lambda.best
write.table(pred,"assessment_file.csv",sep=",")



