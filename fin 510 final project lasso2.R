library(dplyr) 
library(leaps)
library(glmnet)
options("scipen"=100)
Hdf= read.csv("historic_property_data.csv")
Hdf1=na.omit(Hdf[,c(1,9,10,14,15,16,18,19,22,23,24,29,34,35,36,37,
            44,45,46,47,50,59,62)])

set.seed(1) 
y<-Hdf1$sale_price
x=model.matrix(sale_price~.,Hdf1)[,-1]
train.index <- sample(c(1:dim(x)[1]), dim(x)[1]*0.5) 
test.index <- (-train.index)
head(y[train.index])
y.test <- y[test.index]
#Lasso Regression: https://www.statology.org/lasso-regression-in-r/#:~:text=Lasso%20Regression%20in%20R%20%28Step-by-Step%29%20Lasso%20regression%20is,that%20minimize%20the%20sum%20of%20squared%20residuals%20%28RSS%29%3A
cv_model <- cv.glmnet(x[train.index,], y[train.index], alpha = 1,type.measure="mse",nfolds = 5)
plot(cv_model)
best_lambda <- cv_model$lambda.min
best_lambda
best_model <- glmnet(x[train.index,], y[train.index], alpha = 1, lambda = best_lambda)
coef(best_model)
y_predicted <- predict(best_model, s = best_lambda, newx=x[test.index,])
summary(y_predicted)
summary(y)
mean((y.test-y_predicted)^2)
