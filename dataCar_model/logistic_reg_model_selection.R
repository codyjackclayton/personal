## dataCar Logistic Regression model selection
## Cody Clayton
## Sept 12, 2017

library(dplyr)
library(ggplot2)
library(ROCR)
library(grid)

car = read.csv('dataCar_clean.csv',header=TRUE)

set.seed(42)

## convert clm to Logical
car$clm <- car$clm == 1

## generate test & train data sets

car$random <- runif(dim(car)[1])

testCar <- car %>% filter(random <= 0.1)
trainCar <- car %>% filter(random > 0.1)

## variable selection 
y <- "clm"
x <- c( "veh_value_log10",
        "exposure",
        "veh_body",
##        "veh_age",
##        "gender",
##        "area",
        "agecat")
fmla <- paste(y, paste(x, collapse="+"), sep="~")
print(fmla)

## build logistic regression model
model <- glm(fmla, data=trainCar, family=binomial(link="logit"))

## predict clm for trainCar & testCar datasets 
trainCar$pred <- predict(model, newdata=trainCar, type="response")
testCar$pred <- predict(model, newdata=testCar, type="response")


summary(model)

## null deviance 
trainNullDev = model$null.deviance

## model deviance 
trainResidualDev = model$deviance


## chi-squared test 

degFreedomNull <- dim(trainCar)[[1]] - 1
degFreedomModel <- dim(trainCar)[[1]] -length(model$coefficients)

degFreedomNull
degFreedomModel

devDiff <- trainNullDev - trainResidualDev
degFreeDiff <- degFreedomNull - degFreedomModel
pVal <- pchisq(devDiff, degFreeDiff, lower.tail=F)

pVal

## Calculating the pseudo R-squared

trainPr2 <- 1-(trainResidualDev/trainNullDev)
trainPr2