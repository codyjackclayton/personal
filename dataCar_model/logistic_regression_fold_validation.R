## dataCar Logistic Regression model creation and evaluation 
## Cody Clayton
## Sept 11, 2017


library(dplyr)
library(ggplot2)
library(ROCR)
library(grid)

car = read.csv('dataCar_clean.csv',header=TRUE)

set.seed(42)

## computing deviance
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y)*log(1 - py))
}

## horizontal plot 
hor_plot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,n)))
  vplayout=function(x,y) {viewport(layout.pos.col=x, layout.pos.row=y)} 
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}



## convert clm to Logical
car$clm <- car$clm == 1

## generate 10 80/20 splits for the data & pseudo r-squard vector

index <- replicate(n = 100,expr = {sample(67803,54242)})
testPr2 = vector()
chiPval = vector()
allAic = vector()

for (rep in 1:100){ 
trainCar = car[index[,rep],]
testCar = car[-index[,rep],]


## build model formula
y <- "clm"
x <- c( "veh_value_log10",
        "exposure",
        "veh_body",
        "agecat")
fmla <- paste(y, paste(x, collapse="+"), sep="~")

## build logistic regression model
model <- glm(fmla, data=trainCar, family=binomial(link="logit"))

## predict clm for testCar and trainCar datasets 
testCar$pred <- predict(model, newdata=testCar, type="response")
trainCar$pred <- predict(model, newdata=trainCar, type="response")

##clm percent in testCar
clmTest = as.numeric(testCar$clm)
clmRateTest <- mean(as.numeric(testCar$clm))

## null deviance 
trainNullDev = model$null.deviance

## model deviance 
trainResidualDev = model$deviance

##testCar null deviance
testNullDev <- -2*loglikelihood(clmTest, clmRateTest)

##testCar residual deviance
testResidualDev <- -2*loglikelihood(clmTest, testCar$pred)


## calculate desgrees of freedom
degFreedomNull <- dim(trainCar)[[1]] - 1
degFreedomModel <- dim(trainCar)[[1]] -length(model$coefficients)

devDiff <- trainNullDev - trainResidualDev
degFreeDiff <- degFreedomNull - degFreedomModel

## chi squared test 

pVal = pchisq(devDiff, degFreeDiff, lower.tail=F)
chiPval = c(chiPval,pVal)

## calc the pseudo R-squared

pr2 <- 1-(testResidualDev/testNullDev)

testPr2 = c(testPr2,pr2)

## calc aic


aic = 2*(length(model$coefficients) - loglikelihood(as.numeric(trainCar$clm), trainCar$pred))
allAic = c(allAic,aic)
}

p1 = qplot(testPr2, geom="histogram", bins = 10, fill='indianred') + 
  labs(title='      ' ,x='pseudo R-squared', y='Number of Occurances') +
  scale_x_continuous(labels = percent) + 
  guides(fill=FALSE)

p2 = qplot(allAic, geom="histogram", bins = 10, fill='indianred') + 
  labs(title = 'Quality Metrics for 100 models',x='Akaike Information Criterion', y='Number of Occurances') +
  scale_x_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = +7.5)) + 
  guides(fill=FALSE)

hor_plot(list(p1,p2))


