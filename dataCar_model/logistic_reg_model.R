## dataCar Logistic Regression model creation and evaluation 
## Cody Clayton
## Sept 11, 2017


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

testCar <- car %>% filter(random <= 0.2)
trainCar <- car %>% filter(random > 0.2)

## build model formula
y <- "clm"
x <- c( "veh_value_log10",
  "exposure",
  "veh_body",
  "agecat")
fmla <- paste(y, paste(x, collapse="+"), sep="~")
print(fmla)

## build logistic regression model
model <- glm(fmla, data=trainCar, family=binomial(link="logit"))

## predict clm for trainCar & testCar datasets 
trainCar$pred <- predict(model, newdata=trainCar, type="response")
testCar$pred <- predict(model, newdata=testCar, type="response")

##plot predictions 
plot(trainCar$pred)

ggplot(trainCar, aes(x=pred, color=clm, linetype=clm)) +
  geom_density()

ggplot(testCar, aes(x=pred, y=..scaled..,color=clm, linetype=clm)) +
  geom_density() + 
  labs(title='Probability Density by Claim Submission',x='Probability', y='Density', color='Claim Submitted',  linetype='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5))

## selecting a probability threshold 

## trainCar

## ROCR prediction
trainCarPrediction <- prediction(trainCar$pred, trainCar$clm)

## ROCR Precision & Recall 
trainCarPreciscion <- performance(trainCarPrediction, measure="prec")
precision <- (trainCarPreciscion@y.values)[[1]]

trainCarRecall <- performance(trainCarPrediction, measure="rec")
recall <- (trainCarRecall@y.values)[[1]]

## threshold (same in both trainCarPrecision and trainCarRecall) 
threshold <- (trainCarPreciscion@x.values)[[1]]

## build precision / recall dataframe
rocFrame <- data.frame(threshold=threshold, precision=precision,
                       recall=recall)

nplot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)} 
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}


## clm percent in trainCar
clmRate <- mean(as.numeric(trainCar$clm))

##ratio of classifier precision to the average rate of positives (how much better than average)
## precision over clm percent vs threshold  
p1 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=precision/clmRate)) +
  coord_cartesian(xlim = c(0,0.25), ylim=c(0,6) ) +
  labs(title='Claim Density By Exposure',x='Probability Threshold', y='Enrichment Rate') +
  theme(plot.title = element_text(hjust = 0.5))

## recall vs threshold 
p2 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=recall)) +
  coord_cartesian(xlim = c(0,0.25) ) +
  labs(x='Probability Threshold', y='Recall') +
  theme(plot.title = element_text(hjust = 0.5))

nplot(list(p1, p2))

## precision 
p3 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=precision)) +
  coord_cartesian(xlim = c(0,0.25), ylim=c(0,.5) )

nplot(list(p3, p2))


## confusion matrix for specific threshold 
confMat <- table(pred=trainCar$pred>0.10, clm=trainCar$clm)
confMat

confPrec <- confMat[2,2]/sum(confMat[2,])
confPrec

confRecall <- confMat[2,2]/sum(confMat[,2])
confRecall

confPrecRatio <- confPrec/mean(as.numeric(trainCar$clm))
confPrecRatio

## model coefficients 
coefficients(model)

## model 
summary(model)

## computing deviance
loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y)*log(1 - py))
}

## null deviance 
trainNullDev = model$null.deviance

## model deviance 
trainResidualDev = model$deviance

##clm percent in testCar
clmTest = as.numeric(testCar$clm)
clmRateTest <- mean(as.numeric(testCar$clm))

##testCar null deviance
testNullDev <- -2*loglikelihood(clmTest, clmRateTest)

##testCar residual deviance
testResidualDev <- -2*loglikelihood(clmTest, testCar$pred)

clmRateTest
testNullDev
testResidualDev

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

testPr2 <- 1-(testResidualDev/testNullDev)
testPr2

## model predicts about 4% of the overall varaince 


aic <- 2*(length(model$coefficients) - loglikelihood(as.numeric(trainCar$clm), trainCarPrediction))

