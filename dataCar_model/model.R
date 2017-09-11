## dataCar model creation and evaluation 
## Cody Clayton
## Sept 9, 2017


library(dplyr)
library(ROCR)
library(rpart)
library(class)

car = read.csv('dataCar_clean.csv',header=TRUE)

set.seed(42)

## generate test & train data sets
car$clm = as.factor(car$clm)

car$random <- runif(dim(car)[1])

testCar = car %>% filter(random <= 0.1)
trainCar = car %>% filter(random > 0.1)

## single variable models

single_var2 = function(var_col,out_col){
  varTable <- table(
    var= var_col,
    out= out_col)
  ret = (varTable[,2]/(varTable[,1]+varTable[,2]))
  return(ret)
}

make_pred = function(modelTable, test_col){
  ret = modelTable[test_col]
}

calc_auc = function(pred_col, out_col){
  eval <- prediction(pred_col,out_col)
  plot(performance(eval,"tpr","fpr"))
  ret = attributes(performance(eval,'auc'))$y.values[[1]]
}


## Creates NAs (NEED TO FIX)
exposure_cut = as.numeric(quantile(trainCar$exposure, probs=seq(0, 1, 0.1),na.rm=T))
trainCar = trainCar %>% mutate(exposure_cut = cut(trainCar$exposure,exposure_cut))
testCar = testCar %>% mutate(exposure_cut = cut(testCar$exposure,exposure_cut))

veh_value_cut = as.numeric(quantile(trainCar$veh_value, probs=seq(0, 1, 0.1),na.rm=T))
trainCar = trainCar %>% mutate(veh_value_cut = cut(trainCar$veh_value,veh_value_cut))
testCar = testCar %>% mutate(veh_value_cut = cut(testCar$veh_value,veh_value_cut))

head(trainCar$veh_value_cut)

catVars = c('gender', 'veh_body', 'veh_age', 'area', 'agecat', 'exposure_cut', 'veh_value_cut')

for (var in catVars) { 
  varTable = single_var2(trainCar[[var]], trainCar$clm)
  trainPred = make_pred(varTable, trainCar[[var]])
  trainAuc = calc_auc(trainPred,trainCar$'clm')
  testPred = make_pred(varTable, testCar[[var]])
  testAuc = calc_auc(testPred,testCar$'clm')
  cat('var:', var, '    train AUC:', trainAuc,'    test AUC:', testAuc, '\n')
}

ggplot(data=trainCar) + geom_density(aes(x=exposure,color=as.factor(clm)))



## simple decision tree


tmodel <- rpart(clm != 0 ~ gender + veh_body + veh_age + area + agecat + exposure_cut + veh_value_cut ,data=trainCar)
print(tmodel)
print(calc_auc(predict(tmodel,newdata=trainCar),trainCar$clm))
print(calc_auc(predict(tmodel,newdata=testCar),testCar$clm))

par(cex=0.7)
plot(tmodel)
text(tmodel)

## simple knn model

knnVars = c('veh_age', 'agecat', 'veh_value', 'exposure', 'gender', 'veh_body', 'area')
knnTrain = trainCar[,knnVars]
knnTrain$gender = as.numeric(knnTrain$gender)
knnTrain$veh_body = as.numeric(knnTrain$veh_body)
knnTrain$area = as.numeric(knnTrain$area)
knnCl <- trainCar[,'clm'] == 1
nk = 200

knnPred <- function(df) {
  knnDecision <- knn(knnTrain,df,knnCl,k=200,prob=T)
  ifelse(knnDecision==TRUE,
         attributes(knnDecision)$prob,
         1-(attributes(knnDecision)$prob))
}

knnPreds = knnPred(knnTrain)

print(calc_auc(knnPred(knnTrain),trainCar[,'clm']))


resultframe <- data.frame(clm=trainCar$clm, pred=knnPred(knnTrain))
plot(resultframe$pred)
## confusion matrix
conMat = table(truth=resultframe$clm,prediction=resultframe$pred>=0.15)
conMat

## accuracy
sum(diag(conMat))/sum(conMat)
(conMat[1,1]+conMat[2,2])/sum(conMat)

##precision 
conMat[2,2]/(conMat[2,2]+conMat[1,2])

##recall
conMat[2,2]/ (conMat[2,2]+conMat[2,1])


## simple glm model 
glmModel <- glm(clm ~ veh_value + exposure + veh_body + veh_age + gender + area + agecat,family=binomial(link='logit'),trainCar)

glmtrainPred <- predict(glmModel,newdata=trainCar,
                          type='response')
glmtestPred <- predict(glmModel,newdata=testCar,
                         type='response') 
plot(glmtrainPred)

## confusion matrix
conMat = table(truth=trainCar$clm,prediction=glmtrainPred>0.15)
conMat

## accuracy
sum(diag(conMat))/sum(conMat)
(conMat[1,1]+conMat[2,2])/sum(conMat)

##precision 
conMat[2,2]/(conMat[2,2]+conMat[1,2])

##recall
conMat[2,2]/ (conMat[2,2]+conMat[2,1])

## double density plot
ggplot(data=testCar) + geom_density(aes(x=pred,color=clm,linetype=clm))

## ROC curve
eval <- prediction(testCar$pred,testCar$clm)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])



## null model 
## check percent that default in data set 
sum(dataCar3$clm == 1) / nrow(dataCar3)

## 6.810908% -- model must be better than this (always predicting no claim have 93.18909% accuracy)