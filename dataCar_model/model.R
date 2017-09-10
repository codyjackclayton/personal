## dataCar model creation and evaluation 
## Cody Clayton
## Sept 9, 2017


library(dplyr)
library(ROCR)

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

exposure_cut = as.numeric(quantile(trainCar$exposure, probs=seq(0, 1, 0.1),na.rm=T))
trainCar = trainCar %>% mutate(exposure_cut = cut(trainCar$exposure,exposure_cut))
testCar = testCar %>% mutate(exposure_cut = cut(testCar$exposure,exposure_cut))

cat_vars = c('gender', 'veh_body', 'veh_age', 'area', 'agecat', 'exposure_cut')

for (var in cat_vars) { 
  varTable = single_var2(trainCar[[var]], trainCar$clm)
  trainPred = make_pred(varTable, trainCar[[var]])
  trainAuc = calc_auc(trainPred,trainCar$'clm')
  testPred = make_pred(varTable, testCar[[var]])
  testAuc = calc_auc(testPred,testCar$'clm')
  cat('var:', var, '    train AUC:', trainAuc,'    test AUC:', testAuc, '\n')
}

ggplot(data=trainCar) + geom_density(aes(x=exposure,color=as.factor(clm)))



## simple glm model 
glmModel <- glm(clm ~ veh_value + exposure + veh_body + veh_age + gender + area + agecat,family=binomial(link='logit'),trainCar)

trainCar$pred <- predict(glmModel,newdata=trainCar,
                          type='response')
testCar$pred <- predict(glmModel,newdata=testCar,
                         type='response') 

## confusion matrix
conMat = table(truth=testCar$clm,prediction=testCar$pred>0.15)
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