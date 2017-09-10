## dataCar exploration and cleaning
## Cody Clayton
## Sept 9, 2017

## load libraries
library(dplyr)
library(stats)
library(rpart)
library(ggplot2)

## import data

dataCar = read.csv('dataCar.csv',header=TRUE)

## review data structure 
head(dataCar)
str(dataCar)

## X_OBSTAT_ is factor with 1 level, not useful for prediction is value present for all obversations 
sum(dataCar$X_OBSTAT_ == '01101    0    0    0') == nrow(dataCar)

## remove X_OBSTAT_ from dataframe
## also remove claimcst0 & numclaims are neither are helpful to predict if 1 **or more** claims will occur
dataCar1 = dataCar %>% select(X, veh_value, exposure, clm, veh_body, veh_age, gender, area, agecat)

## review summary of dataset 
summary(dataCar1)

## convert clm to factor
dataCar1$clm = as.factor(dataCar1$clm)

## no missing or unrealistic data for exposure, clm, veh_body, veh_age, gender, area and agecat
ggplot(data=dataCar1, aes(exposure)) + geom_histogram(binwidth=.05)

ggplot(dataCar1) + geom_bar(aes(x=clm), fill="gray")
ggplot(dataCar1) + geom_bar(aes(x=veh_body), fill="gray") + 
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.8)))
ggplot(dataCar1) + geom_bar(aes(x=veh_age), fill="gray")
ggplot(dataCar1) + geom_bar(aes(x=gender), fill="gray")
ggplot(dataCar1) + geom_bar(aes(x=area), fill="gray")
ggplot(dataCar1) + geom_bar(aes(x=agecat), fill="gray")

## veh_value may have outliers, max looks very high and min is 0.000 (remove them if unrealistic)
plot(sort(dataCar1$veh_value))
sum(dataCar1$veh_value == 0)

## 53 entries have a veh_value of 0, bad data?
no_veh_value = dataCar1[dataCar1$veh_value == 0,]
no_veh_value

## looks like a lot of these entries have very high exposure (> .99)
ggplot(data=no_veh_value, aes(exposure)) + geom_histogram(binwidth=.01)
sum(no_veh_value$exposure > .99) / nrow(no_veh_value)
sum(dataCar1$exposure > .99) / nrow(dataCar1)
## 44% compared to 2.4% for entire data set 

## the rest of the data looks okay but since veh_value doesn't make sense and exposure looks odd these rows should be removed instead of replaced with the mean veh_value

dataCar2 = dataCar1[dataCar1$veh_value != 0,]

## investigate veh_value max
ggplot(dataCar2) + geom_density(aes(x=veh_value))

max(dataCar2$veh_value) * 10000

## max vehicle value is $345,600 which is realistic, no need to remove data points but may want to convert to log since range is so large
ggplot(dataCar2) + geom_density(aes(x=log10(veh_value*10000)))

## in the log space veh_value looks to have a more 'normal' looking distribution

dataCar3 = dataCar2

dataCar3$veh_value= log10(dataCar3$veh_value)


## observe relationship between variables and claims
## agecat
ggplot(dataCar3) + geom_bar(aes(x=agecat, fill=clm), position="fill")
## obvious correlation between age and claims
## veh_body
ggplot(dataCar3) + geom_bar(aes(x=veh_body, fill=clm), position="fill") + 
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.8)))
## veh_age
ggplot(dataCar3) + geom_bar(aes(x=veh_age, fill=clm), position="fill")
## gender
ggplot(dataCar3) + geom_bar(aes(x=gender, fill=clm), position="fill")
## area
ggplot(dataCar3) + geom_bar(aes(x=area, fill=clm), position="fill")

## compare 2 variables 

## agecat by veh_age
ggplot(dataCar3) +
  geom_bar(aes(x=agecat), position="dodge") +
  facet_wrap(~veh_age, scales="free_y")

## agecat by gender
ggplot(dataCar3) +
  geom_bar(aes(x=agecat), position="dodge") +
  facet_wrap(~gender, scales="free_y")

## veh_age by gender
ggplot(dataCar3) +
  geom_bar(aes(x=veh_age), position="dodge") +
  facet_wrap(~gender, scales="free_y")
## males more likely to have older cars


## check percent that default in data set 
sum(dataCar3$clm == 1) / nrow(dataCar3)

## 6.810908% -- model must be better than this (always predicting no claim have 93.18909% accuracy)


## exposure density graph
ggplot(data=dataCar3) + geom_density(aes(x=exposure,color=as.factor(clm)))

## exposure looks to be good predictor 


## export cleaned data
write.csv(dataCar3, file = "dataCar_clean.csv")










