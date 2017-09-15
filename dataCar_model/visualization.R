library(dplyr)
library(stats)
library(rpart)
library(ggplot2)
library(grid)
library(scales)

dataCar = read.csv('dataCar.csv',header=TRUE)
dataCar1 = dataCar %>% select(X, veh_value, exposure, clm, veh_body, veh_age, gender, area, agecat)
dataCar1$clm = as.factor(dataCar1$clm)
dataCar2 = dataCar1[dataCar1$veh_value != 0,]


## age density graph
ggplot(dataCar2, aes(x=agecat, fill=clm)) + geom_bar(position="fill") + labs(title='Proportion of Policies That Submit Claims By Age',x='Age', y='Proportion', fill='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5))

## exposure density graph
ggplot(data=dataCar2) + 
  geom_density(aes(x=exposure,color=clm, y=..scaled..)) +
  labs(title='Claim Density By Exposure',x='Exposure', y='Density', color='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5))

## vehicle body 

ggplot(dataCar2) + geom_bar(aes(x=veh_body, fill=clm), position="fill") + 
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.8))) + 
  labs(title='Proportion of Policies That Submit Claims By Vehicle Body',x='Proportion', y='Vehicle Body', fill='Claim Submitted')

multi_plot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)} 
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

p1 = ggplot(dataCar2) + geom_density(aes(x=veh_value*10000,y=..scaled..)) + 
  labs(title='Vehicle Value & Log Transformed Distribution ',x='Vehicle Value', y='Density') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(labels = dollar)

p2 = ggplot(dataCar2) + geom_density(aes(x=log10(veh_value*10000),y=..scaled..)) + 
  labs(x='Log10 Vehicle Value', y='Density')


multi_plot(list(p1, p2))
