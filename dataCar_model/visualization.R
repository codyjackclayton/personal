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

## claim submission percentage by age category
age_group = dataCar2 %>% group_by(agecat)
age_table = age_group %>% summarize(total = n(), clm_count = sum(clm==1), clm_percent = sum(clm==1) / n())

ggplot(age_table, aes(x=agecat, y=clm_percent, fill='indianred')) + 
  geom_bar(stat="identity") + 
  labs(title='Percentage of Policies that Submit a Claim by Age Category',x='Age Category (1 = youngest)', y='Percentage of Claims') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = percent) + 
  guides(fill=FALSE) + 
  geom_text(aes( label = paste0(formatC(as.numeric(clm_percent)*100, format="f", digits=2),'%'), y= clm_percent ), vjust = +2)


## exposure density graph

ggplot(data=dataCar2) + 
  geom_density(aes(x=exposure,color=clm, y=..scaled..)) +
  labs(title='Claim Density By Exposure',x='Exposure', y='Density', color='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(data=dataCar2) + 
  geom_histogram(aes(x=exposure,fill=clm),bins=20,position='fill') +
  labs(title='Claim Density By Exposure',x='Exposure', y='Density', fill='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(labels = c("No", "Yes"), values=c('#F8766D','#00BFC4'))


expDf = dataCar2
expDf$exp_bins = cut(dataCar2$exposure, breaks=c(seq(0,1,0.05)))

exp_group = expDf %>% group_by(exp_bins) 
exp_table = exp_group %>% summarise(total = n(), clm_count = sum(clm==1), clm_percent = sum(clm==1) / n())
exp_table = exp_table %>% mutate(format_bins = seq(0,0.95,0.05))

ggplot(exp_table, aes(x=format_bins, y=clm_percent, fill='indianred')) + 
  geom_bar(stat="identity") + 
  labs(title='Percentage of Policies that Submit a Claim by Exposure',x='Exposure', y='Percentage of Claims') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = percent) + 
  scale_x_continuous(limits = c(0.05, 0.95)) + 
  guides(fill=FALSE) 


## vehicle body 


ggplot(dataCar2) + geom_bar(aes(x=veh_body, fill=clm), position="fill") + 
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.8))) + 
  labs(title='Proportion of Policies That Submit Claims By Vehicle Body',x='Proportion', y='Vehicle Body', fill='Claim Submitted') +
  theme(plot.title = element_text(hjust = 0.5))

veh_group = expDf %>% group_by(veh_body) 
veh_table = veh_group %>% summarise(total = n(), clm_count = sum(clm==1), clm_percent = sum(clm==1) / n())

veh_table$veh_body <- factor(veh_table$veh_body, levels = veh_table$veh_body[order(veh_table$clm_percent)])

ggplot(veh_table, aes(x=veh_body, y=clm_percent, fill='indianred')) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme(axis.text.y=element_text(size=rel(0.8))) + 
  labs(title='Percentage of Policies that Submit a Claim by Vehicle Body',y='Percentage of Claims', x='Vehicle Body') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels = percent) + 
  guides(fill=FALSE) 

## vehicle value

multi_plot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)} 
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

p1 = ggplot(dataCar2) + geom_density(aes(x=veh_value*10000, y=..scaled..)) + 
  labs(title='Vehicle Value Distribution',x='Vehicle Value', y='Density') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(labels = dollar)

p2 = ggplot(dataCar2) + geom_density(aes(x=veh_value*10000, y=..scaled..)) + 
  labs(title='Log10 Vehicle Value Distribution',x='Vehicle Value', y='Density') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_log10(label = dollar) +
  coord_trans(x="log10") 


multi_plot(list(p1, p2))

