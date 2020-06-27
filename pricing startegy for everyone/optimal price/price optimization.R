rm(list=ls(all=TRUE))

install.packages("magrittr")
library(magrittr)
library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)


#path set#
setwd("C:/Users/User/Desktop/data science project/machine learning/pricing startegy for everyone/optimal price")

data<-read.csv(file="mango_sales.csv")

dim(data)
str(data)
summary(data)



#eda#
hist(data$price)
boxplot(data$price)
hist(data$kilo)
boxplot(data$kilo)

#define alpha beta##
model2=lm(kilo~price,data=data)
print(model2)

#function for demand#
linear = function(p, alpha, beta) alpha*p + beta

#demand#
p = data$price
d = linear(p, alpha = -0.75, beta = 116) + rnorm(sd = 5, length(p))
c = 75
profit = d*(p-c)

# Fit of the demand model
model3 = lm(d~p)
profit.fitted = model3$fitted.values*(p - c)

# Pricing Optimization
alpha = model3$coefficients[2]
beta = model3$coefficients[1]
p.max.profit = (alpha*c - beta)/(2*alpha)

# Plots
df.linear = data.frame('Prices' = p, 
                       'Demand' = d,
                       'Profit.fitted' = profit.fitted,
                       'Profit' = profit)

ggplot(select(df.linear, Prices, Demand))
+ aes(x = Prices, y = Demand) +
  geom_point() + 
  geom_smooth(method = lm)


#optimized price##
ggplot(select(df.linear, Prices, Profit)) +
  aes(x = Prices, y = Profit) +
  geom_point()+
  geom_vline(xintercept = p.max.profit, lty = 2) +
  geom_line(data = df.linear, aes(x = Prices, y = Profit.fitted), color = 'blue')+
  ggtitle("Price optimisation") 
##optimized price 116##