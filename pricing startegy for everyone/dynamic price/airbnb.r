rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)

setwd("C:/Users/User/Desktop/data science project/pricing startegy for everyone/dynamic price")


#reading
#Loading data
data<-read.csv(file="airbnb.csv",header=T,sep=",")

dim(data)
str(data)
summary(data)

#checking missin vaalue##
sum(is.na(data))      

data=na.omit(data)

##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)



library(esquisse)
esquisse::esquisser(data)


library(ggplot2)

ggplot(data) +
 aes(x = longitude, y = latitude) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 theme_minimal()
library(ggplot2)

ggplot(data) +
 aes(x = availability_365, y = price, colour = room_type) +
 geom_point(size = 1L) +
 scale_color_hue() +
 theme_minimal()





data$id=NULL
data$name=NULL
data$host_id=NULL
data$host_name=NULL

data$neighbourhood_group=as.factor(data$neighbourhood_group)
data$neighbourhood=NULL
data$room_type=as.factor(data$room_type)
data$latitude=NULL
data$longitude=NULL
data$last_review=NULL

data$neighbourhood_group=NULL
data=data[1:5000,]

library(earth)
marsModel <- earth(price ~ ., data=data) # build model
ev <- evimp (marsModel)
plot(ev)


set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




model <- lm(price~ ., data= train)
plot(model)

pred1=predict(model,test)
regr.eval(train$price,pred1)

mae          mse         rmse         mape 
9.992202e+01 6.420238e+04 2.533819e+02 8.121194e-01


library(MASS)
stepAIC(model, direction = "both")

