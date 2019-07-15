a <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/restaurant_Rev3.xlsx')

data <- a[,c(4:11,13)]



#naive bayes

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

data$classify <- factor(data$classify)

set.seed(666)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]

pairs.panels(data[-1])


model <- naive_bayes(classify~., data = training)



plot(model)

pred <- predict(model,testing)            


tab <- table(predicted=pred, actual=testing$classify)
accuracy <- sum(diag(tab)/sum(tab))

