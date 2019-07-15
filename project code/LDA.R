a <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/restaurant_Rev3.xlsx')

data <- a[,c(4:7,9:11,13)]

boxplot(data, horizontal = T)
data1 <-data[data<7000] 
library(psych)

pairs.panels(data[1:7],
             gap=0,
             bg=c('red','blue')[data$classify],
             pch = 21)

set.seed(555)
ind <- sample(2, nrow(data),
              replace = T,
              prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]

library(MASS)
linear <- lda(training$classify~., training)
attributes(linear)
p <- predict(linear,training)
ldahist(data = p$x,g = training$classify)


library(klaR)
training$classify <- factor(training$classify)
partimat(classify~.,data = training, method='lda')
partimat(classify~.,data = training, method='qda')



pred <- predict(linear,testing)$class
tab1 <- table(predicted=pred, actual=testing$classify)
accuracy <- sum(diag(tab1)/sum(tab1))
