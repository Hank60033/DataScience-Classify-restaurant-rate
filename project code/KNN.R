a <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/restaurant_Rev3.xlsx')

data <- a[,c(4:7,9:11,13)]

test_sample <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/test sample.xlsx')

test_data <- test_sample[,c(4:7,9:11,13)]
#knn

library(caret)
library(pROC)
library(mlbench)



set.seed(1234)
ind <- sample(2, nrow(data),replace = T, prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]

training[1:8] <- scale(training[1:8])
testing[1:8] <- scale(testing[1:8])

training$classify <- factor(training$classify)

trcontrol <- trainControl(method='repeatedcv',
                          number=10,
                          repeats=3)

set.seed(222)
fit <- train(classify~.,
             data = training,
             method='knn',
             tuneLength=20,
             trControl=trcontrol,
             preProc=c('center','scale'))


plot(fit)

pred <- predict(fit, newdata = testing)
tab <- table(predicted=pred,actual=testing$classify)
accuracy <- sum(diag(tab)/sum(tab))
