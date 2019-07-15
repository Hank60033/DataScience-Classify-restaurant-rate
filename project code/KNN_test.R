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

training[1:7] <- scale(training[1:7])
testing[1:7] <- scale(testing[1:7])
test_data[1:7] <- scale(test_data[1:7])


training$classify <- factor(training$classify)


library(class)
y_pred <- knn(classify~.,
              train = training[,-8],
              test = testing[,-8],
              k=5,
              prob = T)











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

pred <- predict(fit, newdata = test_data,type = 'prob')
tab <- table(predicted=pred,actual=test_data$classify)
accuracy <- sum(diag(tab)/sum(tab))
