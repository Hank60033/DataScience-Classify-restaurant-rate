a <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/restaurant_Rev3.xlsx')

data <- a[,c(4:11,13)]


#logistic regression

set.seed(6666)
ind <- sample(2, nrow(data),
              replace = T,
              prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]

linear<- glm(classify~.,data = training,family = binomial)

pred <- predict(linear,testing,type = 'response')

pred1 <- ifelse(pred>0.5,1,0)
tab <- table(predicted=pred1,actual=testing$classify)
accuracy <- sum(diag(tab)/sum(tab))

