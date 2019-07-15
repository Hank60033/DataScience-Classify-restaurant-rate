

a <- readxl::read_xlsx('/Users/hankchen/Desktop/semester 1/652/final project/restaurant_Rev3.xlsx')

data <- a[,c(4:7,9:11,13)]


lda_model <- lda(data$classify~., data)



pred_lda <- predict(lda_model,data, type='prob')$posterior
pred_lda[,1] <- double(pred_lda[,1])

library(caret)
library(pROC)
library(mlbench)

data$classify <- factor(data$classify)
trcontrol <- trainControl(method='repeatedcv',
                          number=10,
                          repeats=3)

set.seed(222)
knn_model<- train(classify~.,
             data = data,
             method='knn',
             tuneLength=20,
             trControl=trcontrol,
             preProc=c('center','scale'))

pred_knn <- predict(knn_model, newdata = data, type = 'prob')

glm_model<- glm(classify~.,data = data,family = binomial)

pred_glm <- predict(glm_model,data,type = 'response')

pred_glm <- ifelse(pred_glm>0.5,1,0)

nb_model <- naive_bayes(classify~., data = data)

pred_nb <- predict(nb_model,data,type='prob')

#ROC




pred_lda <- prediction(pred_lda[,2],data$classify)
pred_knn <- prediction(pred_knn[,2],data$classify)
pred_glm <- prediction(pred_glm,data$classify)
pred_nb <- prediction(pred_nb[,2],data$classify )

library(ROCR)
library(nnet)


roc1 <- performance(pred_lda,'tpr','fpr')
roc2 <- performance(pred_knn,'tpr','fpr')
roc3 <- performance(pred_glm,'tpr','fpr')
roc4 <- performance(pred_nb,'tpr','fpr')
plot(roc1,
     main='ROC curve',
     xlab='1-specificity',
     ylab='sensitivity',
     col='red')
plot(roc2,col='blue', add=T)
plot(roc3, col='yellow',add=T)
plot(roc4, col='green', add=T)
abline(a=0,b=1)

legend(0.5,0.3,legend = c('linear discriminant analysis(AUC=0.7182)','KNN(AUC=0.8601)','logistic regression(AUC=0.6998)','naive bayes(AUC=0.6712)')
       ,col = c('red','blue','yellow','green'),
       box.lty=0,
       lty = 1,
       cex = 0.8)

library(pROC)
auc_lda <- performance(pred_lda,'auc')@y.values
auc_knn <- performance(pred_knn,'auc')@y.values
auc_glm <- performance(pred_glm,'auc')@y.values
auc_nb <- performance(pred_nb,'auc')@y.values
