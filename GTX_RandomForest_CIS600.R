HR<-read.csv("E:/Spring 2017/foo1.csv")
head(HR)
splitIndex <- createDataPartition(HR$left, p = .80, list = FALSE, times = 1)
trainSplit <- HR[splitIndex,]
testSplit <- HR[-splitIndex,]
rf_mod <- randomForest(as.factor(left) ~. , data = trainSplit)
importance(rf_mod)
print(table(HR$left))


print(table(trainSplit$left))
print(table(testSplit$left))
pred_rf <- predict(rf_mod,testSplit)
summary(pred_rf)
confusionMatrix(pred_rf,testSplit$left)
new_HR1 <- HR[HR$satisfaction_level < 0.6 & !salh & HR$last_evaluation < 0.6 & prom_wrk & (HR$sales == "sales" | HR$sales == "technical" | HR$sales == "support") ,]
n_splitIndex <- createDataPartition(new_HR1$left, p = .80, list = FALSE, times = 1)
n_trainSplit <- new_HR1[n_splitIndex,]
n_testSplit <- new_HR1[-n_splitIndex,]
nrow(n_trainSplit)
rf1_mod <- randomForest(as.factor(left) ~. , data = n_trainSplit)
importance(rf1_mod)
pred_rf1 <- predict(rf1_mod,n_testSplit)
summary(pred_rf1)
confusionMatrix(pred_rf1,n_testSplit$left)
auc_rf <- roc(as.numeric(testSplit$left), as.numeric(pred_rf),  ci=TRUE)
plot(auc_rf, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf$auc[[1]],3)),col = 'red')
auc_rf1 <- roc(as.numeric(n_testSplit$left), as.numeric(pred_rf1),  ci=TRUE)
plot(auc_rf1, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf1$auc[[1]],3)),col = 'red')
