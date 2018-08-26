#Install packages e1071, tm  for naive bayes, ggplot2 for graphs and caret for confusion matrix
install.packages("e1071")
install.packages("ggplot2")
install.packages("caret")
install.packages("tm")
install.packages("pROC")
#Load those libraries
library(e1071)
library(tm)
library(caret)
library(ggplot2)
library(pROC)
# read the dataset
hra <-read.csv("C:/Users/maksh/Desktop/foo1.csv", stringsAsFactors <- TRUE)
# Display the contents of the dataset
str(hra)
summary(hra)
# Display the number of people who left the company
table(hra$left)
#split the dataset to 80% training and 20% test
split.hra <- floor(0.8 * nrow(hra))
set.seed(1000)
train <- sample(seq_len(nrow(hra)), size = split.hra)
train_hra <- hra[train, ]
test_hra <- hra[-train, ]
# Display the rows of training dataset and Test Dataset
nrow(train_hra)
nrow(test_hra)
#Display the no.of people left the ompany separately in training and test set
table(test_hra$left)
table(train_hra$left)
#Train the dataset using naive bayes classifier
model.bayes <- naiveBayes(as.factor(left) ~ ., data = train_hra)
model.bayes
#Show the probability of salary w.r.t people left
prop.table(table(train_hra$salary, train_hra$left),2)
# Plot the graph comparing various attributes with people who left the company
ggplot(train_hra,aes(x=salary,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
ggplot(train_hra,aes(x=sales,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
ggplot(train_hra,aes(x=satisfaction_level,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
ggplot(train_hra,aes(x=number_project,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
ggplot(train_hra,aes(x=promotion_last_5years,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
# predict the naive bayes model stats for training data set
res_nb=predict(model.bayes,train_hra)
# Confusion Matrix for Train dataset
confusionMatrix(res_nb,train_hra$left)
#plot the accuracy for train dataset
auc <- roc(as.numeric(train_hra$left)-1, as.numeric(res_nb)-1)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],3)),col = 'green')
# predict the naive bayes model stats for test data set
test_resnb=predict(model.bayes,test_hra)
#Confusion Matrix for Test dataset
confusionMatrix(test_resnb,test_hra$left)
# plot the accuracy for test dataset
auc2 <- roc(as.numeric(test_hra$left)-1, as.numeric(test_resnb)-1)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc2$auc[[1]],3)),col = 'green')


