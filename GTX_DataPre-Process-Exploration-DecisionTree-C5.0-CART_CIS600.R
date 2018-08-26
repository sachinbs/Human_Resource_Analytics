# Read the input file
HR<-read.csv("HR_comma_sep.csv")
HR
summary(HR)
###########################################################

# Data Pre-processing
# Employees with evaluation > 0.8 and salary as low
mut1 <- HR$last_evaluation >'0.87' & HR$salary=='low'
HR[mut1, "ImproperEvaluation"] <- "Yes"
HR

HR[!mut1, "ImproperEvaluation"] <- "No"
HR

# Employees with satisfaction <0.6, evaluation <0.6, number of projects <4 and got promoted
mut2 <- HR$satisfaction_level <'0.6' & HR$ last_evaluation <'0.6'  & HR$number_project <='4' & HR$ promotion_last_5years =='1'
HR[mut2, "Over_Rated"] <- "Yes"
HR

HR[!mut2, "Over_Rated"] <- "No"
HR

# Calculate the average daily hours of every employee
HR$average_daily_hours<-HR$average_montly_hours/22
HR

# Round the average daily hours to two decimal places
HR$average_daily_hours<-round(HR$average_daily_hours,digits=2)
HR
write.csv(HR, file = "foo1.csv",row.names = F)
###########################################################
# Converting Sales,salary and promotion_last_5years to factors
Fsales<-as.factor(HR$sales)
Fsalary<-as.factor(HR$salary)
Fpromotion_last_5years<-as.factor(HR$promotion_last_5years)
Ftimespent<-as.factor(HR$time_spend_company)
Fnumber_project<-as.factor(HR$number_project)
Fsalary<-ordered(HR$salary,levels=c("low","medium","high"))

###########################################################

# Data Exploration
library(ggplot2)
library(gridExtra)

# Analyze Salary:
CSalary<-table(HR$salary)
CSalary

# Observation:
# Only a small number of employees have a high salary

# Interaction between sales and salary:
psales<-ggplot(HR,aes(x=Fsales))+geom_bar(fill="#FF00FF")+coord_flip()
psalary<-ggplot(HR,aes(x=Fsalary))+geom_bar(fill="#FF00FF")+coord_flip()
psales
psalary

psales_left<-ggplot(HR,aes(x=Fsales,fill=as.factor(left)))+geom_bar(position="fill")+coord_flip()+scale_fill_brewer(palette="PiYG")
psalary_left<-ggplot(HR,aes(x=Fsalary,fill=as.factor(left)))+geom_bar()+coord_flip()+scale_fill_brewer(palette="PiYG")
psales_left
psalary_left

psalary_sales<-ggplot(HR,aes(x=Fsales,fill=Fsalary))+geom_bar(position="fill")+scale_fill_brewer(palette="PiYG")+coord_flip()
grid.arrange(psales,psalary,psales_left,psalary_left,psalary_sales,ncol=2)

# Observation:
# Management Department, has the least attrition rate as it has a higher proportion of highly paid employees.
# There is almost no attrition in High Salary Paid Employees.

# Analyse number of people promoted
Cpromoted<-table(HR$promotion_last_5years)
Cpromoted

# Observation:
# Very few people have been promoted
# Interaction between people promoted and salary
ppromoted<-ggplot(HR,aes(x=Fpromotion_last_5years,fill=as.factor(Fsalary)))+geom_bar(position="fill")
ppromoted

# Analyse Promotion in last 5 years and Over rated employees
pOver<-ggplot(HR,aes(x=Fpromotion_last_5years,fill=as.factor(Over_Rated)))+geom_bar(position="fill")
pOver


# Analyse time spent at the company
Ctimespent<-table(HR$time_spend_company)
Ctimespent
# Observation:
# We can note that as years pass by the number of employees are reducing

#	Interaction between the number of employees promoted and time spent at the company
ppromoted_timespent<-ggplot(HR,aes(x=Ftimespent,fill=as.factor(Fpromotion_last_5years)))+geom_bar()
ppromoted_timespent

# Observation:
# More Experienced employees are never promoted

# Interaction between the number of projects and the employees who left

pprojects_left<-ggplot(HR,aes(x=Fnumber_project,fill=as.factor(left)))+geom_bar()
pprojects_left

# Observation:
# Employees with 2 projects have left the company most. Secondly employees with 6 and 7 projects

# Interaction between satisfaction and ImproperEvaluation
psatisfacttion_Overrated<-ggplot(HR,aes(x=satisfaction_level,fill=as.factor(ImproperEvaluation)))+geom_bar()
psatisfacttion_Overrated

# Observation:
# Employees with higher satisfaction level are evaluated incorrectly

# Interaction between Satisfaction levels and Salary
ggplot(HR, aes(x = Fsalary, y = satisfaction_level, fill = factor(left), colour = factor(left))) + 
  +     geom_boxplot(outlier.colour = "black") + xlab("Salary") + ylab("Satisfacion level")

# Observation:
# The average satisfaction of the employees who left is lower than who haven't left

#	Interaction between Time Spent in Company and Salary
ggplot(HR, aes(x =  Fsalary, y = time_spend_company, fill = factor(left), colour = factor(left))) + 
  +     geom_boxplot(outlier.colour = NA) + xlab("Salary") + ylab("time_spend_company")

# Observation:
# Employees leaving the company have spent more years with salary levels low and medium


# Analyse only the employees who have left the company
Cleft<-table(HR$left)
Cleft

Overwork <- subset(HR, average_daily_hours > 8 , select=c(left, salary))
Overwork

# Visualization based on different departments and salary paid
pdepart_Salary<-ggplot(HR,aes(x=Fsalary,fill=as.factor(Fsales)))+geom_bar()
pdepart_Salary


##############################################################################
# Decision Tree:
library(caret)
library(rattle)
# Seed function is used for possible desire of reproducible results when selecting variables at random
set.seed(1234)
# Read the final pre-processed dataset into a new variable
HR_DT<-read.csv("foo1.csv")
HR_DT

# Divide the data set into train and test data.
# Test data holds 5000 records and the rest are train data
test_set_indexes <- sample(1:nrow(HR_DT), 5000)
train_set_indexes <- setdiff(1:nrow(HR_DT),test_set_indexes)
test_data <- HR_DT[test_set_indexes, ]
train_data <-HR_DT[train_set_indexes, ]
print(table(train_data$left))


## C5.0
ctrl1 <- trainControl(method = "cv", number = 5)
model1 <- train(as.factor(left) ~., data = train_data, method = "C5.0Tree", trControl = ctrl1)
model1
pred <- predict(model1, test_data)
confusionMatrix(pred,test_data$left)

library(pROC)
Rmodel1 <- roc(as.numeric(test_data$left), as.numeric(pred))
print(Rmodel1)
plot(Rmodel1, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(Rmodel1$auc[[1]],3)),col = 'blue')

# CPART Decision Tree
# Without any specific attributes
library(rpart)
library(rpart.plot)
model2<- rpart(factor(left) ~.,data=train_data)
model2
pred1 <- predict(model2, test_data)
# Accuracy of testing data
a<-auc(as.numeric(test_data$left) - 1, pred1[, 2])
a
pred11 <- predict(model2, train_data)
# Accuracy of training data
a1<-auc(as.numeric(train_data$left) - 1, pred11[, 2])
a1
rpart.plot(model2, type = 2, fallen.leaves = F, cex = 1, extra = 2)
rpart.plot(model2,extra = 2)
fancyRpartPlot(model2)
printcp(model2)

#Using specific attributes
model3<- rpart(factor(left) ~ satisfaction_level+number_project+time_spend_company+promotion_last_5years,data=train_data)
model3
plot(model3)
text(model3)
pred2 <- predict(model3, test_data)
# Accuracy of testing data
b<-auc(as.numeric(test_data$left) - 1, pred2[, 2])
pred21 <- predict(model3, train_data)
# Accuracy of training data
b1<-auc(as.numeric(train_data$left) - 1, pred21[, 2])
fancyRpartPlot(model3)
printcp(model3)


model4<- rpart(factor(left) ~ last_evaluation+average_daily_hours+sales+salary,data=train_data)
model4
plot(model4)
text(model4)
pred3 <- predict(model4, test_data)
# Accuracy of testing data
c<-auc(as.numeric(test_data$left) - 1, pred3[, 2])
pred31 <- predict(model4, train_data)
# Accuracy of training data
c1<-auc(as.numeric(train_data$left) - 1, pred31[, 2])
fancyRpartPlot(model4)
printcp(model4)



barplot(matrix(c(a1,a,b1,b,c1,c),nr=2), beside=T, 
        col=c("aquamarine3","coral"),ylim=c(0,1),xlab = "Model 1, Model 2, Model 3",ylab="Accuracy")
legend("topleft", c("Train","Test"), pch=15, 
       col=c("aquamarine3","coral"), 
       bty="n")

