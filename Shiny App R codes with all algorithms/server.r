library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(ggplot2)
library(gridExtra)
library(shiny)
library(pROC)
library(tm)
library(arules)
library(arulesViz)
library(rsconnect)
library(rdrop2)
token <- drop_auth()
saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")

##############################################################
# Read the input file
HR<-drop_read_csv("Responses/HR_comma_sep.csv", dtoken = token)
HR1 <- HR
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
HR2 <- HR
###########################################################
# Converting Sales,salary and promotion_last_5years to factors
Fsales<-as.factor(HR$sales)
Fsalary<-as.factor(HR$salary)
Fpromotion_last_5years<-as.factor(HR$promotion_last_5years)
Ftimespent<-as.factor(HR$time_spend_company)
Fnumber_project<-as.factor(HR$number_project)
Fsalary<-ordered(HR$salary,levels=c("low","medium","high"))
###########################################################
# Analyze Salary:
CSalary <- table(HR$salary)
CSalary
# Interaction between sales and salary:
psales<-ggplot(HR,aes(x=Fsales))+geom_bar(fill="#FF00FF")+coord_flip()
psalary<-ggplot(HR,aes(x=Fsalary))+geom_bar(fill="#FF00FF")+coord_flip()
psales
psalary

psales_left<-ggplot(HR,aes(x=Fsales,fill=as.factor(left)))+geom_bar(position="fill")+coord_flip()+scale_fill_brewer(palette="PiYG")
psalary_left<-ggplot(HR,aes(x=Fsalary,fill=as.factor(left)))+geom_bar()+coord_flip()+scale_fill_brewer(palette="PiYG")
psales_left
psalary_left

psalary_sales <- ggplot(HR,aes(x=Fsales,fill=Fsalary))+geom_bar(position="fill")+scale_fill_brewer(palette="PiYG")+coord_flip()
grid.arrange(psales,psalary,psales_left,psalary_left,psalary_sales,ncol=2)

# Analyse number of people promoted
Cpromoted<-table(HR$promotion_last_5years)
Cpromoted

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
p1 <- ggplot(HR, aes(x = Fsalary, y = satisfaction_level, fill = factor(left), colour = factor(left))) + geom_boxplot(outlier.colour = "black") + xlab("Salary") + ylab("Satisfacion level")
# Observation:
# The average satisfaction of the employees who left is lower than who haven't left

#	Interaction between Time Spent in Company and Salary
p2 <- ggplot(HR, aes(x =  Fsalary, y = time_spend_company, fill = factor(left), colour = factor(left))) + geom_boxplot(outlier.colour = NA) + xlab("Salary") + ylab("time_spend_company")
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

############################################################################
# # Decision Tree:
library(caret)
# library(rattle)
# # Seed function is used for possible desire of reproducible results when selecting variables at random
set.seed(1234)
# # Read the final pre-processed dataset into a new variable
HR_DT<-read.csv("foo1.csv")
HR_DT

# Divide the data set into train and test data.
# Test data holds 5000 records and the rest are train data
test_set_indexes <- sample(1:nrow(HR_DT), 5000)
train_set_indexes <- setdiff(1:nrow(HR_DT),test_set_indexes)
test_data <- HR_DT[test_set_indexes, ]
train_data <-HR_DT[train_set_indexes, ]
print(table(train_data$left))


# ## C5.0
ctrl1 <- trainControl(method = "cv", number = 5)
model1 <- train(as.factor(left) ~., data = train_data, method = "C5.0Tree", trControl = ctrl1)
model1
pred <- predict(model1, test_data)
cm1 <- confusionMatrix(pred,test_data$left)
Rmodel1 <- roc(as.numeric(test_data$left), as.numeric(pred))
print(Rmodel1)
pmodel1 <- plot(Rmodel1, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(Rmodel1$auc[[1]],3)),col = 'blue')
# 
# # CPART Decision Tree
# # Without any specific attributes
# 
model2<- rpart(factor(left) ~.,data=train_data)
model2
pred1 <- predict(model2, test_data)
# # Accuracy of testing data
auc(as.numeric(test_data$left) - 1, pred1[, 2])
pred11 <- predict(model2, train_data)
# # Accuracy of training data
auc(as.numeric(train_data$left) - 1, pred11[, 2])
model2p1 <- rpart.plot(model2, type = 2, fallen.leaves = F, cex = 1, extra = 2)
model2p2 <- rpart.plot(model2,extra = 2)
fancyRpartPlot(model2)
printcp(model2)
# 
# #Using specific attributes
model3<- rpart(factor(left) ~ satisfaction_level+number_project+time_spend_company+promotion_last_5years,data=train_data)
model3
#plot(model3)
#text(model3)
pred2 <- predict(model3, test_data)
# # Accuracy of testing data
auc(as.numeric(test_data$left) - 1, pred2[, 2])
pred21 <- predict(model3, train_data)
# # Accuracy of training data
auc(as.numeric(train_data$left) - 1, pred21[, 2])
fancyRpartPlot(model3)
printcp(model3)
# 
model4<- rpart(factor(left) ~ last_evaluation+average_daily_hours+sales+salary,data=train_data)
model4
plot(model4)
text(model4)
pred3 <- predict(model4, test_data)
# # Accuracy of testing data
auc(as.numeric(test_data$left) - 1, pred3[, 2])
pred31 <- predict(model4, train_data)
# # Accuracy of training data
auc(as.numeric(train_data$left) - 1, pred31[, 2])
fancyRpartPlot(model4)
printcp(model4)
##################################################################################################
#finalp1Code

# read the dataset
hra <-read.csv("foo1.csv", stringsAsFactors <- TRUE)
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
naivep1 <- ggplot(train_hra,aes(x=salary,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
naivep2 <- ggplot(train_hra,aes(x=sales,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
naivep3 <- ggplot(train_hra,aes(x=satisfaction_level,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
naivep4 <- ggplot(train_hra,aes(x=number_project,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")
naivep5 <- ggplot(train_hra,aes(x=promotion_last_5years,fill=as.factor(left)))+geom_bar(position = "fill")+coord_flip()+scale_fill_brewer(palette = "PiYG")

# predict the naive bayes model stats for training data set
res_nb=predict(model.bayes,train_hra)
# Confusion Matrix for Train dataset
naivecmt1 <- confusionMatrix(res_nb,train_hra$left)


#plot the accuracy for train dataset
auc <- roc(as.numeric(train_hra$left)-1, as.numeric(res_nb)-1)
naiverocp <- plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],3)),col = 'green')
# predict the naive bayes model stats for test data set
test_resnb=predict(model.bayes,test_hra)
#Confusion Matrix for Test dataset
naivecmt2 <- confusionMatrix(test_resnb,test_hra$left)
# plot the accuracy for test dataset
auc2 <- roc(as.numeric(test_hra$left)-1, as.numeric(test_resnb)-1)
naiverocp2 <- plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc2$auc[[1]],3)),col = 'green')

############################################################################
#Random Forest
HR<-read.csv("foo1.csv")
HR$X <- NULL
head(HR)
splitIndex <- createDataPartition(HR$left, p = .80, list = FALSE, times = 1)
trainSplit <- HR[splitIndex,]
testSplit <- HR[-splitIndex,]
rf_mod <- randomForest(as.factor(left) ~. , data = trainSplit)

imp1 <- importance(rf_mod)
pr1 <- print(table(HR$left))

importance(rf_mod)
print(table(HR$left))

print(table(trainSplit$left))
print(table(testSplit$left))

pred_rf <- predict(rf_mod,testSplit)
summary(pred_rf)
randomcmtb1 <- confusionMatrix(pred_rf,testSplit$left)
salh <- HR$salary == "high"
new_HR1 <- HR[HR$satisfaction_level < 0.6 & !salh & HR$last_evaluation < 0.6 & (HR$sales == "sales" | HR$sales == "technical" | HR$sales == "support") ,]
n_splitIndex <- createDataPartition(new_HR1$left, p = .80, list = FALSE, times = 1)
n_trainSplit <- new_HR1[n_splitIndex,]
n_testSplit <- new_HR1[-n_splitIndex,]
nrow(n_trainSplit)

rf1_mod <- randomForest(as.factor(left) ~. , data = n_trainSplit)
importance(rf1_mod)
pred_rf1 <- predict(rf1_mod,n_testSplit)
summary(pred_rf1)

randomcmtb2 <- confusionMatrix(pred_rf1,n_testSplit$left)

auc_rf <- roc(as.numeric(testSplit$left), as.numeric(pred_rf),  ci=TRUE)
plot(auc_rf, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf$auc[[1]],3)),col = 'red')

auc_rf1 <- roc(as.numeric(n_testSplit$left), as.numeric(pred_rf1),  ci=TRUE)
plot(auc_rf1, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf1$auc[[1]],3)),col = 'red')

#####################################################################################
# Association analysis

library(arules)
library(arulesViz)
# Reading in data
ppdata <- drop_read_csv("Responses/foo1.csv", dtoken = token)

##### Pre-Processing ##################################################################
# Removing the first coulumn (serial number)
# ppdata = ppdata[,c(2:14)]

# Discretizing (into "low", "average", "high") the satisfaction level based on the "satisfaction_level" variable
ppdata$satisfaction_level = discretize(ppdata$satisfaction_level, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$satisfaction_level, main = 'satisfaction_level', col = 'lightblue')

# Discretizing (into "low", "average", "high") the last evaluation based on the "last_evaluation" variable
ppdata$last_evaluation = discretize(ppdata$last_evaluation, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$last_evaluation, main = 'last_evaluation', col = 'lightblue')

# Discretizing (into "low", "average", "high") the number of projects based on the "number_project" variable
ppdata$number_project = discretize(ppdata$number_project, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$number_project, main = 'number_project', col = 'lightblue')

# Discretizing (into "low", "average", "high") the time spent in company based on the "time_spend_company" variable
ppdata$time_spend_company = discretize(ppdata$time_spend_company, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$time_spend_company, main = 'time_spend_company', col = 'lightblue')

# Discretizing (into "low", "average", "high") the average monthly hours based on the "average_montly_hours" variable
ppdata$average_montly_hours = discretize(ppdata$average_montly_hours, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$average_montly_hours, main = 'average_montly_hours', col = 'lightblue')

# Discretizing (into "low", "average", "high") the average daily hours based on the "average_daily_hours" variable
ppdata$average_daily_hours = discretize(ppdata$average_daily_hours, method = "interval", categories = 3, labels = c("low", "average", "high"))
# plot(ppdata$average_daily_hours, main = 'average_daily_hours', col = 'lightblue')

# Factorizing other attributes (Work_accident, left, promotion_last_5years)
ppdata$Work_accident <- as.factor(ppdata$Work_accident)
ppdata$left <- as.factor(ppdata$left)
ppdata$promotion_last_5years <- as.factor(ppdata$promotion_last_5years)

write.csv(ppdata, file = "Assoc_Analy_Proce.csv")
drop_upload("Assoc_Analy_Proce.csv", dest = "Responses", dtoken = token)

##### Association Analysis through Apriori Algorithm #################################
# Generating rules

# Generating rules for the whole dataset
rulea <- apriori(ppdata, parameter = list(minlen = 3, support = 0.1, confidence = 0.5))
quality(rulea) <- round(quality(rulea), digits = 3)
rulea.sorted <- sort(rulea, by = "confidence")
# inspect(rulea.sorted)

# Part A: Analysis of the characteristics of employees who are still working {left = 0}.
Rule1 <- apriori(ppdata, parameter = list(minlen = 3, support = 0.1, confidence = 0.5),
                 appearance = list(rhs = c("left=0"),
                                   lhs = c("satisfaction_level=high", "satisfaction_level=low", "satisfaction_level=average",
                                           "average_daily_hours=average", "average_daily_hours=low", "average_daily_hours=high",
                                           "last_evaluation=high", "last_evaluation=low", "last_evaluation=average",
                                           "number_project=average", "number_project=low", "number_project=high",
                                           "salary=medium", "salary=low", "salary=high",
                                           "ImproperEvaluation=No", "ImproperEvaluation=Yes",
                                           "Over_Rated=No", "Over_Rated=Yes"),
                                   default = "none"))
quality(Rule1) <- round(quality(Rule1), digits = 3)
Rule1.sorted <- sort(Rule1, by = "confidence")
# inspect(Rule1.sorted)

# Part B: Analysis of the characteristics of employees who have left the company {left = 1}
Rule2 <- apriori(ppdata, parameter = list(minlen = 3, support = 0.06, confidence = 0.4),
                 appearance = list(rhs = c("left=1"),
                                   lhs = c("satisfaction_level=high", "satisfaction_level=low", "satisfaction_level=average",
                                           "average_daily_hours=average", "average_daily_hours=low", "average_daily_hours=high",
                                           "last_evaluation=high", "last_evaluation=low", "last_evaluation=average",
                                           "number_project=average", "number_project=low", "number_project=high",
                                           "salary=medium", "salary=low", "salary=high",
                                           "ImproperEvaluation=No", "ImproperEvaluation=Yes",
                                           "Over_Rated=No", "Over_Rated=Yes"),
                                   default = "none"))

quality(Rule2) <- round(quality(Rule2), digits = 3)
Rule2.sorted <- sort(Rule2, by = "confidence")
# inspect(Rule2.sorted)

##### Analysis #######################################################################
# Plotting rules for analysis

# Plotting scatter plot for rules
sg1 <- plot(rulea)
sg2 <- plot(Rule1)
sg3 <- plot(Rule2)

# Plotting distribution graph for rules
# Takes time (because of large number of rules)
# plot(rulea, method = "graph", control = list(type = "items"))
dg2 <- plot(Rule1, method = "graph", control = list(type = "items"))
dg3 <- plot(Rule2, method = "graph", control = list(type = "items"))

# Plotting Parallel co-ordinate plots
# Takes time (because of large number of rules)
# plot(rulea.sorted, method="paracoord", control=list(reorder=TRUE))
pg2 <- plot(Rule1, method="paracoord", control=list(reorder=TRUE))
pg3 <- plot(Rule2, method="paracoord", control=list(reorder=TRUE))

# Saving all rules to csv for manual analysis
rulea_csv <- as(rulea.sorted, "data.frame")
write.csv(rulea_csv, "rulea.csv")
drop_upload("rulea.csv", dest = "Responses", dtoken = token)
Rule1_csv <- as(Rule1.sorted, "data.frame")
write.csv(Rule1_csv, "Rule1.csv")
drop_upload("Rule1.csv", dest = "Responses", dtoken = token)
Rule2_csv <- as(Rule2.sorted, "data.frame")
write.csv(Rule2_csv, "Rule2.csv")
drop_upload("Rule2.csv", dest = "Responses", dtoken = token)
##############################################################
shinyServer(
  function(input,output){
    
    output$contents <- renderTable(head(HR1))
    
    observeEvent(input$Preprocessing,
                 {
                   write.csv(HR, file = "foo1.csv",row.names = F)
                   drop_upload("foo1.csv", dest = "Responses", dtoken = token)
                   output$contents2 <- renderTable(head(HR2))
                   output$prepro <- renderText("Preprocessed Data : ")
                 }
                 
    )
    observeEvent(input$analyzeSalary, 
                 {
                   output$Salary <- renderTable({CSalary}, caption = "Only Small number of Employees have a high salary")
                 }
                 )
    observeEvent(input$salesandsalary,
                 {
                   output$graph1 <- renderPlot(grid.arrange(psales,psalary,psales_left,psalary_left,psalary_sales,ncol=2))
                   output$observation1 <- renderText("Observation: Management Department, has the least attrition rate as it has a higher proportion of highly paid employees.
                                                       There is almost no attrition in High Salary Paid Employees.")
                 }
                 )
    observeEvent(input$promoted,
                 {
                   output$promotedtb <- renderTable({Cpromoted}, caption = "Very few people have been promoted")
                 }
    )
    observeEvent(input$promoted2, 
                 {
                   output$promotedg <- renderPlot(ppromoted)
                 })
    observeEvent(input$promoted3,
                 {
                   output$promotedg2 <- renderPlot(pOver)
                 }
    )
    observeEvent(input$timespent,
                 {
                   output$timespenttb <- renderTable({Ctimespent}, caption = "As the years pass by the number of employees reduces")
                 })
    observeEvent(input$promoted4,
                 {
                   output$promoted4g <- renderPlot(ppromoted_timespent)
                   output$observation2 <- renderText("More experienced employees are never promoted")
                 })
    observeEvent(input$projects,
                 {
                   output$projectsg <- renderPlot(pprojects_left)
                   output$observation3 <- renderText("Employees with 2 projects have left the most. Secondly with 6 and 7 projects.")
                 })
    observeEvent(input$satisfaction,
                 {
                   output$satisfactiong <- renderPlot(psatisfacttion_Overrated)
                   output$observation4 <- renderText("Employees with higher satisfaction level are evaluated incorrectly.")
                 })
    observeEvent(input$satisfaction1,
                 {
                   output$satisfaction1g <- renderPlot(p1)
                   output$observation5 <- renderText("The average satisfaction of the employees who left is lower than who haven't left")
                 })
    observeEvent(input$salary,
                 {
                   output$salaryg <- renderPlot(p2)
                   output$observation6 <- renderText("Employees leaving the company have spent more years with salary levels low and medium")
                 })
    observeEvent(input$left,
                 {
                   output$lefttb <- renderTable(Cleft)
                 })
    observeEvent(input$visualization,
                 {
                   output$visualizationg <- renderPlot(pdepart_Salary)
                 })
    observeEvent(input$model11,
                 {
                   output$Model1text <- renderPrint(model1)
                   output$Model1cm <- renderTable(cm1$table)
                   output$Model1rm1 <- renderPrint(Rmodel1)
                   output$model1auc1 <- renderPlot(plot.roc(pmodel1))
                 })
    observeEvent(input$model22,
                 {
                   output$Model2text <- renderPrint(model2)
                   output$helptext2 <- renderText("AUC for testing and training data respectively:")
                   output$auc2 <- renderPrint(auc(as.numeric(test_data$left) - 1, pred1[, 2]))
                   output$auc3 <- renderPrint(auc(as.numeric(train_data$left) - 1, pred11[, 2]))
                   output$model22g1 <- renderPlot(rpart.plot(model2, type = 2, fallen.leaves = F, cex = 1, extra = 2))
                   output$model22g2 <- renderPlot(rpart.plot(model2,extra = 2))
                   output$model22g3 <- renderPlot(fancyRpartPlot(model2))
                   output$model22cp <- renderTable(model2$cptable)
                 }
    )
    observeEvent(input$model33,
                 {
                   output$Model3text <- renderPrint(model3)
                   output$helptext3 <- renderText("The auc for testing and training data respectively:")
                   output$auc4 <- renderPrint(auc(as.numeric(test_data$left) - 1, pred2[, 2]))
                   output$auc5 <- renderPrint(auc(as.numeric(train_data$left) - 1, pred21[, 2]))
                   output$model33g1 <- renderPlot(plot(model3))
                   output$model33g2 <- renderPlot(fancyRpartPlot(model3))
                   output$model33cp <- renderTable(model3$cptable)
                 }
    )
    observeEvent(input$model44,
                 {
                   output$Model4text <- renderPrint(model4)
                   output$helptext4 <- renderText("The auc for testing and training data respectively:")
                   output$auc6 <- renderPrint(auc(as.numeric(test_data$left) - 1, pred2[, 2]))
                   output$auc7 <- renderPrint(auc(as.numeric(train_data$left) - 1, pred21[, 2]))
                   output$model44g1 <- renderPlot(plot(model4))
                   output$model44g2 <- renderPlot(fancyRpartPlot(model4))
                   output$model44cp <- renderTable(model4$cptable)
                 })
    # observeEvent(input$naivemodel,
    #              {
    #                output$modeltext <- renderPrint(model.bayes)
    #                
    #              }
      
    #)
    observeEvent(input$probtable,
                 {
                   output$bayestable <- renderTable(prop.table(table(train_hra$salary, train_hra$left),2))
                 }
      
    )
    observeEvent(input$probplot,
                 {
                   output$naivep1g <- renderPlot(naivep1)
                   output$naivep2g <- renderPlot(naivep2)
                   output$naivep3g <- renderPlot(naivep3)
                   output$naivep4g <- renderPlot(naivep4)
                   output$naivep5g <- renderPlot(naivep5)
                 })
    
    observeEvent(input$naivecm,
                 {
                   #output$naivecmtb <- renderTable(naivecmt1$table)
                   output$rocplot <- renderPlot(plot.roc(naiverocp))
                 })
    
    observeEvent(input$naivecm2,
                 {
                   #output$naivecmtb1 <- renderTable(naivecmt2$table)
                   output$rocplot1 <- renderPlot(plot.roc(naiverocp2))
                 })
    
    observeEvent(input$randommodel1,
                 {
                   output$importance1 <- renderPrint(importance(rf_mod))
                   output$summary1 <- renderPrint(summary(pred_rf))
                   output$randomcm1 <- renderTable(randomcmtb1$table)
                   output$randomroc1 <- renderPlot(plot(auc_rf, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf$auc[[1]],3)),col = 'red'))
               
                 })
    
    observeEvent(input$randommodel2,
                 {
                   output$importance2 <- renderPrint(importance(rf1_mod))
                   output$summary2 <- renderPrint(summary(pred_rf1))
                   output$randomcm2 <- renderTable(randomcmtb2$table)
                   output$randomroc2 <- renderPlot(plot(auc_rf1, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(auc_rf1$auc[[1]],3)),col = 'red'))
                 })
    
    observeEvent(input$ana1,
                 {
                   output$scatter1g <- renderPlot(plot(rulea))
                 })
    
    observeEvent(input$ana2,
                 {
                 output$scatter2g <- renderPlot(plot(Rule1))
                 output$dist2g <- renderPlot(plot(Rule1, method = "graph", control = list(type = "items")))
                 output$paral2g <- renderPlot(plot(Rule1, method="paracoord", control=list(reorder=TRUE)))
                 })
    
    observeEvent(input$ana3,
                 {
                   output$scatter3g <- renderPlot(plot(Rule2))
                   output$dist3g <- renderPlot(plot(Rule2, method = "graph", control = list(type = "items")))
                   output$paral3g <- renderPlot(plot(Rule2, method="paracoord", control=list(reorder=TRUE)))
                 })
    observeEvent(input$Dwnld,
                 {
                   
                   output$dwn <- renderText(".csv files uploaded to Dropbox for manual analysis")
                   rulea_csv <- as(rulea.sorted, "data.frame")
                   write.csv(rulea_csv, "rulea.csv")
                   drop_upload("rulea.csv", dest = "Responses", dtoken = token)
                   Rule1_csv <- as(Rule1.sorted, "data.frame")
                   write.csv(Rule1_csv, "Rule1.csv")
                   drop_upload("Rule1.csv", dest = "Responses", dtoken = token)
                   Rule2_csv <- as(Rule2.sorted, "data.frame")
                   write.csv(Rule2_csv, "Rule2.csv")
                   drop_upload("Rule2.csv", dest = "Responses", dtoken = token)
                   output$dropdwn <- renderText("Follow the link to download : https://www.dropbox.com/sh/0dmjqkubclz482a/AADwlW_7B1LtAaITfvO-SUYQa?dl=0")
              
                   }
      
    )
  
    }
)


      