# Reading in data
ppdata <- read.csv("../foo1.csv")

##### Pre-Processing ##################################################################
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
plot(rulea)
plot(Rule1)
plot(Rule2)

# Plotting distribution graph for rules
# Takes time (because of large number of rules)
# plot(rulea, method = "graph", control = list(type = "items"))
plot(Rule1, method = "graph", control = list(type = "items"))
plot(Rule2, method = "graph", control = list(type = "items"))

# Plotting Parallel co-ordinate plots
# Takes time (because of large number of rules)
# plot(rulea.sorted, method="paracoord", control=list(reorder=TRUE))
plot(Rule1, method="paracoord", control=list(reorder=TRUE))
plot(Rule2, method="paracoord", control=list(reorder=TRUE))

# Saving all rules to csv for manual analysis
rulea_csv <- as(rulea.sorted, "data.frame")
write.csv(rulea_csv, "rulea.csv")
Rule1_csv <- as(Rule1.sorted, "data.frame")
write.csv(Rule1_csv, "Rule1.csv")
Rule2_csv <- as(Rule2.sorted, "data.frame")
write.csv(Rule2_csv, "Rule2.csv")