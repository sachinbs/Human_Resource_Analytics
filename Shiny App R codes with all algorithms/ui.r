library(shiny)

shinyUI(
  navbarPage("HR Analytics and Data Mining",
  tabPanel("Data Preprocessing",
  sidebarLayout(
    sidebarPanel(
            actionButton(inputId = "Preprocessing", label = "Preprocess the Data!")
    ),
    mainPanel(
      tableOutput("contents"),
      textOutput("prepro"),
      tableOutput("contents2")
    )
  )
  ),
  tabPanel("Data Exploration",
           sidebarLayout(
             sidebarPanel(
               actionButton(inputId = "analyzeSalary", label = "Analyze salary"),
               actionButton(inputId = "salesandsalary", label = "Sales and Salary Interaction"),
               actionButton(inputId = "promoted", label = "Number of People promoted"),
               actionButton(inputId = "promoted2", label = "Employees promoted vs salary"),
               actionButton(inputId = "promoted3", label = "Analysis of promotion, last 5 years"),
               actionButton(inputId = "timespent", label = "Time spent at the company"),
               actionButton(inputId = "promoted4", label = "No. of employees promoted and time spent"),
               actionButton(inputId = "projects", label = "No. of projects and employees left"),
               actionButton(inputId = "satisfaction", label = "Satisfaction vs improper evaluation"),
               actionButton(inputId = "satisfaction1", label = "Satisfaction level vs Salary"),
               actionButton(inputId = "salary", label = "Time spent vs Salary"),
               actionButton(inputId = "left", label = "Employees who left"),
               actionButton(inputId = "visualization", label = "Departments and salary paid")
             ),
             mainPanel(
               tableOutput("Salary"),
               plotOutput("graph1"),
               textOutput("observation1"),
               tableOutput("promotedtb"),
               plotOutput("promotedg"),
               plotOutput("promotedg2"),
               tableOutput("timespenttb"),
               plotOutput("promoted4g"),
               textOutput("observation2"),
               plotOutput("projectsg"),
               textOutput("observation3"),
               plotOutput("satisfactiong"),
               textOutput("observation4"),
               plotOutput("satisfaction1g"),
               textOutput("observation5"),
               plotOutput("salaryg"),
               textOutput("observation6"),
               tableOutput("lefttb"),
               plotOutput("visualizationg")
             )
           )
           
           
  ),
  
  tabPanel("Decision Tree",
           sidebarLayout(
             sidebarPanel(
               actionButton(inputId = "model11", label = "Model 1 - C 5.0 Tree"),
               actionButton(inputId = "model22", label = "Model 2 - CPART Tree"),
               actionButton(inputId = "model33", label = "Model 3 - CPART Tree" ),
               actionButton(inputId = "model44", label = "Model 4- CPART Tree")
             ),
             mainPanel(
               textOutput("Model1text"),
               tableOutput("Model1cm"),
               textOutput("Model1rm1"),
               plotOutput("model1auc1"),
               textOutput("Model2text"),
               textOutput("helptext2"),
               textOutput("auc2"),
               textOutput("auc3"),
               plotOutput("model22g1"),
               plotOutput("model22g2"),
               plotOutput("model22g3"),
               tableOutput("model22cp"),
               textOutput("Model3text"),
               textOutput("helptext3"),
               textOutput("auc4"),
               textOutput("auc5"),
               plotOutput("model33g1"),
               plotOutput("model33g2"),
               tableOutput("model33cp"),
               textOutput("Model4text"),
               textOutput("helptext4"),
               textOutput("auc6"),
               textOutput("auc7"),
               plotOutput("model44g1"),
               plotOutput("model44g2"),
               tableOutput("model44cp")
             )
           )
  ),
  
  tabPanel("Naive Bayes",
           sidebarLayout(
             sidebarPanel(
               
               #actionButton(inputId = "naivemodel", label ="Display the Model"),
               actionButton(inputId = "probtable", label = "Probability of salary wr to people left"),
               actionButton(inputId = "probplot", label = "Comparing various attributes - employees left"),
               actionButton(inputId = "naivecm", label = "Confusion Matrix and ROC Plot for training set"),
               actionButton(inputId = "naivecm2", label = "Confusion Matrix and ROC Plot for testing data")
             ),
             mainPanel(
               textOutput("naivemodeltext"),
               tableOutput("bayestable"),
               plotOutput("naivep1g"),
               plotOutput("naivep2g"),
               plotOutput("naivep3g"),
               plotOutput("naivep4g"),
               plotOutput("naivep5g"),
               tableOutput("naivecmtb"),
               plotOutput("rocplot"),
               tableOutput("naivecmtb1"),
               plotOutput("rocplot1")
               
             )
           )
           
  ),
  
  tabPanel("Random Forest",
           sidebarLayout(
             sidebarPanel(
               
               actionButton(inputId = "randommodel1", label = "importance, summary, confusion matrix for model 1"),
               actionButton(inputId = "randommodel2", label = "importance, summary, confusion matrix for model 2")
               
               
             ),
             mainPanel(
               
               textOutput("importance1"),
               textOutput("summary1"),
               tableOutput("randomcm1"),
               plotOutput("randomroc1"),
               textOutput("importance2"),
               textOutput("summary2"),
               tableOutput("randomcm2"),
               plotOutput("randomroc2")
               
               
             )
           )),
  
  tabPanel("Association Analysis",
           sidebarLayout(
             sidebarPanel(
               
               actionButton(inputId = "ana1", label = "Analysis- Rules for the entire dataset"),
               actionButton(inputId = "ana2", label = "Analysis - Characteristics of employees who are still working"),
               actionButton(inputId = "ana3", label = "Analysis - characteristics of employees who have left the company"),
               actionButton(inputId = "Dwnld", label = "Download")
               
             ),
             
             mainPanel(
               
               plotOutput("scatter1g"),
               plotOutput("scatter2g"),
               plotOutput("dist2g"),
               plotOutput("paral2g"),
               plotOutput("scatter3g"),
               plotOutput("dist3g"),
               plotOutput("paral3g"),
               textOutput("dwn"),
               textOutput("dropdwn")
               
               
             )
           ))
)
)
