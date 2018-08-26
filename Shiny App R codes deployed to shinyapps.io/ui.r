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
