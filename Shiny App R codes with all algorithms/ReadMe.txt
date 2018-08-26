This shiny app code includes all the algorithms, including Decision Trees, Naive Bayes and Random Forest 
which are not present in the app that has been deployed.

We have used the following CRAN packages in our project - caret, rattle and e1071, which have reverse 
dependency on RGtk2 package. 
RGtk package is not supported by Shiny - rsconnect for the simple reason that, it doesn't have a native GUI. 
This Shiny App works very well locally but fails to launch on shinyapps.io.  
