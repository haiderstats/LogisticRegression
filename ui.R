#ui.R

library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  title = "Interactive Logistic Regression",
  
  fluidRow(
    column(3,
           h2("Interactive Logistic Regression"),
           fileInput("file", "Upload file"),
           
           uiOutput("dependent"),
           uiOutput("independentFactor"),
           uiOutput("independentContinuous"),
           uiOutput("interactions")
          
           
    ),
    column(9,
           h1("Your Current Model"),
           hr(),
           h3(textOutput("model")))
  )
))