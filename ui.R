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
           conditionalPanel(
             condition  = "input.independentFactor != null && input.independentFactor.length > 1",
           textInput("interactions","Please enter interactions you would like to observe in X*Y format.", value = ""),
           actionButton("addInteraction", label = "Enter"))
           
           
           
    ),
    column(9,
           h1("Your Current Model"),
           hr(),
           h3(textOutput("model")))
  )
))