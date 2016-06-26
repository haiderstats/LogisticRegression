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
           
           # checkboxGroupInput("independentFactor", "Choose your independent factor variables.", "" ),
          # checkboxGroupInput("independentContinuous", label = "Choose your independent continuous variables.", "" ),
           uiOutput("interactions"),
           actionButton("go", "Build The Model")
           
           
    ),
    column(9,
           h1("Your Current Model"),
           hr(),
           h3(textOutput("model")),
           hr(),
           br(),
           uiOutput("modelResults"),
           uiOutput("modelExtras")
           
  )
)))