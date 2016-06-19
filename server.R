#server.R

shinyServer(function(input, output) {
  
  values <- reactiveValues(dataset = NULL)
  
  observeEvent(input$file, {
    values$dataset <- read.csv(input$file$datapath)
  })
  
  output$dependent <- renderUI({
    deps = names(values$dataset)
    validate(need(!is.null(deps), "Please upload a file to see more options."))

    radioButtons("dependent", "Choose your dependent variable.", deps)
  })
  
  output$independent <- renderUI({
    remove = input$dependent
    indeps = setdiff(names(values$dataset),remove)
    
    validate(need(!is.null(indeps), ""))
    
    checkboxGroupInput("independent", "Choose your independent variables.", indeps)
  })
})
