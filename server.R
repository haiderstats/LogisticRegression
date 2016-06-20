#server.R

shinyServer(function(input, output,session) {
  
  values <- reactiveValues(dataset = NULL)

  observeEvent(input$file, {
    values$dataset <- read.csv(input$file$datapath)
  })
  
  output$dependent <- renderUI({
    deps = names(values$dataset)
    validate(need(!is.null(deps), "Please upload a file to see more options."))
    deps = names(which(apply(values$dataset, 2, function(x) length(unique(x))) < 3))
    
    radioButtons("dependent", "Choose your dependent variable.", deps)
  })
  
  output$independentFactor <- renderUI({
    remove = c(input$dependent)
    indepsFactor = setdiff(names(values$dataset),remove)
    validate(need(!is.null(indepsFactor), ""))
    
    checkboxGroupInput("independentFactor", "Choose your independent factor variables.", indepsFactor)
    
  })
  
  
  output$independentContinuous <- renderUI({
    remove = c(input$dependent, input$independentFactor)
    indepsCont = setdiff(names(values$dataset),remove)
    validate(need(!is.null(indepsCont), ""))
    
    checkboxGroupInput("independentContinuous", label = "Choose your independent continuous variables.", indepsCont)
    
  })
  
  output$interactions <- renderUI({
    modelVars = c(input$independentContinuous, input$independentFactor)
    validate(need(!is.null(modelVars) ,""))
    
    if(length(modelVars)>1){
      theInteractions = c()
      for(i in 1:(length(modelVars)-1)){
        for(j in 1:(length(modelVars)-i)){
          add = paste(modelVars[i],modelVars[j+i], sep = "*")
          theInteractions = c(theInteractions, add)
        }
      }
      checkboxGroupInput("interactions", label = "Potential Interactions to Include", choices = theInteractions )
      
    }
    else{
    checkboxGroupInput("interactions", label = "", choices = NULL )
    }
  })
  
  output$model <- renderText({
    validate(need(!is.null(input$dependent), ""))
    
    dep = paste(input$dependent, '~')
    indeps = c(input$independentFactor, input$independentContinuous)
    modelVars = paste(indeps, collapse = " + ")
    interactions = input$interactions
    
    print(input$interactions)
    
    if(!is.null(interactions)){
      if(length(interactions) <2){
        theModel = paste(modelVars, interactions, sep = " + ")
      }
      else{
        interactions = paste(interactions, collapse = " + " )
        theModel = paste(modelVars, interactions, sep = " + ")
      }
      finalMod = paste(dep, theModel)
    }
    else{
      finalMod = paste(dep, modelVars)
    }
    

    
    finalMod

  })
  
  
  
  
})
