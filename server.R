#server.R
library(ReporteRs)
shinyServer(function(input, output,session) {
  
  values <- reactiveValues(dataset = NULL)

  #Read in the data and determine which ones are binary and offer those as potential variables to be analyzed.
  data = eventReactive(input$file, {
    values$dataset <- read.csv(input$file$datapath)
    deps = names(which(apply(values$dataset, 2, function(x) length(unique(x))) < 3))
    names(values$dataset)[-which(deps == names(values$dataset))]
    
  })
  
  #Create the UI for the dependent variable portion. Supply error message if no data is present.
  output$dependent <- renderUI({
    deps = names(values$dataset)
    validate(need(!is.null(deps), "Please upload a file to see more options."))
    deps = names(which(apply(values$dataset, 2, function(x) length(unique(x))) < 3))
    
    radioButtons("dependent", "Choose your dependent variable.", deps)
  })
  
  #When data gets loaded in list the factor vairables and the indepedent variables.
  observeEvent(data(),{
    output$independentFactor <- renderUI({
      checkboxGroupInput("independentFactor", "Choose your independent factor variables.", data())
    })
    
    output$independentContinuous <- renderUI({
      checkboxGroupInput("independentContinuous", "Choose your independent continuous variables.", data())
    })
  })
  
  #The following two function lay out which variables have been selected in which category.
  #If all of the variables are in one category we remove the option and return the empty string (which effectively)
  #removes it from the UI.
  contVars = eventReactive(input$independentFactor,{
    validate(need(input$file, ""))
    if(setequal(names(values$dataset), c(input$dependent, input$independentFactor))){
      return("")
    }
    else{
      return(setdiff(names(values$dataset), c(input$dependent, input$independentFactor)))
    }
  }, ignoreNULL = FALSE)
  
  factorVars = eventReactive(input$independentContinuous,{
    validate(need(input$file, ""))
    if(setequal(names(values$dataset), c(input$dependent, input$independentContinuous))){
      return("")
    }
    else{
      return(setdiff(names(values$dataset), c(input$dependent, input$independentContinuous)))
    }
    
  }, ignoreNULL = FALSE)
  
  #If all variables are in one category we need to remove the other category (factor or cont.) from the UI
  #However, if a variable is unselected the UI needs to refresh. The next two functions handle this for us.
  observeEvent(contVars(),{
    if(contVars()[1] == ""){
      output$independentContinuous = renderUI({
        checkboxGroupInput("independentContinuous","", choices = NULL)
      })
    }
    else{
      updateCheckboxGroupInput(session, "independentContinuous", "Choose your independent continuous variables.", contVars(), selected = input$independentContinuous)
    }
  })
  
  observeEvent(factorVars(),{
   if(factorVars()[1] == ""){
     output$independentFactor = renderUI({
       checkboxGroupInput("independentFactor","", choices = NULL)
     })
   }
    else{
      updateCheckboxGroupInput(session, "independentFactor", "Choose your independent factor variables.", factorVars(), selected = input$independentFactor)
    }
  })
  
  #We need to allow to have the abillity to have interactions in our model. To do this we need to loop
  #over all the variables and paste in a * sign since glm uses * for interaction.
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
  
  #This function will render our model as we add variables. This way the user can see the model as they build it.
  output$model <- renderText({
    validate(need(!is.null(input$dependent), ""))
    
    dep = paste(input$dependent, '~')
    indeps = c(input$independentFactor, input$independentContinuous)
    modelVars = paste(indeps, collapse = " + ")
    interactions = input$interactions

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
    
    model <<- finalMod
    finalMod

  })
  
  #This will build our model so we can get some analytical results.
  buildModel = eventReactive(input$go,{
    
    #Runs through thefactor variables for the data set
    for(i in 1:length(input$independentFactor)){
      index = which(input$independentFactor[i] == names(values$dataset))
      values$dataset[,index] = factor(values$dataset[,index])
    }
    
    #Grabs the continuous variables 
    for(i in 1:length(input$independentContinuous)){
      index = which(input$independentContinuous[i] == names(values$dataset))
      values$dataset[,index] = as.numeric(as.character(values$dataset[,index]))
    }
    
    print(values$dataset)
    theModel =glm(model, family = binomial, data = values$dataset)
    modelSummary = summary(theModel)
    summaryTable = vanilla.table(round(modelSummary$coefficients, digits = 3), add.rownames = T)
    
    return(HTML(as.html(summaryTable)))
    
  })
  
  
  #This is where we are gonna do a lot of the extra stuff. This will give us our AIC and residuals as of now.
  buildModelExtras = eventReactive(input$go,{
    for(i in 1:length(input$independentFactor)){
      index = which(input$independentFactor[i] == names(values$dataset))
      values$dataset[,index] = factor(values$dataset[,index])
    }
    for(i in 1:length(input$independentContinuous)){
      index = which(input$independentContinuous[i] == names(values$dataset))
      values$dataset[,index] = as.numeric(as.character(values$dataset[,index]))
    }
    theModel =glm(model, family = binomial, data = values$dataset)
    modelSummary = summary(theModel)
    modelExtras= data.frame(modelSummary$aic, modelSummary$df.residual, modelSummary$deviance)
    colnames(modelExtras) = c("AIC","DF", "Deviance")
    tableExtras = vanilla.table(modelExtras)
    return(HTML(as.html(tableExtras)))
    
  })
  
  output$modelResults = renderUI({
    buildModel()
  })
  
  output$modelExtras = renderUI({
    buildModelExtras()
  })
  
  
})
