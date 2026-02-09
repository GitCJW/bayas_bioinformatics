
init_model_selection_function <- function(input, output, session, dataModel){

  runningId <<- 0

  shinyjs::addClass("selectStanModelAvailable", class="borderColor-primary")

  #Used to let the change prior button now, for which the new prior is set to parameter
  global.selectPriorDistribution <<- list()
  button_trigger <<- reactiveValues()
  god <<- reactiveVal(F)

  observeOptTermAddButton <- T

  modalIdObj <<- ModalID$new()

  
  #Used to show some information about this model
  selectedModel <- reactiveVal(NULL)

  observeEvent(input$selectStanModelAvailable, ignoreNULL = F, ignoreInit = T, {

    selectedStanModel <- input$selectStanModelAvailable
    
    #Remove current selected model
    if(is.null(selectedStanModel) || selectedStanModel == ""){
      output$formulaOutput <- renderUI(tags$div())
      output$previewDataSelectModelLeft <- renderUI(tags$div())
      output$previewDataSelectModelRight <- renderUI(tags$div())
      shinyjs::removeClass("panelSelectModel", class="borderColor-primary")
      return()
    }


    all_models <- get_all_stan_models()
    stanModelLists <- filter_stan_models_2(stan_models = all_models, dataModel) 
    
    #Only to show information about this model
    selectedModel(selectedStanModel)
    
    for(models in all_models){
      if(models$display_name == selectedStanModel){
        
        #Available or not?
        if(!models$id %in% stanModelLists$no_fit_models){
          tmp_in <- models
          # tmp_in$myDataModel <- dataModel
          tmp_in$myPerIterationDataModel <- dataModel$get.cPerIterationDataModel()
          
          dataModel$get.cPerIterationDataModel()$set.selected_BAYSIS_stan_model(tmp_in)
          break
        }else{
          return()
        }
      }
    }

    #Adjust prior parameters based on users input data
    dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParameters()

    #New Formula view
    buildFormula(input, output, session, dataModel)

    #Remove the highlight to end focus of select model and highlight model formula
    shinyjs::removeClass("selectStanModelAvailable", class="borderColor-primary")
    shinyjs::addClass("panelSelectModel", class="borderColor-primary")
  })

  # Add observer for predictor
  addPredictor(input, output, session, dataModel)


  observe({
    sM <- selectedModel()
    if(is.null(sM) || sM==""){
      if(!is.null(input$selectStanModelAvailable) &&
         input$selectStanModelAvailable!=""){
        sM <- input$selectStanModelAvailable
      }
    }
    isolate({
      all_models <- get_all_stan_models()

      infoText <- ""
      if(!is.null(sM)){
        for(models in all_models){
          if(models$display_name == sM){
            infoText <- models$description
            break;
          }
        }
      }
      #Update information of seleected model
      updateTextAreaInput(session=session, inputId="selectStanModelNotAvailableInfoBox",
                          value=infoText)
    })
  })


  # changes in long-format, refresh of stan model list
  observe({
    
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("longFormat")
    
    isolate({
      # Deselect selected stan model, otherwise it could cause misbehaviour
      updateSelectInput(session = session, inputId = "selectStanModelAvailable",
                        selected = "")
  
      ## Create variables overview. Against the upload var overview, the var properties cannot be changed.
      refreshStanModelList(input, output, session, dataModel)
    })
  })


  # changes in input properties, refresh of stan model list
  observe({
    dMID <- dataModel$getDataModelInputData()
    dMID$dependReactiveValue("inputProperties")
    
    
    isolate({
      # Update ui-elements
      refreshParametersOverview(input, output, session, dataModel)
      refreshStanModelList(input, output, session, dataModel)
      
      #Remove the selected model only if the properties of the variables used in the formula change.
      # Deselect selected stan model, otherwise it could cause misbehaviour
      updateSelectInput(session = session, inputId = "selectStanModelAvailable",
                        selected = "")

      
    })
  })

  
  #When the selected baysis model changes
  observe({
    
    cPIDM <- dataModel$get.cPerIterationDataModel()
    cPIDM$dependReactiveValue("selected_BAYSIS_stan_model")
  
    isolate({
      
      baysis <- cPIDM$get.selected_BAYSIS_stan_model()
      if(is.null(baysis)){
        # Deselect selected stan model, otherwise it could cause misbehaviour
        updateSelectInput(session = session, inputId = "selectStanModelAvailable",
                          selected = "")
      }
    })
    
  })
  
  
  observeEvent(input$btnModelSelectionNext, {
    updateNavbarPage(session, "navbar", selected = "Model fitting") 
  })
  
  observeEvent(input$btnModelSelectionBack, {
    updateNavbarPage(session, "navbar", selected = "Data visualization") 
  })

}


## Refresh parameters overview, Variable name, characteristic etc.
refreshParametersOverview <- function(input, output, session, dataModel){
 
  dMID <- dataModel$getDataModelInputData()
  inputProp <- NULL
  if(!is.null(dMID)) inputProp <- dMID$getInputProperties()
  
  if(!is.null(inputProp)){
    tInp <- t(inputProp)
    tInp <- tInp[-1,,drop=F]
    tInp[4, tInp[4,] == "TRUE"] <- as.character(icon(style = "", "check"))
    tInp[4, tInp[4,] == "FALSE"] <- ""
    rownames(tInp) <- c("<b>Type</b>", "<b>Lower limit</b>", "<b>Upper limit</b>", "<b>Response</b>")
    
    output$parametersOverviewTable <- renderDataTable(tInp, escape = F, options=list(paging=F,searching=F, info=F, ordering=F), rownames=T) 
  }else{
    output$parametersOverviewTable <- renderDataTable(NULL) 
  }
  
}



## data_frame_variables contain the users input variable. First row the response variable.
## Returns the data frame containing the used (checkbox=TRUE) variabels
refreshStanModelList <- function(input, output, session, dataModel){
  users_var <- dataModel$get.cPerIterationDataModel()$get.users_variables()
  numberVar <- length(users_var$response)
  
  dMID <- dataModel$getDataModelInputData()
  inpProp <- dMID$getInputProperties()
  
  if(!is.empty(inpProp) && dim(inpProp)[1] > 0){
    
    # Filter stan model on all available models
    stan_models <- get_all_stan_models()
    stanModelLists <- filter_stan_models_2(stan_models = stan_models, dataModel) 
    flog.debug(paste0("fit: ", stanModelLists$fit_models))
    flog.debug(paste0("nofit: ", stanModelLists$no_fit_models))
    
    # Update Stan Model lists
    # fit_models
    # no_fit_models
    
    choices <- c()

    i <- 1
    for(index in stanModelLists$fit_models){
      choices <- c(choices,stan_models[[index]]$display_name)
      flog.debug(stan_models[[index]]$display_name)
    }
    rec <- c()
    for(index in stanModelLists$not_recommended){
      rec <- c(rec,stan_models[[index]]$display_name)
      names(rec) <- rec
    }
    names(choices) <- choices
    if(length(rec)>0){
      finalChoices <- list('Choose a model'="", 'recommended'=choices, 'not recommended'=rec)
      choices <- c(choices,rec)
    }else{
      finalChoices <- list('Choose a model'="",'recommended'=choices)
    }
    # if(length(choices) == 0) finalChoices <- character(0)
    # When the previous selected Stan model are not more available, deselect it in the data model
    selected <- input$selectStanModelAvailable
    if((!is.null(selected)) && (!selected %in% choices)){
      selected <- ""
      dataModel$get.cPerIterationDataModel()$set.selected_BAYSIS_stan_model(NULL)
    } 
    if(is.null(selected)) selected <- ""

    choices <- rep("",length(stanModelLists$no_fit_models))
    i <- 1
    for(index in stanModelLists$no_fit_models){
      choices[i] <- stan_models[[index]]$display_name
      i <- i+1
      flog.debug(stan_models[[index]]$display_name)
    }

    finalChoices <- list.append(finalChoices, choices, 'Not available')
    updateSelectInput(session = session, inputId = "selectStanModelAvailable", 
                      choices = finalChoices, selected = selected)
  }
}


## Build the formula and its functionality, like adding predictors or changing priors.
buildFormula <- function(input, output, session, dataModel){

  
  #panelSelectModel
  stanModel <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()
  dMID <- dataModel$getDataModelInputData()
  responseVariable <- dMID$getResponseVariable(onlyName=T)
  formula_elements <- stanModel$new_formula(responseVariable)
  
  
  rows <- list()
  tagCount <- 1

  #Iterate through "FormulaElements" objects
  for(element in formula_elements){
    # Add Predictor, Prior Button?
    uiRightSide <- ""
    if(element$predictorLine){
      add2 <- actionButton("addPredictor", label = "+ Predictor" , class= "btn-primary", 
                           style="height:25px;margin:0px 0px 0px 20px;padding:0px 2px 0px 2px;")
      uiRightSide <- bslib::tooltip(
          trigger = h6(
            id="howToRemoveParameter", 
            "How to remove predictors?",
            style="cursor:pointer; width: -moz-fit-content;"),
          HTML(tooltip$howToRemovePredictor),
          options = list(trigger="hover")
        )
    }else if(element$addPriorButton){
      t <- element$myRemovableParameter
      uiRightSide <- actionButton(class= "btn-primary", paste0("setPrior",element$myRemovableParameter$modalID), 
                                  HTML(paste0("Set prior/value for ", element$myRemovableParameter$getFullDisplayName())),
                                  style="height:25px;margin:2px;padding:2px;")
    }

    math <- ""
    if(element$predictorLine){
      math_divs <- c()
      index <- 1
      for(pred in stanModel$get_predictor_line()){
        para <- pred$modelParameter
        #e.g. intercept that is displayed by just the parameter (b0)
        if(is.null(pred$display_name)){
          if(index > 1){
            mathString <- tags$div(" + ", tags$span(para$getFullDisplayName(),class="formulaParameter"), 
                                           style="display:inline;")
          }else{
            mathString <- tags$div(tags$span(para$getFullDisplayName(),class="formulaParameter"), 
                                           style="display:inline;")
          }
        }else{
          if(index > 1){
            #right side of = ~ ... with more than one term
            mathString <- tags$div(" + ", HTML(paste0(tags$span(para$getFullDisplayName(),class="formulaParameter"), "&sdot;", pred$display_name)),
                                                                           style="display:inline;")
          }else{
            #right side of = ~ ... with just one term
            mathString <- tags$div(HTML(paste0(tags$span(para$getFullDisplayName(),class="formulaParameter"), "&sdot;", pred$display_name)), 
                                                                           style="display:inline;")
          }
        }
        # inputId <- paste0("parameter",para$id)
        if(is.null(para$modalID)) para$modalID <- modalIdObj$getNext()
        inputId <- paste0("parameter",para$modalID)
        if(pred$optional){
          #This div is removable
          math_divs[[index]] <- tags$div(id = inputId, mathString,
                                         style="display:inline;cursor:url(Images/Icons/Mouse_remove_icon4.png) 6 9, auto;") #cursor:url(Images/Icons/Mouse_remove_icon4.png) 6 9, auto;

          #Add observer for removing this div
          removePredictor(input, output, session, dataModel, inputId, pred)
          
        }else{
          #This div is not removable
          math_divs[[index]] <- tags$div(id = inputId, mathString, 
                                         style="display:inline;") 
        }
        index <- index+1
      }
      math <- tags$div(element$rightSide, math_divs, add2)
    }else{
      #add observeEvent for "Set prior" by clicking on the formula element. Same as clicking on the "Set prior" button.
      if(element$addPriorButton){
        math <- tags$div(id = paste0("setPrior2",element$myRemovableParameter$modalID),element$rightSide, style="display:inline; cursor:pointer;")
        setPriorForParameter(input, output, session, dataModel, element$myRemovableParameter)
      }else{
        tmpId <- modalIdObj$getNext()
        math <- tags$div(id = paste0("setPrior2",tmpId),element$rightSide, style="display:inline;")
      } 
      
    }
    
    #Row to be added
    rows[[tagCount]] <- tags$div(
      fluidRow(
        column(3, 
               
               bslib::tooltip(
                 trigger = tags$span(id=paste0("formulaLeft",tagCount), element$leftSide, style="text-align:right;"),
                 HTML(element$description),
                 options = list(trigger="hover", delay=list(show= 500, hide= 100))
               ),
               
               tags$span(element$center, style="text-align:center;padding:2px;margin:0px;"),
               style="text-align:right;padding:2px;margin:0px; overflow-x:clip;"),
        column(5, tags$div(math, style = "margin:0px;"), style="padding:2px;"), #9
        column(4, uiRightSide)))

    tagCount <- tagCount+1
  }
  
  #Changing formula ui
  output$formulaOutput <- renderUI({
    tags$div(tagList(rows), 
             class = "borderColor-regular",
             style = "border:none; border-top:1px solid; box-shadow:none; border-radius:5px;")})
  
  #Model description
  ret <- stanModel$getExplanation(responseVariable)
  
  if(is.null(ret)){
    output$previewDataSelectModelLeft <- renderUI(tags$div("Empty"))
    output$previewDataSelectModelRight <- renderUI(tags$div("Empty"))
  }else{
    
    wellStyle <- "border:none; border-top:1px solid; box-shadow:none;"
    
    checklistMust <- ret$checklistMust
    checklistCan <- ret$checklistCan
    
    importance <- ret$importance
    legend <- ret$legend
    
    runningId <<- runningId+1
    
    checklist_ui_must <- modelExplanationUI(id="modelFormulaChecklistMust",checklistMust, importance=T, output=output)
    checklist_ui_can <- modelExplanationUI(id="modelFormulaChecklistCan",checklistCan, importance=F, output=output)
    legend_ui <- modelLegendUI(id=paste0("modelFormulaLegend_",runningId), legend, output=output)
    
    fin <- tags$div(checklist_ui_must,
                    checklist_ui_can,
                    legend_ui)
    
    output$previewDataSelectModelLeft <- renderUI(fin)
    info <- paste0(h5("Description"))
    output$previewDataSelectModelRight <- renderUI(
      wellPanel(
        class="getActiveColor borderColor-regular", 
        style="text-align:justify; overflow:auto;", 
        style=wellStyle, 
        HTML(info,"Click on a key point for further information.")))
  }
}

#When user add a predictor
addPredictor <- function(input, output, session, dataModel){
  
  observeEvent(input$addPredictor, ignoreInit=T, {
    showModal(modalDialog(
      footer = tags$div(
        style="display:flex; width: 100%;",
        tags$div(
          style = "flex:1",
          tags$div(actionButton("exitAddPredictorModal","Cancel"), style="float:left;")),
        tags$div(
          style = "flex:1; text-align:right;",
          actionButton("FinallyAddPredictor","Add", class="btn-primary")
        )
      ),
      size = "m", easyClose=F,
      add_new_predictor(dataModel, input$selectInputAvailPred) #UiModelSelection
    ))
  })


  # Check number of variables from stanModel (avail_predictor)
  observeEvent(input$selectInputAvailPred, {
    availPred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$avail_predictor
    numberOfItems <- 0
    for(pred in availPred){
      if(pred$name == input$selectInputAvailPred) numberOfItems <- max(pred$numberVar)
    }
    updateSelectizeInput(session, "selectInputAvailVar", options = list(maxItems = numberOfItems))
  })
  
  
  observeEvent(input$selectInputAvailVar, ignoreNULL = F,{
    selectedVars <- input$selectInputAvailVar
    uiElements <- list()
    if(length(selectedVars) > 1){
      uiElements <- list.append(uiElements, tags$span("You are using an interaction. For single terms, add each variable particularly. Use keys 'Delete' or 'Backspace' for removing variables."))
    }
    flag <- T
    availPred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$avail_predictor
    for(pred in availPred){
      if(pred$name == input$selectInputAvailPred){
        #Check if any variable is selected
        if(pred$numberVar[1] != 0 && length(input$selectInputAvailVar) == 0){
          uiElements <- list.append(uiElements, tags$span("Please select your variable for single terms or more variables for interactions!"))
          flag <- F
          break;
        }
      } 
    }
    if(flag){
      preds <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$used_predictor
      for(i in preds){
        if(setequal(i$userVariable,input$selectInputAvailVar)){
          if(length(i$userVariable) == 1){
            uiElements <- list.append(uiElements, tags$span("This variable is already in use!", class = "fontColor-warning"))
          }else{
            uiElements <- list.append(uiElements, tags$span("This interaction is already in use!", class = "fontColor-warning"))
          }
          break;
        } 
      }
    }
    
    
    output$AddPredictorMessageBox <- renderUI(tags$div(uiElements, style="word-wrap: break-word; word-break:break-word;"))
  })
  
  # Adding a new predictor
  observeEvent(input$FinallyAddPredictor, ignoreInit=T, {
    ignore <- F
    
    #Check if any variable is selected and decrease possible amount
    availPred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$avail_predictor
    for(pred in availPred){
      if(pred$name == input$selectInputAvailPred){
        #Check if any variable is selected
        if(pred$numberVar[1] != 0 && length(input$selectInputAvailVar) == 0){
          output$AddPredictorMessageBox <- renderUI(
            tags$div(
              tags$span("Please select your variable(s)!"),
              class = "fontColor-warning",
              style= "word-wrap: break-word; word-break:break-word;"))
          ignore <- T
          break;
        }
      }
    }

    #Check if selected variables are a single categorical element
    if(!ignore){
 
      vars <- input$selectInputAvailVar
      # Are all vars categorical?
      dMId <- dataModel$getDataModelInputData()
      allCat <- sapply(vars, function(var) dMId$getInputProperty(var, type = "type") == characteristicEnum()$Categorical)
      
      if(!is.empty(allCat) && all(allCat)){
        data <- dMId$getLongFormatVariable(vars, completeCases=T)
        inter <- unique(interaction(data))
        if(length(inter) < 2){
          if(length(vars) == 1){
            output$AddPredictorMessageBox <- renderUI(
              tags$div(
                tags$span("This variable has only one single element and will be redundant."), 
                class = "fontColor-warning",
                style= "word-wrap: break-word; word-break:break-word;"))
          }else{
            output$AddPredictorMessageBox <- renderUI(
              tags$div(
                tags$span("This interaction has only one single element and will be redundant"), 
                class = "fontColor-warning",
                style= "word-wrap: break-word; word-break:break-word;"))
          }        
          ignore <- T
        }
      }
    }
    
    #Check if the selected variable(s) are already used as a term
    if(!ignore){
      preds <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$used_predictor
      for(i in preds){
        if(setequal(i$userVariable,input$selectInputAvailVar)){
          if(length(i$userVariable) == 1){
            output$AddPredictorMessageBox <- renderUI(
              tags$div(
                tags$span("This variable is already in use! Select another variable or interaction."), 
                class = "fontColor-warning",
                style= "word-wrap: break-word; word-break:break-word;"))
          }else{
            output$AddPredictorMessageBox <- renderUI(
              tags$div(
                tags$span("This interaction is already in use! Select another variable or interaction."), 
                class = "fontColor-warning",
                style = "word-wrap: break-word; word-break:break-word;"))
          }
          ignore <- T
          break;
        }
      }
    }
    
    #ignore, when user selected no needed variable
    if(!ignore){
      
      output$AddPredictorMessageBox <- renderUI(
        tags$div(
          tags$span(""), 
          class = "fontColor-warning",
          style= "word-wrap: break-word; word-break:break-word;"))
      
      #Add predictor --> changes in dataModel...
      pred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$create_predictor_by_name(input$selectInputAvailPred)
      
      # Set the type of prior distribution to selected prior distribution of the same type of predictor
      list_of_pred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$get_predictor_of_type(pred$name)
      if(length(list_of_pred) > 0){
        #if the added predictor is no vector
        pred$modelParameter$distribution <- list_of_pred[[1]]$modelParameter$distribution$getInstance()
      } 

      #No intercept?
      if(!is.null(pred$display_name)){
        pred$addVariable(input$selectInputAvailVar)
        
        #Just numerical, or is one of the var categorical?
        if(characteristicEnum()$Categorical %in% dataModel$getDataModelInputData()$getOtherVariables(varName=input$selectInputAvailVar)$type){
          pred$set.is.vector(T)
        }else{
          pred$set.is.vector(F)
          pred$modelParameter$distribution$element_name <- input$selectInputAvailVar
        }
      } 

      #add predictor
      dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$add_predictor(pred)
      dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParametersOfPredictor(pred)
      
      #Update formula
      buildFormula(input, output, session, dataModel)
      
      #Close modal
      removeModal(session)
      
      #Edit button label from "+ Predictor/Intercept" to "+ Predictor"
      if(input$selectInputAvailPred=="Intercept")
        updateActionButton(session=session, "addPredictor", label="+ Predictor")
    }
  })
  
  
  #close add predictor modal
  observeEvent(input$exitAddPredictorModal, {
    removeModal(session)
  })
  
}


#Remove predictor by an user mouse event e.g. "click" or "dblclick"
removePredictor <- function(input, output, session, dataModel, name, pred){

  # Interesting! 
  # The following line have to be there, otherwise removing the intercept a second (etc.) time leads to a strange behavior.
  # Instead of removing the intercept, another parameter will removed.
  pred$id

  shinyjs::onevent("click", name, {
 
    for(i in global.selectPriorDistribution){
      i$destroy()
    }

    #Remove from model
    dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$remove_predictor(pred)

    #Reindex left predictors 
    dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$reindex_predictors()

    #Edit elements of vectors
    dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$revisePredictors()
    
    #Rebuild formula
    buildFormula(input, output, session, dataModel)

    if(!dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$is_predictor_used()){
      updateActionButton(session=session, "addPredictor", label="+ Predictor/Intercept")
    }
  })
}

# Building the ui for setting a new prior
setPriorForParameter <- function(input, output, session, dataModel, para){

  
  if(is.null(para$modalID)) para$modalID <- modalIdObj$getNext()
  id <- paste0("setPrior", para$modalID)
  id2 <- paste0("setPrior2", para$modalID)
  id3 <- paste0("setPrior2", para$modalID)
  
  
  shinyjs::onevent("click", id2, {
    if(is.null(button_trigger[[paste0("click",id2)]])){
      button_trigger[[paste0("click",id2)]] <<- T
    }else{
      if(button_trigger[[paste0("click",id2)]]){
        button_trigger[[paste0("click",id2)]] <<- F
      }else{
        button_trigger[[paste0("click",id2)]] <<- T
      }
    }
  })
  
  if(!is.null(global.selectPriorDistribution[[id]])){
    global.selectPriorDistribution[[id]]$destroy()
  }
  global.selectPriorDistribution[[id]] <<- observeEvent(input[[id]], ignoreInit=T, {
    if(is.null(button_trigger[[paste0("click",id2)]])){
      button_trigger[[paste0("click",id2)]] <<- T
    }else{
      if(button_trigger[[paste0("click",id2)]]){
        button_trigger[[paste0("click",id2)]] <<- F
      }else{
        button_trigger[[paste0("click",id2)]] <<- T
      }
    }
  })
  
  if(!is.null(global.selectPriorDistribution[[id3]])){
    global.selectPriorDistribution[[id3]]$destroy()
  }
  global.selectPriorDistribution[[id3]] <<- observeEvent(button_trigger[[paste0("click",id2)]], ignoreInit=T, {
    removePriorModalView(dataModel, input, session)
    showModal(modalDialog(
      footer = tags$div(
        style="display:flex; width: 100%;",
        tags$div(
          style = "flex:1",
          tags$div(actionButton("exitDistributionModal","Cancel"), style="float:left;")),
        tags$div(
          style = "flex:1; text-align:right;",
          actionButton("finallyChangePrior","Confirm", class="btn-primary")
        )
      ),
      
      size = "xl", easyClose=F,
      
      createPriorModalView(input,dataModel,para) #UiModelSelection
    ))
    # print("create modal observers")
    #Add observer for change prior button
    changePriorObserver(input, output, session, dataModel, para)
  })
}


# used
removePriorModalView <- function(dataModel, input, session){
  # print("remove observers...")
  global_names <- names(global.selectPriorDistribution)
  for(i in global_names){
    if(!startsWith(i, "setPrior")){
      global.selectPriorDistribution[[i]]$destroy()
      global.selectPriorDistribution[[i]] <<- NULL
    } 
  }
}

#Recommended button for prior aux parameter
#observer for finally change prior button
aux_names <<- c()
changePriorObserver <- function(input, output, session, dataModel, para){

  if(is.null(para$modalID)) para$modalID <- modalIdObj$getNext()
  
  # Change UI elements depending on the selected distribution e.g. mu, sigma for normal
  # selectPriorDistribution is a selectInput
  if(!is.null(global.selectPriorDistribution$selectPriorDistribution)){
    global.selectPriorDistribution$selectPriorDistribution$destroy()
  }
  global.selectPriorDistribution[["selectPriorDistribution"]] <<- observeEvent(input$selectPriorDistribution, ignoreInit=F, {

    inp <- input$selectPriorDistribution

    #When "fixed values" is selected as a distribution
    if(distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
      shinyjs::disable("selectPriorAttributesDefault")
      updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
    }else{
      shinyjs::enable("selectPriorAttributesDefault")
    }
    
    #When parameter is a vector
    if(para$is.vector){
      if(para$distribution$dist_name != inp){
        para$distribution_tmp <- FactoryDistribution(inp, inp, adjustable=T, is.vector=T, paraProp=para$get.properties())
        para$distribution_tmp$element_name <- input$selectParameterOfVector
        #Adjust Distribution aux parameter
        dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParametersOfVector(parameter=para,tmpValue=T,tmpDist=T)
        if(!distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
          updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
        }
        tmpDist <- para$distributions_tmp[[input$selectParameterOfVector]]
      }else{
        para$distToTmpDist()
        tmpDist <- para$distributions_tmp[[input$selectParameterOfVector]]
        if(!distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
          equal <- tmpDist$amIUsingAdjustedValues(T)
          if(!equal) updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
          if(equal && !input$selectPriorAttributesDefault) updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
        }
      }
      auxParameter <- tmpDist$auxParameter
    }else{
      if(para$distribution$dist_name != inp){
        l <- list(inp, inp, adjustable=T, is.vector=F, paraProp=para$get.properties())
        para$distribution_tmp <- FactoryDistribution(inp, inp, adjustable=T, is.vector=F, paraProp=para$get.properties())
        #Adjust Distribution aux parameter
        pred_var <- para$getParentPredictor()$userVariable
        para$distribution_tmp$element_name <- pred_var
        x <- NULL
        if(!is.null(pred_var)){
          dMID <- dataModel$getDataModelInputData()
          x <- dMID$getLongFormatVariable(pred_var, completeCases=T)
        } 
        para$distribution_tmp$setDataAndProperties(x, dataModel$get.cPerIterationDataModel(), para)
        para$distribution_tmp$adjustMyself(tmpValue=T)
        auxParameter <- para$distribution_tmp$auxParameter
        if(!distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe))
          updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
      }else{
        para$distToTmpDist()
        auxParameter <- para$distribution_tmp$auxParameter
        if(!distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
          equal <- para$distribution_tmp$amIUsingAdjustedValues(T)
          if(!equal) updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
          if(equal && !input[["selectPriorAttributesDefault"]]) updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
        }
      }
    }


    # Distribution aux parameter
    output[["selectPriorAttributesDiv"]] <- renderUI({
      inputElements <- list()
      # If fixed values dist is selected the ui is slight different.
      if(distDisplayName(inp) == dEnum$FixedValues){

        if(length(auxParameter) > 1) stop("The number of aux parameter could be greater than 1?")
        element <- auxParameter[[1]]
        
        dMID <- dataModel$getDataModelInputData()
        userVarsExceptResponse <- dMID$getOtherVariables()        
        
        choices <- c("(Select a variable)"="", para$distribution_tmp$whichUserVarFitsToThisParameter(userVarsExceptResponse))
        
        val <- element$getTmpVal()
        if(!is.null(val) && val == "?") val <- NULL
        inputElements[[1]] <- tags$div(
          
          bayasNumericInput(inputId = paste0("aux_",element$name), label=NULL,
                            value=val, min=element$min_val, 
                            max=element$max_val, integer=element$discrete,
                            placeholder=100, emptyWarning = F,
                            invalidMessage ="Invalid input. (?)",
                            invalidTooltip = element$isValidInput(NA)),
          
          tags$div(class="wordWrappedInHorizLine", " or "),
          tags$div(selectizeInput("aux_selectUserVariable",NULL,choices=choices, selected=element$getTmpVal()))
        )
      }else{
        for(i in 1:length(auxParameter)){
          element <- auxParameter[[i]]
          inputElements[[i]] <- tags$div(
            fluidRow(
              style="margin-bottom:5px;",
              column(1, tags$span(HTML(element$display_name)), offset=1, style="padding:6px;"),
              column(10, 
                 bayasNumericInput(inputId = paste0("aux_",element$name), label=NULL,
                                   value=element$getTmpVal(), min=element$min_val, 
                                   max=element$max_val, integer=element$discrete,
                                   placeholder = "100",
                                   invalidMessage ="Invalid input. (?)",
                                   invalidTooltip = element$isValidInput(NA)))
            )
          )
        }
      }


      #Special case: "fixed values"
      if(distDisplayName(inp) == dEnum$FixedValues){
        
        element <- auxParameter[[1]]
        
        if(!is.null(global.selectPriorDistribution$aux_selectUserVariable)){
          global.selectPriorDistribution$aux_selectUserVariable$destroy()
        }
        global.selectPriorDistribution$aux_selectUserVariable <<- observeEvent(input$aux_selectUserVariable, ignoreInit=T, ignoreNULL = F,{
          
          if(input$aux_selectUserVariable != ""){
            para$setValue(NULL, element$name, input$aux_selectUserVariable, tmpValue=T, tmpDist=T)
            updateBayasNumericInput(session, paste0("aux_",element$name), value="")
          }
        })
        
        if(!is.null(global.selectPriorDistribution[[paste0("aux_",element$name)]])){
          global.selectPriorDistribution[[paste0("aux_",element$name)]]$destroy()
        }
        global.selectPriorDistribution[[paste0("aux_",element$name)]] <<- observeEvent(input[[paste0("aux_",element$name)]], ignoreInit=F,{
          
          x <- input[[paste0("aux_",element$name)]]
 
          x_value <- x
          isValid <- !is.null(x)
          
          if(isValid){
            updateSelectizeInput(session, "aux_selectUserVariable", selected = "")
          }
          
          if(input$aux_selectUserVariable != ""){
            isValid <- T
            x[[2]] <- input$aux_selectUserVariable
          }
          
          if(isValid){
            #If vector, set to tmp_value for storing it
            if(para$is.vector){
              para$setValue(input[["selectParameterOfVector"]], element$name, x_value, tmpValue=T, tmpDist=T)
            }else{
              para$setValue(NULL,element$name, x_value, tmpValue=T, tmpDist=T)
            }
          }
        })
        
      }else{
        
        #For changes in prior (aux) elements (NumericInputs)
        aux_names <<- c()
        lapply(1:length(auxParameter), function(i) {
          element <- auxParameter[[i]]
          aux_names <<- c(aux_names, paste0("aux_",element$name))
          if(!is.null(global.selectPriorDistribution[[paste0("aux_",element$name)]])){
            global.selectPriorDistribution[[paste0("aux_",element$name)]]$destroy()
          }
          global.selectPriorDistribution[[paste0("aux_",element$name)]] <<- observeEvent(input[[paste0("aux_",element$name)]], ignoreInit=F, ignoreNULL =F, {
            x <- input[[paste0("aux_",element$name)]]

            if(!distDisplayName(inp) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
              if(!is.null(x)){
                #If vector, set to tmp_value for storing it
                if(para$is.vector){
                  para$setValue(input[["selectParameterOfVector"]], element$name, x, tmpValue=T, tmpDist=T)
                  equal <- para$distributions_tmp[[input[["selectParameterOfVector"]]]]$amIUsingAdjustedValues(T)
                }else{
                  para$setValue(NULL,element$name, x, tmpValue=T, tmpDist=T)
                  equal <- para$distribution_tmp$amIUsingAdjustedValues(T)
                }
                if(!equal) updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
                if(equal && !input[["selectPriorAttributesDefault"]]) updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
                
              }else{
                updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
              }
            }else{
              if(para$is.vector){
                para$setValue(input[["selectParameterOfVector"]], element$name, x, tmpValue=T, tmpDist=T)
              }else{
                para$setValue(NULL,element$name, x, tmpValue=T, tmpDist=T)
              }
            }
              
          })
        })
      }
      tagList(
        inputElements
      )
    })
  
    # #Info box
    # tmpDist <- FactoryDistribution(inp, "tmp", adjustable=F, is.vector=F)
    # updateTextAreaInput(session, inputId = "previewPriorDistributionInfo", 
    #                     value=HTML(tmpDist$description))
  })
  
  #Info box
  output$previewPriorDistributionInfo <- renderUI({
    if(!is.null(input$selectPriorDistribution)){
      tmpDist <- FactoryDistribution(input$selectPriorDistribution, "tmp", adjustable=F, is.vector=F)
      HTML(tmpDist$description)
    }
  })

  
  #plot
  output[["previewPriorDistribution"]] <- renderPlot({
    if(is.null(input$selectPriorDistribution)) return(ggplot())
    tmpDist <- FactoryDistribution(input[["selectPriorDistribution"]], input[["selectPriorDistribution"]], adjustable=T, is.vector=F, paraProp=para$get.properties())
    auxParameter <- tmpDist$auxParameter
    values  <- c()
    for(i in auxParameter){
      val <- input[[paste0("aux_",i$name)]]
      if(!is.null(val)){
        values <- c(values, val)
      }else{
        return(ggplot())
      }
    }
    theme_set(new=theme_bw(base_size=14, base_family=""))
    ggplot <- ggplot()
    ggplot <- tmpDist$plotMe(values)
    return(ggplot)
  })
  
  #Checkbox for adjusted values of single predictor
  if(!is.null(global.selectPriorDistribution$selectPriorAttributesDefault)){
    global.selectPriorDistribution$selectPriorAttributesDefault$destroy()
  }
  global.selectPriorDistribution[["selectPriorAttributesDefault"]] <<- observeEvent(input$selectPriorAttributesDefault, ignoreInit=T,{
    if(input[["selectPriorAttributesDefault"]]){

      #Get adjusted parameters in a named list
      if(para$is.vector){
        dist <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParameterOfVector(para, input[["selectParameterOfVector"]],T,T)
        distParameter <- para$distributions_tmp[[input[["selectParameterOfVector"]]]]$getParameters(tmpValue=T)
      }else{
        para$distribution_tmp <- FactoryDistribution(input[["selectPriorDistribution"]],input[["selectPriorDistribution"]], adjustable=T, is.vector=F, paraProp=para$get.properties())
        dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParametersOfParameter(para, tmpValue=T,tmpDist=T)
        distParameter <- para$distribution_tmp$getParameters(tmpValue=T)
        para$distribution_tmp$element_name <- para$getParentPredictor()$userVariable
      }


      # update numericInputs where the id is "aux_" + name of distParameter elements
      for(i in 1:length(distParameter)){
        d <- distParameter
        updateBayasNumericInput(session, paste0("aux_",names(distParameter)[[i]]), value=distParameter[[i]])
      }

      #Are all elements of a vector predictor adjusted? Set the global to true
      if(para$is.vector){
        flag <- T
        for(dist in para$distributions_tmp){
          if(!dist$singleParameterization && !dist$amIUsingAdjustedValues(T)) flag <- F
        }
        # updateCheckboxInput(session, "selectPriorAttributesDefaultForAll", value=flag)
      }
    }else{
      # updateCheckboxInput(session, "selectPriorAttributesDefaultForAll", value=F)
    }
  })
  
  if(para$is.vector){
    if(!is.null(global.selectPriorDistribution$selectParameterOfVector)){
      global.selectPriorDistribution$selectParameterOfVector$destroy()
    }
    global.selectPriorDistribution[["selectParameterOfVector"]] <<- observeEvent(input$selectParameterOfVector,ignoreInit=F, {
      
      #Change aux values depended to selected vector element
      dist <- para$distributions_tmp[[input[["selectParameterOfVector"]]]]

      if(is.null(dist)) {warning("No dist...");return()}
      auxParameter <- dist$auxParameter

      if(length(para$distributions_tmp) > 0){
        lapply(1:length(auxParameter), function(i) {
          element <- auxParameter[[i]]
          x_new <- dist$getValueOf(element$name,T)
          if(is.na(x_new)) warning("should not be na...")
          if(is.na(x_new)) x_new <- dist$getValueOf(element$name,F)
          updateBayasNumericInput(session, paste0("aux_",element$name), value=x_new)
          #check if the current distribution uses the adjustes values
          if(!distDisplayName(input$selectPriorDistribution) %in% c(dEnum$FixedValues, dEnum$Horseshoe)){
            equal <- dist$amIUsingAdjustedValues(T)
            if(!equal) updateCheckboxInput(session, "selectPriorAttributesDefault", value=F)
            if(equal && !input$selectPriorAttributesDefault) updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
          }
          
        })
      }
    })
    
    
    # #Checkbox for adjusted values of vector predictors
    # if(!is.null(global.selectPriorDistribution$selectPriorAttributesDefaultForAll)){
    #   global.selectPriorDistribution$selectPriorAttributesDefaultForAll$destroy()
    # }
    # global.selectPriorDistribution[["selectPriorAttributesDefaultForAll"]] <<- observeEvent(input$selectPriorAttributesDefaultForAll, ignoreInit=T,{
    #   
    #   if(input$selectPriorAttributesDefaultForAll){
    #     
    #     #Adjust parameter, but save into tmp_val!
    #     dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParametersOfVector(para, tmpValue=T, tmpDist=T)
    #     
    #     #Are all elements of a vector predictor adjusted? Set the global to true
    #     updateCheckboxInput(session, "selectPriorAttributesDefault", value=T)
    #   }
    # })
  }

  #Finally confirm priors
  if(!is.null(global.selectPriorDistribution$finallyChangePrior)){
    global.selectPriorDistribution$finallyChangePrior$destroy()
  }
  global.selectPriorDistribution[["finallyChangePrior"]] <<- observeEvent(input$finallyChangePrior, ignoreInit=T, {

    auxParameter <- para$distribution_tmp$auxParameter

    if(is.null(input$selectPriorDistribution)) return()
    
    #If there are warnings due to invalid inputs it is not possible to confirm
    if(distDisplayName(input$selectPriorDistribution) == dEnum$FixedValues){
      x <- input[[paste0("aux_",auxParameter[[1]]$name)]]
      isValid <- !is.null(x)
      if(!isValid && input$aux_selectUserVariable == ""){
        showNotification("You have some invalid inputs.", type="error")
        return() 
      }
    }else{
      for(auxPara in auxParameter){
        x <- input[[paste0("aux_",auxPara$name)]]
        isValid <- !is.null(x)
        if(!isValid){
          showNotification("You have some invalid inputs.", type="error")
          return()
        }
      }
    }
    
    #Set the tmp values to the actual values.
    para$distTmpValToVal(tmpDist=T)
    para$removeTmpval(tmpDist=T)
    
    #Set parameter distribution to tmp distribution 
    para$tmpDistToDist()
    
    # Set also the prior Distributions of the other group predictors:
    # In rstanarm each predictor has to have the same distribution (but with different parameters)
    # predictor <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$myParentPredictor(para)
    predictor <- para$getParentPredictor()
    
    
    if(!is.null(predictor) && predictor$same_prio_dist){
      list_of_pred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$get_predictor_of_type(predictor$name)
      for(oPred in list_of_pred){
        #Break, if the selected is equal to the current distribution.
        #Except it is a distribution where singleParameterization is true (e.g. horseshoe prior for all coefficients).
        if(oPred$modelParameter$distribution$dist_name == para$distribution$dist_name && !para$distribution$singleParameterization) next
        is.vector <- oPred$modelParameter$distribution$is.vector
        oPred$modelParameter$distribution <- para$distribution$getInstance()
        oPred$modelParameter$distribution$is.vector <- is.vector
        oPred$modelParameter$distribution$element_name <- oPred$display_name
        dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$adjustDistributionParametersOfPredictor(oPred)
      }
    }

    #Update formula
    buildFormula(input, output, session, dataModel)
    
    
    output[["selectPriorAttributesDiv"]] <- renderUI({tags$div()})
    session$sendCustomMessage(type = "resetValue", message = "selectParameterOfVector")
    session$sendCustomMessage(type = "resetValue", message = "selectPriorDistribution")

    for(i in aux_names) session$sendCustomMessage(type = "resetValue", message = i)
    
    #exit modal
    removeModal(session)
  })
  
  
  #Close modal
  global.selectPriorDistribution[["exitDistributionModal"]] <<- observeEvent(input$exitDistributionModal, ignoreInit=T,{
    output[["selectPriorAttributesDiv"]] <- renderUI({tags$div()})
    session$sendCustomMessage(type = "resetValue", message = "selectParameterOfVector")
    session$sendCustomMessage(type = "resetValue", message = "selectPriorDistribution")
    for(i in aux_names) session$sendCustomMessage(type = "resetValue", message = i)
    removeModal(session)
  })

}


add_new_predictor <- function(dataModel, selected = NULL){
  availPred <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$avail_predictor
  # selected <- NULL
  numberOfVar <- -1
  predictorNames <- c()
  for(pred in availPred){
    if(pred$possibleAmount > 0){
      predictorNames <- c(predictorNames, pred$name)
      if(is.null(selected)){
        selected <- pred$name
      }
      numberOfVar <- max(numberOfVar,pred$numberVar)
    }   
  }
  # var <- dataModel$get.cPerIterationDataModel()$get.users_variables(response = F)$var_name
  var <- dataModel$getDataModelInputData()$getOtherVariables(onlyName=T)
  
  var <- c(var, "Select variable(s)"="")
  return(fixedRow(column(7,
                         tags$div(
                           selectInput("selectInputAvailPred", "Type of formula term", 
                                       choices=predictorNames, selected=selected),
                           selectizeInput("selectInputAvailVar", "Variable", choices=var, multiple=T, options = list(maxItems = numberOfVar,
                                                                                                                     hideSelected=F)))
  ),
  column(5,
         tags$div(uiOutput("AddPredictorMessageBox", inline=T), style="padding-top:30px; word-break: break-all;")))
  
  )
}

