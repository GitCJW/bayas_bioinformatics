init_model_prediction_function <- function(input, output, session, 
                                           dataModel, global_reportProgressModel){
  
  ns <- NS("predictionTab")
  
  
  ## Counter for result tabs
  counterResultTabs <- reactiveVal(0)
  idWordMappingResultTabs <- reactiveVal(data.frame(id=numeric(0), title=numeric(0), 
                                                    modelname=numeric(0), selected=numeric(0)))
  
  #Refresh fit list
  observe({
    dataModel$dependReactiveValue("perIterationDataModels")
    isolate({
      ret <- dataModel$get.perIterationDataModelNames()
      selected <- dataModel$getSelectedPIDM()
      updateSelectInput(session = session, inputId = ns("selectModelFit"), 
                        choices = ret$res, selected = selected)
    })
  })
  
  ## Select a model fit
  observeEvent(input[[ns("selectModelFit")]], {
    if(is.null(input[[ns("selectModelFit")]]) || input[[ns("selectModelFit")]] == ""){
      output[[ns("selectedModelFormula")]] <- renderUI(NULL)
    }else{
      pIDM <- dataModel$get.perIterationDataModel(name=input[[ns("selectModelFit")]])
      output[[ns("selectedModelFormula")]] <- renderUI(buildStaticFormula(pIDM, session))
    }
  })
  
  # Click on the "Calculate effects ..." button
  observeEvent(input[[ns("btnStableEffect")]], {

    #get selected fit
    if(is.null(input[[ns("selectModelFit")]])) return()
    pIDM <- dataModel$get.perIterationDataModel(input[[ns("selectModelFit")]])
    if(is.null(pIDM)){
      showNotification("Please fit a model before and select a fit.", type="warning")
      return()
    }
    bayasModel <- pIDM$get.selected_BAYSIS_stan_model()
    usedVars <- bayasModel$get_used_vars()
    
    
    tabId <- paste0("tab", counterResultTabs())
    counterResultTabs(counterResultTabs()+1)
    
    
    dMID <- pIDM$getDataModelInputData()
    if(is.null(dMID)){
      return()
    }
    
    #insert tab 
    #if there is already a result tab for the given title. Remove this one.
    title <- paste0(input[[ns("selectModelFit")]],":MP")
    
    mapping <- idWordMappingResultTabs()
    if(title %in% mapping$title){
      mapping$selected <- 0
      mapping[mapping$title==title,]$selected <- 1
      idWordMappingResultTabs(mapping)
      return()
    }
    
    
    oV <- dMID$getOtherVariables()
    
    #Get categorical variables
    catChoices <- oV$variable[oV$type == cEnum$Categorical]
    catChoices <- catChoices[catChoices %in% usedVars]
    
    #Get numerical variables
    numChoices <- oV[oV$type %in% c(cEnum$Discrete, cEnum$Continuous),]
    numChoices <- numChoices[numChoices$variable %in% usedVars,]
    
    usedVarsForData <- bayasModel$get_used_vars(response = T, extras=T)
    data <- pIDM$getDataModelInputData()$getLongFormatVariable(usedVarsForData, completeCases=T)
    vec <- list()

    #insert numeric inputs, if available
    startValues <- c()
    for(num in numChoices$variable){
      vec <- list.append(vec, data[[num]], num)
      #get mean value of num
      startValues <- c(startValues, mean(vec[[num]]))
    }


    # Create result model
    resultTab <- ModelPredictionResultModel$new(type="effect", 
                                                fitName=input[[ns("selectModelFit")]], resultName=title, tabId=tabId, 
                                                catChoices=catChoices, numChoices=numChoices, startValues=startValues,
                                                catValues=NULL, usedVarsAdd=NULL, pIDMId=pIDM$get.id())
    dataModel$add.model_prediction_result_tabs(resultTab, title)

  })

  
  # Click on the "Make predictions ..." button
  observeEvent(input[[ns("verifyAndPredictSingleData")]], {
    
    #get selected fit
    if(is.null(input[[ns("selectModelFit")]])) return()
    pIDM <- dataModel$get.perIterationDataModel(input[[ns("selectModelFit")]])
    if(is.null(pIDM)){
      showNotification("Please fit a model before and select a fit.", type="warning")
      return()
    } 
    bayasModel <- pIDM$get.selected_BAYSIS_stan_model()
    usedVars <- bayasModel$get_used_vars()
    usedVarsAdd <- bayasModel$get_used_vars_additional()
    if(!is.null(usedVarsAdd)) usedVars <- c(usedVars, usedVarsAdd)
    
    tabId <- paste0("tab", counterResultTabs())
    counterResultTabs(counterResultTabs()+1)
    
    dMID <- pIDM$getDataModelInputData()
    if(is.null(dMID)){
      return()
    }
    
    #if there is already a result tab for the given title, don't add another one.
    title <- paste0(input[[ns("selectModelFit")]],":PT")

    mapping <- idWordMappingResultTabs()
    if(title %in% mapping$title){
      mapping$selected <- 0
      mapping[mapping$title==title,]$selected <- 1
      idWordMappingResultTabs(mapping)
      return()
    }
    
    
    oV <- dMID$getOtherVariables()
    
    #Get categorical variables
    catChoices <- oV$variable[oV$type == cEnum$Categorical]
    catChoices <- catChoices[catChoices %in% usedVars]
    
    #Get numerical variables
    numChoices <- oV[oV$type %in% c(cEnum$Discrete, cEnum$Continuous),]
    numChoices <- numChoices[numChoices$variable %in% usedVars,]

    usedVarsForData <- bayasModel$get_used_vars(response = T, extras=T)
    data <- pIDM$getDataModelInputData()$getLongFormatVariable(usedVarsForData, completeCases=T)
    numVec <- list()
    catValues <- list()
    
    #insert numeric inputs, if available
    numStartValues <- c()
    for(num in numChoices$variable){
      numVec <- list.append(numVec, data[[num]], num)
      #get mean value of num
      numStartValues <- c(numStartValues, mean(numVec[[num]]))
    }
    for(cat in catChoices){
      catValues <- list.append(catValues, unique(data[[cat]]), cat)
    }

   
    # Create result model
    resultTab <- ModelPredictionResultModel$new(type="prediction", 
                                                fitName=input[[ns("selectModelFit")]], resultName=title, tabId=tabId, 
                                                catChoices=catChoices, numChoices=numChoices, startValues=numStartValues,
                                                catValues=catValues, usedVarsAdd=usedVarsAdd, pIDMId=pIDM$get.id())
    dataModel$add.model_prediction_result_tabs(resultTab, title)

  })
  
  

  
  observe({
    dataModel$dependReactiveValue("model_prediction_result_tabs")
    
    isolate({

      result_tabs <- dataModel$get.model_prediction_result_tabs()
      mapping <- idWordMappingResultTabs()
      
      
      newIds <- c()
      for(tab in result_tabs){
        
        resultTab <- tab
        
        tabId <- tab$get.tabId()
        title <- tab$get.resultName()
        
        newIds <- c(newIds, tabId)
        if(title %in% mapping$title){
          next
        }
        
        catChoices <- tab$get.catChoices()
        catValues <- tab$get.catValues() 
        numChoices <- tab$get.numChoices()
        numStartValues <- tab$get.startValues() 
        usedVarsAdd <- tab$get.usedVarsAdd()
        
        modelname <- tab$get.fitName()
        
        
        pIDM <- dataModel$get.perIterationDataModel(id=tab$get.pIDMId()) #
        
        
        
        if(tab$get.type() == "prediction"){
          
          insertTab(ns("resultTabs"),
                    tab = model_prediction_predict_result_tab(tabId=tabId, title=title, 
                                                              catChoices=catChoices, catValues=catValues,  
                                                              numChoices=numChoices, numStartValues=numStartValues,
                                                              usedVarsAdd=usedVarsAdd),
                    select=T)
          
          modelPredictionPredictResultTabs(id=tabId, numChoices=numChoices$variable, catChoices=catChoices, 
                                           dataModel=dataModel, pIDM=pIDM, mPRM=resultTab,
                                           usedVarsAdd=usedVarsAdd,
                                           global_reportProgressModel=global_reportProgressModel)
   
        }else{
          
          insertTab(ns("resultTabs"), 
                    tab = model_prediction_effect_result_tab(tabId=tabId, title=title, 
                                                             catChoices=catChoices, numChoices=numChoices, startValues=numStartValues), 
                    select=T)

          
          modelPredictionEffectResultTabs(id=tabId, numChoices=numChoices$variable, dataModel=dataModel, 
                                          pIDM=pIDM, mPRM=resultTab,
                                          global_reportProgressModel=global_reportProgressModel)
          
        }
        

        
        if(dim(mapping)[1] > 0) mapping$selected <- 0
        df <- data.frame(id=tabId, title=title, modelname=modelname,
                         selected=1)
        mapping <- rbind(mapping,df)
        idWordMappingResultTabs(mapping)
        
      }
      
      #Remove tabs
      rmIds <- mapping$id[!mapping$id %in% newIds]
      for(rmId in rmIds){
        removeTab(ns("resultTabs"), target=paste0(rmId,"-effectTab"))
      }
      
      mapping <- idWordMappingResultTabs()
      mapping <- mapping[mapping$id %in% newIds,]
      if(!is.null(dim(mapping)[2]) && dim(mapping)[2] == 0){
        mapping <- data.frame(id=numeric(0), title=numeric(0), 
                              modelname=numeric(0), selected=numeric(0))
      }
      idWordMappingResultTabs(mapping)
      
    })
  })
  
  
  
  #Update result selectinput
  observe({
    mapping <- idWordMappingResultTabs()
    
    ret <- list()
    for(i in seq_len(dim(mapping)[1])){
      if(mapping$modelname[i] %in% names(ret)){
        ret[[mapping$modelname[i]]] <- list.append(ret[[mapping$modelname[i]]],
                                                   mapping$title[i])
      }else{
        ret[[mapping$modelname[i]]] <- list(mapping$title[i])
      }
    }

    updateSelectInput(session=session,inputId=ns("selectResults"), choices=ret, 
                      selected=mapping[mapping$selected==1,]$title)
  })
  
  # Opens the corresponding result tab of the selected item
  observeEvent(input[[ns("selectResults")]], {
    mapping <- idWordMappingResultTabs()
    selectTab <- mapping[mapping$title==input[[ns("selectResults")]],]$id
    selectTab <- paste0(selectTab,"-effectTab")
    updateTabsetPanel(session=session, inputId=ns("resultTabs"),
                      selected=selectTab)
  })
  
  #back to model comparison/fitting
  observeEvent(input[[ns("buttonBackModelComparison")]], {
    updateNavbarPage(session, "navbar", selected = "Model comparison")
    shinyjs::addClass("selectInputRunModelPreviousModels", class="borderColor-primary")
  })
  observeEvent(input[[ns("buttonBackModelFitting")]], {
    updateNavbarPage(session, "navbar", selected = "Model fitting")
    shinyjs::addClass("selectInputRunModelPreviousModels", class="borderColor-primary")
  })
  
  selectCoresspondingListItem <- reactiveVal(0)

  observeEvent(input[[ns("resultTabs")]], {
    selectCoresspondingListItem(selectCoresspondingListItem()+1)
  })
  
  selectCoresspondingListItem_d <- debounce(selectCoresspondingListItem, 50)
    
  observe({
    selectCoresspondingListItem_d()
    isolate({
      tabValue <- input[[ns("resultTabs")]]
      tabValue <- substr(tabValue, 1, nchar(tabValue)-10)
      mapping <- idWordMappingResultTabs()
      selectTab <- mapping[mapping$id==tabValue,]$title
      updateSelectInput(session=session,inputId=ns("selectResults"), selected=selectTab)
    })
  })
  
}



## Module that creates separate resulting tab panels
#all variable have to be reactive values
modelPredictionEffectResultTabs <- function(id, numChoices, dataModel, pIDM, mPRM,
                                            global_reportProgressModel){
  fit <- pIDM$get.calculated_stan_object()
  
  dMID <- pIDM$getDataModelInputData()
  response <- dMID$getResponseVariable()$variable
  
  
  bayasModel <- pIDM$get.selected_BAYSIS_stan_model()
  usedVarsForData <- bayasModel$get_used_vars(response = T, extras=T)
  data <- pIDM$getDataModelInputData()$getLongFormatVariable(usedVarsForData, completeCases=T)

  moduleServer(
    id,
    function(input, output, session){

      output$verbalSummary <- renderUI(tags$p("No effect selected"))
      
      numericValues <- reactiveVal(list())
      
      rVEffectMatrix <- reactiveVal(NULL)
      

      ##observe numeric inputs (if avail) for valid input and update selection type
      if(length(numChoices) > 0 ){
        lapply(1:length(numChoices), function(i){
          observeEvent(input[[paste0("num-", numChoices[i])]], ignoreInit=T, priority = 1,{
            inp <- input[[paste0("num-", numChoices[i])]]
            
            # if(is.numeric(inp)){
            if(!is.null(inp)){
              inpVal <- inp

              #Is value equal (min, mean, median, max)?
              if(inpVal == str_trim(formatC(mean(data[[numChoices[i]]]),digits=4))){
                updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="mean")
              }else if(inpVal == str_trim(formatC(median(data[[numChoices[i]]]),digits=4))){
                updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="median")
              }else if(inpVal == str_trim(formatC(min(data[[numChoices[i]]]),digits=4))){
                updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="min")
              }else if(inpVal == str_trim(formatC(max(data[[numChoices[i]]]),digits=4))){
                updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="max")
              }else{
                updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected=NULL)
              }
              
            }else{
              updateBayasGroupedButtons(session, inputId=paste0("mMM-", numChoices[i]), selected=NULL)
            }
          
            isolate({
              numericVal <- numericValues()
              inp <- input[[paste0("num-",numChoices[i])]]
              if(!is.null(inp)){
                numericVal[[numChoices[i]]] <- inp
              }else{
                numericVal[[numChoices[i]]] <- NA
              }
              numericValues(numericVal)
            })
          })
          
          
          ##observe selection type for numeric input
          observeEvent(input[[paste0("mMM-", numChoices[i])]], {
            inp <- input[[paste0("mMM-", numChoices[i])]]
            val <- 0
            if(inp == "mean"){
             val <- mean(data[[numChoices[i]]])
            }else if(inp == "median"){
              val <- median(data[[numChoices[i]]])
            }else if(inp == "min"){
              val <- min(data[[numChoices[i]]])
            }else if(inp == "max"){
              val <- max(data[[numChoices[i]]])
            }

            updateBayasNumericInput(session = session, inputId=session$ns(paste0("num-", numChoices[i])), 
                                    value=str_trim(formatC(val,digits=4)))
          })
          

        })
      }

      
      ##observe group and slope selection
      #remove selection if one of the opposites is choosen
      observeEvent(input$selectCatVar, ignoreNULL=F, {
        
        if(is.null(input$selectCatVar)){
          shinyjs::hide(id="dependsOnString")
          
          if(length(numChoices)>0){
            sapply(1:length(numChoices), function(i){
              shinyjs::hide(id=paste0("div-",numChoices[i]))
            })
          }
          return()
        }
        #Deselect slope effects
        updateSelectInput(session=session, inputId="selectNumVar", selected = "")
        selectedCell(NULL)
        
        #Depends the group effect also on given numeric sizes?
        numVal <- dependsOnNumeric(fit, input$selectCatVar, response=response) 
        usedNums <- numVal$used
        if(length(usedNums)>0){
          shinyjs::show(id="dependsOnString")
          sapply(1:length(usedNums), function(i){
            shinyjs::show(id=paste0("div-",usedNums[i]))
          })
        }else{
          shinyjs::hide(id="dependsOnString")
        }
        unusedNums <- numVal$unused
        if(length(unusedNums)>0){
          sapply(1:length(unusedNums), function(i){
            shinyjs::hide(id=paste0("div-",unusedNums[i]))
          })
        }
      })
      observeEvent(input$selectNumVar, {
        #Deselect group effects
        updateSelectInput(session=session, inputId="selectCatVar", selected = "")
        selectedCell(NULL)
      })

      
      ##Matrix that depends on
      #selected group effect or selected slope effect
      #given values for numeric inputs (if avail)
      #given type of matrix (pi, mean, median)
      observe({
        
        selGroup <- input$selectCatVar
        selSlope <- input$selectNumVar
        matrixType <- input$matrixValues
        numericVal <- numericValues()
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange

        if(length(numericVal) != length(numChoices) ||  any(is.na(numericVal)) ||
           any(is.na(as.numeric(numericVal)))){
          output$matrix <- renderDT(NULL)
          output$verbalSummary <- renderUI(tags$p("No effect selected"))
          shinyjs::hide(id="globalEffectDiv")
          removeCssClass("reportEffectMatrix", class="btn-primary")
          rVEffectMatrix(NULL)
          return()
        }else if((!is.null(selGroup) && (length(selGroup) > 1 || !selGroup == "") &&
                 !is.null(selSlope) && (length(selSlope) > 1 || !selSlope == "")) ||
                 (is.null(selGroup) && is.null(selSlope))){
          output$verbalSummary <- renderUI(tags$p("No effect selected"))
          shinyjs::hide(id="globalEffectDiv")
          return()
        }
        isolate(sCell <- selectedCell())
        if(is.null(sCell) || (sCell[1]==0 && sCell[2]==0)){
          output$summary <- renderDT(NULL)
          output$densityPlot <- renderPlot(NULL)
          output$distPlotPlaceholder <- renderUI(tags$div(style="margin:10px;","Select any matrix cell that contains a value."))
        }

        
        # Numeric value (combination) is not possible. Happens, if a numeric variable
        # is chosen that is not present as a single predictor but as an interaction.
        isPoss <- isNumericPossible(pIDM$get.calculated_stan_object(),selGroup, selSlope, 
                                    numericVal, response)
        
        if(!is.null(selSlope) && !isPoss$bool){
          output$verbalSummary <- renderUI(tags$p(HTML(paste0("The slope of <b>",
                                                              paste0(selSlope,collapse=":"),
                                                              "</b> is not present. ",
                                                              "Possible are: <b>",
                                                              paste0(isPoss$alt,collapse=", "),
                                                              "</b>"))))
          
          output$matrix <- renderDT(NULL)
          shinyjs::hide(id="globalEffectDiv")
          removeCssClass("reportEffectMatrix", class="btn-primary")
          rVEffectMatrix(NULL)
          return()
        }
        

        #list(dt=dt, latex = list(headerNames, headers))
        ret <- calculateEffectMatrix(selGroup, selSlope, numericVal, matrixType, 
                                    pIDM$get.calculated_stan_object(),
                                    response, sCell,
                                    hdiType, hdiRange)
        
        dt <- ret$dt
        ret$numericVal <- numericVal
        
        output$matrix <- renderDT(dt)
        addCssClass("reportEffectMatrix", class="btn-primary")
        
        shinyjs::show(id="globalEffectDiv")
        rVEffectMatrix(ret)
        
        #Analyse the selection to give some information
        sEV <- summarizeEffectsVerbal(fit, catVar=selGroup, numVar=selSlope, 
                               numVal=numericVal, response=response, pIDM$get.selected_BAYSIS_stan_model()) 
        
        sEV <- tags$div(tags$div(actionLink(session$ns("effectDefinitionInfo"), "What is the definition of 'effects'?")),
                        tags$br(),
                        sEV)
        
        output$verbalSummary <- renderUI(sEV)
      })
      
      observeEvent(input$effectDefinitionInfo, {
        showModal(modalDialog(
          explanation_of_effects(),
          title = "Effects",
          footer = modalButton("Ok"),
          size = "l",
          easyClose = T,
        ))
      })
      
      ## Currently selected cell
      selectedCell <- reactiveVal(NULL)
      observeEvent(input$matrix_cells_selected, ignoreNULL = F, {
        matrixInp <- input$matrix_cells_selected
        if(!any(matrixInp)){
          selectedCell(NULL)
        }else{
          selectedCell(c(matrixInp[1,1],matrixInp[1,2]))
        }
      })
      
      #Stores the numerics for the selected cell
      #Updated when a new cell is selected (or deselected) and when the 
      #numeric inputs are changed
      singleEffect <- reactiveVal(NULL)
      observe({
        sCell <- selectedCell()
        numericVal <- numericValues()
        
        if(is.null(sCell) || length(sCell) != 2 || NA %in% sCell){
          singleEffect(NULL)
          removeCssClass("reportEffectDistribution", class="btn-primary")
          return()
        }
        
        singleEffect(NULL)
        isolate({
          selGroup <- input$selectCatVar
          selSlope <- input$selectNumVar
          singleEffect(single_effect(nrow=sCell[1], ncol=sCell[2],
                                        rstanarmBrmsFit=fit, catVar=selGroup,  numVar=selSlope, numVal=numericVal,
                                        response=response))
          addCssClass("reportEffectDistribution", class="btn-primary")
        })
      })
      
      ##Summary table and density plot that depends on
      #selected cell of matrix
      observe({
        sEffect <- singleEffect()
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange
        isolate({
          if(is.null(sEffect)){
            output$summary <- renderDT(NULL)
            output$densityPlot <- renderPlot(NULL)
            output$distPlotPlaceholder <- renderUI(tags$div(style="margin:10px;","Select any matrix cell that contains a value."))
          }else{
            output$summary <- renderDT(summary_effect(sEffect, hdiType, hdiRange))

            vars <- sEffect$vars
            varsHeader <- ""
            if(length(unique(vars)) == 1){ 
              varsHeader <- unique(vars)
            }else{
              varsHeader <- paste0(sEffect$vars, collapse=" - ")
            }
            
            t <- tags$div(
              style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
              varsHeader
            )
            output$distPlotPlaceholder <- renderUI(t)
            
            data <-NULL
            x_axis <- NULL
            if(sEffect$type == "Slope"){
              data <- c(sEffect$linpred$A)
            }else if(sEffect$type == "Slope difference"){
              data <- c(sEffect$linpred$A)-c(sEffect$linpred$B)
            }else if(sEffect$type == "Group difference"){
              data <- c(sEffect$linpred$A)-c(sEffect$linpred$B)
            }else{
              warning(paste0("Unknown effect type",sEffect$type))
              return()
            }
            gg <- plotDiff(data=data, prop=hdiRange, x_axis, method=hdiType, centerPoint=0)
            
            output$densityPlot <- renderPlot(gg)
          }
        })
      })

      #Report effect matrix
      observeEvent(input$reportEffectMatrix, {
        
        pIDM_id <- pIDM$get.id()
        
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange
        type <- input$matrixValues
        
        # list(dt=dt, latex = list(headerNames, headers))
        eM <- rVEffectMatrix()
        
        #list(headerNames, headers)
        headerStuff <- eM$latex
        
        #If group effects also depends on certain values of slopes (e.g. sex:weight)
        numericVal <- eM$numericVal
        
        eM <- eM$dt

        if(is.null(eM)){
          showNotification("Please select either a group or slope effect.", type="warning")
          return()
        }

        df <- eM$x$data
        
        tEnum <- reportTypeEnum()
        
        catVar <- input$selectCatVar
        effectType <- ifelse(!is.null(catVar),"group","slope")
        model_latex <- wordToLatexConform(pIDM$get.selected_BAYSIS_stan_model()$effects_description_latex(type=effectType))
        
        effectMatrixLatex <- modelEffectMatrix(df, type, hdiType, hdiRange, 
                                               headerStuff, numericVal) 
  

        uiText <- paste0("The table shows the ", type, " value (on a ", hdiRange*100, "% ", hdiType, "). ")
        if(!is.null(numericVal) && length(numericVal) > 0 ){
          uiText <- paste0(uiText, "This effect depends on concrete values for ", 
                                 paste0(names(numericVal),collapse=", "),
                                 " with: ")
          for(i in 1:length(numericVal)){
            uiText <- paste0(uiText, names(numericVal)[i],": ", numericVal[[i]])
            if(i < length(numericVal)) uiText <- paste0(uiText, "; ")
          }
          uiText <- paste0(uiText,".")
        }
        ui <- tags$div(style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
                       uiText)
        
        
        table <- eM
        
        reportDiv <- reportType(div=list(ui=ui, table=table), space="20")
        
        
        #Add formula element to report progress
        addItem(moduleType = "evaluation",
                dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM_id, 
                pDIM_name = pIDM$get.name(),
                imgFile = paste0("Images/Report/EffectMatrix.png"),
                type=tEnum$effectMatrix, object=list(div=reportDiv, latex=effectMatrixLatex, model_latex=model_latex), 
                show=T, singleton=F, global_reportProgressModel=global_reportProgressModel)
      })
      
      #Report effect distribution and summary table
      observeEvent(input$reportEffectDistribution, {

        pIDM_id <- pIDM$get.id()
        
        sEffect <- singleEffect()
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange
        numericVal <- numericValues()
        
        #Catch nothing to report
        if(is.null(sEffect)){
          showNotification("Please select any matrix cell that contains a value.",type="warning")
          return()
        }
        
        
        summaryTable <- summary_effect(sEffect, hdiType, hdiRange)
        
        data <-NULL
        x_axis <- NULL
        if(sEffect$type == "Slope"){
          data <- c(sEffect$linpred$A)
        }else if(sEffect$type == "Slope difference"){
          data <- c(sEffect$linpred$A)-c(sEffect$linpred$B)
        }else if(sEffect$type == "Group difference"){
          data <- c(sEffect$linpred$A)-c(sEffect$linpred$B)
        }else{
          warning(paste0("Unknown effect type",sEffect$type))
          return()
        }
        gg <- plotDiff(data=data, prop=hdiRange, x_axis, method=hdiType, centerPoint=0)
        
        
        tEnum <- reportTypeEnum()
        
        vars <- sEffect$vars
        varsHeader <- ""
        if(length(unique(vars)) == 1){ 
          varsHeader <- unique(vars)
        }else{
          varsHeader <- paste0(sEffect$vars, collapse=" - ")
        }
        
        inputName <- paste0(pIDM$get.name(), "_", varsHeader)
        inputName <- str_replace_all(inputName, " ", "_")
        inputName <- str_replace_all(inputName, ":", "_")
        
        
        effectLatex <- modelEffectItem(summaryTable, vars, hdiType, 
                                       hdiRange, paste0("single_effect_",inputName), gg,
                                       numericVal)
        

        headerText <- paste0("Effect of: ", varsHeader, "<br> ")
        
        headerText <- paste0(headerText, "Based on a ", hdiRange*100, "% ", hdiType, ". ")
        if(!is.null(numericVal) && length(numericVal) > 0){
          headerText <- paste0(headerText, "This effect depends on concrete values for ", 
                           paste0(names(numericVal),collapse=", "),
                           " with: ")
          for(i in 1:length(numericVal)){
            headerText <- paste0(headerText, names(numericVal)[i],": ", numericVal[[i]])
            if(i < length(numericVal)) headerText <- paste0(headerText, "; ")
          }
          headerText <- paste0(headerText,".")
        }
        
        header <- tags$div(
          style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
          HTML(headerText)
        )

        ggsave(paste0(report_folder, "/Thumbnails/single_effect_",inputName,".jpg"), 
               gg, device="jpeg", width=100, height=100, units="px", dpi=25)
        
        reportDiv <- reportType(div=list(ui=header, plot=gg, table=summaryTable),
                                space=c("0","10"))
        
        #Add formula element to report progress
        addItem(moduleType = "evaluation",
                dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM_id, 
                pDIM_name = pIDM$get.name(),
                imgFile = paste0("Report/Thumbnails/single_effect_",inputName,".jpg"),
                type=tEnum$singleEffect, object=list(div=reportDiv, latex=effectLatex), 
                show=T, singleton=F, global_reportProgressModel=global_reportProgressModel)
        
      })
      
      
    }
  )
}


## Module that creates separate resulting tab panels
#all variables have to be reactive values
modelPredictionPredictResultTabs <- function(id, numChoices, catChoices, 
                                             dataModel, pIDM, mPRM,
                                             usedVarsAdd,global_reportProgressModel){
  fit <- pIDM$get.calculated_stan_object()
  
  dMID <- pIDM$getDataModelInputData()
  response <- dMID$getResponseVariable()$variable
  
  rstanModel <- pIDM$get.calculated_stan_object()
  
  bayasModel <- pIDM$get.selected_BAYSIS_stan_model()
  usedVarsForData <- bayasModel$get_used_vars(response = T, extras=T)
  data <- pIDM$getDataModelInputData()$getLongFormatVariable(usedVarsForData, completeCases=T)
  
  moduleServer(
    id,
    function(input, output, session){
      
      plotHistory <- reactiveVal(mPRM$get.plot_history())
      
      numericValues <- reactiveVal(list())
      
      ##observe numeric inputs (if avail) for valid input and update selection type
      lapply(seq_along(numChoices), function(i){
        observeEvent(input[[paste0("num-", numChoices[i])]], ignoreInit=T, priority = 1,{
          inp <- input[[paste0("num-", numChoices[i])]]
          limits <- usedVarsAdd[usedVarsAdd$var==numChoices[i],]
          if(!is.null(limits) && dim(limits)[1]==0) limits <- NULL
          lower <- -Inf
          upper <- Inf
          if(!is.null(limits)){
            lower <- limits$lower
            upper <- limits$upper
          }
          
          if(!is.null(inp)){
            inpVal <- inp
            #Is value equal (min, mean, median, max)?
            if(inpVal == str_trim(formatC(mean(data[[numChoices[i]]]),digits=4))){
              updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="mean")
            }else if(inpVal == str_trim(formatC(median(data[[numChoices[i]]]),digits=4))){
              updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="median")
            }else if(inpVal == str_trim(formatC(min(data[[numChoices[i]]]),digits=4))){
              updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="min")
            }else if(inpVal == str_trim(formatC(max(data[[numChoices[i]]]),digits=4))){
              updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected="max")
            }else{
              updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected=NULL)
            }
            
          }else{
            updateBayasGroupedButtons(session, inputId=session$ns(paste0("mMM-", numChoices[i])), selected=NULL)
          }
          
          isolate({
            numericVal <- numericValues()
            inp <- input[[paste0("num-",numChoices[i])]]
            if(!is.null(inp)){
              numericVal[[numChoices[i]]] <- inp
            }else{
              numericVal[[numChoices[i]]] <- NA
            }
            numericValues(numericVal)
          })
        })
        
        
        ##observe selection type for numeric input
        observeEvent(input[[paste0("mMM-", numChoices[i])]], {
          name <- numChoices[i]
          inp <- input[[paste0("mMM-", numChoices[i])]]
          val <- 0
          if(inp == "mean"){
            val <- mean(data[[numChoices[i]]])
          }else if(inp == "median"){
            val <- median(data[[numChoices[i]]])
          }else if(inp == "min"){
            val <- min(data[[numChoices[i]]])
          }else if(inp == "max"){
            val <- max(data[[numChoices[i]]])
          }
          
          updateBayasNumericInput(session = session, inputId=session$ns(paste0("num-", numChoices[i])), 
                                  value=str_trim(formatC(val,digits=4)))
        })
      })

      
      # Button for predictions
      observeEvent(input$MakePrediction, ignoreInit=T, {
        
        new_data <- data.frame()
        for(i in numChoices){
          n <- input[[paste0("num-", i)]]
          
          limits <- usedVarsAdd[usedVarsAdd$var==i,]
          lower <- -Inf
          upper <- Inf
          if(!is.null(limits)){
            lower <- limits$lower
            upper <- limits$upper
          }
          
          if(!is.null(n)){
            nVal <- n
            if(dim(new_data)[1]==0){
              new_data <- data.frame(nVal)
              names(new_data) <- i
            }else{
              new_data[[i]] <- nVal
            }
          }else{
            showNotification("Please provide for each variable a valid input!", type="warning")
            return()
          }
        }
        
        for(i in catChoices){
          n <- input[[paste0("cat-", i)]]
          if(!is.null(n) && n != ""){
            if(dim(new_data)[1]==0){
              new_data <- data.frame(n)
              names(new_data) <- i
            }else{
              new_data[[i]] <- n
            }
          }else{
            showNotification("Please provide for each variable a valid input!", type="warning")
            return()
          }
        }
        
        
        #If already exists
        if(mPRM$duplicate.plot_history(new_data) != 0) return()
        
        pp <- NULL
        pp <-tryCatch({
          pIDM$get.selected_BAYSIS_stan_model()$make_predictions(rstanModel, new_data, mean=T) #try catch
        },
        error=function(cond){
          return(NULL)
        })
        
        if(is.null(pp)){
          showNotification("An error has occurred, the data is too large.", type="error")
          return()
        }
        
        #get MPRM
        index <- mPRM$next.plot_history.index()
        color <- mPRM$next.plot_history.color()
        
        ## data: data 
        ## index: row index (unique)
        ## values: named list of predictors with their values 
        plot <- list(data=pp,
                     index=index,
                     values=new_data,
                     color=color)
        pH <- plotHistory()
        pH <- list.append(pH, plot)
        plotHistory(pH)
      })
      
      observe({

        pH <- plotHistory()
        mPRM$set.plot_history(pH)

        if(length(pH)==0){
          output$predictionsTable <- renderDT(data.frame())
          return()
        }
        
        #update table
        ret <- data.frame()
        for(i in pH){
          if(dim(ret)[1] == 0){
            ret <- data.frame(index=i$index, Color_coding=i$color)
            if(dim(i$values)[1] > 0) ret <- cbind(ret, as.data.frame(i$values))
          }else{
            df <- as.data.frame(i$values)
            df$Color_coding <- i$color
            df$index <- i$index
            ret <- rbind(ret, df)
          }
        }
        ret[["Color coding"]] <- sapply(1:dim(ret)[1], function(i){
          paste0(tags$div(HTML("█"), 
                          style=paste0("color:",ret$Color_coding[i],";")))
          })
        
        remove_col <- 1

        df <- ret[,!colnames(ret) %in% c( "Color_coding")]
        df$REMOVE_THIS_ROW <- shinyInput(actionLink, dim(df)[1],
                            id = session$ns('remove_prediction_button'),
                            value = df$index,
                            label = "x",
                            class="fontColor-error",
                            style="font-weight:bold;"
        )
        
        cSummary(df)
        cColor(ret$Color_coding)
        
        cnames <- colnames(df)
        cnames[length(cnames)] <- ""

        isolate(sRows <- selectedRows())
        if(length(sRows) != 0) sRows <- sRows-1
        
        dt <- datatable(df, rownames=F, escape=F, colnames=cnames,
                        callback = JS("table.rows([",paste0(sRows,collapse=","),"]).select();return table;"),
                        options=list(paging=F,searching=F, info=F, ordering=T,
                                     columnDefs = list(list(className = 'dt-center', targets = "_all"),
                                                       list(width="30px", targets="REMOVE_THIS_ROW")),
                                     select=list(style='multi', selector="td:not(:last-child)")),
                        extensions = "Select",
                        selection="none") %>%
          formatStyle(colnames(df), lineHeight="70%")
        
        output$predictionsTable <- renderDT(dt, server=F)
      })
      
      
      #Remove rows
      observeEvent(input$remove_prediction_button,{
        if(is.null(input$remove_prediction_button) || input$remove_prediction_button==-1){
          return()
        } 

        ph <- plotHistory()

        c <- 1
        for(i in ph){
          if(i$index == input$remove_prediction_button){
            ph[[c]] <- NULL
            break
          }
          c <- c+1
        }
        sRows <- selectedRows()
        sRowsNew <- c()
        for(i in sRows){
          if(i != c){
            if(i < c){
              sRowsNew <- c(sRowsNew,i)
            }else{
              sRowsNew <- c(sRowsNew,i-1)
            }
          }
        }
        selectedRows_d(sRowsNew)
        plotHistory(ph)
        
        session$sendCustomMessage(type = "resetValue", message = session$ns("remove_prediction_button"))
        
      })

      #Selected rows. If null (nothing selected), do nothing and store the last selected row(s).
      #This is better behavior
      selectedRows_d <- reactiveVal(NULL)
      observeEvent(input$predictionsTable_rows_selected, {
        if(!is.null(input$predictionsTable_rows_selected)) {
          selectedRows_d(input$predictionsTable_rows_selected)
        }else{
          selectedRows_d(NULL)
        }
          
      })
      
      selectedRows <- debounce(selectedRows_d, 500)
      
      
      selectedIndexes <- reactiveVal(c())
      observe({
        sRows <- selectedRows()
        pH <- plotHistory()
        newSIndex <- c()
        isolate(sIndexes <- selectedIndexes())
        for(i in seq_len(length(pH))){
          plot <- pH[[i]]
          if(i %in% sRows) newSIndex <- c(newSIndex, plot$index) 
        }

        selectedIndexes(union(intersect(sIndexes,newSIndex),newSIndex))
      })
      
      cPredictionPlot <- reactiveVal(NULL)
      cPredictionTable <- reactiveVal(NULL)
      cPredictionDiffPlot <- reactiveVal(NULL)
      cPredictionDiffTable <- reactiveVal(NULL)
      cSummary <- reactiveVal(NULL)
      cColor <- reactiveVal(NULL)
      

      # Prediction Summary and distribution plot
      observe({
        
        plotType <- input$plotType
        postType <- input$postType
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange
        
        sIndexes <- selectedIndexes()
        mean <- postType=="Mean"
        
        isolate({
          if(is.null(sIndexes) || length(sIndexes)==0){
   
            output$summaryPrediction <- renderDT(NULL)
            output$summaryPredictionPlaceholder <- renderUI(tags$div(style="margin:10px;","Select at least one row in the upper table."))
            output$densityPredictionPlot <- renderPlot(NULL)
            output$distPredictionPlotPlaceholder <- renderUI(tags$div(style="margin:10px;","Select at least one row in the upper table."))
            output$differencePredictionPlot <- renderPlot(NULL)
            output$differencePrediction <- renderDT(NULL)
            output$differencePredictionPlaceholder <- renderUI(tags$div(style="margin:10px;","Select two(!) rows in the upper table."))
            removeCssClass("reportPrediction","btn-primary")
            removeCssClass("reportPredictionDifference","btn-primary")
            cPredictionPlot(NULL)
            cPredictionTable(NULL)
            cPredictionDiffPlot(NULL)
            cPredictionDiffTable(NULL)
            # cSummary(NULL)
            # cColor(NULL)
          }else{
            
            addCssClass("reportPrediction","btn-primary")
            
            pH <- plotHistory()
            data <- list()
            colors <- c()
            indexes <- c()
            
            summary <- data.frame(matrix(numeric(0), ncol=5))
            summary_header <- c("index", "CI_low", "median",
                                "CI_high", "Color coding")
            names(summary) <- summary_header
            
            for(i in seq_len(length(pH))){
              plot <- pH[[i]]
              if(!plot$index %in% sIndexes) next
              new_data <- plot$values
              pp <- pIDM$get.selected_BAYSIS_stan_model()$get_density(rstanModel, new_data, mean=mean)
              data <- list.append(data, pp)
              colors <- c(colors, plot$color)
              indexes <- c(indexes, plot$index)
              
              interval <- ciOfDens(pp, hdiRange, tolower(hdiType))

              tmp <- data.frame(plot$index, interval$lower,
                                interval$center, interval$upper, 
                                paste0(tags$div(HTML("█"),
                                                style=paste0("color:",plot$color,";"))))

              
              names(tmp) <- summary_header
              summary <- rbind(summary, tmp)
            }
            
            # Is global variance selected and data is count data?
            countData <- !mean && pIDM$get.selected_BAYSIS_stan_model()$is.discrete
            
            
            plot <- NULL
            if(plotType=="Density"){
              plot <- plotSeveralAreas(data,countData=countData, prop=hdiRange, x_axis = response, method=tolower(hdiType), colors) + 
                pIDM$get.selected_BAYSIS_stan_model()$plot_scale(x=T)
              
            }else{
              plot <- plotSeveralViolins(data, prop=hdiRange, x_axis=as.character(indexes), y_axis=response, 
                                         method=tolower(hdiType), colors) + 
                pIDM$get.selected_BAYSIS_stan_model()$plot_scale(y=T)
            }

            #global for report
            cPredictionPlot(removeUnnecessaryEnvInPlot(plot))
            output$densityPredictionPlot <- renderPlot(plot)
            output$distPredictionPlotPlaceholder <- renderUI(tags$div())
            
            #Summary prediction
            summary[,2] <- formatC(summary[,2], digits=4)
            summary[,3] <- formatC(summary[,3], digits=4)
            summary[,4] <- formatC(summary[,4], digits=4)
            
            #index, i_min, median, i_max
            dt_summary <- datatable(summary, rownames=F, escape=F, 
                            options=list(paging=F,searching=F, info=F, ordering=T,
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
              formatStyle(colnames(summary), lineHeight="70%")
          

            #global for report
            cPredictionTable(dt_summary)
            output$summaryPrediction <- renderDT(dt_summary)
            output$summaryPredictionPlaceholder <- renderUI(tags$div())
            
            #Prediction difference
            if(length(data)==2){
              addCssClass("reportPredictionDifference","btn-primary")
              
              new_data1 <- NULL
              new_data2 <- NULL
              
              for(plotData in pH){
                if(!plotData$index %in% indexes) next
                if(is.null(new_data1)){
                  new_data1 <- plotData$values
                }else{
                  new_data2 <- plotData$values
                  break
                }
              }
              
              
              plotDiff <- NULL
              mean_diff <- NULL
              overlap_vector <- NULL
              if(mean){
                mean_diff <- pIDM$get.selected_BAYSIS_stan_model()$get_overlap(rstanModel, new_data1, new_data2, mean=T)
                plotDiff <- plotDiff(c(mean_diff), prop=hdiRange, 
                                 x_axis = response, method=tolower(hdiType)) +
                  ggtitle("Difference")

              }else{
                overlap_vector <- pIDM$get.selected_BAYSIS_stan_model()$get_overlap(rstanModel, new_data1, new_data2, mean=F)
                plotDiff <- ggplot(data.frame(x=overlap_vector)) + 
                  geom_histogram(aes(x=x), bins=50) + 
                  ggtitle("Overlap")
              }
              
              #plot
              cPredictionDiffPlot(removeUnnecessaryEnvInPlot(plotDiff))
              output$differencePredictionPlot <- renderPlot(plotDiff)
              
              #table
              if(mean){
                summaryTable <- data.frame(matrix(numeric(0), ncol=4))
                summaryTable_header <- c("CI_low", "median",
                                         "CI_high", "pi")
                names(summaryTable) <- summaryTable_header
                interval <- c()
                if(tolower(hdiType) == "hdi"){
                  interval <- bayestestR::hdi(mean_diff, ci=hdiRange)
                }else{
                  interval <- eti(mean_diff, ci=hdiRange)
                }
                summaryTable[1,] <- c(interval$CI_low, median(mean_diff), interval$CI_high, PI.value(mean_diff))
                
                summaryTable[,1] <- formatC(summaryTable[,1], digits=4)
                summaryTable[,2] <- formatC(summaryTable[,2], digits=4)
                summaryTable[,3] <- formatC(summaryTable[,3], digits=4)
                summaryTable[,4] <- formatC(summaryTable[,4], digits=4)
                
                dt_prediction <- datatable(summaryTable, rownames=F, escape=F, 
                                options=list(paging=F,searching=F, info=F, ordering=F,
                                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
                  formatStyle(colnames(summaryTable), lineHeight="70%")
                cPredictionDiffTable(dt_prediction)
                output$differencePrediction <- renderDT(dt_prediction)
              }else{
                summaryTable <- data.frame(matrix(numeric(0), ncol=3))
                summaryTable_header <- c( "min", "median", "max")
                names(summaryTable) <- summaryTable_header
                
                summaryTable[1,] <- c(min(overlap_vector), median(overlap_vector), 
                                      max(overlap_vector))
                
                summaryTable[,1] <- formatC(summaryTable[,1], digits=4)
                summaryTable[,2] <- formatC(summaryTable[,2], digits=4)
                summaryTable[,3] <- formatC(summaryTable[,3], digits=4)
                
                dt_prediction <- datatable(summaryTable, rownames=F, escape=F, 
                                options=list(paging=F,searching=F, info=F, ordering=F,
                                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
                  formatStyle(colnames(summaryTable), lineHeight="70%")
                cPredictionDiffTable(dt_prediction)
                
                output$differencePrediction <- renderDT(dt_prediction)
              }
              
              t <- tags$div(
                style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
                
                tags$h5(
                  style="width:fit-content;",
                  "What is shown here",
                  
                  bslib::popover(
                    trigger = icon(id="modelPredictionOverlapGlobal",name="question-circle", 
                                   style="margin:2px 0px 0px 3px;"),
                    title = "Overlapping plot",
                    HTML(tooltip$modelPredictionOverlapGlobal),
                    options = list(trigger="hover")
                  )),
                
                paste0(indexes[length(indexes):1], collapse=" - "),
              )
              
              output$differencePredictionPlaceholder <- renderUI(t)
            }else{
              output$differencePredictionPlot <- renderPlot(NULL)
              output$differencePrediction <- renderDT(NULL)
              output$differencePredictionPlaceholder <- renderUI(tags$div(style="margin:10px;","Select two(!) rows in the upper table."))
              cPredictionDiffPlot(NULL)
              cPredictionDiffTable(NULL)
            }

          }
        })
      })

      
      plot_prediction_distribution_counter <<- 0
      # Report prediction distribution and table
      observeEvent(input$reportPrediction, {

        pIDM_id <- pIDM$get.id()
        
        plotType <- input$plotType
        postType <- input$postType
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange

        sIndexes <- selectedIndexes()
        if(is.null(sIndexes) || length(sIndexes)==0){
          showNotification("Please select at least one element of the top table.", type="warning")
          return()
        }
        
        plot_prediction_distribution_counter <<- plot_prediction_distribution_counter+1
        

        tEnum <- reportTypeEnum()


        uiText <- paste0("The table shows distributions over predictions ", 
                         ifelse(postType=="Full","on the global variance ","on their mean "), 
                         "(using a ", hdiRange*100, "% ", hdiType, "). ")

        ui <- tags$div(style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
                       uiText)
        
        
        table <- cPredictionTable()
        plot <- cPredictionPlot()
        
        inputName <- pIDM$get.name()
        inputName <- str_replace_all(inputName, " ", "_")
        inputName <- paste0(inputName,"_",plot_prediction_distribution_counter)
        
        reportDiv <- reportType(div=list(ui=ui, plot=plot, table=table),
                                space=c("20","20"))
        
        #thumbnail
        ggsave(paste0(report_folder, "/Thumbnails/prediction_distribution_", 
                      inputName,".jpg"), 
               plot, device="jpeg", width=100, height=100, units="px", dpi=25)
        
        
        #latex
        cSu <- cSummary()
        cSu <- cSu[cSu$index %in% table$x$data$index,]
        
        latex <- modelPredictionItem(summaryTable = table$x$data, valueTable = cSu, 
                                     color = cColor(), plotType=plotType,
                                     postType=postType, hdiType=hdiType, hdiRange=hdiRange, 
                                     plotId=paste0("predictions_",inputName), plot=plot)
        
        
        #Add formula element to report progress
        addItem(moduleType = "evaluation",
                dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM_id, 
                pDIM_name = pIDM$get.name(),
                imgFile = paste0("Report/Thumbnails/prediction_distribution_",inputName,".jpg"),
                type=tEnum$prediction, object=list(div=reportDiv, latex=latex, model_latex=""), 
                show=T, singleton=F, global_reportProgressModel=global_reportProgressModel)
        
      })
      
      # Report prediction differenc distribution and table
      observeEvent(input$reportPredictionDifference, {

        pIDM_id <- pIDM$get.id()
        
        postType <- input$postType
        hdiType <- input$hdiType
        hdiRange <- input$hdiRange
        
        sIndexes <- selectedIndexes()
        if(is.null(sIndexes) || length(sIndexes)!=2){
          showNotification("Please select two predictions from the table above.", type="warning")
          return()
        }
        
        plot_prediction_distribution_counter <<- plot_prediction_distribution_counter+1
        
        
        tEnum <- reportTypeEnum()
        
        ui <- tags$div()
        uiText <- ""
        if(postType=="Mean"){
          uiText <- paste0("The distribution shows the difference between the expectaion value of both predictions ",
                           "using a ", hdiRange*100, "% ", toupper(hdiType), ". ")
          ui <- tags$div(style="font-weight:bold; margin: 5px 0px 5px 12px; word-wrap: break-word;",
                         uiText)
        }


        table <- cPredictionDiffTable()
        plot <- cPredictionDiffPlot()
        
        inputName <- pIDM$get.name()
        inputName <- str_replace_all(inputName, " ", "_")
        inputName <- paste0(inputName,"_",plot_prediction_distribution_counter)
        

        #value table
        cSu <- cSummary()
        if(is.null(cSu)) browser()
        cSu <- cSu[cSu$index %in% selectedIndexes(),1:(dim(cSu)[2]-2)]
        cnames <- colnames(cSu)
        valueTable <- datatable(cSu, rownames=F, escape=F, colnames=cnames,
                                              options=list(paging=F,searching=F, info=F, ordering=T,
                                                           columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
          formatStyle(colnames(cSu), lineHeight="70%")
        
        reportDiv <- reportType(div=list(ui=ui, table=valueTable, plot=plot, table=table),
                                space=c("20","20","20"))
        
        #thumbnail
        ggsave(paste0(report_folder, "/Thumbnails/prediction_distribution_diff_", 
                      inputName,".jpg"), 
               plot, device="jpeg", width=100, height=100, units="px", dpi=25)
        
        
        #latex
        latex <- modelPredictionDifferenceItem(summaryTable = table$x$data, valueTable = cSu, 
                                     color = cColor(),
                                     postType=postType, hdiType=hdiType, hdiRange=hdiRange, 
                                     plotId=paste0("prediction_distribution_diff_", inputName), 
                                     plot=plot)
        
        
        #Add formula element to report progress
        addItem(moduleType = "evaluation",
                dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM_id, 
                pDIM_name = pIDM$get.name(),
                imgFile = paste0("Report/Thumbnails/prediction_distribution_diff_",inputName,".jpg"),
                type=tEnum$prediction, object=list(div=reportDiv, latex=latex, model_latex=""), 
                show=T, singleton=F, global_reportProgressModel=global_reportProgressModel)
        
        
      })
    }
  )
}





calculateEffectMatrix <- function(selGroup, selSlope, numericVal, matrixType, 
                                  fit, response, sCell=NULL, hdiType, ci){
  
  
  data <- data.frame()
  cnames <- list()
  
  eff <- list()

  
  if(!is.null(selGroup) && (length(selGroup) > 1 || selGroup != "")){
    eff <- mp_effects(fit, catVar=selGroup, numVar=NULL, numVal=numericVal, 
                      response=response, "linpred") 
    eff <- group_mp_effects(eff, catVar=selGroup, numVar=selSlope)
    mps <- eff$mp
    lMatrix <- length(mps)
    m <- matrix(NA, lMatrix, lMatrix)
    
    for(i in 1:lMatrix){
      cnames <- list.append(cnames, paste0(mps[[i]]$comb, collapse="."))
      for(j in 1:lMatrix){
        if(i==j){
          m[j,i] <- "-"
        } else if(j < i){
          m[j,i] <- ""
        } else{
          m[j,i] <- str_trim(formatC(eff_diff(mps[[j]]$values,mps[[i]]$values,matrixType, hdiType=hdiType, ci=ci), digits=4))
        }
      }
    }
    data <- data.frame(m)
  }else if(!is.null(selSlope) && selSlope != ""){
    eff <- mp_effects(fit, catVar=NULL, numVar=selSlope, numVal=NULL, 
                      response=response,"linpred")
    eff <- group_mp_effects(eff, catVar=selGroup, numVar=selSlope)
    mps <- eff$mp
    lMatrix <- length(mps)
    m <- matrix(NA, lMatrix, lMatrix)
    
    for(i in seq_len(lMatrix)){
      cnames <- list.append(cnames, paste0(mps[[i]]$comb, collapse="."))
      for(j in 1:lMatrix){
        if(j < i){
          m[j,i] <- ""
        } else  if(i==j){
          m[j,i] <- str_trim(formatC(eff_diff(mps[[i]]$values,rep(0,length(mps[[i]]$values)),matrixType, hdiType=hdiType, ci=ci), digits=4))
        }else{
          m[j,i] <- str_trim(formatC(eff_diff(mps[[i]]$values,mps[[j]]$values,matrixType, hdiType=hdiType, ci=ci), digits=4))
        }
      }
    }
    data <- data.frame(m)
  }else{
    return(datatable(data))
  }


  comb <- eff$combinations
  if(is.null(comb)){
    comb <- data.frame(paste0(eff$num, collapse=":"))
    colnames(comb) <- paste0(eff$num, collapse=":")
  }
  header <- createHeader(comb)
  headerNames <- colnames(comb)
  headerNames <- headerNames[length(headerNames):1]

  #list(dt, latex)
  dt_latex <- makeDT(df=data, header=header, headerNames=headerNames, sCell)
  dt <- dt_latex$dt
  
  dt <- dt %>% 
    formatStyle(columns = colnames(data), fontWeight = "bold")
                
  colorize <- T
  if(colorize){
    if(matrixType == "pi"){
      cuts <- seq(0.9,1,length=11)
      colorMap <- paste0("rgba(46, 127, 24, ",0.1+(cuts-0.9)*5,")")
      colorMap <- c("",colorMap)
      dt <- dt %>% 
        formatStyle(columns = colnames(data), 
                    backgroundColor = styleInterval(cuts=cuts,
                                                    values=colorMap))
    }else{
      dd <- c()
      for(i in data){
        for(j in i){
          suppressWarnings(if(!is.na(as.numeric(j))) dd <- c(dd, as.numeric(j)))
        }
      }
      if(length(dd) > 2){
        cuts <- c()
        colorMap <- c()
        if(min(dd) <= 0){
          cuts <- seq(min(dd),0,length=99)
          colfunc <- colorRampPalette(c(BAYAS_COLORS$`--effectMatrix-colors-1`,
                                        BAYAS_COLORS$`--effectMatrix-colors-2`))
          colorMap <- colfunc(101)[1:100]
        }
        if(max(dd) > 0){
          colfunc <- colorRampPalette(c(BAYAS_COLORS$`--effectMatrix-colors-2`,
                                        BAYAS_COLORS$`--effectMatrix-colors-3`))
          if(min(dd) <= 0){
            cuts <- c(cuts, seq(0,max(dd),length=100)[2:100])
            colorMap <- c(colorMap,colfunc(100)[2:100])
          }else{
            cuts <- c(cuts, seq(0,max(dd),length=99))
            colorMap <- c(colorMap,colfunc(101)[2:101])
          }
        }
        
        dt <- dt %>% formatStyle(columns = colnames(data), 
                                 backgroundColor = styleInterval(cuts=cuts,
                                                                 values=colorMap))
      }
    }
  }
  
  ret <- list(dt=dt, latex = list(headerNames=headerNames, headers = dt_latex$l_latex))
  
  return(ret)
}



#returns a named list with nested named lists of header names
#list(male=list(small=c("1","2"), big=c("1","2")),
#     female=list(small=c("1","2"), big=c("1","2")))
createHeader <- function(comb){
  ret <- list()
  if(dim(comb)[2]==1){
    return(comb[,1])
  }else{
    val <- unique(comb[,dim(comb)[2]])
    for(n in val){
      ret <- list.append(ret, createHeader(comb[comb[dim(comb)[2]] == n,-dim(comb)[2],drop=F]), n)
    }
  }
  return(ret)
}

#returns a fully optimized datatable (with grouped headers)
makeDT <- function(df, header, headerNames, sCell){

  depth <- getHeaderDepth(header)
  
  for(i in depth:1){
    h <- getRowHeaderOfDepth(header, i-1)
    new_df <- data.frame(h)
    colnames(new_df) <- paste0("h",i)
    df <- cbind(new_df,df)
  }
  h <- c(rep("",(depth-1)), headerNames[depth], rep("",length(headerNames)-depth))
  
  l <- lapply(0:(depth-1), function(i) HTML(getTableHeaderAtPosition(header, headerNames, i)))
  l_latex <- lapply(0:(depth-1), function(i) getTableHeaderAtPositionForLatex(header, headerNames, i))

  sketch = tags$table(
    class = 'display',
    tags$thead(
      l
    )
  )
  
  headjs <- getHeadJS(header, headerNames)
  
  if(is.null(sCell)){
    dt <- datatable(df,  escape=F,  container=sketch,
                    rownames=F, colnames=h,
                    options=list(paging=F,searching=F, info=F, ordering=F, 
                                 scrollX=T, scrollY=400, scrollCollapse=T,
                                 columnDefs = list(list(className='dt-center', targets="_all")),
                                 headerCallback = JS(headjs)
                    ),
                    selection=list(mode='single', target='cell'))
  }else{
    dt <- datatable(df,  escape=F,  container=sketch,
                    rownames=F, colnames=h,
                    options=list(paging=F,searching=F, info=F, ordering=F, 
                                 scrollX=T, scrollY=400, scrollCollapse=T,
                                 columnDefs = list(list(className='dt-center', targets="_all")),
                                 headerCallback = JS(headjs)
                    ),
                    selection=list(mode='single', target='cell', selected=matrix(sCell,1,2)))
  }
  dt <- dt %>%
    formatStyle(columns = c(paste0("h",1:length(headerNames))), fontWeight = 'bold') %>%
    formatStyle(columns = c(paste0("h",length(headerNames))), borderRight = 'solid 1px')
  
  return(list(dt=dt,l_latex=l_latex))
}



#returns the number of cells (in a row) that have to be merged for the given 
#header element
getCellWidth <- function(element){
  sum <- 0
  if(class(element)=="list"){
    for(i in 1:length(element)){
      sum <- sum + getCellWidth(element[[i]])
    }
  }else{
    return(length(element))
  }
  return(sum)
}

#returns the total depth of header
getHeaderDepth <- function(element){
  sum <- 1
  if(class(element)=="list"){
    tmpSum <- 0
    for(i in 1:length(element)){
      tmpSum <- max(tmpSum, getHeaderDepth(element[[i]]))
    }
    sum <- sum+tmpSum
  }else{
    return(1)
  }
  return(sum)
}


#returns row header of given depth
getRowHeaderOfDepth <- function(header, depth){
  ret <- c()
  if(class(header)=="list" && depth > 0){
    for(i in 1:length(header)){
      ret <- c(ret,as.character(getRowHeaderOfDepth(header[[i]], depth-1)))
    }
  }else if(class(header)=="list" && depth == 0){
    tmp_ret <- c()
    for(n in names(header)){
      tmp_ret <- c(tmp_ret,rep(n,getCellWidth(header[[n]])))
    }
    return(tmp_ret)
  }else{
    if(depth != 0){
      stop("To deep")
    }
    return(header)
  }
  return(ret)
}

#returns a named list, with names equal header names and value equal number of colspan
getColHeaderOfDepth <- function(header, depth){
  ret <- list()
  if(class(header)=="list" && depth > 0){
    for(i in 1:length(header)){
      ret <- c(ret,getColHeaderOfDepth(header[[i]], depth-1))
    }
  }else if(class(header)=="list" && depth == 0){
    tmp_ret <- list()
    for(n in names(header)){
      tmp_ret <- list.append(tmp_ret,getCellWidth(header[[n]]),n)
    }
    return(tmp_ret)
  }else{
    if(depth != 0){
      stop("To deep")
    }
    tmp_ret <- list()
    for(n in header){
      tmp_ret <- list.append(tmp_ret,1,n)
    }
    return(tmp_ret)
  }
  return(ret)
}

#returns a html header (tr,th elements)
getTableHeaderAtPosition <- function(header, headerNames, depth){
  h <- getColHeaderOfDepth(header, depth)
  rowHeader <- c(rep("",depth), headerNames[depth+1], rep("",length(headerNames)-(depth+1)))
  ret <- "<tr>"
  
  for(n in rowHeader){
    ret <- paste0(ret, tags$th(colspan=1,n))
  }
  for(n in names(h)){
    ret <- paste0(ret, tags$th(colspan=h[[n]],n))
  }
  
  ret <- paste0(ret, "</tr>")
  return(ret)
}

#returns a html header (tr,th elements)
getTableHeaderAtPositionForLatex <- function(header, headerNames, depth){
  h <- getColHeaderOfDepth(header, depth)
  return(h)
}

#returns javascript string with header optimization 
getHeadJS <- function(header, headerNames){
  depth <- getHeaderDepth(header)
  ret <- "function(thead) {"
  
  index_bottomBorder<- c()
  index_rightBorder <- c()
  index_center <- c()
  
  startLineIndex <- 0 
  endLineIndex <- 0
  
  for(i in 0:(depth-1)){
    
    endLineIndex <- startLineIndex + length(getColHeaderOfDepth(header, i)) + length(headerNames) -1
    
    #add border bottom
    if(i < depth-1){
      for(j in startLineIndex:endLineIndex){
        index_bottomBorder <- c(index_bottomBorder,j)
        index_center <- c(index_center,j)
      }
    }
    
    #add border right
    index_rightBorder <- c(index_rightBorder, startLineIndex+length(headerNames)-1)
    
    
    if(i == 0 && length(headerNames) > 1){
      for(j in (startLineIndex+length(headerNames)):(endLineIndex-1)){
        index_rightBorder <- c(index_rightBorder,j)
      }
    }else if(length(headerNames) > 1){
      colTop <- getColHeaderOfDepth(header, 0)
      colCur <- getColHeaderOfDepth(header, i)
      sum <- 0
      topIndex <- 1
      topSum <- colTop[[1]]
      for(colIndex in 1:length(colCur)){
        col <- colCur[[colIndex]]
        sum <- sum + col
        if(sum == topSum){
          index_rightBorder <- c(index_rightBorder, startLineIndex+length(headerNames) +colIndex-1)
          topIndex <- topIndex+1
          if(topIndex >= length(colTop)) break
          topSum <- topSum + colTop[[topIndex]]
        }
      }
    }
    
    startLineIndex <- startLineIndex + length(getColHeaderOfDepth(header, i)) + length(headerNames) 
  }
  
  
  for(i in index_bottomBorder){
    ret <- paste0(ret, "$(thead).closest('thead').find('th').eq(",i,").css('border-bottom', '0px');")
  }
  for(i in index_rightBorder){
    ret <- paste0(ret, "$(thead).closest('thead').find('th').eq(",i,").css('border-right', 'solid 1px');")
  }            
  for(i in index_center){
    ret <- paste0(ret, "$(thead).closest('thead').find('th').eq(",i,").css('text-align', 'center');")
  }
  
  ret <- paste0(ret, "}")
  return(ret)
}

shinyInput <- function(FUN, len, id, value, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id,"_", i), 
                                  onclick = paste0('Shiny.setInputValue(\"',id,'\",\"' , value[i] ,'\");'),
                                  ...))
  }
  inputs
}
