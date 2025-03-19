init_creatingModel_function <- function(input, output, session, dataModel, mCDList, global_reportProgressModel){

  ns <- NS("creatingModel")

  #debounce timer
  debounceTime1 <- 50
  debounceTime2 <- 50
  debounceTime3 <- 150
  debounceTime4 <- 150
  debounceTime5 <- 150
  debounceTime6 <- 150
  debounceTime7 <- 50
  debounceTime8 <- 300
  debounceTime9 <- 150
  debounceTime10 <- 30
  debounceTime11 <- 30
  debounceTime12 <- 150
  debounceTime13 <- 150

  
  #Model
  cMCD <<- ModelCreatingData$new("_tmp") #Static!
 
  #Opens a modal by change
  openPlanningModal <- reactiveVal(NULL)
  openPlanningModalOtherVariable <- reactiveVal(NULL)
  
  #Formula model
  pF <- PlanningFormula$new(NS("formula"))
  cMCD$setFormulaObject(pF)
  pFAE <- planningFormulaAttributeEnum()
  pFE <- planningFormulaEnum()
  
  #formula server module
  planningFormulaServer("formula", pF)
  


  
  #Observe list of models
  observe({
    mCDList$dependReactiveValue("mcdList")
    
    isolate({
      
      l <- mCDList$getMCDList()
      if(is.null(l) || is.empty(l)){
        cMCD$setModelName("_tmp")
        updateSelectInput(session=session, inputId=ns("createdExperiments"),
                          choices=character(0), selected="")
        session$sendCustomMessage(type = "resetValue", message = ns("createdExperiments"))
        
      }else{
        updateSelectInput(session=session, inputId=ns("createdExperiments"),
                          choices=names(l), selected = mCDList$getSelected()) 
      }
    })
  })
  
  newExperiment <- reactiveVal(0)
  sapply(1:6, function(i){
    observeEvent(input[[ns(paste0("exampleExperiment_",i))]], {
      shinyjs::removeClass(selector="button.planningExampleBtn", class="planningSelectedExampleBtn")
      shinyjs::addClass(ns(paste0("exampleExperiment_",i)), class="planningSelectedExampleBtn")
      newExperiment(i)
    })
  })
  #Update info box of existing experiments
  output[[ns("existingExperimentInfo")]] <- renderText({
    getExistingExperimentInfo(newExperiment())
  })
  observe({
    name <- getExistingExperimentName(newExperiment())
    updateTextInput(session=session, ns("modelName"), value=name)
  })
  
  #New Experiment
  observeEvent(input[[ns("newModel")]], {
    newExperiment(1)
    showModal(
      planning_newExperiment(ns)
    )
  })
  

  #Start, after closing 'New Experiment' Modal
  observeEvent(input[[ns("startPlanning")]], ignoreInit = F, {
    if(input[[ns("modelName")]] %in% names(mCDList$getMCDList())){
      addCssClass(ns("modelName"), "border_invalid")
      output[[ns("modelNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "This name is already in use"
        )
      )
    }else if(str_trim(input[[ns("modelName")]]) == ""){
      addCssClass(ns("modelName"), "border_invalid")
      output[[ns("modelNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "Please enter a name"
        )
      )
    }else if(input[[ns("modelName")]] == "_tmp"){
      addCssClass(ns("modelName"), "border_invalid")
      output[[ns("modelNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "This name is invalid"
        )
      )
    }else{
      removeCssClass(ns("modelName"), "border_invalid")
      output[[ns("modelNameWarning")]] <- renderUI(NULL)
      
      l <- mCDList$getMCDList()
      
      #save current model (if not "_tmp")
      name <- cMCD$getModelName()
      if(name != "_tmp"){
        copy <- cMCD$getInstance()
        if(name %in% names(l)){
          l[[name]] <- copy
        }else{
          l <- list.append(l, copy, name)
        }
      }
  
      #Load model if an non empty experiment is choosen
      newModel <- NULL
      if(newExperiment()==1){
        newModel <- ModelCreatingData$new(input[[ns("modelName")]])
        newModel$setData(data.frame(Response=NA))
        
        lpF <- PlanningFormula$new(NS("formula"))
        newModel$setFormulaObject(lpF)
        
        mcdResponse <- newModel$getMcdResponse()
        mcdResponse$setStep(c(1))

      }else{
        exp <- case_when(
          newExperiment() == 2 ~ "2_t-test",
          newExperiment() == 3 ~ "3_simple_regression"
        )
        file <- paste0(planning_model_folder, "/", exp, ".rds")

        #new
        newModelState <- readRDS(file)
        newModel <- ModelCreatingData$new(input[[ns("modelName")]])
        lpF <- PlanningFormula$new(NS("formula"))
        newModel$setFormulaObject(lpF)
        loadObject(newModel, "ModelCreatingData", file, F)
        
        newModel$setModelName(input[[ns("modelName")]])
      }
      
      l <- list.append(l, newModel, newModel$getModelName())

      mCDList$setMCDList(l)
      mCDList$setSelected(newModel$getModelName())
      
      cMCD$doTriggerLoadMCD()
    }
  })
  
  #Change in experiment
  observe({
    cMCD$getReactive("loadMCD")
    isolate({
      #Updates ui elements that are "static"
      updateBayasNumericInput(inputId=ns("globalRandomSeed"), value=cMCD$getGenerateSeed())
      updateCheckboxInput(session=session, inputId=ns("createDataSetAutomatically"), 
                            value=cMCD$getGenerateDataAutomatically())
      updateBayasNumericInput(inputId=ns("ssdDesiredPower"), value=cMCD$getMcdSSD()$getPower())
      updateBayasNumericInput(inputId=ns("ssdMaxN"), value=cMCD$getMcdSSD()$getMaxN())
      updateBayasNumericInput(inputId=ns("ssdAcceptedAccuracy"), value=cMCD$getMcdSSD()$getAccuarcy())
      updateBayasNumericInput(inputId=ns("ssdSeed"), value=cMCD$getMcdSSD()$getSeed())
      
      ssd_g(cMCD$getMcdSSD()$getSsdObject())
      
      removeModal()
      
      #Switch to '1. Response' Tab
      updateTabsetPanel(inputId=ns("uiOfSteps"), selected="1. Response")
      
      #Open modal for response definition (if an empty experiment is choosen)
      if(newExperiment()==1)
        openPlanningModal(trigger(openPlanningModal()))
    })
  })
  
  #Clone experiment
  observeEvent(input[[ns("cloneExperiment")]], {
    sel <- mCDList$getSelected()
    if(is.null(sel) || sel == ""){
      showNotification("Nothing to copy.", type="warning")
      return()
    }
    name <- sel
    copyName <- mCDList$getValidName(paste0(name,"-copy"))
    showModal(
      modalDialog(
        tags$div(
          textInput(ns("cloneExperimentName"), label="Name of copy", value=copyName,
                    placeholder=copyName),
          uiOutput(ns("cloneExperimentNameWarning"))
        ),

        footer = tags$div(
          style = "width:100%",
          tags$span(
            modalButton("Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            actionButton(ns("confirmModalCloneExperiment"), "Confirm", class="btn-primary")
          )
        ),
        
        title = paste0("Copy of '", name , "'"),
        easyClose=T,
        size="s"
      )
    )
  })
  
  #Finally clone experiment
  observeEvent(input[[ns("confirmModalCloneExperiment")]], {
    if(input[[ns("cloneExperimentName")]] %in% names(mCDList$getMCDList())){
      addCssClass(ns("cloneExperimentName"), "border_invalid")
      output[[ns("cloneExperimentNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "This name is already in use"
        )
      )
    }else if(str_trim(input[[ns("cloneExperimentName")]]) == ""){
      addCssClass(ns("cloneExperimentName"), "border_invalid")
      output[[ns("cloneExperimentNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "Please enter a name"
        )
      )
    }else if(input[[ns("cloneExperimentName")]] == "_tmp"){
      addCssClass(ns("cloneExperimentName"), "border_invalid")
      output[[ns("cloneExperimentNameWarning")]] <- renderUI(
        tags$div(
          class="fontColor-error",
          style="font-weight:bold; font-size:smaller; text-align: left;",
          style="margin-top: -15px; margin-left:5px;",
          "This name is invalid"
        )
      )
    }else{
      removeModal()
      copy <- cMCD$getInstance()
      copy$setModelName(input[[ns("cloneExperimentName")]])
      mCDList$addMCDList(copy,input[[ns("cloneExperimentName")]])
      mCDList$setSelected(input[[ns("cloneExperimentName")]])
    }
  })
  
  
  #Remove experiment
  observeEvent(input[[ns("removeExperiment")]], {
    l <- mCDList$getMCDList()

    if(is.null(input[[ns("createdExperiments")]])) return()
    if(input[[ns("createdExperiments")]] %in% names(l)){
      #Open modal to be sure to remove
      
      text <- tags$div(
        tags$div(
          class="fontColor-error",
          style="font-size:32px;margin-top:-20px;margin-bottom:15px;",
                 icon("exclamation-circle")),
        tags$div(
          clasS="fontColor-regular-bg",
          style="font-weight:bold; font-size:14px;",
          paste0("Are you sure to remove '",input[[ns("createdExperiments")]],"'?"))
      )
      
      shinyalert::shinyalert(inputId = ns("shinyalertRemoveExperiment"),
                             title=NULL,
                             text=as.character(text),
                             html=T,
                             closeOnClickOutside=F,
                             showCancelButton =T,
                             size="xs")
    }
  })
  
  #finally remove experiment
  observeEvent(input[[ns("shinyalertRemoveExperiment")]], {
    inp <- input[[ns("shinyalertRemoveExperiment")]]
    if(inp){
      #remove experiment
      l <- mCDList$getMCDList()

      newList <- list()
      for(ni in names(l)){
        if(ni != input[[ns("createdExperiments")]]){
          newList <- list.append(newList, l[[ni]], ni)
        }
      }
      mCDList$setMCDList(newList)
      
      #load first one, if exists
      if(!is.empty(newList)){
        selectedOtherVariableId(NULL)
        cMCD$setInstance(newList[[1]])
        mCDList$setSelected(cMCD$getModelName())
        
        #Updates ui elements that are "static"
        updateBayasNumericInput(inputId=ns("globalRandomSeed"), value=cMCD$getGenerateSeed())
        updateCheckboxInput(session=session, inputId=ns("createDataSetAutomatically"), 
                              value=cMCD$getGenerateDataAutomatically())
        updateBayasNumericInput(inputId=ns("ssdDesiredPower"), value=cMCD$getMcdSSD()$getPower())
        updateBayasNumericInput(inputId=ns("ssdMaxN"), value=cMCD$getMcdSSD()$getMaxN())
        updateBayasNumericInput(inputId=ns("ssdAcceptedAccuracy"), value=cMCD$getMcdSSD()$getAccuarcy())
        updateBayasNumericInput(inputId=ns("ssdSeed"), value=cMCD$getMcdSSD()$getSeed())
      }
    }
  })
  
  #Observe change in the 'created experiments' dropdown menu
  #show all input elements of tabs
  observeEvent(input[[ns("createdExperiments")]], ignoreNULL = F, {
    
    mCDList$setSelected(input[[ns("createdExperiments")]])
    if(!is.null(input[[ns("createdExperiments")]]) && input[[ns("createdExperiments")]] != ""){
      addCssClass(ns("reportPlanningExperiment"), "btn-primary")
    }else{
      removeCssClass(ns("reportPlanningExperiment"), "btn-primary")
    }
  })
  
  
  #observe changes in selected experiment
  observe({
    mCDList$dependReactiveValue("selected")

    isolate({
      
      selModel <- mCDList$getSelectedMcd()
      sel <- mCDList$getSelected()
      
      if(is.null(sel) || sel==""){
        hide(ns("tabResponse"))
        hide(ns("tabVariables"))
        hide(ns("tabPredictors"))
        hide(ns("tabParameters"))
        hide(ns("tabGenerateData"))
        hide(ns("tabSSD"))
        
        #remove/hide also formula, datatable, plot
        hide(ns("divStatModel"))
        hide(ns("divDataTable"))
        hide(ns("divDataVisualization"))
        
      }else{
        show(ns("tabResponse"))
        show(ns("tabVariables"))
        show(ns("tabPredictors"))
        show(ns("tabParameters"))
        show(ns("tabGenerateData"))
        show(ns("tabSSD"))
        
        show(ns("divStatModel"))
        show(ns("divDataTable"))
        show(ns("divDataVisualization"))
        
        #save current
        name <- cMCD$getModelName()
        if(equal0(name,sel)) return()
        l <- mCDList$getMCDList()
        
        if(name != "_tmp"){
          copy <- cMCD$getInstance()
          if(name %in% names(l)){
            l[[name]] <- copy
          }else{
            l <- list.append(l, copy, name)
          }
          mCDList$setMCDList(l)
        }

        
        #load selected
        if(sel %in% names(l)){
          selectedOtherVariableId(NULL)
          cMCD$setInstance(selModel)
          
          #Updates ui elements that are "static"
          updateBayasNumericInput(inputId=ns("globalRandomSeed"), value=cMCD$getGenerateSeed())
          updateCheckboxInput(session=session, inputId=ns("createDataSetAutomatically"), 
                                value=cMCD$getGenerateDataAutomatically())
          updateBayasNumericInput(inputId=ns("ssdDesiredPower"), value=cMCD$getMcdSSD()$getPower())
          updateBayasNumericInput(inputId=ns("ssdMaxN"), value=cMCD$getMcdSSD()$getMaxN())
          updateBayasNumericInput(inputId=ns("ssdAcceptedAccuracy"), value=cMCD$getMcdSSD()$getAccuarcy())
          updateBayasNumericInput(inputId=ns("ssdSeed"), value=cMCD$getMcdSSD()$getSeed())
          
          ssd_g(cMCD$getMcdSSD()$getSsdObject())
          
        }else{
          showNotification("Unknown model", type="error")
        }
      }
      
    })
  })
  
  #TabPanel
  observeEvent(input[[ns("uiOfSteps")]], {
    #Remove hightlight column in data
    if(input[[ns("uiOfSteps")]] == "2. Other variables"){
      id <- selectedOtherVariableId()
      cMCD$highlightDataColumn(id, F)
    }else{
      cMCD$highlightDataColumn(NULL)
    }
  })
  
  
  #Save experiment
  output[[ns("saveExperiment")]] <- downloadHandler(
    filename = function(){
      "Experiment.rds"
    },
    content = function(file){
      if(cMCD$getModelName() != "_tmp"){
        copy <- cMCD$getInstance()
        saveRDS(copy, file)
      }else{
        NULL
      }
    }
  )
  #Save experiment as state
  output[[ns("saveExperimentAsState")]] <- downloadHandler(
    filename = function(){
      "Experiment.rds"
    },
    content = function(file){
      saveObject(cMCD, file, encrypt=F)
    }
  )
  #Save ssd model and goals
  output[[ns("saveSSD")]] <- downloadHandler(
    filename = function(){
      "SSD_elements.rds"
    },
    content = function(file){
      ssdMcd <- cMCD$getMcdSSD()
      mcdFormula <- cMCD$getMcdFormula() 
      model <- mcdFormula$buildRstanFormula(round(runif(1,-1e7,1e7)))
      
      obj <- list(
        model = model,
        binomModel = fit_binom,
        goals = ssdMcd$getGoals(forSSD=T),
        maxN = ssdMcd$getMaxN(),
        power = ssdMcd$getPower(),
        con = ssdMcd$getAccuarcy()
      )
  
      saveRDS(obj, file)
    }
  )
  
  #Load experiment
  observeEvent(input[[ns("loadExperiment")]], {
    file <- input[[ns("loadExperiment")]]
    savedMCD <- readRDS(file$datapath)
    
    l <- mCDList$getMCDList()

    #save current model (if not "_tmp")
    name <- cMCD$getModelName()
    if(name != "_tmp"){
      copy <- cMCD$getInstance()
      if(name %in% names(l)){
        l[[name]] <- copy
      }else{
        l <- list.append(l, copy, name)
      }
    }
    
    l <- list.append(l, savedMCD, savedMCD$getModelName())
    mCDList$setMCDList(l)
    
    cMCD$setInstance(savedMCD)
    
    #Updates ui elements that are "static"
    updateBayasNumericInput(inputId=ns("globalRandomSeed"), value=cMCD$getGenerateSeed())
    updateCheckboxInput(session=session, inputId=ns("createDataSetAutomatically"), 
                          value=cMCD$getGenerateDataAutomatically())
    updateBayasNumericInput(inputId=ns("ssdDesiredPower"), value=cMCD$getMcdSSD()$getPower())
    updateBayasNumericInput(inputId=ns("ssdMaxN"), value=cMCD$getMcdSSD()$getMaxN())
    updateBayasNumericInput(inputId=ns("ssdAcceptedAccuracy"), value=cMCD$getMcdSSD()$getAccuarcy())
    updateBayasNumericInput(inputId=ns("ssdSeed"), value=cMCD$getMcdSSD()$getSeed())
    
    ssd_g(cMCD$getMcdSSD()$getSsdObject())
    
  })

  

  
  #Report experiment
  observeEvent(input[[ns("reportPlanningExperiment")]], {

    showModal(
      modalDialog(
        planning_reportExperiment(ns, cMCD),

        footer = tags$div(
          style = "width:100%",
          tags$span(
            actionButton(ns("cancelModalReport"), "Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            actionButton(ns("confirmModalReport"), "Report", class="btn-primary")
          )
        ),
        
        title = "Report experiment",
        easyClose=T,
        size="l"
      )
    )

  })


  observeEvent(input[[ns("switchGeneratedData")]], {
    if(input[[ns("switchGeneratedData")]]){
      enable(ns("switchGeneratedDataMinimal"))
    }else{
      disable(ns("switchGeneratedDataMinimal"))
    }
  })

  observeEvent(input[[ns("switchSSD")]], {
    if(input[[ns("switchSSD")]]){
      enable(ns("switchSSDResult"))
      enable(ns("switchSSDOnlyUsedGoals"))

      mcdSSD <- cMCD$getMcdSSD()
      ssdObject <- mcdSSD$getSsdObject()

      if(!is.null(ssdObject) && !mcdSSD$isMcdOffSame() && input[[ns("switchSSDResult")]]){
        enable(ns("switchSSDOnlyMcdOff"))
      }

    }else{
      disable(ns("switchSSDResult"))
      disable(ns("switchSSDOnlyUsedGoals"))
      disable(ns("switchSSDOnlyMcdOff"))
    }
  })

  observeEvent(input[[ns("switchSSDResult")]], {
    if(input[[ns("switchSSDResult")]]){
      mcdSSD <- cMCD$getMcdSSD()
      ssdObject <- mcdSSD$getSsdObject()
      if(!is.null(ssdObject) && !mcdSSD$isMcdOffSame()){
        enable(ns("switchSSDOnlyMcdOff"))
      }
    }else{
      disable(ns("switchSSDOnlyMcdOff"))
    }
  })
  
  
  
  #Cancel modal
  observeEvent(input[[ns("cancelModalReport")]], {
    removeModal()
  })
  
  #Finally report
  observeEvent(input[[ns("confirmModalReport")]], {

    reportExpInp = list(onlyUsedVars = input[[ns("switchOVOnlyUsed")]],
                        onlyValidParas = input[[ns("switchParametersNonRedundant")]],
                        includeGenData = input[[ns("switchGeneratedData")]],
                        replaceMinData = input[[ns("switchGeneratedDataMinimal")]],
                        includeSSD = input[[ns("switchSSD")]],
                        includeSSDResult = input[[ns("switchSSDResult")]],
                        onlyUsedGoals = input[[ns("switchSSDOnlyUsedGoals")]],
                        onlySSD = input[[ns("switchSSDOnlyMcdOff")]])
    cMCD$setReportExpInp(reportExpInp)

    
    defined <- list(resp=F, oV=F, pred=F, para=F, genData=F, ssdGoals=F, ssd=F, ssdMcdOff=F)
    mcdResp <- cMCD$getMcdResponse()
    if(!is.null(mcdResp$getDist())) defined$resp <- T 
    oVs <- cMCD$getOtherVariables()
    if(!is.empty(oVs)) defined$oV <- T 
    pred <- cMCD$getPredictors()
    if(!is.empty(pred)) defined$pred <- T 
    para <- cMCD$getParameters()
    if(!is.empty(para)) defined$para <- T 
    genData <- cMCD$getData()
    if(defined$resp && !is.null(dim(genData)[1])) defined$genData <- T
    mcdSSD <- cMCD$getMcdSSD()
    ssdObject <- mcdSSD$getSsdObject()
    if(!is.empty(mcdSSD$getGoals())) defined$ssdGoals <- T
    if(!is.null(ssdObject)){
      defined$ssd <- T
      defined$ssdMcdOff <- !mcdSSD$isMcdOffSame()
    }
    if(!any(defined)){
      showNotification("Nothing to report", type="warning")
      return()
    }

    removeModal()
    
    ##  
    ##
    ##

    itemId <- global_reportProgressModel$getNextId()

    reportDiv <- reportType(div=list(ui=tags$div()))
    
    latex <- cMCD$getLatex(itemId)
    obj <- list(ssd = latex$ssd$add)
    latex$ssd$add <- NULL

    tEnum <- reportTypeEnum()
    
    name <- mCDList$getSelected()
    #Add formula element to recommended report progress
    addItem(moduleType = "planning",
            planningName = name,
            imgFile = paste0("Images/Report/Planning.png"),
            type=tEnum$planningExperiment, object=list(div=reportDiv, latex=latex, obj=obj),
            show=T, singleton=F, global_reportProgressModel=global_reportProgressModel,
            recommended=F)
    item <- global_reportProgressModel$getItem(itemId)

    #Set individual data of report item, storing the checkbox inputs
    #defined
    #preSel
    defined <- list(resp=F, oV=F, pred=F, para=F, genData=F, ssdGoals=F, ssd=F, ssdMcdOff=F)
    preSel <- cMCD$getReportExpInp()
    preSel[is.null(preSel)] <- c(T,T,T,T,T,T,T,F)[is.null(preSel)]
    
    mcdResp <- cMCD$getMcdResponse()
    if(!is.null(mcdResp$getDist())) defined$resp <- T 
    
    oVs <- cMCD$getOtherVariables()
    if(!is.empty(oVs)) defined$oV <- T 
    
    pred <- cMCD$getPredictors()
    if(!is.empty(pred)) defined$pred <- T 
    
    para <- cMCD$getParameters()
    if(!is.empty(para)) defined$para <- T 
    
    genData <- cMCD$getData()
    if(defined$resp && !is.null(dim(genData)[1])) defined$genData <- T
    
    mcdSSD <- cMCD$getMcdSSD()
    ssdObject <- mcdSSD$getSsdObject()
    
    if(!is.empty(mcdSSD$getGoals())) defined$ssdGoals <- T
    
    if(!is.null(ssdObject)){
      defined$ssd <- T
      defined$ssdMcdOff <- !mcdSSD$isMcdOffSame()
    }
    nsT <- NS(paste0(ns(""), itemId))
    item$setIndividualData(list(defined=defined, preSel=preSel, ns=nsT))

    
    
    
    #Two items?
    mcdSSD <- cMCD$getMcdSSD()
    ssdObject <- mcdSSD$getSsdObject()
    
    if(!is.null(ssdObject) && !mcdSSD$isMcdOffSame() && 
       !reportExpInp$onlySSD && reportExpInp$includeSSDResult &&
       reportExpInp$includeSSD){
      
      mcdCopy <- cMCD$getInstance()
      mcdCopy$getMcdSSD()$setSsdObject(NULL)
      mcdCopy$getMcdSSD()$setMcdOff(NULL)
      
      itemId <- global_reportProgressModel$getNextId()
      
      reportDiv <- reportType(div=list(ui=tags$div()))
      
      latex <- mcdCopy$getLatex(id=itemId)
      obj <- list(ssd = latex$ssd$add)
      latex$ssd$add <- NULL
      # latex <- paste0(unlist(latex),collapse="")
      # latex <- paste0(latex, "\n end")
      
      tEnum <- reportTypeEnum()
      
      name <- mCDList$getSelected()
      #Add formula element to recommended report progress
      addItem(moduleType = "planning",
              planningName = name,
              imgFile = paste0("Images/Report/Planning.png"),
              type=tEnum$planningExperiment, object=list(div=reportDiv, latex=latex, obj=obj),
              show=T, singleton=F, global_reportProgressModel=global_reportProgressModel,
              recommended=F)
      item2 <- global_reportProgressModel$getItem(itemId)
      
      #Set individual data of report item, storing the checkbox inputs
      #defined
      #preSel
      defined <- list(resp=F, oV=F, pred=F, para=F, genData=F, ssdGoals=F, ssd=F, ssdMcdOff=F)
      preSel <- mcdCopy$getReportExpInp()
      preSel[is.null(preSel)] <- c(T,T,T,T,T,T,T,F)[is.null(preSel)]
      
      mcdResp <- mcdCopy$getMcdResponse()
      if(!is.null(mcdResp$getDist())) defined$resp <- T 
      
      oVs <- mcdCopy$getOtherVariables()
      if(!is.empty(oVs)) defined$oV <- T 
      
      pred <- mcdCopy$getPredictors()
      if(!is.empty(pred)) defined$pred <- T 
      
      para <- mcdCopy$getParameters()
      if(!is.empty(para)) defined$para <- T 
      
      genData <- mcdCopy$getData()
      if(defined$resp && !is.null(dim(genData)[1])) defined$genData <- T
      
      mcdSSD <- mcdCopy$getMcdSSD()
      ssdObject <- mcdSSD$getSsdObject()
      
      if(!is.empty(mcdSSD$getGoals())) defined$ssdGoals <- T
      
      if(!is.null(ssdObject)){
        defined$ssd <- T
        defined$ssdMcdOff <- !mcdSSD$isMcdOffSame()
      }
      nsT <- NS(paste0(ns(""), itemId))
      item2$setIndividualData(list(defined=defined, preSel=preSel, ns=nsT))
      
    }
  })
  
  
  ##############################################################################
  #################################### Data ####################################
  ##############################################################################
  
  data_d <- debounce({
    reactive(cMCD$getReactive("data"))}, 250)
  
  #Observe changes in data (of model)
  observe({
    
    data_d()
    isolate({

      data <- cMCD$getData()
      nas <- is.na(data)
      if(!is.null(data)){
        data <- format(data,digits=3)
        data[nas] <- ""
      }
      
      nCol <- dim(data)[2]
      
      targets <- c()
      # add highlight column to data
      if(!is.null(data) && !is.na(dim(data)[1]) && dim(data)[1] != 0){
        for(i in seq_len(nCol)){
          data <- cbind(data, rep(F, dim(data)[1]))
        }
        targets <- (nCol+1):dim(data)[2]
      }
      nColEdit <- dim(data)[2]
      
      highlight <- cMCD$getHighlightDataColumn()
      if(!is.null(highlight) && highlight <=nCol){
        data[[highlight+nCol]] <- T
      }
      
      #Paging, if number of data rows > 100
      paging <- F
      if(!is.null(data) && !is.na(dim(data)[1])){
        if(dim(data)[1] > 100){
          paging <- T
        }
        if(dim(data)[1] > 10000){
          data <- data[1:10000,]
          shinyjs::show(ns("datatableCuttedInfo"))
        }else{
          shinyjs::hide(ns("datatableCuttedInfo"))
        }
      }
      dt <- datatable(data,
                      options=list(paging=paging, pageLength=100, searching=F, info=F, ordering=F,
                                   columnDefs=list(list(className='dt-center', targets="_all"),
                                                   list(visible=FALSE, targets=targets))))
        
      if(!is.null(data) && !is.na(dim(data)[1]) && dim(data)[1] != 0){
        dt <- dt  %>% formatStyle(
          columns = seq_len(nCol),
          valueColumns = nCol+seq_len(nColEdit-nCol),
          backgroundColor = styleEqual(c(T), BAYAS_COLORS$`--highlight-selection`))
      }

      output[[ns("dataTable")]] <- renderDT(dt)
    })
  })
  
  proxy <- DT::dataTableProxy(ns("dataTable"))
  observe(priority = -1, {
    cMCD$getReactive("dataSelection")
    isolate({
      
      data <- cMCD$getData()
      nas <- is.na(data)
      if(!is.null(data)){
        data <- format(data,digits=3)
        data[nas] <- ""
      }
      
      nCol <- dim(data)[2]
      
      targets <- c()
      # add highlight column to data
      if(!is.null(data) && !is.na(dim(data)[1]) && dim(data)[1] != 0){
        for(i in seq_len(nCol)){
          data <- cbind(data, rep(F, dim(data)[1]))
        }
        targets <- (nCol+1):dim(data)[2]
      }
      nColEdit <- dim(data)[2]
      
      highlight <- cMCD$getHighlightDataColumn()
      if(!is.null(highlight)){
        data[[highlight+nCol]] <- T
      }
      
      if(dim(data)[1] > 10000){
        data <- data[1:10000,]
        shinyjs::show(ns("datatableCuttedInfo"))
      }else{
        shinyjs::hide(ns("datatableCuttedInfo"))
      }
      
      DT::replaceData(proxy, data)
      
    })
  })
  
  ##############################################################################
  ################################ Response tab ################################
  ##############################################################################
  {
  #Set noise term button
  observeEvent(input[[ns("setNoiseTerm")]], {
    #Open modal for response definition
    openPlanningModal(trigger(openPlanningModal()))
  })
  
  observeEvent(input[[ns("responseName")]], {
    name <- input[[ns("responseName")]]
    valid <- otherVariableNameIsValid(name, resp=T, duplicate=T, ovId=NULL, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("responseName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("responseName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) #showFeedbackDanger
      return()
    }
    mcdResponse <- cMCD$getMcdResponse()
    mcdResponse$setName(input[[ns("responseName")]])
  })
  observe({
    cMCD$getReactive("responseDist")
    isolate({
      if(is.null(cMCD$getMcdResponse()$getDist())){
        updateTextInput(session, inputId=ns("noiseTermName"), 
                        value="")
      }else{
        updateTextInput(session, inputId=ns("noiseTermName"), 
                        value=cMCD$getMcdResponse()$getDist())
      }
      
      ##add parameter (remove them first)
      #get all aux parameters of response dist
      resp <- cMCD$getMcdResponse()
      dist <- resp$getDist()
      
      distParas <- getDistributionParameterForFormula(dist)
      
      
      ##remove old unused aux parameter first
      removeList <- c()
      ids <- cMCD$getMcdFormula()$getParameterIdsForResponse()
      for(i in ids){
        para <- cMCD$getParameter(i)
        if(para$getType() == "aux"){
          if(para$getFormulaName() == paste0(dist,"@",distParas$aux$distParaName)){
            distParas$aux <- NULL
            next
          }
        }
        cMCD$removeParameter(id=i)
        cMCD$getMcdFormula()$removeParameterIdsForResponse(i)
      }

      
      for(p_i in seq_len(length(distParas))){
        if(names(distParas)[[p_i]] == "mean") next
        p <- distParas[[p_i]]
        
        newPara <- cMCD$createParameter()
        if(names(distParas)[[p_i]] == "aux"){
          newPara$setType("aux")
          newPara$setFormulaName(paste0(dist,"@",distParas[[p_i]]$distParaName))
        }else if(names(distParas)[[p_i]] == "fixPara"){
          newPara$setType("fixed")
          next
        }
        
        if(!is.null(p$ll)){ 
          if(p$ll == ">0"){
            newPara$addConstraint("lowerLimit",.Machine$double.xmin)
          }else{
            newPara$addConstraint("lowerLimit",as.numeric(p$ll))
          }
        }
        if(!is.null(p$ul)){
          if(p$ll == "<1"){
            newPara$addConstraint("upperLimit",1-.Machine$double.xmin)
          }else{
            newPara$addConstraint("upperLimit",as.numeric(p$ll))
          }
        } 
        
        sub <- newPara$createSub()
        
        paraPrior <- getModelCreatingDataDistributionParameterPrior(dist, glm=T, p_i)
        paraGen <- getModelCreatingDataDistributionParameterPrior(dist, glm=T, p_i)
        
        sub$setPrior(paraPrior)
        sub$setValueDistribution(paraGen)

        newPara$addSub(sub, "")
        newPara$setName(p$name)
        newPara$setPriority(1)
        
        cMCD$addParameter(newPara)
        cMCD$getMcdFormula()$addParameterIdsForResponse(newPara$getId())
      }
      
    })
  })
  observe({
    cMCD$getReactive("responseLink")
    isolate({
      if(is.null(cMCD$getMcdResponse()$getLink())){
        updateTextInput(session, inputId=ns("selectLinkFunction"), 
                        value="")
      }else{
        updateTextInput(session, inputId=ns("selectLinkFunction"), 
                        value=cMCD$getMcdResponse()$getLink())
      }
    })
  })
  observe({
    cMCD$getReactive("responseName")
    isolate({
      if(is.null(cMCD$getMcdResponse()$getName())){
        updateBayasTextInput(session, ns("responseName"),
                             value="")
      }else{
        updateBayasTextInput(session, ns("responseName"),
                             value=cMCD$getMcdResponse()$getName())
      }
    })
  })
  }
  
  
  ############ Response modal  ############ 
  {
  #Open response modal and
  #put response settings to tmp variables
  rvResponseStep <- reactiveVal(c(1))
  rvResponseName <- reactiveVal(NULL)
  rvResponseType <- reactiveVal(NULL)
  rvResponseRange <- reactiveVal(NULL)
  rvResponseDist <- reactiveVal(NULL)
  rvResponseLink <- reactiveVal(NULL)
  rvResponseUseOnlyNegVal <- reactiveVal(NULL)
  observe({
    if(is.null(openPlanningModal())) return()

    isolate({
      #set model values to tmp rvResponse...
      mcdResponse <- cMCD$getMcdResponse()
      
      rvResponseStep(mcdResponse$getSteps())
      rvResponseName(mcdResponse$getName())
      rvResponseType(mcdResponse$getType())
      rvResponseRange(mcdResponse$getRange())
      rvResponseDist(mcdResponse$getDist())
      rvResponseLink(mcdResponse$getLink())
      rvResponseUseOnlyNegVal(mcdResponse$getUseOnlyNegVal())
      
      if(is.null(rvResponseStep()) || length(rvResponseStep()) == 0){
        showNotification("Something went wrong. The operator is notified.", type="error")
        malfunction_report(code=malfunctionCode()$emptyResonseSteps, msg="rvResponseStep() null or empty",
                           type="error", askForReport=T)
        if(localUse) browser()
        return()
      }
      
      #confirm (disabled?)
      confirmButton <- actionButton(ns("confirmModalResponse"),"Confirm")
      if(!is.null(rvResponseDist())){
        confirmButton <- actionButton(ns("confirmModalResponse"),"Confirm", class="btn-primary")
      }else{
        confirmButton <- disabled(confirmButton)
      }
      
      showModal(modalDialog(
        size="xl",
        easyClose=F,
        footer = tags$div(
          style = "width:100%",
          tags$span(
            actionButton(ns("cancelModalResponse"), "Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            confirmButton
          )
        ),
        tags$div(
          uiOutput(ns("planningModal"))
        )
      ))
      
      output[[ns("planningModal")]] <- renderUI({
        stepDiv <- planning_creatingStepsResponse(ns, last(isolate(rvResponseStep())), isolate(rvResponseName()), 
                                                  c(isolate(rvResponseType()), 
                                                    isolate(rvResponseRange()), 
                                                    isolate(rvResponseDist()),
                                                    isolate(rvResponseUseOnlyNegVal())),
                                                  planningLinkFunctionMapping(isolate(rvResponseDist())),
                                                  planningPositiveDistribution(isolate(rvResponseDist())),
                                                  selectedLink=isolate(rvResponseLink()))
        
        helpDiv <- planning_creatingStepsResponse_help(ns, last(rvResponseStep()),
                                                       links=planningLinkFunctionMapping(rvResponseDist()),
                                                       positiveDistribution=planningPositiveDistribution(isolate(rvResponseDist())))
        
        planning_responseModal(stepDiv, helpDiv)
      })
      
    })  
  })
  #Back in response modal
  observeEvent(input[[ns("planningModalBack")]], {
    step <- rvResponseStep()
    if(last(step) > 1){
      rvResponseStep(head(step, -1))
    }
  })
  
  #Changes in response steps (for modal)
  observe({
    steps <- rvResponseStep()
    
    isolate({
      step <- last(steps)
      if(is.null(rvResponseStep()) || length(rvResponseStep()) == 0){
        showNotification("Something went wrong. The operator is notified.", type="error")
        malfunction_report(code=malfunctionCode()$emptyResonseSteps, msg="rvResponseStep() null or empty",
                           type="error", askForReport=T)
        if(localUse) browser()
        return()
      }
      
      if(step<2){
        hide(ns("planningModalBack"))
      }else{
        show(ns("planningModalBack"))
      }
      output[[ns("planningModal")]] <- renderUI({
        stepDiv <- planning_creatingStepsResponse(ns, step, isolate(rvResponseName()), 
                                                  c(isolate(rvResponseType()), 
                                                    isolate(rvResponseRange()), 
                                                    isolate(rvResponseDist()),
                                                    isolate(rvResponseUseOnlyNegVal())),
                                                  planningLinkFunctionMapping(isolate(rvResponseDist())),
                                                  planningPositiveDistribution(isolate(rvResponseDist())),
                                                  selectedLink=isolate(rvResponseLink()))
        
        helpDiv <- planning_creatingStepsResponse_help(ns, step,
                                                       links=planningLinkFunctionMapping(rvResponseDist()),
                                                       positiveDistribution=planningPositiveDistribution(isolate(rvResponseDist())))
        
        planning_responseModal(stepDiv, helpDiv)
      })
      
      if(!is.null(rvResponseDist())){
        addCssClass(ns("confirmModalResponse"), class="btn-primary")
        enable(ns("confirmModalResponse"))
      }else{
        removeCssClass(ns("confirmModalResponse"), class="btn-primary")
        disable(ns("confirmModalResponse"))
      }
      
    })
  })

  ### Data type ###
  observeEvent(input[[ns("typeOfResponseCont")]], {
    rvResponseStep(c(rvResponseStep(),2))
    if(!is.null(rvResponseType()) && rvResponseType() != "cont"){
      rvResponseRange(NULL)
      rvResponseDist(NULL)
    }
    rvResponseType("cont")
  })
  observeEvent(input[[ns("typeOfResponseDiscrete")]], {
    rvResponseStep(c(rvResponseStep(),3))
    if(!is.null(rvResponseType()) && rvResponseType() != "discrete"){
      rvResponseRange(NULL)
      rvResponseDist(NULL)
    }
    rvResponseType("discrete")
  })
  
  ### Range ###
  # Continuous
  rangeMatching <- c("positiveGreater", "positive", "percent",
                     "unlimited","negGreater","negative",
                     "discretePos","limitedN","bernoulli")
  sapply(1:6, function(i){
    observeEvent(input[[ns(paste0("rangeOfResponseCont",i))]], {
      rvResponseStep(c(rvResponseStep(),3+i))
      if(!is.null(rvResponseRange()) && rvResponseRange() != rangeMatching[i]){
        rvResponseDist(NULL)
        rvResponseUseOnlyNegVal(NULL)
      }
      rvResponseRange(rangeMatching[i])
    })
  })
  #Integer
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("rangeOfResponseDiscrete",i))]], {
      rvResponseStep(c(rvResponseStep(),9+i))
      if(!is.null(rvResponseRange()) && rvResponseRange() != rangeMatching[i+6]){
        rvResponseDist(NULL)
      }
      rvResponseRange(rangeMatching[i+6])
    })
  })
  
  ### Negative values ###
  observeEvent(input[[ns("negativeValuesPositiveValuesPossibleYes")]], {
    rvResponseStep(c(rvResponseStep(),40))
    dist <- planningDistribtionsEnum("response")$Normal
    if(is.null(rvResponseDist()) || rvResponseDist() != dist) {
      rvResponseLink(planningLinkFunctionMapping(dist)[1])
    }
    rvResponseDist(dist)
    rvResponseUseOnlyNegVal(T)
  })
  observeEvent(input[[ns("negativeValuesZeroPositiveValuesPossibleYes")]], {
    rvResponseStep(c(rvResponseStep(),40))
    dist <- planningDistribtionsEnum("response")$Normal
    if(is.null(rvResponseDist()) || rvResponseDist() != dist) {
      rvResponseLink(planningLinkFunctionMapping(dist)[1])
    }
    rvResponseDist(dist)
    rvResponseUseOnlyNegVal(T)
    
  })
  observeEvent(input[[ns("negativeValuesPositiveValuesPossibleNo")]], {
    rvResponseStep(c(rvResponseStep(),4))
    if(!is.null(rvResponseRange()) && rvResponseRange() != rangeMatching[5]){
      rvResponseDist(NULL)
    }
    rvResponseUseOnlyNegVal(F)
  })
  observeEvent(input[[ns("negativeValuesZeroPositiveValuesPossibleNo")]], {
    rvResponseStep(c(rvResponseStep(),5))
    if(!is.null(rvResponseRange()) && rvResponseRange() != rangeMatching[6]){
      rvResponseDist(NULL)
    }
    rvResponseUseOnlyNegVal(F)
  })
  
  
  ### Distribution ###
  sapply(1:12, function(i){
    observeEvent(input[[ns(paste0("distributionOfResponse",i))]], {
      rvResponseStep(c(rvResponseStep(),40))
      dist <- planningDistribtionsEnum("response")[[i]]
      if(is.null(rvResponseDist()) || rvResponseDist() != dist) {
        rvResponseLink(planningLinkFunctionMapping(dist)[1])
      }
      rvResponseDist(dist)
    })
  })

  ### Link function ###
  linkFunction=planningLinkEnum()
  sapply(seq_len(length(linkFunction)), function(i){
    observeEvent(input[[ns(paste0("linkFunction_",linkFunction[[i]]))]], {
      rvResponseLink(linkFunction[[i]])
      
      for(j in seq_len(length(linkFunction))){
        removeCssClass(ns(paste0("linkFunction_",linkFunction[[j]])), "planningExampleBtnPrimary")
        addCssClass(ns(paste0("linkFunction_",linkFunction[[j]])), "planningExampleBtn")
      }
      addCssClass(ns(paste0("linkFunction_",linkFunction[[i]])), "planningExampleBtnPrimary")
      removeCssClass(ns(paste0("linkFunction_",linkFunction[[i]])), "planningExampleBtn")
      
    })
  })
  
  #Response name (within modal)
  observeEvent(input[[ns("responseModalName")]], {
    name <- input[[ns("responseModalName")]]
    valid <- otherVariableNameIsValid(name, resp=T, duplicate=T, ovId=NULL, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("responseModalName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("responseModalName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
      return()
    }
    rvResponseName(name)
  })

  # Confirm or dismiss response modal
  observeEvent(input[[ns("confirmModalResponse")]], {
    mcdResponse <- cMCD$getMcdResponse()
    if(!is.null(rvResponseDist()) && !is.null(rvResponseLink())){
      mcdResponse$setStep(rvResponseStep())
      
      name <- input[[ns("responseModalName")]]
      valid <- otherVariableNameIsValid(name, resp=T, duplicate=T, ovId=NULL, cMCD=cMCD)
      if(!(is.null(name) || name == "" || valid$valid)){
        name <- ""
      }
      mcdResponse$setName(name)
      mcdResponse$setType(rvResponseType())
      mcdResponse$setRange(rvResponseRange())
      mcdResponse$setDist(rvResponseDist())
      mcdResponse$setLink(rvResponseLink())
      mcdResponse$setUseOnlyNegVal(rvResponseUseOnlyNegVal())
      
      
      #add N variable for (beta-)binom
      if(rvResponseDist() %in% c(distEnum$Binomial, distEnum$Beta_Binomial)){
        #check whether N other variable is already present
        oVs <- cMCD$getOtherVariables()
        present <- F
        for(ov in oVs){
          if(!is.null(ov$getSpecialRole()) &&
             ov$getSpecialRole() == "BinomN"){
            present <- T
            break
          } 
        }
        if(!present){
          #Add other variable
          otherVar <- cMCD$createOtherVariable()
          otherVar$addConstraints("positive")
          otherVar$addConstraints("integer")
          #Check if name N is available, otherwise add "_" (recursively)
          otherVar$setName(cMCD$getOtherVariableNames("N"))
          otherVar$setSpecialRole("BinomN")
          otherVar$setRemovable(F)
          otherVar$setType("categorical")
          otherVar$setStep(c(1,13,14))
          #also categorical model
          catModel <- ModelCreatingDataOtherVariableCategorical$new(otherVar$getSeed())
          catModel$setType("independent")
          catModel$setValues("100")
          otherVar$setCategoricalModel(catModel$getInstance())
          cMCD$addOtherVariable(otherVar)
          selectedOtherVariableId(otherVar$getId())
        }
      }else{
        oVs <- cMCD$getOtherVariables()
        for(ov in oVs){
          if(!is.null(ov$getSpecialRole()) && 
             ov$getSpecialRole() == "BinomN"){
            cMCD$removeOtherVariable(ov$getId())
            break
          }
        }
      }
      cMCD$setVisualizeData(0, type="yAxis")
    }
    output[[ns("planningModal")]] <- renderUI(NULL)
    removeModal()
  })
  
  # Cancel modal
  observeEvent(input[[ns("cancelModalResponse")]], {
    output[[ns("planningModal")]] <- renderUI(NULL)
    removeModal()
  })

}
  
  
  
  ### checklist ###
  # observe({
  #   cMCD$getReactive("responseDist")
  #   cMCD$getReactive("responseLink")
  #   cMCD$getReactive("responseName")
  #   
  #   dist <- cMCD$getMcdResponse()$getDist()
  #   link <- cMCD$getMcdResponse()$getLink()
  #   name <- cMCD$getMcdResponse()$getName()
  #   bDist <- !is.null(dist) && dist != ""
  #   bLink <- !is.null(link) && link != "" 
  #   bName <- !is.null(name) && name != ""
  #   b <- c(bDist, bLink, bName)
  #   if(all(b)){
  #     hide(ns("checklistResponseLabelAdd"))
  #     show(ns("checklistResponseLabel"), anim=T, animType="fade")
  #   }else if(any(b)){
  #     hide(ns("checklistResponseLabel"))
  #     show(ns("checklistResponseLabelAdd"), anim=T, animType="fade")
  #   }else{
  #     hide(ns("checklistResponseLabel"))
  #     hide(ns("checklistResponseLabelAdd"), anim=T, animType="fade")
  #   }
  # })
  
  
  
  
  ##############################################################################
  ############################### Other variable ###############################
  ##############################################################################
  {
  selectedOtherVariableId <- reactiveVal(NULL)
  
  #'Add variable to data'
  observeEvent(input[[ns("addVariable")]], {
    otherVar <- cMCD$createOtherVariable()
    cMCD$addOtherVariable(otherVar)
    selectedOtherVariableId(otherVar$getId())
  })
  
  #changes in otherVariables (add, remove)
  observe(priority=10, {
    cMCD$getReactive("otherVariable")
    cMCD$getReactive("otherVariableName")
    cMCD$getReactive("otherVariableError")
    
    isolate({
      
      err <- cMCD$getOtherVariableErrors()
      listOfVars <- c()
      listOfVarsName <- c()
      
      oV <- cMCD$getOtherVariables()
      for(i in oV){
        listOfVars <- c(listOfVars,i$getId())
        listOfVarsName <- c(listOfVarsName,i$getName())
      }
      names(listOfVars) <- listOfVarsName
      
      
      selected <- selectedOtherVariableId()
      if(!is.null(selected) && !selected %in% listOfVars) selected <- last(listOfVars)

      errList <- c()
      warnList <- c()
      for(e in err){
        if(e$status=="error"){
          errList <- c(errList, e$id)
        }else if(e$status=="warning"){
          warnList <- c(warnList, e$id)
        }
      }

      errorWarningList <- list()
      for(li in seq_len(length(listOfVars))){
        el <- listOfVars[[li]]
        n <- names(listOfVars)[[li]]
        if(el %in% errList){
          errorWarningList$invalid <- list.append(errorWarningList$invalid, el, n, T)
        }else if(el %in% warnList){
          errorWarningList$warning <- list.append(errorWarningList$warning, el, n, T)
        }else{
          errorWarningList$valid <- list.append(errorWarningList$valid, el, n, T)
        }
      }
      
      updateSelectInput(session, ns("existingVariables"), 
                        choices=errorWarningList, selected=selected)
      if(!is.null(errorWarningList$invalid)){
        showNotification("Some of the variables have invalid inputs and need a revision.",
                         type="error",
                         duration=10,
                         id="invalidOtherVariableDependency")
      }
    })
  })
  
  #When a 'other predictor' name changes
  observe({
    cMCD$getReactive("otherVariableName")
    isolate({
      id <- selectedOtherVariableId()
      oV <- cMCD$getOtherVariable(id)
      if(!is.null(oV) && 
         !is.null(oV$getName()) &&
         oV$getName()!="unnamed"){
        name <- oV$getName()
        updateBayasTextInput(session, ns("variableName"), value=name)
      }else{
        updateBayasTextInput(session, ns("variableName"), value="")
      }
    })
  })
  
  selectedOtherVariableIdRe <- reactive({
    input[[ns("existingVariables")]]
  })
  selectedOtherVariableIdRe_d <- debounce(selectedOtherVariableIdRe, debounceTime1)
  
  #selected other variable in list
  observe({
    ret <- selectedOtherVariableIdRe_d()
    selectedOtherVariableId(ret)
  })
  
  #Change of variable name (ui element) 
  observeEvent(input[[ns("variableName")]], {
    id <- selectedOtherVariableId()
    if(!is.null(id) && id %in% cMCD$getOtherVariableIds()){
      name <- input[[ns("variableName")]]
      valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
      shinyFeedback::hideFeedback(ns("variableName"))
      if(!(is.null(name) || name == "" || valid$valid)){
        shinyFeedback::showFeedbackDanger(ns("variableName"),
                                          text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
        return()
      }
      cMCD$getOtherVariable(id)$setName(input[[ns("variableName")]])
    }
  })
  
  g_removeList <- reactiveVal(c())
  #Remove other variable
  observeEvent(input[[ns("variableRemove")]], {
    id <- selectedOtherVariableId()
    if(!is.null(id) && id %in% cMCD$getOtherVariableIds()){
      
      #if id belongs to a ModelCreatingDataOtherVariableCategorical and another
      # ModelCreatingDataOtherVariableCategorical depends on this, remove also
      # dependent one. Ask before.
      oV <- cMCD$getOtherVariable(id)
      
      #Is the oV removable?
      if(!oV$isRemovable()){
        showNotification("This variable has a special role and can not be removed.",
                         type="error")
        return()
      }

      if(!is.null(oV$getType()) && oV$getType() == "categorical"){
        
        #from most super group to subgroup
        removeList <- c()
        checkids <- oV$getId()
        while(!is.empty(checkids)){
          removeList <- c(removeList, checkids)
          newCheckids <- c()
          for(i in checkids){
            ids <- cMCD$getOtherVariableIdsDependentOnThis(i)
            newCheckids <- c(newCheckids, ids)
          }
          checkids <- newCheckids
        }
        if(length(removeList) > 1){
          g_removeList(removeList)
          text <- tags$div(
            tags$div(
              class="fontColor-error",
              style="font-size:32px;margin-top:-20px;margin-bottom:15px;",
              icon("exclamation-circle")),
            tags$div(
              class="fontColor-error",
              style="font-weight:bold; font-size:14px;",
              "There are other variables depending on this one. They will also removed.")
          )
          
          shinyalert::shinyalert(inputId = ns("shinyalert"),
                                 title=NULL,
                                 text=as.character(text),
                                 html=T,
                                 closeOnClickOutside=F,
                                 showCancelButton =T,
                                 size="xs")
        }else{
          cMCD$removeOtherVariable(id=id)
        }

      }else{
        cMCD$removeOtherVariable(id=id)
      }
    }
  })
  
  observeEvent(input[[ns("shinyalert")]], {
    inp <- input[[ns("shinyalert")]]
    if(inp){
      #remove from leaf to root
      removeList <- g_removeList()
      for(i in length(removeList):1){
        cMCD$removeOtherVariable(id=removeList[i])
      }
    }
  })
  
  #Set Values (open modal)
  observeEvent(input[[ns("variableSetValues")]], {
    id <- selectedOtherVariableId()
    if(!is.null(id) && id %in% cMCD$getOtherVariableIds()){
      openPlanningModalOtherVariable(trigger(openPlanningModalOtherVariable()))
      ov <- cMCD$getOtherVariable(id)
      if(!is.null(ov)){
        type <- ov$getType()
        if(!is.null(type) && type == "categorical"){
          trigger_initCatVar(!trigger_initCatVar())
        }
      }
    }
  })
  
  #As predictor
  observeEvent(input[[ns("variableAddAsPredictor")]], {
    id <- selectedOtherVariableId()
    if(!is.null(id) && id %in% cMCD$getOtherVariableIds()){
      newPred <- cMCD$createPredictor()
      newPred$setType("predictor")
      newPred$setOVIds(id)
      if(!cMCD$isPredictorDuplicate(pred=newPred)){
        cMCD$addPredictor(newPred)
        #create also parameter
        newPara <- cMCD$createParameter()
        newPara$setPredId(newPred$getId())
        newPara$setType("predictor")
        newPara$setPriority(1)
        newPara$setName(name=NULL, predName=T)
        cMCD$addParameter(newPara$getInstance())
        
        showNotification("Predictor set.", type="message")
      }else{
        showNotification("Predictor already set.", type="message")
      }
    }
  })
  
  
  ### Update ui according to changes in models 'ModelCreatingDataOtherVariable'
  
  selectedOtherVariableIdOld <- reactiveVal(NULL)
  #When another variable is selected (by user or tool)
  observe({
    id <- selectedOtherVariableId()
    isolate({
      if(equal0(selectedOtherVariableIdOld(), id)){
        return()
      }else{
        selectedOtherVariableIdOld(id)
      }
    })

    isolate({
      if(is.null(id) || id==""){
        updateBayasTextInput(session, ns("variableName"), value="")
        
        #Change value output
        output[[ns("variableValues")]] <- renderUI(NULL)
        removeCssClass(ns("variableSetValues"), class="btn-primary")
      }else{
        oV <- cMCD$getOtherVariable(id)
        
        #change uis...
        if(oV$getName()=="unnamed"){
          updateBayasTextInput(session, ns("variableName"), value="")
        }else{
          updateBayasTextInput(session, ns("variableName"), value=oV$getName())
        }
        
        #Hightlight column in data
        cMCD$highlightDataColumn(id, F)
      }
    })
  })
  
  #Changes in predictors data (dist, categorical)
  observe({
    cMCD$getReactive("otherVariable")
    cMCD$getReactive("otherVariableDist")
    cMCD$getReactive("otherVariableName")
    selectedOtherVariableId()
    isolate({
      #Change display of values of predictor
      id <- selectedOtherVariableId()
      oV <- cMCD$getOtherVariable(id)
      
      if(!is.null(oV) && !is.null(oV$getType())){
        #Change value output
        if(oV$getType() != "categorical"){
          dist <- oV$getDist()
          if(!is.null(dist)){ # && !is.null(dist$getValues())
            output[[ns("variableValuesPlot")]] <- renderPlot(
              dist$plotValues(values2=dist$getValues())
            )
            removeCssClass(ns("variableSetValues"), class="btn-primary")
            shinyjs::show(ns("variableValuesPlot"))
          }else{
            addCssClass(ns("variableSetValues"), class="btn-primary")
            output[[ns("variableValuesPlot")]] <- renderPlot(NULL)
            shinyjs::hide(ns("variableValuesPlot"))
          }
          output[[ns("variableValuesTable")]] <- renderDT(NULL)
          output[[ns("variableValues")]] <- renderUI(NULL)
          shinyjs::hide(ns("variableValuesTable"))
          shinyjs::hide(ns("variableValues"))
        }else if(oV$getType() == "categorical"){
          output[[ns("variableValuesPlot")]] <- renderPlot(NULL)
          output[[ns("variableValues")]] <- renderUI(NULL)
          if(oV$getCategoricalModel()$getType() == "replacement"){
            toRepalceOV <- cMCD$getOtherVariable(oV$getCategoricalModel()$getReplaceValuesOf())
            output[[ns("variableValuesTable")]] <- renderDT(
              oV$getCategoricalModel()$getReplacementTable(toRepalceOV, oV$getName(),
                                                           filter=F))
          }else{
            output[[ns("variableValuesTable")]] <- renderDT(
              oV$getCategoricalModel()$getTable(oV$getName(), filter=F))
          }
          removeCssClass(ns("variableSetValues"), class="btn-primary")
          
          shinyjs::hide(ns("variableValuesPlot"))
          shinyjs::show(ns("variableValuesTable"))
          shinyjs::hide(ns("variableValues"))
        }
      }else{
        if(!is.null(oV)){
          addCssClass(ns("variableSetValues"), class="btn-primary")
        }else{
          removeCssClass(ns("variableSetValues"), class="btn-primary")
        }
        output[[ns("variableValuesPlot")]] <- renderPlot(NULL)
        output[[ns("variableValues")]] <- renderUI(NULL)
        output[[ns("variableValuesTable")]] <- renderDT(NULL)
        
        shinyjs::hide(ns("variableValuesPlot"))
        shinyjs::hide(ns("variableValues"))
        shinyjs::hide(ns("variableValuesTable"))
      }
      
    })
  })
  }


  ############ Other variable modal  ############ 
  {
  #Open response modal and
  #put oV settings to tmp variables
  rvOtherVariableStep <- reactiveVal(c(1))
  rvOtherVariableName <- reactiveVal(NULL)
  rvOtherVariableType <- reactiveVal(NULL)
  rvOtherVariableRange <- reactiveVal(NULL)
  rvOtherVariableDistName <- reactiveVal(NULL)
  rvOtherVariableDist <- reactiveVal(NULL)
  rvOtherVariableCategorical <- reactiveVal(NULL)
  observe({
    if(is.null(openPlanningModalOtherVariable())) return()
    
    isolate({
      #set model values to tmp rvOtherVariable...
      id <- selectedOtherVariableId()
      mcdOtherVariable <- cMCD$getOtherVariable(id)
      
      rvOtherVariableStep(mcdOtherVariable$getSteps())
      rvOtherVariableName(mcdOtherVariable$getName())
      rvOtherVariableType(mcdOtherVariable$getType())
      rvOtherVariableRange(mcdOtherVariable$getRange())
      rvOtherVariableDistName(mcdOtherVariable$getDistName())
      
      if(!is.null(mcdOtherVariable$getDist())){
        rvOtherVariableDist(mcdOtherVariable$getDist()$getInstance())
      }else{
        rvOtherVariableDist(NULL)
      }
      
      if(!is.null(mcdOtherVariable$getCategoricalModel())){
        rvOtherVariableCategorical(mcdOtherVariable$getCategoricalModel()$getInstance())
      }else{
        rvOtherVariableCategorical(NULL)
      }
      
      if(is.null(rvOtherVariableStep()) || length(rvOtherVariableStep()) == 0){
        showNotification("Something went wrong. The operator is notified.", type="error")
        malfunction_report(code=malfunctionCode()$emptyOVSteps, msg="rvOtherVariableStep() null or empty",
                           type="error", askForReport=T)
        if(localUse) browser()
        return()
      }
      
      #confirm (disabled?)
      confirmButton <- actionButton(ns("confirmModalOtherVariable"),"Confirm")
      if(!is.null(rvOtherVariableDistName()) || !is.null(rvOtherVariableCategorical())){
        confirmButton <- actionButton(ns("confirmModalOtherVariable"),"Confirm", class="btn-primary")
      }else{
        confirmButton <- disabled(confirmButton)
      }
      
      showModal(modalDialog(
        size="xl",
        easyClose=F,
        footer = tags$div(
          style = "width:100%",
          tags$span(
            actionButton(ns("cancelModalOtherVariable"), "Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            confirmButton
          )
        ),
        tags$div(
          uiOutput(ns("planningModalOtherVariable"))
        )
      ))
      output[[ns("planningModalOtherVariable")]] <- renderUI({
        stepDiv <- planning_creatingStepsOtherVariable(ns, last(isolate(rvOtherVariableStep())), isolate(rvOtherVariableName()), 
                                                  c(isolate(rvOtherVariableType()), 
                                                    isolate(rvOtherVariableRange()), 
                                                    isolate(rvOtherVariableDistName())),
                                                  isolate(rvOtherVariableDist()),
                                                  isolate(rvOtherVariableCategorical()),
                                                  cMCD, varId=selectedOtherVariableId(),
                                                  tmp=isolate(input[[ns("otherVariableSubgroupOf")]]))
        
        helpDiv <- planning_creatingStepsOtherVariable_help(ns, last(isolate(rvOtherVariableStep())))
        
        planning_OtherVariableModal(stepDiv, helpDiv)
      })
      
    })  
  })
  
  #Back in OtherVariable modal
  observeEvent(input[[ns("planningOtherVariableModalBack")]], {
    step <- rvOtherVariableStep()
    if(last(step) > 1){
      rvOtherVariableStep(head(step, -1))
    }
  })
  
  #Changes in OtherVariable steps (for modal)
  observe({
    steps <- rvOtherVariableStep()
    
    isolate({
      step <- last(steps)
      if(is.null(rvOtherVariableStep()) || length(rvOtherVariableStep()) == 0){
        showNotification("Something went wrong. The operator is notified.", type="error")
        malfunction_report(code=malfunctionCode()$emptyOVSteps, msg="rvOtherVariableStep() null or empty",
                           type="error", askForReport=T)
        if(localUse) browser()
        return()
      }
      
      if(step<2){
        hide(ns("planningOtherVariableModalBack"))
      }else{
        show(ns("planningOtherVariableModalBack"))
      }
      output[[ns("planningModalOtherVariable")]] <- renderUI({
        
        stepDiv <- planning_creatingStepsOtherVariable(ns, step, isolate(rvOtherVariableName()), 
                                                       c(isolate(rvOtherVariableType()), 
                                                         isolate(rvOtherVariableRange()), 
                                                         isolate(rvOtherVariableDistName())),
                                                       isolate(rvOtherVariableDist()),
                                                       isolate(rvOtherVariableCategorical()),
                                                       cMCD, varId=selectedOtherVariableId())
        
        helpDiv <- planning_creatingStepsOtherVariable_help(ns, step)
        
        
        planning_OtherVariableModal(stepDiv, helpDiv)
      })
      
      if(!is.null(rvOtherVariableDistName()) || !is.null(rvOtherVariableCategorical())){
        addCssClass(ns("confirmModalOtherVariable"), class="btn-primary")
        enable(ns("confirmModalOtherVariable"))
      }else{
        removeCssClass(ns("confirmModalOtherVariable"), class="btn-primary")
        disable(ns("confirmModalOtherVariable"))
      }
      
    })
  })
  
  ### Data type ###
  observeEvent(input[[ns("typeOfOtherVariableCont")]], {
    rvOtherVariableStep(c(rvOtherVariableStep(),2))
    if(!is.null(rvOtherVariableType()) && rvOtherVariableType() != "cont"){
      rvOtherVariableRange(NULL)
      rvOtherVariableDistName(NULL)
      rvOtherVariableDist(NULL)
      rvOtherVariableCategorical(NULL)
    }
    rvOtherVariableType("cont")
  })
  observeEvent(input[[ns("typeOfOtherVariableDiscrete")]], {
    rvOtherVariableStep(c(rvOtherVariableStep(),3))
    if(!is.null(rvOtherVariableType()) && rvOtherVariableType() != "discrete"){
      rvOtherVariableRange(NULL)
      rvOtherVariableDistName(NULL)
      rvOtherVariableDist(NULL)
      rvOtherVariableCategorical(NULL)
    }
    rvOtherVariableType("discrete")
  })
  observeEvent(input[[ns("typeOfOtherVariableCategorical")]], {
    rvOtherVariableStep(c(rvOtherVariableStep(),13))
    if(!is.null(rvOtherVariableType()) && rvOtherVariableType() != "categorical"){
      rvOtherVariableRange(NULL)
      rvOtherVariableDistName(NULL)
      rvOtherVariableDist(NULL)
      rvOtherVariableCategorical(NULL)
    }
    rvOtherVariableType("categorical")
  })
  
  ### Range ###
  # Continuous
  rangeMatching <- c("positiveGreater", "positive", "percent",
                     "unlimited","negGreater","negative",
                     "discretePos","limitedN","bernoulli")
  sapply(1:4, function(i){
    observeEvent(input[[ns(paste0("rangeOfOtherVariableCont",i))]], {
      if(i>4) i <- (i-7)*-1
      rvOtherVariableStep(c(rvOtherVariableStep(),3+i))
      if(!is.null(rvOtherVariableRange()) && rvOtherVariableRange() != rangeMatching[i]){
        rvOtherVariableDistName(NULL)
        rvOtherVariableDist(NULL)
        rvOtherVariableCategorical(NULL)
      }
      rvOtherVariableRange(rangeMatching[i])
    })
  })
  #Integer
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("rangeOfOtherVariableDiscrete",i))]], {
      rvOtherVariableStep(c(rvOtherVariableStep(),9+i))
      if(!is.null(rvOtherVariableRange()) && rvOtherVariableRange() != rangeMatching[i+6]){
        rvOtherVariableDistName(NULL)
        rvOtherVariableDist(NULL)
        rvOtherVariableCategorical(NULL)
      }
      rvOtherVariableRange(rangeMatching[i+6])
    })
  })
  #Categorical
  catRangeMatching <- c("independent", "subgroup", "replacement")
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("rangeOfOtherVariableCat",i))]], {
      rvOtherVariableStep(c(rvOtherVariableStep(),13+i))
      rvOtherVariableDistName(NULL)
      rvOtherVariableDist(NULL)
      if(is.null(rvOtherVariableRange()) || rvOtherVariableRange() != catRangeMatching[i]){
        rvOtherVariableRange(catRangeMatching[i])
        id <- selectedOtherVariableId()
        mcdOtherVariable <- cMCD$getOtherVariable(id)
        if(is.null(rvOtherVariableCategorical())){
          new <- ModelCreatingDataOtherVariableCategorical$new(mcdOtherVariable$getSeed())
          new$setType(rvOtherVariableRange())
          rvOtherVariableCategorical(new)
        }else{
          cat <- rvOtherVariableCategorical()
          cat$setType(rvOtherVariableRange())
          rvOtherVariableCategorical(cat)
        }
      }
    })
  })
  
  ### Distribution ###
  sapply(1:14, function(i){
    observeEvent(input[[ns(paste0("distributionOfOtherVariable",i))]], {
      rvOtherVariableStep(c(rvOtherVariableStep(),40))
      distName <- planningDistribtionsEnum("OtherVariable")[[i]]
      rvOtherVariableDistName(distName)
      id <- selectedOtherVariableId()
      mcdOtherVariable <- cMCD$getOtherVariable(id)
      dist <- ModelCreatingDataOtherVariableDistributionFactory(distName, 
                                                                mcdOtherVariable$getSeed())
      rvOtherVariableDist(dist)
      rvOtherVariableCategorical(NULL)
    })
  })
  sapply(1:7, function(i){
    observeEvent(input[[ns(paste0("distributionOfOtherVariable",i+19))]], {
      rvOtherVariableStep(c(rvOtherVariableStep(),40))
      distName <- planningDistribtionsEnum("OtherVariable")[[i+14]]

      rvOtherVariableDistName(distName)
      id <- selectedOtherVariableId()
      mcdOtherVariable <- cMCD$getOtherVariable(id)
      dist <- ModelCreatingDataOtherVariableDistributionFactory(distName, 
                                                                mcdOtherVariable$getSeed())
      rvOtherVariableDist(dist)
      rvOtherVariableCategorical(NULL)
    })
  })
  
  
  ### Distribution parameter ###
  updatePlot <- reactive({
    changeParameterVar()
    seed <- input[[ns("otherVariableRandomSeed")]]
    input[[ns("otherVariableNegation")]]
    sapply(1:3, function(i){
      input[[ns(paste0("otherVariableDistributionParameter",i))]]
      input[[ns(paste0("otherVariableDistributionParameterVar",i))]]
      input[[ns(paste0("otherVariableDistributionParameterSwitch",i,"-value"))]]
      input[[ns(paste0("otherVariableDistributionParameterSwitch",i,"-variable"))]]
      input[[ns(paste0("otherVariableDistributionParameterAltSwitch",i,"-value"))]]
      input[[ns(paste0("otherVariableDistributionParameterAltSwitch",i,"-variable"))]]
    })
    list(seed=seed)
  })
  updatePlot_d <- debounce(updatePlot, debounceTime2)
  observe({
    res <- updatePlot_d()
    seed <- res$seed
    
    isolate({
      dist <- rvOtherVariableDist()
      #seed
      if(randomSeedValid(seed)){
        if(!is.null(dist)){ 
          dist$setSeed(seed)
          rvOtherVariableDist(dist)
        }
        dist$setNegateValues(input[[ns("otherVariableNegation")]])
      }
      if(!is.null(dist)){
        output[[ns("otherVariableDistributionPlot")]] <- renderPlot(dist$plotValues())
      }
    })
  })

  #change distribution parameter value
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("otherVariableDistributionParameter",i))]], priority= 1, {
      dist <- rvOtherVariableDist()
      if(!is.null(dist)){
        dist$setParameterValue(i,input[[ns(paste0("otherVariableDistributionParameter",i))]],F)
        rvOtherVariableDist(dist)

        #Only for hypergeometric
        #k (#draws / para3) <= m+n (para1 and para2)
        if(dist$getName() == planningDistribtionsEnum("predictor")$Hypergeometric){
          k_lim <- input[[ns(paste0("otherVariableDistributionParameter",1))]]+
            input[[ns(paste0("otherVariableDistributionParameter",2))]]
          p <- dist$getParameter()[[3]]
          p$max_val <- k_lim
          updateBayasNumericInput(session, 
                                  inputId=ns(paste0("otherVariableDistributionParameter",3)),
                                  max=k_lim)
        }

      }
      if(!is.null(dist) && dist$hasAlternative() &&
         !input[[ns("otherVariableDistributionParameterCheckbox")]]){
        dist$transform()
        para <- dist$getAlternativeParameter()
        sapply(seq_len(length(para)), function(j){
          updateBayasNumericInput(session, inputId=ns(paste0("otherVariableDistributionParameterAlt",j)),
                                  value=para[[j]]$value)
        })
      }
      
    })
  })
  
  #change distribution alternative parameter value
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterAlt",i))]], priority= 1,
                 ignoreInit=T, {
      dist <- rvOtherVariableDist()
      if(!is.null(dist)){
        dist$setParameterValue(i,input[[ns(paste0("otherVariableDistributionParameterAlt",i))]],T)
        rvOtherVariableDist(dist)
      }
      if(!is.null(dist) && dist$hasAlternative() &&
         input[[ns("otherVariableDistributionParameterCheckbox")]]){
        dist$transform(F)
        para <- dist$getParameter()
        sapply(seq_len(length(para)), function(j){
          updateBayasNumericInput(session, inputId=ns(paste0("otherVariableDistributionParameter",j)),
                                  value=para[[j]]$value)
        })
      }
    })
  })
  
  #Switch between parameter and alternative parameter
  observeEvent(input[[ns("otherVariableDistributionParameterCheckbox")]], {
    dist <- rvOtherVariableDist()
    if(!input[[ns("otherVariableDistributionParameterCheckbox")]]){
      show(ns("otherVariableDistributionParameter"))
      hide(ns("otherVariableDistributionParameterAlt"))
      dist$setUseAlternative(F)
    }else{
      show(ns("otherVariableDistributionParameterAlt"))
      hide(ns("otherVariableDistributionParameter"))
      dist$setUseAlternative(T)
    }
    rvOtherVariableDist(dist)
  })
  
  
  ##Switch between value and variable
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterSwitch",i,"-value"))]], {
      show(ns(paste0("otherVariableDistributionParameterDiv",i)))
      hide(ns(paste0("otherVariableDistributionParameterVarDiv",i)))
      dist <- rvOtherVariableDist()
      dist$setParameterType(i, "value")
      rvOtherVariableDist(dist)
    })
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterSwitch",i,"-variable"))]], {
      hide(ns(paste0("otherVariableDistributionParameterDiv",i)))
      show(ns(paste0("otherVariableDistributionParameterVarDiv",i)))
      dist <- rvOtherVariableDist()
      dist$setParameterType(i, "variable")
      rvOtherVariableDist(dist)
    })
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterAltSwitch",i,"-value"))]], {
      show(ns(paste0("otherVariableDistributionParameterAltDiv",i)))
      hide(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)))
      dist <- rvOtherVariableDist()
      dist$setParameterType(i, "value", T)
      rvOtherVariableDist(dist)
    })
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterAltSwitch",i,"-variable"))]], {
      hide(ns(paste0("otherVariableDistributionParameterAltDiv",i)))
      show(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)))
      dist <- rvOtherVariableDist()
      dist$setParameterType(i, "variable", T)
      rvOtherVariableDist(dist)
    })
  })
  
  #observe distribution dependency on variables 
  #if variable is choosen
  changeParameterVar <- reactiveVal(F)
  sapply(1:3, function(i){
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterVar",i))]], priority=-1, {
      
      dist <- rvOtherVariableDist()
      if(is.null(dist) || is.null(dist$getParameter()[[i]])) return()
      type <- dist$getParameter()[[i]]$getType()
      if(!is.null(type) && type == "variable"){
        #is valid?
        #returns a list(valid=c("valid", "warning", "invalid"), message="")
        valid <- cMCD$isOtherVariableValidForDistParameter(dist$getParameter()[[i]], distOVid = selectedOtherVariableId(),
                                                           input[[ns(paste0("otherVariableDistributionParameterVar",i))]])
        
        oVid <- input[[ns(paste0("otherVariableDistributionParameterVar",i))]]
        
        
        if(is.null(valid)){
          dist$setParameterValueToDefault(i, F)
        }else if(valid$valid=="valid"){
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, oVid, F, T)
          #set actual value(s) of other predictor to distribution parameter
          oV <- cMCD$getOtherVariable(oVid)
          valuesOfOV <- oV$getValues()
          
          
          dist$setParameterValue(i, valuesOfOV, F, F)
          
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                                  "selectInputInvalidInput")
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                                  "selectInputDangerInput")

          hide(ns(paste0("otherVariableDistributionParameterVarDivPopoverDiv",i)))
          
        }else if(valid$valid=="warning"){
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, oVid, F, T)
          #set actual value(s) of other predictor to distribution parameter
          oV <- cMCD$getOtherVariable(oVid)
          valuesOfOV <- oV$getValues()
          
          dist$setParameterValue(i, valuesOfOV, F, F)
          
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                                  "selectInputInvalidInput")
          shinyjs::addCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                               "selectInputDangerInput")
          
          show(ns(paste0("otherVariableDistributionParameterVarDivPopoverDiv",i)))
          bslib::update_popover(
            id=ns(paste0("otherVariableDistributionParameterVarDivPopover",i)),
            title="Invalid variable",
            valid$message
          )
          
        }else{
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, NULL, F, T)
          #set default values of distribution parameter
          dist$setParameterValueToDefault(i,F)
          
          shinyjs::addCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                               "selectInputInvalidInput")
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterVarDiv",i)),
                                  "selectInputDangerInput")
          
          show(ns(paste0("otherVariableDistributionParameterVarDivPopoverDiv",i)))
          bslib::update_popover(
            id=ns(paste0("otherVariableDistributionParameterVarDivPopover",i)),
            title="Invalid variable",
            valid$message
          )

        }
        rvOtherVariableDist(dist)
        changeParameterVar(!changeParameterVar())
      }
    })
    observeEvent(input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]], priority=-1, {
      dist <- rvOtherVariableDist()
      if(is.null(dist) || is.null(dist$getAlternativeParameter()[[i]])) return()
      type <- dist$getAlternativeParameter()[[i]]$getType()
      if(!is.null(type) && type == "variable"){

        #is valid?
        #returns a list(valid=c("valid", "warning", "invalid"), message="")
        valid <- cMCD$isOtherVariableValidForDistParameter(dist$getParameter()[[i]], distOVid = selectedOtherVariableId(),
                                                           input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]])
        

        bslib::toggle_popover(id=ns(paste0("otherVariableDistributionParameterAltVarDivPopover",i)), show=F)
        
        
        if(is.null(valid)){
          dist$setParameterValueToDefault(i, T)
        }else if(valid$valid=="valid"){
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]],
                                 T, T)
          #set actual value(s) of other predictor to distribution parameter
          oV <- cMCD$getOtherVariable(input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]])
          valuesOfOV <- oV$getValues()
          
          dist$setParameterValue(i, valuesOfOV, T, F)
          
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                                  "selectInputInvalidInput")
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                                  "selectInputDangerInput")
          
          hide(ns(paste0("otherVariableDistributionParameterAltVarDivPopoverDiv",i)))
        }else if(valid$valid=="warning"){
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]],
                                 T, T)
          #set actual value(s) of other predictor to distribution parameter
          oV <- cMCD$getOtherVariable(input[[ns(paste0("otherVariableDistributionParameterAltVar",i))]])
          valuesOfOV <- oV$getValues()
          
          dist$setParameterValue(i, valuesOfOV, T, F)
          
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                                  "selectInputInvalidInput")
          
          shinyjs::addCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                               "selectInputDangerInput")

          show(ns(paste0("otherVariableDistributionParameterAltVarDivPopoverDiv",i)))
          bslib::update_popover(
            id=ns(paste0("otherVariableDistributionParameterAltVarDivPopover",i)),
            title="Invalid variable",
            valid$message
          )
          
          # shinyjs::runjs(paste0("$('#",ns(paste0("otherVariableDistributionParameterAltVarDiv",i)), "').popover('show');"))
        }else{
          #set dependency to other variable to distribution parameter
          dist$setParameterValue(i, NULL, T, T)
          #set default values of distribution parameter
          dist$setParameterValueToDefault(i,T)
          
          shinyjs::addCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                               "selectInputInvalidInput")
          shinyjs::removeCssClass(ns(paste0("otherVariableDistributionParameterAltVarDiv",i)),
                                  "selectInputDangerInput")
          
          show(ns(paste0("otherVariableDistributionParameterAltVarDivPopoverDiv",i)))
          bslib::update_popover(
            id=ns(paste0("otherVariableDistributionParameterAltVarDivPopover",i)),
            title="Invalid variable",
            valid$message
          )
          
          # shinyjs::runjs(paste0("$('#",ns(paste0("otherVariableDistributionParameterAltVarDiv",i)), "').popover('show');"))
        }
        changeParameterVar(!changeParameterVar())
        rvOtherVariableDist(dist)
      }
    })
  })

  
  #Variable name within modal
  observeEvent(input[[ns("variableModalName")]] ,{
    name <- input[[ns("variableModalName")]]
    id <- selectedOtherVariableId()
    valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("variableModalName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("variableModalName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
      return()
    }
  })
  
  observeEvent(input[[ns("otherVariableRandomSeedDice")]], {
    updateBayasNumericInput(inputId=ns("otherVariableRandomSeed"), value=drawRandomSeed())
  })
  
  
  ### Categorical ###
  trigger_initCatVar <- reactiveVal(T)
  ## Independent ##
{  
  
  updateIndependentTable <- reactive({
    trigger_initCatVar()
    rvOtherVariableRange()
    input[[ns("otherVariableCatIndependentValues")]]
    input[[ns("otherVariableCatIndependentDistributed")]]
    input[[ns("otherVariableCatIndependentValuesDistributed")]]
    name <- input[[ns("variableIndependentModalName")]]
    list(name)
  })
  updateIndependentTable_d <- debounce(updateIndependentTable, debounceTime3)
  observe({
    res <- updateIndependentTable_d()
   
    isolate({
      catModel <- rvOtherVariableCategorical()

      if(!is.null(catModel)){
        
        oV <- cMCD$getOtherVariable(selectedOtherVariableId())
        valid <-  catModel$isValid(constraints=oV$getConstraints())
  
        name <- input[[ns("variableIndependentModalName")]]
        
        if(is.null(name) || str_trim(name) == ""){
          if(!is.null(oV$getName())){
            name <- oV$getName()
          }else{
            name <- "unnamed"
          }
        } 
        
        valid <- otherVariableNameIsValid(name, duplicate=T, ovId=selectedOtherVariableId(), cMCD=cMCD)
        
        if(!valid$valid) return()

        output[[ns("otherVariableCatIndependentTable")]] <- renderDT(catModel$getTable(varName=name))
      }
    })
  })
  

  verifyValuesAndValFreq <- reactiveVal(T)
  #values
  observeEvent(input[[ns("otherVariableCatIndependentValues")]], {
    val <- input[[ns("otherVariableCatIndependentValues")]]
    val_s <- str_trim(str_split(val, ",")[[1]])
    
    shinyFeedback::hideFeedback(ns("otherVariableCatIndependentValues"))
    
    for(i in val_s){
      valid <- otherVariableElementIsValid(i)
      if(!valid$valid){
        shinyFeedback::showFeedbackDanger(ns("otherVariableCatIndependentValues"),
                                          text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL)
        return()
      }
    }

    oV <- cMCD$getOtherVariable(selectedOtherVariableId())

    if(any(val_s=="") && length(unique(val_s))!=1){
      shinyFeedback::showFeedbackDanger(ns("otherVariableCatIndependentValues"),
                                        text="Empty elements are not valid.", color=BAYAS_COLORS$`--font-warning`, icon=NULL)
      return()
    }
    
    catModel <- rvOtherVariableCategorical()
    catModel$setValues(val_s)
    rvOtherVariableCategorical(catModel)
    verifyValuesAndValFreq(!verifyValuesAndValFreq())
    
    #Update values freq
    withr::with_seed(1337, {
      s <- sample(4,length(val_s),replace=T)
    })
    placeholder <- paste0(s, collapse=", ")
    
    dist <- input[[ns("otherVariableCatIndependentDistributed")]]
    if(!is.null(dist) && dist=="equally"){
      valFreq <- rep(1,length(val_s))
      updateTextAreaInput(session, ns("otherVariableCatIndependentValuesDistributed"), 
                          value=paste0(valFreq, collapse=", "),
                          placeholder=placeholder)
    }else{
      updateTextAreaInput(session, ns("otherVariableCatIndependentValuesDistributed"), 
                          placeholder=placeholder)
    }
  })
  
  #values distributed
  observeEvent(input[[ns("otherVariableCatIndependentDistributed")]], {
    dist <- input[[ns("otherVariableCatIndependentDistributed")]]
    if(is.null(dist)) return()
    catModel <- rvOtherVariableCategorical()
    if(dist == "explicit"){
      show(ns("otherVariableCatIndependentValuesDistributedDiv"))
      valFreq <- input[[ns("otherVariableCatIndependentValuesDistributed")]]
      valFreq_s <- as.numeric(str_trim(str_split(valFreq,",")[[1]]))
      catModel$setValuesFrequency(valFreq_s)
      
      hide(ns("otherVariableCatRandomSeedDiv"))
      hide(ns("otherVariableCatRandomizeDiv"))
    }else{
      hide(ns("otherVariableCatIndependentValuesDistributedDiv"))
      show(ns("otherVariableCatRandomizeDiv"))
      if(input[[ns("otherVariableCatRandomize")]])
        show(ns("otherVariableCatRandomSeedDiv"))
    }
    catModel$setValuesDistributed(dist)
    rvOtherVariableCategorical(catModel)
    verifyValuesAndValFreq(!verifyValuesAndValFreq())
  })
  
  
  #values frequency
  observeEvent(input[[ns("otherVariableCatIndependentValuesDistributed")]], {
    valDist <- input[[ns("otherVariableCatIndependentDistributed")]]
    if(valDist=="explicit"){
      valFreq <- input[[ns("otherVariableCatIndependentValuesDistributed")]]
      valFreq_s <- as.numeric(str_trim(str_split(valFreq,",")[[1]]))
      catModel <- rvOtherVariableCategorical()
      catModel$setValuesFrequency(valFreq_s)
      rvOtherVariableCategorical(catModel)
    }
    verifyValuesAndValFreq(!verifyValuesAndValFreq())
  })
  
  #Use capped?
  observeEvent(input[[ns("otherVariableCatCapped")]], {
    catModel <- rvOtherVariableCategorical()
    catModel$setCapped(input[[ns("otherVariableCatCapped")]])
    rvOtherVariableCategorical(catModel)
  })
  
  #Use randomization?
  observeEvent(input[[ns("otherVariableCatRandomize")]], {
    
    oV <- cMCD$getOtherVariable(selectedOtherVariableId())
    randAvail <- oV$isRandomizeAvailableForCatagorical()
    
    styleClass <- "randomizeFrequencyRadioButton" 
    styleClassHint <- "randomizeFrequencyHint" 
    if(!randAvail$valid){
      addCssClass(ns("otherVariableCatRandomizeSubDiv"), styleClass)
      if(input[[ns("otherVariableCatRandomize")]]){
        addCssClass(ns("otherVariableCatRandomizeHelp-par"), styleClassHint)
        show(ns("otherVariableCatRandomizeWarning"))
      }else{
        removeCssClass(ns("otherVariableCatRandomizeHelp-par"), styleClassHint)
        hide(ns("otherVariableCatRandomizeWarning"))
      }
    }else{
      removeCssClass(ns("otherVariableCatRandomizeHelp-par"), styleClassHint)
      removeCssClass(ns("otherVariableCatRandomizeSubDiv"), styleClass)
      hide(ns("otherVariableCatRandomizeWarning"))
    }
    
    
    if(input[[ns("otherVariableCatRandomize")]] && randAvail$valid){
      show(ns("otherVariableCatRandomSeedDiv"))
    }else{
      hide(ns("otherVariableCatRandomSeedDiv"))
    }
    catModel <- rvOtherVariableCategorical()
    
    catModel$setRandomize(input[[ns("otherVariableCatRandomize")]])
    rvOtherVariableCategorical(catModel)
  })

  #random parameter
  observeEvent(input[[ns("otherVariableCatRandomParameter")]], {
    catModel <- rvOtherVariableCategorical()
    catModel$setRandomParameter(input[[ns("otherVariableCatRandomParameter")]])
    rvOtherVariableCategorical(catModel)
  })
  
  #random seed
  observeEvent(input[[ns("otherVariableCatRandomSeed")]], {
    catModel <- rvOtherVariableCategorical()
    seed <- input[[ns("otherVariableCatRandomSeed")]]
    #seed
    if(randomSeedValid(seed)){
      if(!is.null(catModel)){
        catModel$setSeed(seed)
        rvOtherVariableCategorical(catModel)
      }
    }
  })
  
  
  #If the number of elements and the frequencies do not match
  observe({
    verifyValuesAndValFreq()
    isolate({
      catModel <- rvOtherVariableCategorical()
      if(is.null(catModel)) return()
      oV <- cMCD$getOtherVariable(selectedOtherVariableId())
      valid <- catModel$isValid(constraints=oV$getConstraints())
      if(valid$valid){
        #remove red and warning
        shinyjs::hide(ns("otherVariableCatIndependentValuesDistributedWarning"))
        shinyjs::removeCssClass(ns("otherVariableCatIndependentValuesDistributedDiv"),
                             "textAreaOtherVariableFreqInvalid")
        output[[ns("otherVariableCatIndependentValuesDistributedWarningText")]] <- renderText(NULL)
      }else{
        #add red and warning
        shinyjs::show(ns("otherVariableCatIndependentValuesDistributedWarning"))
        shinyjs::addCssClass(ns("otherVariableCatIndependentValuesDistributedDiv"),
                             "textAreaOtherVariableFreqInvalid")
        output[[ns("otherVariableCatIndependentValuesDistributedWarningText")]] <- renderText(
          valid$message
        )
      }
    })
  })
  
  #Variable name within modal
  observeEvent(input[[ns("variableIndependentModalName")]] ,{
    name <- input[[ns("variableIndependentModalName")]]
    id <- selectedOtherVariableId()
    valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("variableIndependentModalName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("variableIndependentModalName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
      return()
    }
  })
  
  observeEvent(input[[ns("otherVariableCatRandomSeedDice")]], {
    updateBayasNumericInput(inputId=ns("otherVariableCatRandomSeed"), value=drawRandomSeed())
  })
}
  
  
  ### Categorical ###
  ## Subgroup of ##
{
  trigger_UpperGroup <- reactiveVal(T)
  #Subgroup of ...
  observeEvent(input[[ns("otherVariableSubgroupOf")]], priority = 10, ignoreInit=T, {
    subgroup <- input[[ns("otherVariableSubgroupOf")]]
    if(is.null(subgroup) || subgroup==""){
      disable(ns("otherVariableSubgroupCreateDiv"))
      disable(ns("otherVariableSubgroupExplicitPerSubgroup"))
      updateCheckboxInput(session,
                            ns("otherVariableSubgroupExplicitPerSubgroup"), 
                            value=F)
    }else{

      enable(ns("otherVariableSubgroupCreateDiv"))
      enable(ns("otherVariableSubgroupExplicitPerSubgroup"))
      elements <- cMCD$getOtherVariable(subgroup)$getCategoricalModel()$getValues()
      el_for_list <- elements
      sel_for_list <- elements[1]
      if(is.list(elements)){ 
        el_for_list <- nested_list_for_cat_subgroups(elements)
        sel_for_list <- el_for_list[[1]][1]
      }
      freq <- cMCD$getOtherVariable(subgroup)$getCategoricalModel()$getValuesFrequency()
      
      updateSelectizeInput(session, ns("otherVariableSubgroupElements"),
                        choices=el_for_list, selected=sel_for_list)
      catModel <- rvOtherVariableCategorical()
      
      if(!is.null(catModel$getSubgroupOf()) && 
         catModel$getSubgroupOf() == subgroup) return()
      
      catModel$clear()
      catModel$setSubgroupOf(subgroup)
      catModel$setSubgroupOfNames(cMCD$getOtherVariableNamesForCategoricalSubgroup(subgroup))
      catModel$setUpperGroupElements(elements)

      superElement <- NULL
      
      values <- str_trim(str_split(input[[ns("otherVariableSubgroupCatIndependentValues")]], ",")[[1]])
      
      catModel$setValuesDistributed(input[[ns("otherVariableSubgroupCatIndependentDistributed")]], superElement=superElement)
      catModel$setValues(values, superElement=superElement)
      if(input[[ns("otherVariableSubgroupCatIndependentDistributed")]] == "explicit"){
        catModel$setValuesFrequency(str_trim(str_split(input[[ns("otherVariableSubgroupCatIndependentValuesDistributed")]], ",")[[1]]), 
                                    superElement=superElement)
      }else{
        # catModel$setValuesFrequency(1, superElement=superElement) #repalce with:
        
        catModel$setValuesFrequency(rep(1, length(values)), superElement=superElement)
      }
      catModel$setCapped(input[[ns("otherVariableSubgroupCatCapped")]], superElement=superElement)
      
      
      oV <- cMCD$getOtherVariable(selectedOtherVariableId())
      randAvail <- oV$isRandomizeAvailableForCatagorical()
      if(is.null(oV$getCategoricalModel())){
        oV <- cMCD$getOtherVariable(subgroup)
        randAvail <- oV$isRandomizeAvailableForCatagorical(top=F)
      }

      styleClass <- "randomizeFrequencyRadioButton" 
      styleClassHint <- "randomizeFrequencyHint" 
      if(!randAvail$valid){
        addCssClass(ns("otherVariableSubgroupCatRandomizeSubDiv"), styleClass)
        if(input[[ns("otherVariableSubgroupCatRandomize")]]){
          addCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
          show(ns("otherVariableSubgroupCatRandomizeWarning"))
        }else{
          removeCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
          hide(ns("otherVariableSubgroupCatRandomizeWarning"))
        }
      }else{
        removeCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
        removeCssClass(ns("otherVariableSubgroupCatRandomizeSubDiv"), styleClass)
        hide(ns("otherVariableSubgroupCatRandomizeWarning"))
      }
      
      
      catModel$setRandomize(input[[ns("otherVariableSubgroupCatRandomize")]], superElement=superElement)
      if(input[[ns("otherVariableSubgroupCatRandomize")]]){
        catModel$setRandomParameter(input[[ns("otherVariableSubgroupCatRandomParameter")]], superElement=superElement)
        catModel$setSeed(input[[ns("otherVariableSubgroupCatRandomSeed")]], superElement=superElement)
      }
      inp <- input[[ns("otherVariableSubgroupExplicitPerSubgroup")]]
      if(is.null(inp)) inp <- F
      catModel$setSubgroupExplicit(inp)
      
      rvOtherVariableCategorical(catModel)
      trigger_UpperGroup(!trigger_UpperGroup())
    }
  })

  #Define subgroup explicit for each element of group?
  observeEvent(input[[ns("otherVariableSubgroupExplicitPerSubgroup")]], {
    inp <- input[[ns("otherVariableSubgroupExplicitPerSubgroup")]]
    if(is.null(inp)) inp <- F
    
    catModel <- rvOtherVariableCategorical()
    if(!is.null(catModel$getSubgroupExplicit()) && 
       catModel$getSubgroupExplicit() == inp) return()
    catModel$setSubgroupExplicit(inp)
    
    val <- input[[ns("otherVariableSubgroupCatIndependentValues")]]
    val_s <- str_trim(str_split(val, ",")[[1]])
    
    catModel$setValues(val_s)
    
    if(inp){
      show(ns("otherVariableSubgroupElements"))
      addCssClass(ns("otherVariableSubgroupCreateDiv"), "otherVariableSubgroupExplicit")
    }else{
      hide(ns("otherVariableSubgroupElements"))
      removeCssClass(ns("otherVariableSubgroupCreateDiv"), "otherVariableSubgroupExplicit")
    }
    rvOtherVariableCategorical(catModel)
  })
  
  
  #Select element of super group
  observeEvent(input[[ns("otherVariableSubgroupElements")]], {
    el <- input[[ns("otherVariableSubgroupElements")]]
    
    if(!is.null(input[[ns("otherVariableSubgroupExplicitPerSubgroup")]]) &&
       input[[ns("otherVariableSubgroupExplicitPerSubgroup")]]){
     
      #update all following inputs
      catModel <- rvOtherVariableCategorical()
      
      values <- catModel$getValues(superElement=el)
      valuesDistributed <- catModel$getValuesDistributed(superElement=el)
      valuesFreq <- catModel$getValuesFrequency(superElement=el)
      capped <- catModel$getCapped(superElement=el)
      randomize <- catModel$getRandomize(superElement=el)
      randomizeParameter <- catModel$getRandomParameter(superElement=el)
      seed <- catModel$getSeed(superElement=el)
      
      if(is.null(values)) values <- ""
      updateTextAreaInput(session, 
                          ns("otherVariableSubgroupCatIndependentValues"), 
                          value=paste0(values, collapse = ", "))
      if(is.null(valuesDistributed) || !valuesDistributed %in% c("equally","explicit")) valuesDistributed  <- "equally"
      updateBayasGroupedButtons(session, ns("otherVariableSubgroupCatIndependentDistributed"),
                                selected=str_to_title(valuesDistributed), value=valuesDistributed)
      updateTextAreaInput(session, 
                          ns("otherVariableSubgroupCatIndependentValuesDistributed"), 
                          value=valuesFreq)
      updateCheckboxInput(session, 
                            ns("otherVariableSubgroupCatCapped"), 
                            value=capped)
      updateCheckboxInput(session, 
                            ns("otherVariableSubgroupCatRandomize"), 
                            value=randomize)
      updateBayasNumericInput(session, 
                              inputId=ns("otherVariableSubgroupCatRandomParameter"), 
                              value=randomizeParameter)
      updateBayasNumericInput(session, 
                              inputId=ns("otherVariableSubgroupCatRandomSeed"), 
                              value=seed)
    }
  })
  
  
  updateIndependentTableSubgroup <- reactive({
    trigger_initCatVar()
    trigger_UpperGroup()
    rvOtherVariableRange()
    input[[ns("otherVariableSubgroupOf")]]
    input[[ns("otherVariableSubgroupExplicitPerSubgroup")]]
    input[[ns("otherVariableSubgroupCatIndependentValues")]]
    input[[ns("otherVariableSubgroupCatIndependentDistributed")]]
    input[[ns("otherVariableSubgroupCatIndependentValuesDistributed")]]
    name <- input[[ns("variableSubgroupModalName")]]
    list(name)
  })
  updateIndependentTableSubgroup_d <- debounce(updateIndependentTableSubgroup, debounceTime4)
  observe({
    res <- updateIndependentTableSubgroup_d()
    
    isolate({
      catModel <- rvOtherVariableCategorical()

      if(!is.null(catModel) && !is.null(catModel$getSubgroupOf()) && catModel$getSubgroupOf() != ""){
        oV <- cMCD$getOtherVariable(selectedOtherVariableId())
        valid <- catModel$isValid(constraints=oV$getConstraints())
        # if(valid$valid){

          name <- input[[ns("variableSubgroupModalName")]]
          if(is.null(name) || str_trim(name) == "") name <- "unnamed"

          output[[ns("otherVariableSubgroupCatTable")]] <- renderDT(catModel$getTable(name))
        # }else{
        #   warning(valid$message)
        # }
      }
    })
  })


  verifyValuesAndValFreqSubgroup <- reactiveVal(T)
  #values
  observeEvent(input[[ns("otherVariableSubgroupCatIndependentValues")]], {
    val <- input[[ns("otherVariableSubgroupCatIndependentValues")]]
    val_s <- str_trim(str_split(val, ",")[[1]])
    
    shinyFeedback::hideFeedback(ns("otherVariableSubgroupCatIndependentValues"))
    for(i in val_s){
      valid <- otherVariableElementIsValid(i)
      if(!valid$valid){
        shinyFeedback::showFeedbackDanger(ns("otherVariableSubgroupCatIndependentValues"),
                                          text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL)
        return()
      }
    }
    
    if(length(unique(val_s))==1 && val_s[1]!=""){
      shinyFeedback::showFeedbackDanger(ns("otherVariableSubgroupCatIndependentValues"),
                                        text="A single element is not valid.", color=BAYAS_COLORS$`--font-warning`, icon=NULL)
      return()
    }else if(any(val_s=="") && length(unique(val_s))!=1){
      shinyFeedback::showFeedbackDanger(ns("otherVariableSubgroupCatIndependentValues"),
                                        text="Empty elements are not valid.", color=BAYAS_COLORS$`--font-warning`, icon=NULL)
      return()
    }
    
    catModel <- rvOtherVariableCategorical()
    
    if(catModel$getSubgroupExplicit()){
      el <- input[[ns("otherVariableSubgroupElements")]]
      catModel$setValues(val_s, el)
    }else{
      catModel$setValues(val_s)
    }
    
    #update values frequency ui, just that by switching to 'explicit' is shows 1s for each value
    withr::with_seed(1337, {
      s <- sample(4,length(val_s),replace=T)
    })
    placeholder <- paste0(s, collapse=", ")
    
    dist <- input[[ns("otherVariableSubgroupCatIndependentDistributed")]]
    if(!is.null(dist) && dist=="equally"){
      valFreq <- rep(1,length(val_s))
      updateTextAreaInput(session, ns("otherVariableSubgroupCatIndependentValuesDistributed"), 
                          value=paste0(valFreq, collapse=", "),
                          placeholder=placeholder)
    }else{
      updateTextAreaInput(session, ns("otherVariableSubgroupCatIndependentValuesDistributed"), 
                          placeholder=placeholder)
    }
    rvOtherVariableCategorical(catModel)
    verifyValuesAndValFreqSubgroup(!verifyValuesAndValFreqSubgroup())
  })


  #values distributed
  observeEvent(input[[ns("otherVariableSubgroupCatIndependentDistributed")]], {
    dist <- input[[ns("otherVariableSubgroupCatIndependentDistributed")]]
    if(is.null(dist)) return()
    catModel <- rvOtherVariableCategorical()
    if(dist == "explicit"){
      show(ns("otherVariableSubgroupCatIndependentValuesDistributedDiv"))
      valFreq <- input[[ns("otherVariableSubgroupCatIndependentValuesDistributed")]]
      valFreq_s <- as.numeric(str_trim(str_split(valFreq,",")[[1]]))
      if(catModel$getSubgroupExplicit()){
        el <- input[[ns("otherVariableSubgroupElements")]]
        catModel$setValuesFrequency(valFreq_s, el)
      }else{
        catModel$setValuesFrequency(valFreq_s)
      }
      
      hide(ns("otherVariableSubgroupCatRandomSeedDiv"))
      hide(ns("otherVariableSubgroupCatRandomizeDiv"))
    }else{
      val <- input[[ns("otherVariableSubgroupCatIndependentValues")]]
      val_s <- str_trim(str_split(val, ",")[[1]])
      valFreq <- rep(1,length(val_s))
      updateTextAreaInput(session, ns("otherVariableSubgroupCatIndependentValuesDistributed"), 
                          value=paste0(valFreq, collapse=", "))
      hide(ns("otherVariableSubgroupCatIndependentValuesDistributedDiv"))
      
      show(ns("otherVariableSubgroupCatRandomizeDiv"))
      if(input[[ns("otherVariableSubgroupCatRandomize")]])
        show(ns("otherVariableSubgroupCatRandomSeedDiv"))
    }
    if(catModel$getSubgroupExplicit()){
      el <- input[[ns("otherVariableSubgroupElements")]]
      catModel$setValuesDistributed(dist, el)
    }else{
      catModel$setValuesDistributed(dist)
    }
    rvOtherVariableCategorical(catModel)
    verifyValuesAndValFreqSubgroup(!verifyValuesAndValFreqSubgroup())
  })


  #values frequency
  observeEvent(input[[ns("otherVariableSubgroupCatIndependentValuesDistributed")]], {
    valDist <- input[[ns("otherVariableSubgroupCatIndependentDistributed")]]
    if(valDist=="explicit"){
      valFreq <- input[[ns("otherVariableSubgroupCatIndependentValuesDistributed")]]
      valFreq_s <- as.numeric(str_trim(str_split(valFreq,",")[[1]]))
      catModel <- rvOtherVariableCategorical()
      
      if(catModel$getSubgroupExplicit()){
        el <- input[[ns("otherVariableSubgroupElements")]]
        catModel$setValuesFrequency(valFreq_s, el)
      }else{
        catModel$setValuesFrequency(valFreq_s)
      }
      rvOtherVariableCategorical(catModel)
    }
    verifyValuesAndValFreqSubgroup(!verifyValuesAndValFreqSubgroup())
  })

  #Use capped?
  observeEvent(input[[ns("otherVariableSubgroupCatCapped")]], {
    catModel <- rvOtherVariableCategorical()
    if(catModel$getSubgroupExplicit()){
      el <- input[[ns("otherVariableSubgroupElements")]]
      catModel$setCapped(input[[ns("otherVariableSubgroupCatCapped")]], el)
    }else{
      catModel$setCapped(input[[ns("otherVariableSubgroupCatCapped")]])
    }
    rvOtherVariableCategorical(catModel)
  })

  #Use randomization?
  observeEvent(input[[ns("otherVariableSubgroupCatRandomize")]], {

    catModel <- rvOtherVariableCategorical()
    
    oV <- cMCD$getOtherVariable(selectedOtherVariableId())
    randAvail <- oV$isRandomizeAvailableForCatagorical()

    if(is.null(oV$getCategoricalModel())){
      oV <- cMCD$getOtherVariable(input[[ns("otherVariableSubgroupOf")]])
      if(!is.null(oV))
        randAvail <- oV$isRandomizeAvailableForCatagorical(top=F)
    }
    
    styleClass <- "randomizeFrequencyRadioButton" 
    styleClassHint <- "randomizeFrequencyHint" 
    if(!randAvail$valid){
      addCssClass(ns("otherVariableSubgroupCatRandomizeSubDiv"), styleClass)
      if(input[[ns("otherVariableSubgroupCatRandomize")]]){
        addCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
        show(ns("otherVariableSubgroupCatRandomizeWarning"))
      }else{
        removeCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
        hide(ns("otherVariableSubgroupCatRandomizeWarning"))
      }
    }else{
      removeCssClass(ns("otherVariableSubgroupCatRandomizeHelp-par"), styleClassHint)
      removeCssClass(ns("otherVariableSubgroupCatRandomizeSubDiv"), styleClass)
      hide(ns("otherVariableSubgroupCatRandomizeWarning"))
    }
    
    if(input[[ns("otherVariableSubgroupCatRandomize")]] && randAvail$valid){
      show(ns("otherVariableSubgroupCatRandomSeedDiv"))
    }else{
      hide(ns("otherVariableSubgroupCatRandomSeedDiv"))
    }
    
    if(catModel$getSubgroupExplicit()){
      el <- input[[ns("otherVariableSubgroupElements")]]
      catModel$setRandomize(input[[ns("otherVariableSubgroupCatRandomize")]], el)
      catModel$setRandomParameter(input[[ns("otherVariableSubgroupCatRandomParameter")]], el)
      catModel$setSeed(input[[ns("otherVariableSubgroupCatRandomSeed")]], el)
    }else{
      catModel$setRandomize(input[[ns("otherVariableSubgroupCatRandomize")]])
      catModel$setRandomParameter(input[[ns("otherVariableSubgroupCatRandomParameter")]])
      catModel$setSeed(input[[ns("otherVariableSubgroupCatRandomSeed")]])
    }
    rvOtherVariableCategorical(catModel)
  })

  #random parameter
  observeEvent(input[[ns("otherVariableSubgroupCatRandomParameter")]], {
    catModel <- rvOtherVariableCategorical()
    if(catModel$getSubgroupExplicit()){
      el <- input[[ns("otherVariableSubgroupElements")]]
      catModel$setRandomParameter(input[[ns("otherVariableSubgroupCatRandomParameter")]], el)
    }else{
      catModel$setRandomParameter(input[[ns("otherVariableSubgroupCatRandomParameter")]])
    }
    rvOtherVariableCategorical(catModel)
  })

  #random seed
  observeEvent(input[[ns("otherVariableSubgroupCatRandomSeed")]], {
    catModel <- rvOtherVariableCategorical()
    seed <- input[[ns("otherVariableSubgroupCatRandomSeed")]]
    #seed
    if(randomSeedValid(seed)){
      if(!is.null(catModel)){
        if(catModel$getSubgroupExplicit()){
          el <- input[[ns("otherVariableSubgroupElements")]]
          catModel$setSeed(seed, el)
        }else{
          catModel$setSeed(seed)
        }
        rvOtherVariableCategorical(catModel)
      }
    }
  })


  #If the number of elements and the frequencies do not match
  observe({
    verifyValuesAndValFreqSubgroup()
    isolate({
      catModel <- rvOtherVariableCategorical()
      if(is.null(catModel)) return()
      valid <- NULL

      oV <- cMCD$getOtherVariable(selectedOtherVariableId())
      if(catModel$getSubgroupExplicit()){
        el <- input[[ns("otherVariableSubgroupElements")]]
        valid <- catModel$isValid(superElement=el, constraints=oV$getConstraints())
      }else{
        valid <- catModel$isValid(constraints=oV$getConstraints())
      }
      if(valid$valid){
        #remove red and warning
        shinyjs::hide(ns("otherVariableSubgroupCatIndependentValuesDistributedWarning"))
        shinyjs::removeCssClass(ns("otherVariableSubgroupCatIndependentValuesDistributedDiv"),
                                "textAreaOtherVariableSubgroupFreqInvalid")
        output[[ns("otherVariableSubgroupCatIndependentValuesDistributedWarningText")]] <- renderText(NULL)
      }else{
        #add red and warning
        shinyjs::show(ns("otherVariableSubgroupCatIndependentValuesDistributedWarning"))
        shinyjs::addCssClass(ns("otherVariableSubgroupCatIndependentValuesDistributedDiv"),
                             "textAreaOtherVariableSubgroupFreqInvalid")
        output[[ns("otherVariableSubgroupCatIndependentValuesDistributedWarningText")]] <- renderText(
          valid$message
        )
      }
    })
  })
  
  #Variable name within modal
  observeEvent(input[[ns("variableSubgroupModalName")]] ,{
    name <- input[[ns("variableSubgroupModalName")]]
    id <- selectedOtherVariableId()
    valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("variableSubgroupModalName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("variableSubgroupModalName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
      return()
    }
  })
  
  
  observeEvent(input[[ns("otherVariableSubgroupCatRandomSeedDice")]], {
    updateBayasNumericInput(inputId=ns("otherVariableSubgroupCatRandomSeed"), value=drawRandomSeed())
  })
} 
  
  ### Categorical ###
  ## Replacement ##
{  
  #An oV id selected, update element list
  observeEvent(input[[ns("otherVariableReplacementOf")]], {
    
    possbileVariablesForReplacement <- cMCD$getOtherVariableIdsForReplacement()
    var <- input[[ns("otherVariableReplacementOf")]]
    if(!is.null(var) && var %in% possbileVariablesForReplacement){
      
      oV <- cMCD$getOtherVariable(var)
      cate <- oV$getCategoricalModel()
      catType <- cate$getType()
      if(is.null(catType)) return()
      elements <- c()
      
      elements <- oV$getCategoricalModel()$getValues()
      if(catType == "independent"){

      }else if(catType == "subgroup"){
        if(is.list(elements)){ 
          elements <- nested_list_for_cat_subgroups(elements)
        }
      }else{
        warning("catType should be independent or subgroup!")
      }
      
      catModel <- rvOtherVariableCategorical()
      catModel$setReplaceValuesOf(var)
      catModel$setSubgroupOf(var)
      rvOtherVariableCategorical(catModel)
      
      
      #update element list
      updateSelectizeInput(session, ns("otherVariableReplacementElement"), choices=elements)
      shinyjs::enable(ns("otherVariableReplacementElement"))
    }else{
      updateSelectizeInput(session, ns("otherVariableReplacementElement"), choices="")
      shinyjs::disable(ns("otherVariableReplacementElement"))
      catModel <- rvOtherVariableCategorical()
      catModel$setReplaceValuesOf(NULL)
      catModel$setSubgroupOf(NULL)
      rvOtherVariableCategorical(catModel)
    }
    
  })
  
  
  observeEvent(input[[ns("otherVariableReplacementElement")]], {
    
    catModel <- rvOtherVariableCategorical()
    el <- input[[ns("otherVariableReplacementElement")]]
    
    if(!is.null(el) && el != ""){
      
      mapping <- catModel$getReplaceValues(el)

      el <- last(str_split(el,":")[[1]])
      if(is.null(mapping)){
        updateTextAreaInput(session, ns("otherVariableCatReplacementValues"),
                            placeholder=el, value="")
      }else{
        updateTextAreaInput(session, ns("otherVariableCatReplacementValues"),
                            placeholder=el, value=mapping[[2]])
      }

      shinyjs::enable(ns("otherVariableCatReplacementValues"))
    }else{
      shinyjs::disable(ns("otherVariableCatReplacementValues"))
    }
  })
  
  #values
  observeEvent(input[[ns("otherVariableCatReplacementValues")]], {
    
    catModel <- rvOtherVariableCategorical()
    el <- input[[ns("otherVariableReplacementElement")]]
    val <- input[[ns("otherVariableCatReplacementValues")]]
    
    if(!is.null(el) && el != ""){
      if(is.null(val) || val ==""){
        catModel$removeReplaceValue(el)
      }else{
        #Does val contains invalid inputs? e.g.":"
        valid <- otherVariableElementIsValid(val)
        if(valid$valid){
          shinyFeedback::hideFeedback(ns("otherVariableCatReplacementValues"))
          catModel$setReplaceValues(replaceValues=val, el=el)
        }else{
          shinyFeedback::showFeedbackDanger(ns("otherVariableCatReplacementValues"), 
                                        text=valid$msg,
                                        color=BAYAS_COLORS$`--font-error`)
        }
      }
    }
    rvOtherVariableCategorical(catModel)
  })
  
  updateReplacementTableSubgroup <- reactive({
    trigger_initCatVar()
    input[[ns("otherVariableReplacementOf")]]
    input[[ns("otherVariableReplacementElement")]]
    input[[ns("otherVariableCatReplacementValues")]]
    name <- input[[ns("variableReplacementModalName")]]
    list(name)
  })
  updateReplacementTableSubgroup_d <- debounce(updateReplacementTableSubgroup, debounceTime5)
  observe({
    res <- updateReplacementTableSubgroup_d()

    isolate({
      catModel <- rvOtherVariableCategorical()
      
      if(!is.null(catModel)){
        
        replaceOfId <- catModel$getReplaceValuesOf()
        replaceOfoV <- cMCD$getOtherVariable(replaceOfId)
        if(is.null(replaceOfoV)) return()
        replaceOfoVCatModel <- replaceOfoV$getCategoricalModel()
        
        name <- input[[ns("variableReplacementModalName")]]
        if(is.null(name) || str_trim(name) == "") name <- "unnamed"
        output[[ns("otherVariableCatReplacementTable")]] <- renderDT(catModel$getReplacementTable(replaceOfoV, name))
      }
    })
  })
  
  #Variable name within modal
  observeEvent(input[[ns("variableReplacementModalName")]] ,{
    name <- input[[ns("variableReplacementModalName")]]
    id <- selectedOtherVariableId()
    valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
    shinyFeedback::hideFeedback(ns("variableReplacementModalName"))
    if(!(is.null(name) || name == "" || valid$valid)){
      shinyFeedback::showFeedbackDanger(ns("variableReplacementModalName"),
                                        text=valid$msg, color=BAYAS_COLORS$`--font-error`, icon=NULL) 
      return()
    }
  })
}  
  

  #Confirm other variable modal
  observeEvent(input[[ns("confirmModalOtherVariable")]], {
    dist <- rvOtherVariableDist()
    distName <- rvOtherVariableDistName()
    catModel <- rvOtherVariableCategorical()
    if(!is.null(dist)){
      
      id <- selectedOtherVariableId()
      mcdOtherVariable <- cMCD$getOtherVariable(id)
      
      constraints <- mcdOtherVariable$getConstraints()
      if("integer" %in% constraints){
        if(rvOtherVariableType()=="cont"){
          showNotification("Only integers are allowed!")
          return()
        }
      }
      if("positive" %in% constraints){
        if(dist$getNegateValues()){
          showNotification("Only positive values are allowed!")
          return()
        }
      }
      
      
      alt <- dist$hasAlternative() && input[[ns("otherVariableDistributionParameterCheckbox")]]
      dependsOnOV <- dist$dependsOnOtherVariable(alternative=alt)
      mcdOtherVariable$setDependsOnOV(dependsOnOV)
      dist$setUseAlternative(alt)
      
      mcdOtherVariable$setStep(rvOtherVariableStep())
      mcdOtherVariable$setRange(rvOtherVariableRange())
      
      name <- input[[ns("variableModalName")]]
      valid <- otherVariableNameIsValid(name, duplicate=T, ovId=id, cMCD=cMCD)
      if(!(is.null(name) || name == "" || valid$valid)){
        name <- ""
      }
      mcdOtherVariable$setType(rvOtherVariableType())
      mcdOtherVariable$setName(name)
      mcdOtherVariable$setDistName(distName)
      mcdOtherVariable$setDist(dist$getInstance())
      mcdOtherVariable$setSeed(input[[ns("otherVariableRandomSeed")]])
    }else if(!is.null(catModel)){
      oV <- cMCD$getOtherVariable(selectedOtherVariableId())
      valid <- catModel$isValid(constraints=oV$getConstraints())
      if(valid$valid){
        
        varName <- "" 
        if(catModel$getType() == "independent"){
          varName <- input[[ns("variableIndependentModalName")]]
        }else if(catModel$getType() == "subgroup"){
          varName <- input[[ns("variableSubgroupModalName")]]
        }else if(catModel$getType() == "replacement"){
          varName <- input[[ns("variableReplacementModalName")]]
        }
        
        id <- selectedOtherVariableId()
        mcdOtherVariable <- cMCD$getOtherVariable(id)
        if(catModel$getType() == "subgroup"){
          mcdOtherVariable$setDependsOnOV(catModel$getSubgroupOf())
          #replace factor order when explicitly values are setted
          catModel$reorderValues()
        }else if(catModel$getType() == "replacement"){
          mcdOtherVariable$setDependsOnOV(catModel$getReplaceValuesOf())
          oV <- cMCD$getOtherVariable(catModel$getReplaceValuesOf())
          el <- oV$getCategoricalModel()$getValues()
          
          if(oV$getCategoricalModel()$getType() == "subgroup"){
            if(is.list(el)){ 
              el <- nested_list_for_cat_subgroups(el)
              el <- unlist(el)
            }
          }
          
          #set non replaced elements to ""
          for(e in el){
            if(is.null(catModel$getReplaceValues(e[[1]])))
              catModel$setReplaceValues("",last(str_split(e,":")[[1]]))
          }
          
        }else{
          catModel$reorderValues()
          mcdOtherVariable$setDependsOnOV(NULL)
        }
        mcdOtherVariable$setStep(rvOtherVariableStep())
        mcdOtherVariable$setRange(rvOtherVariableRange())
        
        valid <- otherVariableNameIsValid(varName, duplicate=T, ovId=id, cMCD=cMCD)
        if(!(is.null(varName) || varName == "" || valid$valid)){
          varName <- ""
        }
        
        mcdOtherVariable$setName(varName, trigger=T)
        mcdOtherVariable$setType(rvOtherVariableType(), trigger=F)
        mcdOtherVariable$setSeed(catModel$getSeed())
        mcdOtherVariable$setCategoricalModel(catModel$getInstance())
      }else{
        text <- tags$div(
          tags$div(
            class="fontColor-error",
            style="font-size:32px;margin-top:-20px;margin-bottom:15px;",
            icon("exclamation-circle")),
          tags$div(
            class="fontColor-error",
            style="font-weight:bold; font-size:14px;",
            valid$message)
        )
        shinyalert::shinyalert(title=NULL,
                               text=as.character(text),
                               html=T,
                               closeOnClickOutside=T,
                               size="xs")
        return()
      }

    }
    output[[ns("planningModalOtherVariable")]] <- renderUI(NULL)

    output[[ns("otherVariableCatIndependentTable")]] <- renderDT(NULL)
    output[[ns("otherVariableSubgroupCatTable")]] <- renderDT(NULL)
    output[[ns("otherVariableCatReplacementTable")]] <- renderDT(NULL)
    removeModal()
  })
  
  # Cancel modal
  observeEvent(input[[ns("cancelModalOtherVariable")]], {
    output[[ns("planningModalOtherVariable")]] <- renderUI(NULL)

    output[[ns("otherVariableCatIndependentTable")]] <- renderDT(NULL)
    output[[ns("otherVariableSubgroupCatTable")]] <- renderDT(NULL)
    output[[ns("otherVariableCatReplacementTable")]] <- renderDT(NULL)
    removeModal()
  })
  
}
  
  
  
  
  
  
  
  ##############################################################################
  ################################## Predictor #################################
  ##############################################################################
  {
    
    #current selected predictor id
    rvPredictorSelectedId <- reactiveVal(NULL)
    
    
    updatePredictorList <- reactive({
      cMCD$getReactive("otherVariable")
      cMCD$getReactive("otherVariableName")
      cMCD$getReactive("otherVariableDist")
      list()
    })
    updatePredictorList_d <- debounce(updatePredictorList, debounceTime6)
    #update predictors due to changes in oV
    observe({
      res <- updatePredictorList_d()

      isolate({
        predictors <- cMCD$getPredictors()
        oVs <- cMCD$getOtherVariables()
        oVsId <- cMCD$getOtherVariableIds()
        for(pred in predictors){
          predOvs <- pred$getOVIds()
          
          #remove ids in pred that are not more present
          for(ovId in predOvs){
            if(!ovId %in% oVsId) pred$removeOVId(ovId)
          }
          pred$setNameBasedOnOvs()
          
          #update predictor regarding of elements (cat: a,b, and new "c")
          pred$doTrigger()
        }
      })
    })
      
    #Update predictor list
    observe({
      cMCD$getReactive("predictor")
      cMCD$getReactive("parameter")
      
      isolate({
        errorWarningList_sorted <- cMCD$validationOfPredictors(T)
        
        selected <- rvPredictorSelectedId()
        if(!is.null(selected) && !selected %in% unlist(errorWarningList_sorted)){
          if(length(unlist(errorWarningList_sorted)) > 0){
            selected <- last(unlist(errorWarningList_sorted))
          }else{
            #no element available; empty ui
            updateTextInput(session, ns("predictorName"), value="")
            updateSelectizeInput(session, ns("predictorInvolvedVariables"), 
                                 choices=c(""), selected="")
            disable(ns("predictorInvolvedVariables"))
            output[[ns("predictorInfo")]] <- renderUI(tags$div())
          }
        }
        
        updateSelectInput(session, ns("existingPredictors"),
                          choices=errorWarningList_sorted, selected=selected)
      })
    })
    
    
    #Select predictor out of list
    observeEvent(input[[ns("existingPredictors")]], {
      rvPredictorSelectedId(input[[ns("existingPredictors")]])
    })
    
    
    #Update involved variables choice list and selection when variable name is changed
    observe({
      selId <- rvPredictorSelectedId()
      cMCD$getReactive("predictor")
      
      isolate({
        
        if(is.null(selId) || selId == ""){
          updateTextInput(session, ns("predictorName"), value="")
          updateSelectizeInput(session, ns("predictorInvolvedVariables"), 
                               choices="", selected="")
          updateTextAreaInput(session, ns("predictorInvolvedVariables"), value="")
        }else{
          
          pred <- cMCD$getPredictor(selId)
          if(is.null(pred)) return()
          if(pred$getType() == "intercept"){
            
            updateTextInput(session, ns("predictorName"), value=pred$getName())
            
            updateSelectizeInput(session, ns("predictorInvolvedVariables"), 
                                 choices=c(), selected="")
            disable(ns("predictorInvolvedVariables"))
            output[[ns("predictorInfo")]] <- renderUI(tags$div("Intercept ..."))
            
          }else{
            
            ovIds <- cMCD$getOtherVariablesforPredictors()
            selOv <- pred$getOVIds()
            if(!all(selOv %in% unlist(ovIds))) stop("wie anzeigen?")
            

            updateTextInput(session, ns("predictorName"), value=pred$getName())
            
            
            enable(ns("predictorInvolvedVariables"))
            updateSelectizeInput(session, ns("predictorInvolvedVariables"), 
                                 choices=ovIds, selected=selOv)
            output[[ns("predictorInfo")]] <- renderUI(tags$div())
          }
          
          info <- pred$getInfo()
          warning <- pred$isValid()
          warningDiv <- NULL
          if(warning$valid != "valid"){
            color <- BAYAS_COLORS$`--font-error`
            if(warning$valid == "warning") color <- BAYAS_COLORS$`--font-warning`
            warningDiv <- tags$div(
              style=paste0("color: ",color,"; text-align: justify; margin-bottom:10px;"),
              HTML(warning$msg)
            )
          }
          infoDiv <- tags$div(
            warningDiv,
            tags$div(
              style="text-align: justify;",
              HTML(info)
            )
          )
          
          output[[ns("predictorInfo")]] <- renderUI(infoDiv)
        }
      })
    })
    
    
    #change name when involved variables are changed
    observeEvent(input[[ns("predictorInvolvedVariables")]], ignoreNULL = F, {
      selId <- rvPredictorSelectedId()
      if(!is.null(selId) && selId != ""){
        pred <- cMCD$getPredictor(selId)
        if(is.null(pred)) return()
        if(pred$getType() == "predictor"){
          ovIds <- input[[ns("predictorInvolvedVariables")]]
          pred$setOVIds(ovIds)
        }
      }
    })
    
    observeEvent(input[[ns("predictorRemove")]], {
      selId <- rvPredictorSelectedId()
      if(!is.null(selId) && selId != ""){
        pred <- cMCD$getPredictor(selId)
        if(!is.null(pred)){
          cMCD$removePredictor(pred=pred)
        }
      }
    })
    
    showParameter <- reactiveVal(NULL)
    observeEvent(input[[ns("predictorGoToParameter")]], {
      selPred <- input[[ns("existingPredictors")]]
      if(!is.null(selPred) && selPred !=""){
        updateTabsetPanel(session=session, input=ns("uiOfSteps"), 
                          selected="4. Parameters")
        showParameter(selPred)
      }else{
        showNotification("Please select a predictor.")
      }
    })
    
    
    ##########################################
    ############ Predictor modal  ############ 
    {    
    rvPredictor <- reactiveVal(NULL)
    rvPredictorStep <- reactiveVal(c(1))
    
    observeEvent(input[[ns("addPredictor")]], {
      
      newPred <- cMCD$createPredictor()
      rvPredictorStep(c(1))
      rvPredictor(newPred)
      
      #confirm (disabled?)
      confirmButton <- actionButton(ns("confirmModalPredictor"),"Confirm")
      if(last(rvPredictorStep()) > 1){
        confirmButton <- actionButton(ns("confirmModalPredictor"),"Confirm", class="btn-primary")
      }else{
        confirmButton <- disabled(confirmButton)
      }
      
      showModal(modalDialog(
        size="xl",
        easyClose=F,
        footer = tags$div(
          style = "width:100%",
          tags$span(
            actionButton(ns("cancelModalPredictor"), "Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            confirmButton
          )
        ),
        
        tags$div(
          uiOutput(ns("planningModalPredictor"))
        )
      ))
      
      output[[ns("planningModalPredictor")]] <- renderUI({
        
        stepDiv <- planning_creatingStepsPredictor(ns, last(rvPredictorStep()), 
                                                       cMCD, pred=newPred)
        
        helpDiv <- planning_creatingStepsPredictor_help(ns, last(rvPredictorStep()))
        
        planning_PredictorModal(stepDiv, helpDiv)
      })
      
    })

    #Back in OtherVariable modal
    observeEvent(input[[ns("planningPredictorModalBack")]], {
      step <- rvPredictorStep()
      if(last(step) > 1){
        rvPredictorStep(head(step, -1))
      }
    })
    
    # Cancel modal
    observeEvent(input[[ns("cancelModalPredictor")]], {
      removeModal()
      output[[ns("planningModalPredictor")]] <- renderUI(NULL)
    })

    
    #set type 'intercept'
    observeEvent(input[[ns("typeOfPredictorIntercept")]], {
      setPrimaryImageButtonTitle(ns("typeOfPredictorIntercept"), T)
      setPrimaryImageButtonTitle(ns("typeOfPredictorPredictor"), F)
      newPred <- rvPredictor()
      newPred$setType("intercept")
      rvPredictor(newPred)
      enable(ns("confirmModalPredictor"))
      addCssClass(ns("confirmModalPredictor"), "btn-primary")
    })
    
    #set step (only if 'predictor' is choosen)
    observeEvent(input[[ns("typeOfPredictorPredictor")]], {
      setPrimaryImageButtonTitle(ns("typeOfPredictorIntercept"), F)
      setPrimaryImageButtonTitle(ns("typeOfPredictorPredictor"), T)
      rvPredictorStep(c(rvPredictorStep(),2))
      newPred <- rvPredictor()
      newPred$setType("predictor")
      rvPredictor(newPred)
      enable(ns("confirmModalPredictor"))
      removeCssClass(ns("confirmModalPredictor"), "btn-primary")
    })
    
    #changes in steps
    observe({
      steps <- rvPredictorStep()
      isolate({
        step <- last(steps)
        if(is.null(rvPredictorStep()) || length(rvPredictorStep()) == 0){
          showNotification("Something went wrong. The operator is notified.", type="error")
          malfunction_report(code=malfunctionCode()$emptyPredictorSteps, msg="rvPredictorStep() null or empty",
                             type="error", askForReport=T)
          if(localUse) browser()
          return()
        }

        output[[ns("planningModalPredictor")]] <- renderUI({

          stepDiv <- planning_creatingStepsPredictor(ns, last(rvPredictorStep()),
                                                     cMCD, pred=rvPredictor())

          helpDiv <- planning_creatingStepsPredictor_help(ns, last(rvPredictorStep()))

          planning_PredictorModal(stepDiv, helpDiv)
        })

      })
      
    })
    
    
    #changes in selected variable(s)
    observeEvent(input[[ns("planningPredictorModalVariables")]], ignoreNULL = F, {
      
      ovIds <- input[[ns("planningPredictorModalVariables")]]
      
      newPred <- rvPredictor()
      if(is.null(newPred)) return()
      newPred$setOVIds(ovIds)

      
      updateTextInput(session, ns("planningPredictorModalName"), value=newPred$getName())
      
      #update info box
      updateTextAreaInput(session, ns("planningPredictorModalInfo"), value=newPred$getInfo())
      
      #update plot
      output[[ns("planningPredictorModalPlot")]] <- renderPlot(newPred$getInfoPlot())
      output[[ns("planningPredictorModalPlotInfo")]] <- renderUI(tags$div(style="padding:5px; font-size:12px;",newPred$getInfoPlotCaption()))
      
      rvPredictor(newPred)
    })
    
    
    
    #Confirm
    observeEvent(input[[ns("confirmModalPredictor")]], {
      newPred <- rvPredictor()
      
      #create also parameter

      newPara <- cMCD$createParameter()
      newPara$setPredId(newPred$getId())
      
      if(newPred$getType() == "predictor"){
        newPred$setOVIds(input[[ns("planningPredictorModalVariables")]])
        newPara$setType("predictor")
        newPara$setPriority(1)
        newPara$setName(name=HTML(paste0("b<sub>",newPred$getName(),"</sub>")))
      }else if(newPred$getType() == "intercept"){
        newPred$setName("(Intercept)")
        newPara$setType("intercept")
        newPara$setName(name=HTML(paste0("b<sub>",newPred$getName(),"</sub>")))
      }
      cMCD$addPredictor(newPred$getInstance(cMCD))
      cMCD$addParameter(newPara$getInstance(cMCD))
      removeModal()
      output[[ns("planningModalPredictor")]] <- renderUI(NULL)
      rvPredictorSelectedId(newPred$getId())
    })
    
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  ##############################################################################
  ################################## Parameter #################################
  ##############################################################################
  
  {
    
    rvParameters <- reactiveVal(list())
    
    #Update parameter list (rvParameters)
    observe({
      cMCD$getReactive("parameter")
      
      isolate({
        oldList <- rvParameters()
        newList <- list()
        
        parameters <- cMCD$getParameters(T)
        for(para in parameters){
          subs <- para$getSubs()
          subList <- names(subs)
          subListNames <- subList
          if(!is.empty(subs)){ 
            if(length(subList) > 1){
              subListNames <- subList
              subList <- paste0("b<sub>",subList,"</sub>")
            }
            subList <- as.list(subList)
            
            if(length(subs) != length(subList) && localUse) browser()
            
            for(s_i in seq_along(subs)){
              sub <- subs[[s_i]]
              if(sub$getAmIRedundant()){
                subList[[s_i]] <- paste0("<s style=\"color:", BAYAS_COLORS$`--formula-color-5`,";\">",subList[[s_i]],"</s>")
                attr(subList[[s_i]], "bayasRedundant") <- T
              }
              attr(subList[[s_i]], "bayasParaId") <- para$getId()
              attr(subList[[s_i]], "bayasSubName") <- subListNames[[s_i]]
            }
            

            paraName <- para$getName() 
            if(length(subs) > 1){
              na <- subList
              subList <- as.list(subList)
              names(subList) <- na
              subList <- lapply(subList, function(x) structure(x, bayasParaId = para$getId()))
            }
            attr(subList, "bayasParaId") <- para$getId()
            if(para$getAmIRedundant()){
              paraName <- paste0("<s style=\"color:", BAYAS_COLORS$`--formula-color-5`,";\">",paraName,"</s>")
              attr(subList, "bayasRedundant") <- T
            }
            newList <- list.append(newList, subList, paraName)
          }
        }
        update <- F
        if(length(oldList) != length(newList) ||
           any(names(oldList) != names(newList))){
          update <- T
        }else{
          for(i in seq_len(length(oldList))){
            parasOld <- oldList[[i]]
            parasNew <- newList[[i]]
            if((length(parasOld) != length(parasNew)) || 
               any(unlist(parasOld) != unlist(parasNew))){
              update <- T
              break
            }
          }
        }
        if(update){
          rvParameters(newList)
        }
        discardRedundantPredictors()
      })
    })
    
    showParameterTrigger <- reactiveVal(T)
    observe({
      selPred <- showParameter()
      if(is.null(selPred)) return()
      isolate({
        showParameterTrigger(!showParameterTrigger())
      })
    })
    
    observe({
      showParameterTrigger()
      paraList <- rvParameters()
      
      isolate({
        selPred <- showParameter()
        newTree <- paraList
        newTree <- lapply(newTree, function(x) structure(x, stopened = TRUE))
        if(!is.null(selPred)){
          pred <- cMCD$getPredictor(id=selPred)
          pName <- pred$getName()
          for(t_i in seq_len(length(newTree))){
            name <- names(newTree)[t_i]
            name <- str_split(name, "<sub>")[[1]][2]
            name <- str_split(name, "</sub>")[[1]][1]
            if(equal(pName,name)){
              newTree[[t_i]] <- structure(newTree[[t_i]], stselected = TRUE)
            }
          }
        }
        
        output[[ns("parameterExistingPredictors")]] <- renderTree(newTree)
        showParameter(NULL)
      })
    })
    
    
    #Update parameters (sub) paras according to changes in predictors
    observe({
      cMCD$getReactive("predictor")

      isolate({
        paras <- cMCD$getParameters()
        for(para in paras){
          if(!is.null(para$getPredId())){
            pred <- cMCD$getPredictor(para$getPredId())
            
            if(!is.null(pred)){
              subs <- para$getSubs()
              
              if(para$getType() == "intercept"){
                para$setName(name=HTML(paste0("b<sub>",pred$getName(),"</sub>")))
                
                if(is.empty(subs)){
                  newsub <- para$createSub()
                  
                  paraPrior <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Normal)
                  paraPrior$setParameterValue(2, 10)
                  paraGen <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Normal)
                  
                  newsub$setPrior(paraPrior)
                  newsub$setValueDistribution(paraGen)

                  para$addSub(newsub, "")
                }
              }else if(para$getType() == "predictor"){
                para$setName(name=HTML(paste0("b<sub>",pred$getName(),"</sub>")))
                
                if(is.empty(subs)){
                  elements <- pred$getUniqueElements()
                  for(el in elements){
                    newsub <- para$createSub()
                    paraPrior <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Normal)
                    paraPrior$setParameterValue(2, 10)
                    paraGen <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Normal)
                    
                    newsub$setPrior(paraPrior)
                    newsub$setValueDistribution(paraGen)
                    para$addSub(newsub, el)
                  }
                }else{
                  firstSub <- subs[[1]]

                  
                  elements <- pred$getUniqueElements()
                  saved_paras <- list()
                  #save non removes subs
                  for(sub_name in names(subs)){
                    # sub_name <- names(subs)[[2]]
                    if(sub_name %in% elements){
                      saved_paras <- list.append(saved_paras, subs[[sub_name]]$getInstance() , sub_name)
                    }
                  }
                  
                  #remove paras
                  para$setSubs(NULL, silent=T)
                  
                  #add new paras
                  for(el in elements){
                    if(el %in% names(saved_paras)){
                      para$addSub(saved_paras[[el]], el)
                    }else{
                      newsub <- para$createSub()
                      newsub$setPrior(firstSub$getPrior()$getInstance())
                      newsub$setValueDistribution(firstSub$getValueDistribution()$getInstance())
                      para$addSub(newsub, el)
                    }
                  }
                }
              }
            }else{
              #predictor removed...
              cMCD$removeParameter(id=para$getId())
            }
          }
        }
      })
    })
    
    
    #Show selected parameter
    observe({
      tree <- input[[ns("parameterExistingPredictors")]]
      cMCD$getReactive("parameter")
      isolate({
        
        sub <- cMCD$getParameterSubByTree(tree) #should be one
        if(length(sub) > 1){
          warning("More than one sub returned")
          if(localuse) browser()
          showNotification("Please select only one parameter.", type="warning")
          return()
        } 
        
        if(!is.empty(sub)){
          sub <- sub[[1]]
          if("ModelCreatingDataParameter" %in% class(sub)){

            para <- sub
            sub <- para$getSubs()[[1]]
            
            text <- ""
            if(para$getAmIRedundant()){
              text <- paste0("The whole parameter vector is omitted due to collinearity (redundancy). \\n")
              output[[ns("parameterInfo")]] <- renderUI(wellPanel(style="padding: 6px 12px;",
                                                                  HTML(text)))
            }
            
            if(!para$getUseSameDistForAllSubs() && length(para$getSubs())>1){
              output[[ns("parameterValueOverviewPlot")]] <- renderPlot(ggplot())
              hide(ns("parameterValueOverviewPlot"))
              
              text <- paste0(text, " This parameter is a vector.",
                             " Currently, all sub parameters may use different generation and prior distributions.",
                             " Select a certain child element to examine its distributions.")
              output[[ns("parameterInfo")]] <- renderUI(wellPanel(style="padding: 6px 12px;",
                                                                  HTML(text)))
              return()
            }
            
            output[[ns("parameterInfo")]] <- renderUI(wellPanel(style="padding: 6px 12px;"))
            show(ns("parameterValueOverviewPlot"))
            output[[ns("parameterValueOverviewPlot")]] <- renderPlot(sub$getPlot())
          }else{
            
            if(sub$getAmIRedundant()){
              
              text <- paste0("This parameter is omitted due to collinearity (redundancy). ")
                             
              red <- sub$getRedundant()
              comb <- red$comb
              equal <- red$equal
              
              if(is.empty(comb)){
                text <- paste0(text,
                               "It describes the same data as the parameter")
              }else{
                combTmp <- ""
                for(co in comb){
                  para <- cMCD$getParameter(co[2])
                  tmp <- ""
                  if(co[3] != ""){
                    tmp <- paste0("b<sub>",co[3],"</sub> (", para$getName(), ")")
                  }else{
                    tmp <- para$getName()
                  }
                  tmp <- paste0("<b>",tmp,"</b>")
                  if(combTmp == ""){
                    combTmp <- tmp
                  }else{
                    combTmp <- paste0(combTmp, ", ", tmp)
                  }
                }
                text <- paste0(text,
                               "It describes combined with ", combTmp,
                               " the same data as the parameter")
              }
              if(is.empty(equal)){
                stop("error ...")
              }else{
                equalTmp <- ""
                if(length(equal) > 1){
                  text <- paste0(text, "s")
                }
                for(co in equal){
                  para <- cMCD$getParameter(co[2])
                  tmp <- ""
                  if(length(co) ==3){
                    tmp <- paste0("b<sub>",co[3],"</sub> (", para$getName(), ")")
                  }else{
                    tmp <- para$getName()
                  }
                  tmp <- paste0("<b>",tmp,"</b>")
                  if(equalTmp==""){
                    equalTmp <- tmp
                  }else{
                    equalTmp <- paste0(equalTmp, ", ", tmp)
                  }
                  
                }
                text <- paste0(text," ", equalTmp, ".")
              }
              
              output[[ns("parameterInfo")]] <- renderUI(wellPanel(style="padding: 6px 12px;",
                                                                  HTML(text)))
              
            }else{
              text <- paste0("This parameter describes the effect between ... ")
              text <- ""
              output[[ns("parameterInfo")]] <- renderUI(wellPanel(style="padding: 6px 12px;",HTML(text)))
            }

            show(ns("parameterValueOverviewPlot"))
            output[[ns("parameterValueOverviewPlot")]] <- renderPlot(sub$getPlot())
          }
        }

      })
    })
    

    
    ##########################################
    ############ Parameter modal  ############ 
    {
      #Duplicate of parameters (getInstance)
      paraDup <- reactiveVal(NULL)
      
      #Duplicate of the first sub parameter (getInstance)
      #Used if the same prior/gen dist is used for all subs
      paraDistGlobal <- reactiveVal(NULL)
      
      g_subTree <- reactiveVal(NULL)
      selectedTreeElement <- reactiveVal(NULL)
      
      ##Open Modal
      observeEvent(input[[ns("parameterSetValue")]], {

        tree <- input[[ns("parameterExistingPredictors")]]
        if(is.null(tree)) return()
        
        sub <- cMCD$getParameterSubByTree(tree)
        if(length(sub) > 1){
          warning("More than one sub returned")
          if(localuse) browser()
          showNotification("Please select only one parameter.", type="warning")
          return()
        } 
        
        if(is.empty(sub)){
          showNotification("First select a parameter.")
          return()
        } 
        sub <- sub[[1]]
        
        parameter <- NULL
        if("ModelCreatingDataParameter" %in% class(sub)){
          parameter <- sub
          
          sub <- sub$getSubs()[[1]]
        }else{
          parameter <- sub$getParameter()
        }
        # if(is.null(sub) && !is.null(parameter$getSingleSub())) sub <- parameter$getSingleSub()


        top <- planning_creatingStepsParameter(ns, parameter, sub)
        help <- planning_creatingStepsParameter_help(ns)
        whole <- planning_ParameterModal(top, help)
        
        showModal(modalDialog(
          size="xl",
          easyClose=F,

          footer = tags$div(
            style = "width:100%",
            tags$span(
              actionButton(ns("cancelModalParameter"), "Cancel"),
              style = "float: left;"
            ),
            tags$div(
              style= "text-align: right;",
              actionButton(ns("confirmModalParameter"), "Confirm", class="btn-primary")
            )
          ),
          
          whole
        ))
        

        #Display just a subtree of all parameters
        selected <- get_selected(tree)
        subTree <- list()
        anc <- attr(selected[[1]], "ancestry")
        if(!is.empty(anc)){
          if(anc %in% names(tree)){
            for(el_id in seq_len(length(tree))){
              if(anc==names(tree)[[el_id]]){
                el <- tree[[el_id]]
                subTree <- list.append(subTree, el, anc)
              }
            }
          }
        }else{
          subTree <- list(tree[[selected[[1]]]])
          names(subTree) <- selected[[1]][1]
          for(e_i in seq_len(length(subTree[[1]]))){
            attr(subTree[[1]][[e_i]], "stselected") <- F
          }
          attr(subTree[[1]], "stselected") <- T
        }
        g_subTree(subTree)
        if(!is.empty(anc)){
          parameter$setUseSameDistForAllSubs(F)
          updateCheckboxInput(session=session, 
                              inputId=ns("parameterModalSameValuePrior"), 
                              value=F)
        } 
        if(parameter$getUseSameDistForAllSubs() && is.null(parameter$getSingleSub()) 
           && length(parameter$getSubs()) > 1){ 
          for(e_i in seq_len(length(subTree[[1]]))){
            attr(subTree[[1]][[e_i]], "stselected") <- F
          }
          attr(subTree[[1]], "stselected") <- T
          g_subTree(subTree)
          subTree[[1]] <- ""
          attr(subTree[[1]], "stselected") <- T
          enable(ns("parameterModalGenPriorUI"))
        }
        output[[ns("parameterModalTree")]] <- renderTree(subTree)
        
        
        paraDup(parameter$getInstance(silent=T))
        firstSub <- paraDup()$getSubs()[[1]]
        paraDistGlobal(firstSub$getInstance(silent=T))
        
        #render distribution parameter
        subGenDist <- sub$getValueDistribution()
        subPriorDist <- sub$getPrior()
        if(!is.null(subGenDist)){
          output[[ns("parameterModalGenUi")]] <- renderUI({
            planning_creatingStepsParameter_parameter(ns=ns, 
                                                      name="parameterModalGenDistPara", 
                                                      dist=subGenDist)
            })
        }
        if(!is.null(subPriorDist)){
          output[[ns("parameterModalPriorUi")]] <- renderUI({
            planning_creatingStepsParameter_parameter(ns=ns, 
                                                      name="parameterModalPriorDistPara", 
                                                      dist=subPriorDist)
          })
        }
      })
      
      ##selected tree element
      observeEvent(input[[ns("parameterModalTree")]], {
        selected <- cMCD$getParameterSubByTree(input[[ns("parameterModalTree")]], returnNULL=T,
                                                parameters=list(paraDup()))
        if(!is.empty(selected)) selected <- selected[[1]]
        selectedTreeElement(selected)
      })
      
      
      ##selected tree element
      observe({
        sub <- selectedTreeElement()
        if(is.empty(sub)){
          shinyjs::disable(ns("parameterModalGenPriorUIDiv"))
          return()
        }
        shinyjs::enable(ns("parameterModalGenPriorUIDiv"))
        
        isolate({
          parameter <- paraDup()
          
          # if(is.null(top)){
          if("ModelCreatingDataParameter" %in% class(sub)){
            sub <- paraDistGlobal()
            if(length(parameter$getSubs()) > 1){
              show(ns("parameterModalSameValuePrior"))
              if(input[[ns("parameterModalSameValuePrior")]]){
                enable(ns("parameterModalGenPriorUI"))
              }else{
                disable(ns("parameterModalGenPriorUI"))
              }
            }else{
              hide(ns("parameterModalSameValuePrior"))
              enable(ns("parameterModalGenPriorUI"))
            }
          }else{
            hide(ns("parameterModalSameValuePrior"))
            enable(ns("parameterModalGenPriorUI"))
          }
          
          prior <- sub$getPrior()
          gen <- sub$getValueDistribution()
          
          #Update  distribution parameter
          if(input[[ns("parameterModalPriorDist")]] == prior$getName()){
            if(prior$getUseAlternative()){
              priorPara <- prior$getAlternativeParameter()
              sapply(seq_len(length(priorPara)), function(i){
                updateBayasNumericInput(inputId=ns(paste0("parameterModalPriorDistParaAlt",i)), 
                                        value=priorPara[[i]]$value)
              })
            }else{
              priorPara <- prior$getParameter()
              sapply(seq_len(length(priorPara)), function(i){
                updateBayasNumericInput(inputId=ns(paste0("parameterModalPriorDistPara",i)), 
                                        value=priorPara[[i]]$value)
              })
            }
          }
          if(input[[ns("parameterModalGenDist")]] == gen$getName()){
            if(gen$getUseAlternative()){
              priorPara <- gen$getAlternativeParameter()
              sapply(seq_len(length(priorPara)), function(i){
                updateBayasNumericInput(inputId=ns(paste0("parameterModalGenDistParaAlt",i)), 
                                        value=priorPara[[i]]$value)
              })
            }else{
              priorPara <- gen$getParameter()
              sapply(seq_len(length(priorPara)), function(i){
                updateBayasNumericInput(inputId=ns(paste0("parameterModalGenDistPara",i)), 
                                        value=priorPara[[i]]$value)
              })
            }
          }
          
          mcd <- parameter$getMcd()
          mcdResp <- mcd$getMcdResponse()
          respDist <- mcdResp$getDist()
          respLink <- mcdResp$getLink()
          
          distPriorChoices <- getParametersPossiblePriorDistributions(parameter$getType(), 
                                                                      "inference", respDist, respLink)
          distGenChoices <- getParametersPossiblePriorDistributions(parameter$getType(), 
                                                                    "generation", respDist, respLink)
          
          updateSelectInput(inputId=ns("parameterModalPriorDist"), 
                            choices=distPriorChoices,
                            selected=prior$getName())
          updateSelectInput(inputId=ns("parameterModalGenDist"), 
                            choices=distGenChoices,
                            selected=gen$getName())
          
          #Update random seed
          updateBayasNumericInput(session=session, 
                                  inputId=ns("parameterModalGenDistParaRandomSeed"),
                                  value = gen$getSeed())
          updateBayasNumericInput(session=session, 
                                  inputId=ns("parameterModalPriorDistParaRandomSeed"),
                                  value = prior$getSeed())
        })
      })
      
      
      ##Use same value/prior for all subs?
      observeEvent(input[[ns("parameterModalSameValuePrior")]], ignoreInit=T, {
        subTree <- g_subTree()
        if(input[[ns("parameterModalSameValuePrior")]]){
          for(e_i in seq_len(length(subTree[[1]]))){
            attr(subTree[[1]][[e_i]], "stselected") <- F
          }
          attr(subTree[[1]], "stselected") <- T
          g_subTree(subTree)
          subTree[[1]] <- ""
          attr(subTree[[1]], "stselected") <- T
          attr(subTree[[1]], "bayasParaId") <- attr(g_subTree()[[1]], "bayasParaId")
          output[[ns("parameterModalTree")]] <- renderTree(subTree)
          enable(ns("parameterModalGenPriorUI"))
        }else{
          output[[ns("parameterModalTree")]] <- renderTree(subTree)
          disable(ns("parameterModalGenPriorUI"))
        }
      })
      
      
      ##Update generation distribution parameter
      observeEvent(input[[ns("parameterModalGenDist")]], {
        para <- paraDup()
        paraGlobal <- paraDistGlobal()

        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        
        parameter <- sub$getParameter()

        dist <- sub$getValueDistribution()
        if(is.null(dist) || input[[ns("parameterModalGenDist")]]!= dist$getName())
          dist <- ModelCreatingDataOtherVariableDistributionFactory(input[[ns("parameterModalGenDist")]])
 
        if(!is.null(dist)){
          
          sub$setValueDistribution(dist, silent=T)
          paraDistGlobal(paraGlobal)
          
          paraDup(para)

          div <- planning_creatingStepsParameter_parameter(ns=ns, 
                                                           name="parameterModalGenDistPara", 
                                                           dist=dist)

          if(!input[[ns("parameterModalSameValuePrior")]] && 
             "ModelCreatingDataParameter" %in% class(sub)) div <- disabled(div)
          output[[ns("parameterModalGenUi")]] <- renderUI({
            div
          })
        }else{
          output[[ns("parameterModalGenUi")]] <- renderUI(tags$div())
        }
      })
      
      
      ##Update prior distribution parameter
      observeEvent(input[[ns("parameterModalPriorDist")]], {
        para <- paraDup()
        paraGlobal <- paraDistGlobal()
        
        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        
        parameter <- sub$getParameter()
        
        dist <- sub$getPrior()
        if(is.null(dist) || input[[ns("parameterModalPriorDist")]]!= dist$getName())
          dist <- ModelCreatingDataOtherVariableDistributionFactory(input[[ns("parameterModalPriorDist")]])
        
        if(!is.null(dist)){
          sub$setPrior(dist, silent=T)
          paraDistGlobal(paraGlobal)
          
          paraDup(para)
          
          div <- planning_creatingStepsParameter_parameter(ns=ns, 
                                                           name="parameterModalPriorDistPara", 
                                                           dist=dist)
          if(!input[[ns("parameterModalSameValuePrior")]] && 
             "ModelCreatingDataParameter" %in% class(sub)) div <- disabled(div)
          output[[ns("parameterModalPriorUi")]] <- renderUI({
            div
          })
        }else{
          output[[ns("parameterModalPriorUi")]] <- renderUI(tags$div())
        }
      })
      
      
      ##Switch between parameter and alternative parameter for generation dist
      observeEvent(input[[ns("parameterModalGenDistParaCheckbox")]], {
        
        para <- paraDup()
        paraGlobal <- paraDistGlobal()

        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        
        parameter <- sub$getParameter()
        
        subDist <- sub$getValueDistribution()

        if(input[[ns("parameterModalGenDist")]] != subDist$getName()) return()
        
        if(!input[[ns("parameterModalGenDistParaCheckbox")]]){
          show(ns("parameterModalGenDistPara"))
          hide(ns("parameterModalGenDistParaAlt"))
          subDist$setUseAlternative(F)
          
          distPara <- subDist$getParameter()
          sapply(seq_len(length(distPara)), function(i){
            updateBayasNumericInput(inputId=ns(paste0("parameterModalGenDistPara",i)), 
                                    value=distPara[[i]]$value)
          })
        }else{
          show(ns("parameterModalGenDistParaAlt"))
          hide(ns("parameterModalGenDistPara"))
          subDist$setUseAlternative(T)

          distPara <- subDist$getAlternativeParameter()
          sapply(seq_len(length(distPara)), function(i){
            updateBayasNumericInput(inputId=ns(paste0("parameterModalGenDistParaAlt",i)), 
                                    value=distPara[[i]]$value)
          })
        }
        paraDistGlobal(paraGlobal)
        paraDup(para)
      })
      
      ##Switch between parameter and alternative parameter for prior 
      observeEvent(input[[ns("parameterModalPriorDistParaCheckbox")]], {
        para <- paraDup()
        paraGlobal <- paraDistGlobal()
        
        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        parameter <- sub$getParameter()
        
        subDist <- sub$getPrior()
        
        if(input[[ns("parameterModalPriorDist")]] != subDist$getName()) return()
        
        if(!input[[ns("parameterModalPriorDistParaCheckbox")]]){
          show(ns("parameterModalPriorDistPara"))
          hide(ns("parameterModalPriorDistParaAlt"))
          subDist$setUseAlternative(F)
        }else{
          show(ns("parameterModalPriorDistParaAlt"))
          hide(ns("parameterModalPriorDistPara"))
          subDist$setUseAlternative(T)
        }
        paraDistGlobal(paraGlobal)
        
        paraDup(para)
      })
      

      ##Random seed
      #Generation
      observeEvent(input[[ns("parameterModalGenDistParaRandomSeed")]], {
        para <- paraDup()
        paraGlobal <- paraDistGlobal()

        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        
        subDist <- sub$getValueDistribution()
        subDist$setSeed(input[[ns("parameterModalGenDistParaRandomSeed")]])
        
        paraDup(para)
      })
      observeEvent(input[[ns("parameterModalGenDistParaRandomSeedDice")]], {
        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        updateBayasNumericInput(inputId=ns("parameterModalGenDistParaRandomSeed"), 
                                value=drawRandomSeed())
      })
      #Prior
      observeEvent(input[[ns("parameterModalPriorDistParaRandomSeed")]], {
        para <- paraDup()
        paraGlobal <- paraDistGlobal()

        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        
        if("ModelCreatingDataParameter" %in% class(sub)) 
          sub <- paraDistGlobal()
        
        subDist <- sub$getPrior()
        subDist$setSeed(input[[ns("parameterModalPriorDistParaRandomSeed")]])
        
        paraDup(para)
      })
      observeEvent(input[[ns("parameterModalPriorDistParaRandomSeedDice")]], {
        sub <- selectedTreeElement()
        if(is.empty(sub)) return()
        updateBayasNumericInput(inputId=ns("parameterModalPriorDistParaRandomSeed"), 
                                value=drawRandomSeed())
      })
      ##Distribution parameters
      lapply(1:5, function(i){
        observeEvent(input[[ns(paste0("parameterModalGenDistPara",i))]], {
          para <- paraDup()
          paraGlobal <- paraDistGlobal()
          
          sub <- selectedTreeElement()
          if(is.empty(sub)) return()
          
          if("ModelCreatingDataParameter" %in% class(sub)) 
            sub <- paraDistGlobal()
          
          parameter <- sub$getParameter()
          
          subDist <- sub$getValueDistribution()
          
          inp <- input[[ns(paste0("parameterModalGenDistPara",i))]]
          subDist$setParameterValue(i,inp,F)
          subDist$transform(toAlt=T)
          
          paraDistGlobal(paraGlobal)
          
          paraDup(para)
        })
        observeEvent(input[[ns(paste0("parameterModalGenDistParaAlt",i))]], {
          para <- paraDup()
          paraGlobal <- paraDistGlobal()
          
          sub <- selectedTreeElement()
          if(is.empty(sub)) return()
          
          if("ModelCreatingDataParameter" %in% class(sub)) 
            sub <- paraDistGlobal()
          
          parameter <- sub$getParameter()
          
          subDist <- sub$getValueDistribution()
          
          inp <- input[[ns(paste0("parameterModalGenDistParaAlt",i))]]
          subDist$setParameterValue(i,inp, T)
          subDist$transform(toAlt=F)
          
          paraDistGlobal(paraGlobal)
          
          paraDup(para)
        })
        observeEvent(input[[ns(paste0("parameterModalPriorDistPara",i))]], {

          para <- paraDup()
          paraGlobal <- paraDistGlobal()
          
          sub <- selectedTreeElement()
          if(is.empty(sub)) return()
          
          if("ModelCreatingDataParameter" %in% class(sub)) 
            sub <- paraDistGlobal()
          
          parameter <- sub$getParameter()
          
          subDist <- sub$getPrior()
          
          inp <- input[[ns(paste0("parameterModalPriorDistPara",i))]]
          subDist$setParameterValue(i,inp,F)
          subDist$transform(toAlt=T)
          
          paraDistGlobal(paraGlobal)
          
          paraDup(para)
        })
        observeEvent(input[[ns(paste0("parameterModalPriorDistParaAlt",i))]], {

          para <- paraDup()
          paraGlobal <- paraDistGlobal()
          
          sub <- selectedTreeElement()
          if(is.empty(sub)) return()
          
          if("ModelCreatingDataParameter" %in% class(sub)) 
            sub <- paraDistGlobal()
          
          parameter <- sub$getParameter()
          
          subDist <- sub$getPrior()
          
          inp <- input[[ns(paste0("parameterModalPriorDistParaAlt",i))]]
          subDist$setParameterValue(i,inp,T)
          subDist$transform(toAlt=F)
          
          paraDistGlobal(paraGlobal)
          
          paraDup(para)
        })

      })
      
      
      updateParameterModalPlot <- reactive({
        input[[ns("parameterModalGenDist")]]
        input[[ns("parameterModalPriorDist")]]
        input[[ns("parameterModalGenDistParaCheckbox")]]
        input[[ns("parameterModalPriorDistParaCheckbox")]]
        
        input[[ns("parameterModalGenDistParaRandomSeed")]]
        input[[ns("parameterModalPriorDistParaRandomSeed")]]
        lapply(1:3, function(i){
          input[[ns(paste0("parameterModalGenDistPara",i))]]
          input[[ns(paste0("parameterModalGenDistParaAlt",i))]]
          input[[ns(paste0("parameterModalPriorDistPara",i))]]
          input[[ns(paste0("parameterModalPriorDistParaAlt",i))]]
        })
        list()
      })
      updateParameterModalPlot_d <- debounce(updateParameterModalPlot, debounceTime7)
      ##Update plot 
      observe({
        res <- updateParameterModalPlot_d()
        
        isolate({
          para <- paraDup()
          paraGlobal <- paraDistGlobal()
          
          sub <- selectedTreeElement()
          if(is.empty(sub)) return()
          
          if("ModelCreatingDataParameter" %in% class(sub)) 
            sub <- paraDistGlobal() 
          
          output[[ns("parameterModalPlot")]] <- renderPlot(sub$getPlot())
        })
      })
      
      
      ##Cancel
      observeEvent(input[[ns("cancelModalParameter")]], {
        output[[ns("parameterModalGenUi")]] <- renderUI(tags$div())
        output[[ns("parameterModalPriorUi")]] <- renderUI(tags$div())
        selectedTreeElement(NULL)
        removeModal()
      })
      
      
      ##Confirm modal
      observeEvent(input[[ns("confirmModalParameter")]], {
        

        para <- paraDup()
        singleDist <- paraDistGlobal()
        
        subsTmp <- NULL
        if(input[[ns("parameterModalSameValuePrior")]] && is.null(para$getSingleSub())) {
          subsTmp <- list(singleDist)
        }else{
          subsTmp <- para$getSubs()
        }
        
        paraId <- attr(input[[ns("parameterModalTree")]][[1]], "bayasParaId")

        if(is.null(paraId)){
          if(localUse) browser()
          showNotification("Something went wrong. The operator is notified.", type="error")
          malfunction_report(code=malfunctionCode()$parameterModal, msg="paraId: modal parameter tree is empty",
                             type="error", askForReport=T)
          if(localUse) browser()
          return()
        }
        
        parameter <- cMCD$getParameter(paraId)
        subs <- parameter$getSubs()
        
        
        #replace parameter with tmp
        for(p_i in seq_len(length(subs))){
          p <- subs[[p_i]]
          
          subTmp <- subsTmp[[1]]
          if(length(subsTmp)>1){
            subTmp <- subsTmp[[p_i]]
          }
          
          p$setPrior(subTmp$getPrior()$getInstance())
          p$setValueDistribution(subTmp$getValueDistribution()$getInstance())
        }
        
        parameter$setUseSameDistForAllSubs(input[[ns("parameterModalSameValuePrior")]])
        
        output[[ns("parameterModalGenUi")]] <- renderUI(tags$div())
        output[[ns("parameterModalPriorUi")]] <- renderUI(tags$div())
        selectedTreeElement(NULL)
        removeModal()
      })
    }
  }
  
  
  
  #Edits paras amIRedundant 
  discardRedundantPredictors = function(){
    
    formula <- cMCD$getMcdFormula()
    if(formula$verifyFormula()$valid){
      
      preds <- formula$getUsedPredictors()
      preds <- cMCD$getPredictors()
      
      data <- cMCD$getData()
      intercept <- F
      
      # reduce data to predictors
      usedVarsIds <- c()
      for(p in preds){
        if(p$getType()=="intercept") intercept <- T
        ovIds <- p$getOVIds()
        for(i in ovIds){
          usedVarsIds <- c(usedVarsIds, i)
        }
      }
      usedVarsIds <- unique(usedVarsIds)
      usedVars <- c()
      for(i in usedVarsIds){
        oV <- cMCD$getOtherVariable(i)
        usedVars <- c(usedVars, oV$getName())
      }
      subdata <- data[,usedVars, F]

      
      describedData <- list() # elements of vectors of row indexes
      describedDataSlopes <- list() # elements are lists (name=slopenames:) of vectors of row indexes
      
      if(intercept){
        describedData <- list.append(describedData, 1:dim(subdata)[1])
      }

      
      for(p in preds){
        if(p$getType() != "predictor") next
        
        # elements <- p$getUniqueElements()
        elements <- unique(p$getUniqueElements())
        ovIds <- p$getOVIds()
        ovNames <- c()
        ovNums <- c()
        for(i in ovIds){
          oV <- cMCD$getOtherVariable(i)
          ovNames <- c(ovNames, oV$getName())
          ovNums <- c(ovNums, oV$isNumeric())
        }
        para <- cMCD$getParameterOfPredictor(p$getId())
        paraSubs <- para$getSubs()
        paraNames <- names(paraSubs)
        
        for(el in rev(elements)){
          predSubdata <- subdata
          
          subEl <- str_split(el, ":")[[1]]
          for(s in seq_len(length(subEl))){
            sEl <- subEl[s]
            num <- ovNums[s]
            if(num) next
            predSubdata <- predSubdata[predSubdata[[ovNames[s]]] == subEl[s],,F]
          }
          
          nums <- paste0(sort(ovNames[ovNums]), collapse=":")
          nname <- paste0(ovNames,collapse=":")
          nname <- paste0(nname, "@", el)
          if(nums==""){
            describedData <- list.append(describedData, 
                                         as.numeric(rownames(predSubdata)),
                                         name=nname)
          }else{
            subList <- describedDataSlopes[[nums]]
            
            subList <- list.append(subList, 
                                   as.numeric(rownames(predSubdata)),
                                   name=nname,
                                   extendBySameName=F)
            describedDataSlopes <- list.append(describedDataSlopes, 
                                               subList,
                                               nums, extendBySameName=F)
          }
        }
      }
      
 
      #proof
      reducedDescribedData <- checkForRedundants(describedData)
      reducedDescribedDataSlopes <- list()
      for(slope_i in seq_len(length(describedDataSlopes))){
        slope <- describedDataSlopes[[slope_i]]
        reducedDescribedDataSlopes <- list.append(reducedDescribedDataSlopes, 
                                                  checkForRedundants(slope),
                                                  name=names(describedDataSlopes)[slope_i])
      }

      
      #mark parameters as redundant
      for(p in preds){
       
        para <- cMCD$getParameterOfPredictor(p$getId())
        paraSubs <- para$getSubs()
        paraNames <- names(paraSubs)
        
        elements <- p$getUniqueElements()
        elements <- unique(elements)
        ovIds <- p$getOVIds()
        ovNames <- c()
        ovNums <- c()
        for(i in ovIds){
          oV <- cMCD$getOtherVariable(i)
          ovNames <- c(ovNames, oV$getName())
          ovNums <- c(ovNums, oV$isNumeric())
        }
        
        checkIn <- list()
        if(!any(ovNums)){
          checkIn <- reducedDescribedData
        }else{
          nums <- paste0(sort(ovNames[ovNums]), collapse=":")
          checkIn <- reducedDescribedDataSlopes[[nums]]
        }

        for(el in elements){
          if(is.na(el)) next
          sub <- paraSubs[[el]]
          nname <- paste0(ovNames,collapse=":")
          nname <- paste0(nname, "@", el)
          if(!nname %in% names(checkIn$new)){
            sub$setAmIRedundant(T)
            newRed <- list(comb=list(), equal=list())
            subRed <- checkIn$omit[[nname]]
            
            if(is.null(subRed) || is.empty(subRed)){
              warning("subRed should not be null!")
              next 
            }
            comb <- subRed$comb
            red <- subRed$red
            s_red <- str_split(red, "@@")[[1]]
            s_comb <- list()
            if(is.null(comb)) s_comb <- str_split(comb, "@@")[[1]]
            for(tmp in s_comb){
              s <- str_split(tmp, "@")[[1]]
              predName <- s[1]
              paraSubName <- s[2]
              pred <- cMCD$getPredictorByName(predName)
              predId <- pred$getId()
              para <- cMCD$getParameterOfPredictor(predId)
              paraId <- para$getId()
              subId <- paraSubName
              newComb <- newRed$comb
              newComb <- list.append(newComb, c(predId, paraId, subId))
              newRed$comb <- newComb
            }
            if(is.empty(s_red) || (length(s_red) == 1 && s_red == "")){
              #intercept
              pred <- cMCD$getPredictorByName("(Intercept)")
              predId <- pred$getId()
              para <- cMCD$getParameterOfPredictor(predId)
              paraId <- para$getId()
              newEqual <- newRed$equal
              newEqual <- list.append(newEqual, c(predId, paraId))
              newRed$equal <- newEqual
            }else{
              for(tmp in s_red){
                s <- str_split(tmp, "@")[[1]]
                predName <- s[1]
                paraSubName <- s[2]
                pred <- cMCD$getPredictorByName(predName)
                predId <- pred$getId()
                para <- cMCD$getParameterOfPredictor(predId)
                paraId <- para$getId()
                subId <- paraSubName
                newEqual <- newRed$equal
                newEqual <- list.append(newEqual, c(predId, paraId, subId))
                newRed$equal <- newEqual
              }
            }
            
            sub$setRedundant(newRed)
          }else{
            sub$setAmIRedundant(F)
          }
          
        }
      }
    }
  }

  ##list is a named list (variable names or element names of categorical variables) of 
  ##vectors of row numbers of each variable element (list(sex@male=c(1,2,3), sex@female=c(4,5,6), ...)
  ##Returns a list of two lists 'new' and 'omit'
  #new: named (varName@elementName) list of all parameters that won't be omitted
  #omit: named (varName@elementName) list of lists (red, comb) 
  #$red: The variable (elements) (varName@elementName) why this element is redundant
  #$comb: The variable (elements) (varName@elementName) combined with this one 
  #explaining the same data as the variable (elements) in $red
  checkForRedundants = function(list){
    allPoss <- list()
    omitList <- list()
    for(l_i in seq_len(length(list))){
      l <- list[[l_i]]
      if(is.empty(allPoss)){
        allPoss <- list.append(allPoss, l, name=names(list)[l_i])
      }else{
        lccv <- list.contains.conc.vec(allPoss, l) #, names(list)[l_i]
        if(list.contains.vec(allPoss, l)){
          for(i in seq_len(length(allPoss))){
            if(vectorEqual(allPoss[[i]], l)){
              omitList <- list.append(omitList, list(red=names(allPoss)[i], comb=""), names(list)[l_i])
              break
            }
          }
        }else if(lccv[[1]]){
          omitList <- list.append(omitList, list(red=lccv$red, comb=lccv$comb), names(list)[l_i])
        }else{
          allPoss <- list.append(allPoss, l, name=names(list)[l_i])
        }
      }
    }
    return(list(new=allPoss, omit=omitList))
  }
  
  
  
  
  
  
  ##############################################################################
  ############################### Data Generation ##############################
  ##############################################################################
  {
    
    observe({
      cMCD$getReactive("otherVariable")
      cMCD$getReactive("otherVariableDist")
      cMCD$getReactive("otherVariableName")
      
      oVs <- cMCD$getOtherVariables()
      varList <- list('Categorical variable'='')
      for(oV in oVs){
        type <- oV$getType()
        if(!is.null(type) && type=="categorical"){
          varList <- list.append(varList, oV$getId(), oV$getName())
        }
      }

      sel <- input[[ns("sampleSizeUnitVar")]]
      updateSelectizeInput(session, ns("sampleSizeUnitVar"),
                           choices=varList, selected = sel)
    })
    
    observeEvent(input[[ns("sampleSizeUnitVar")]], {
      cMCD$setGenerateData(input[[ns("sampleSizeUnitVar")]], "ssuVar")
    })
    observeEvent(input[[ns("sampleSizeUnitNumber")]], {
      cMCD$setGenerateData(input[[ns("sampleSizeUnitNumber")]], "ssu")
    })
    
    
    lockUpdate <- reactiveVal(F)
    updateTotalDP <- reactive({
      input[[ns("sampleSizeDataPoints")]]
    })
    updateTotalDP_d <- updateTotalDP %>% debounce(debounceTime8) 
    
    observe({
      inp <- updateTotalDP_d()

      isolate({
        lockUpdate(T)
        
        # inp <- input[[ns("sampleSizeDataPoints")]]
        if(!is.null(inp) && is.numeric(inp)){
          check <- cMCD$checkSampleSize(inp)
          if(check[[1]]){
            cMCD$setGenerateData(input[[ns("sampleSizeDataPoints")]], "tdp")
            if(!cMCD$getGenerateDataAutomatically())highlightUpdateDataButton(T)
          }else{

            text <- paste0("This number is not possible due to uncapped categorical variables. ",
                           "Next possible number would be: ", check[[2]][1])
            
            if(check[[2]][1] <check[[2]][2]){
              text <- paste0(text, " or ", check[[2]][2], "." )
            }else{
              text <- paste0(text, "." )
            }
            output[[ns("sampleSizeTotalPointsValidMsg")]] <- renderUI(
              tags$div(
                class="fontColor-error",
                text
              )
            )

          } 
        }
        
      })
    })
    

    observe({
      cMCD$getReactive("generateData")
      isolate({
        gData <- cMCD$getGenerateData(NULL)
        updateSelectizeInput(session, ns("sampleSizeUnitVar"), selected=gData$ssuVar)
        updateBayasNumericInput(session, inputId=ns("sampleSizeUnitNumber"), value=gData$ssu)
        if(lockUpdate()){
          lockUpdate(F)
        }else{
          updateBayasNumericInput(session, inputId=ns("sampleSizeDataPoints"), value=gData$tdp)
        }
        
        inp <- gData$tdp
        
        if(!is.null(inp) && is.numeric(inp)){
          check <- cMCD$checkSampleSize(inp)
          if(!check[[1]]){
            text <- paste0("This number is not possible due to uncapped categorical variables. ",
                           "Next possible number would be: ", check[[2]][1])
            
            if(check[[2]][1] <check[[2]][2]){
              text <- paste0(text, " or ", check[[2]][2], "." )
            }else{
              text <- paste0(text, "." )
            }
            output[[ns("sampleSizeTotalPointsValidMsg")]] <- renderUI(
              tags$div(
                class="fontColor-error",
                text
              )
            )
          }else{
            output[[ns("sampleSizeTotalPointsValidMsg")]] <- renderUI(tags$div())
          }
        }else{
          output[[ns("sampleSizeTotalPointsValidMsg")]] <- renderUI(tags$div())        
        }
        
      })
    })
    

    observeEvent(input[[ns("globalRandomSeed")]], {
      inp <- input[[ns("globalRandomSeed")]]
      if(randomSeedValid(inp)) {
        cMCD$setGenerateSeed(inp)
      }
    })
        
    observeEvent(input[[ns("globalRandomSeedDice")]], {
      updateBayasNumericInput(inputId=ns("globalRandomSeed"), value=drawRandomSeed())
    })
    
    #Update randomize seed of 'other variabes'
    observeEvent(input[[ns("sampleSizeGenerateDataVariable")]], {
      oVs <- cMCD$getOtherVariables()
      for(oV in oVs){
        oV$setSeed(drawRandomSeed(min=-100000,max=100000), silent=T)
        oV$setRandomValues()
      }
      if(cMCD$getGenerateDataAutomatically()){
        activeUpdateAutomatically(activeUpdateAutomatically()+1)
      }else{
        highlightUpdateDataButton(T)
      }
    })
    
    #Update randomize seed of 'parameters'
    observeEvent(input[[ns("sampleSizeGenerateParameters")]], {
      paras <- cMCD$getParameters()
      for(p in paras){
        subs <- p$getSubs()
        r <- drawRandomSeed(n=length(subs)*2, min=-100000,max=100000)
        for(s_i in seq_len(length(subs))){
          s <- subs[[s_i]]
          prior <- s$getPrior()
          val <- s$getValueDistribution()
          prior$setSeed(r[s_i])
          val$setSeed(r[s_i+length(subs)])
          s$setPrior(prior, silent=T) #silent due to the call of 'activeUpdateAutomatically', to prevent calling twice
          s$setValueDistribution(val, silent=T)
        }
      }
      if(cMCD$getGenerateDataAutomatically()){
        activeUpdateAutomatically(activeUpdateAutomatically()+1)
      }else{
        highlightUpdateDataButton(T)
      }
    })

    
    
    #Highlight 'Update data' button, when data has changed
    highlightUpdateDataButton <- function(flag){
      if(flag){
        addCssClass(ns("sampleSizeUpdateData"), "btn-primary")
        shinyjs::show(ns("dataHint"))
      }else{
        removeCssClass(ns("sampleSizeUpdateData"), "btn-primary")
        hide(ns("dataHint"))
      }
    }
    
    #Update data manually
    updateDataManually <- reactiveVal(0)
    observeEvent(input[[ns("dataHintLink")]], ignoreNULL = T, {
      updateDataManually(updateDataManually()+1)
    })
    observeEvent(input[[ns("sampleSizeUpdateData")]], ignoreNULL = T, {
      updateDataManually(updateDataManually()+1)
    })
    observe( {
      inp <- updateDataManually()
      
      if(inp==0)return()
      isolate({
        highlightUpdateDataButton(F)
        
        oVs <- cMCD$getOtherVariables()
        for(oV in oVs){
          oV$setRandomValues()
        }
        cMCD$refreshData()
        
        ## Check if any predictor and other variable that is used, is valid
        preds <- cMCD$getPredictors()
        for(p in preds){
          valid <- p$isValid()
          if(valid$valid == "error"){
            showNotification("Can't generate response data. Some predictors are invalid!", type="error")
            updateTabsetPanel(inputId=ns("uiOfSteps"), selected="3. Predictors")
            
            return()
          }
        }
        
        
        #if n > 10000 show progressbar
        samples <- c()
        if(cMCD$getValidTotalDatapoints()>10000){
          disable(ns("createDataSetAutomatically"))
          show(ns("createDataSetAutomaticallyHint"))
          
          withProgress(samples <- cMCD$getMcdFormula()$drawResponeData(cMCD$getValidTotalDatapoints()),
                       message ="Draw samples ...")
        }else{
          enable(ns("createDataSetAutomatically"))
          hide(ns("createDataSetAutomaticallyHint"))
          
          samples <- cMCD$getMcdFormula()$drawResponeData(cMCD$getValidTotalDatapoints())
        }
        
        if(samples[[1]]){
          cMCD$setDataColumn("Response", samples[[2]])
        }else{
          showNotification(samples$msg, type="warning")
        }
      })
    })

    
    
    ## Generate response data automatically if enabled
    activeUpdateAutomatically <- reactiveVal(0)
    #Delay reactive
    updateDatasetC <- reactiveVal(0)
    updateDataset <- reactive({
      activeUpdateAutomatically()
      cMCD$getReactive("otherVariable")
      cMCD$getReactive("otherVariableDist")

      cMCD$getReactive("responseDist")
      cMCD$getReactive("responseLink")

      cMCD$getReactive("predictor")

      cMCD$getReactive("parameter")
      
      cMCD$getReactive("formula")

      isolate({
        updateDatasetC(updateDatasetC()+1)
        return(updateDatasetC())
      })
    })
    updateDataset_d <- updateDataset %>% debounce(debounceTime9) 
    observe({
      updateDataset_d()
      updateTotalDP_d()

      isolate({
        if(!cMCD$getGenerateDataAutomatically() || 
           cMCD$getValidTotalDatapoints()>10000) return()

        oVs <- cMCD$getOtherVariables()
        for(oV in oVs){
          oV$setRandomValues()
        }

        cMCD$refreshData()

        ## Check if any predictor and other variable that is used, is valid
        preds <- cMCD$getPredictors()
        for(p in preds){
          valid <- p$isValid()
          if(valid$valid == "error"){
            addCssClass(ns("sampleSizeUpdateData"), "btn-primary")
            show(ns("dataHint"))
            return()
          }
        }
        
        samples <- c()
        if(cMCD$getValidTotalDatapoints()>10000){
          withProgress(samples <- cMCD$getMcdFormula()$drawResponeData(cMCD$getValidTotalDatapoints()),
                       message ="Draw samples ...")
        }else{
          samples <- cMCD$getMcdFormula()$drawResponeData(cMCD$getValidTotalDatapoints())
        }

        if(samples[[1]]){
          cMCD$setDataColumn("Response", samples[[2]])
        }
        
        highlightUpdateDataButton(F)
      })

    })
    
    observeEvent(input[[ns("createDataSetAutomatically")]], {
      if(!is.null(input[[ns("createDataSetAutomatically")]]))
        cMCD$setGenerateDataAutomatically(input[[ns("createDataSetAutomatically")]])
      if(input[[ns("createDataSetAutomatically")]]){
        activeUpdateAutomatically(activeUpdateAutomatically()+1)
      }
    })
    
    #Download dataset
    output[[ns("downloadWholeDataset")]] <- downloadHandler(
      filename = function() {
        name <- paste0("BAYAS_model_data.csv")
        if(!is.null(cMCD)){
          cName <- cMCD$getModelName()
          if(!is.null(cName)) name <- paste0(cName,"_data.csv")
        }
        return(name)
      },
      content = function(file) {
        data <- data.frame()
        if(!is.null(cMCD)){
          cData <- cMCD$getData()
          if(!is.null(cData)) data <- cData
        }
        write.csv(data, file, row.names=F)
      }
    )
  }
  
  
  
  
  
  
  ##############################################################################
  ########################## Sample Size Determination #########################
  ##############################################################################
  {
    
    selGoalId <- reactiveVal(NULL)
    goalModal <- reactiveVal(NULL)
    
    #Add new goal
    observeEvent(input[[ns("ssdAddGoal")]], {
      
      mcdSSD <- cMCD$getMcdSSD()
      goal <- mcdSSD$createGoal()
      mcdSSD$addGoal(goal)
      
      selGoalId(goal$getId())
      
      openSSDModal(goal)
    })
    
    
    #Show goals
    observe({
      cMCD$getReactive("ssdGoals")
      
      isolate({
        
        mcdSSD <- cMCD$getMcdSSD()
        goals <- mcdSSD$getGoals()
        
        listOfGoals <- list()
        errList <- c()
        unusedList <- c()
        for(g in goals){
          listOfGoals <- list.append(listOfGoals, g$getId(), g$getName())
          if(!g$getIsValid()){
            errList <- c(errList, g$getId())
          }else if(!g$getInUse()){
            unusedList <- c(unusedList, g$getId())
          }
        }
        
        errorUnusedList <- list()
        for(li in seq_len(length(listOfGoals))){
          el <- listOfGoals[[li]]
          n <- names(listOfGoals)[[li]]
          if(el %in% errList){
            errorUnusedList$invalid <- list.append(errorUnusedList$invalid, el, n, T)
          }else if(el %in% unusedList){
            errorUnusedList$unused <- list.append(errorUnusedList$unused, el, n, T)
          }else{
            errorUnusedList$valid <- list.append(errorUnusedList$valid, el, n, T)
          }
        }
        
        updateSelectInput(session=session, inputId=ns("ssdGoals"),
                          choices=errorUnusedList, selected = selGoalId())
      })
    })
    
    
    #set goal name
    observeEvent(input[[ns("ssdGoalName")]], {
      
      #check if name is already in use
      mcdSSD <- cMCD$getMcdSSD()
      goal <- mcdSSD$getGoal(selGoalId())
      
      shinyFeedback::hideFeedback(ns("ssdGoalName"))
      
      if(!is.null(goal)){
        name <- input[[ns("ssdGoalName")]]
        valid <- otherVariableNameIsValid(name, duplicate=F)
        if(valid$valid){
          if(mcdSSD$isGoalNameInUse(name, ignore=goal$getId())){
            valid <- list(valid=F, msg="This name is already in use.")
          }
        }
        if(!(is.null(name) || name == "" || valid$valid)){
          shinyFeedback::showFeedbackDanger(ns("ssdGoalName"),
                                            text=valid$msg, 
                                            color=BAYAS_COLORS$`--font-error`, 
                                            icon=NULL) 
          return()
        }
        goal$setName(name)
      }
    })
    
    
    #remove goal
    observeEvent(input[[ns("ssdGoalRemove")]], {
      mcdSSD <- cMCD$getMcdSSD()
      goal <- mcdSSD$getGoal(selGoalId())
      if(!is.null(goal)){
        mcdSSD$removeGoal(goal$getId())
      }
    })
    
    #Set goal (open SSD modal)
    observeEvent(input[[ns("ssdSetGoal")]], {
      mcdSSD <- cMCD$getMcdSSD()
      goal <- mcdSSD$getGoal(selGoalId())
      if(!is.null(goal)){
        openSSDModal(goal)
      }
    })
    
    #Use this goal
    observeEvent(input[[ns("ssdInUse")]], {
      if(is.null(input[[ns("ssdInUse")]])) return()
      mcdSSD <- cMCD$getMcdSSD()
      goal <- mcdSSD$getGoal(selGoalId())
      if(!is.null(goal)){
        goal$setInUse(input[[ns("ssdInUse")]])
      }
    })
    
    
    #Modify goals in case parameters are no longer available
    observe({
      cMCD$getReactive("parameter")
      isolate({
        mcdSSD <- cMCD$getMcdSSD()
        mcdSSD$verifyGoals()
      })
    })
    
    
    #Open Modal
    openSSDModal = function(goal){
      top <- planning_creatingStepsSSD(ns, goal)
      help <- planning_creatingStepsSSD_help(ns)
      whole <- planning_SSDModal(top, help)

      showModal(modalDialog(
        size="xl",
        easyClose=F,

        footer = tags$div(
          style = "width:100%",
          tags$span(
            actionButton(ns("cancelModalSSD"), "Cancel"),
            style = "float: left;"
          ),
          tags$div(
            style= "text-align: right;",
            actionButton(ns("confirmModalSSD"), "Confirm", class="btn-primary")
          )
        ),
        
        whole
      ))
      
      
      #Fill trees
      paraList <- rvParameters()
      newTree <- paraList
      newTree <- lapply(newTree, function(x) structure(x, stopened = TRUE))


      #remove not used paras
      newTree <- discardRedundantParametersInTree(newTree)

      newTreeA <- newTreeB <- newTree
      
      #add checked
      parasA <- goal$getParametersA(twoListFormat=T)
      parasB <- goal$getParametersB(twoListFormat=T)

      newTreeA <- selectParametersInTree(newTreeA, parasA)
      newTreeB <- selectParametersInTree(newTreeB, parasB)

      #update parameter trees
      output[[ns("ssdModalTreeA")]] <- renderTree(newTreeA)
      output[[ns("ssdModalTreeB")]] <- renderTree(newTreeB)
      
      goalM <- goal$getInstance()
      goalM$setTrigger(F)
      goalModal(goalM)
    }
    
    
    
    #set selGoalId
    observeEvent(input[[ns("ssdGoals")]], ignoreNULL = F, {
      selGoalId(input[[ns("ssdGoals")]])
    })
    
    #display name/ROPE/Precision etc.
    observe({
      
      selGoalId()
      cMCD$getReactive("ssdGoals")
      
      isolate({
        mcdSSD <- cMCD$getMcdSSD()
        goal <- mcdSSD$getGoal(selGoalId())
        
        shinyFeedback::hideFeedback(ns("ssdGoalName"))
        
        if(!is.null(goal)){
          #update name
          if(goal$getNameSetted()){
            updateBayasTextInput(session=session, inputId = ns("ssdGoalName"),
                                 value=goal$getName())
          }else{
            updateBayasTextInput(session=session, inputId = ns("ssdGoalName"),
                                 value="")
          }
          
          #update is in use?
          updateCheckboxInput(session, inputId=ns("ssdInUse"), value=goal$getInUse())
          
          if(goal$getIsValid()){
            hide(ns("parameterForSSDInfo"))
            show(ns("ssdPlot"))
            
            output[[ns("ssdPlot")]] <- renderPlot({
              goal$getPlot()
            })
            
          }else{
            #update info box 
            hide(ns("ssdPlot"))
            show(ns("parameterForSSDInfo"))
            output[[ns("parameterForSSDInfo")]] <- renderUI({
              tags$div(
                class="infoBox-light",
                style="padding: 10px; margin-bottom: 20px;",
                style="border-radius: 4px;font-weight: bold;",
                goal$getInvalidMsg()
              )
            })
          }
        }else{
          #update name
          updateBayasTextInput(session=session, inputId = ns("ssdGoalName"),
                               value="")
        }
        
      })
    })
    
    
    #set desired power
    observeEvent(input[[ns("ssdDesiredPower")]], {
      mcdSSD <- cMCD$getMcdSSD()
      mcdSSD$setPower(input[[ns("ssdDesiredPower")]])
    })
    
    #set max N
    observeEvent(input[[ns("ssdMaxN")]], {
      mcdSSD <- cMCD$getMcdSSD()
      mcdSSD$setMaxN(input[[ns("ssdMaxN")]])
    })
    
    #set accuarcy
    observeEvent(input[[ns("ssdAcceptedAccuracy")]], {
      mcdSSD <- cMCD$getMcdSSD()
      mcdSSD$setAccuarcy(input[[ns("ssdAcceptedAccuracy")]])
    })
    
    #set seed
    observeEvent(input[[ns("ssdSeed")]], {
      mcdSSD <- cMCD$getMcdSSD()
      mcdSSD$setSeed(input[[ns("ssdSeed")]])
    })
    
    
    ssd_g <- reactiveVal(NULL)
    continue_ssd <- reactiveVal(F)
    start_ssd <- reactiveVal(F)
    close_ssd <- reactiveVal(F)
    init_ssd <- reactiveVal(F)
    start_ssd_data <- reactiveVal(NULL)
    stop_ssd <- reactiveVal(NULL)
    
    #progressbar that is shown in the lower right corner
    globalSSDProgressBar <- reactiveVal(NULL)
    
    #Start ssd
    observeEvent(input[[ns("ssdStart")]], {

      #Is model complete and valid?
      #check response
      mcdResponse <- cMCD$getMcdResponse()
      if(is.null(mcdResponse$getDist())){
        showNotification("Please select a response distribution before.",
                         type="error")
        return()
      }
      #check predictor
      predictors <- cMCD$getPredictors()
      if(is.null(predictors) || length(predictors)==0){
        showNotification("Please define at least a single predictor (or an intercept) before.",
                         type="error")
        return()
      }
      anyValid <- F
      for(pred in predictors){
        if(!pred$getAmIRedundant() && pred$isValid()$valid != "valid"){
          showNotification("Please revise your predictors.",
                           type="error")
          return()
        }
        if(pred$isValid()$valid == "valid") anyValid <- T
      }
      if(!anyValid){
        showNotification("Please define at least a single valid predictor (or an intercept) before.",
                         type="error")
        return()
      }

      #Are all used goals valid?
      mcdSSD <- cMCD$getMcdSSD()
      goals <- mcdSSD$getGoals()
      if(is.null(goals) || length(goals)==0){
        showNotification("Please define at least a single goal before.", 
                         type="error")
        return()
      }
      if(!mcdSSD$isValid()){
        showNotification("Some of the used goals are not valid. Please revise them.", 
                         type="error")
        return()
      }

      #Is maxN valid?
      mcdSSD <- cMCD$getMcdSSD()
      maxN <- mcdSSD$getMaxN()
      checkedN <- cMCD$checkSampleSize(maxN)
      if(!checkedN[[1]]){
        if(checkedN[[2]][1] > maxN){
          showNotification(paste0("The 'Max N' value must be at least ", 
                                  checkedN[[2]][1] ," based on your data."), 
                           type="error", duration = 15)
          
          return()
        }
      }
      
      
      #Init gobal probressbar (lower right corner)
      globalSSDProgressBar(shiny::Progress$new())
      
      #Confirm restart - discard of current ssd
      #TODO
      
      hide(ns("ssdStart"))
      show(ns("ssdStop"))
      
      #Open Estimate 'N' tabsetpanel
      updateTabsetPanel(session = session, 
                        inputId = ns("tabsetShowStatModelAndSSD"), 
                        selected = "Estimate 'N'")
      updateTabsetPanel(session = session, 
                        inputId = ns("tabsetDataVisualizationSSD"), 
                        selected = "Estimate 'N' visualization")
      
      #set global seed
      set.seed(mcdSSD$getSeed())

      
      stopValue <- getValueOfStopButton()
      stop_ssd(stopValue)
      init_ssd(T)
    })

    
    #Continue ssd
    observeEvent(input[[ns("ssdContinue")]], {
      return()
      
      #Init gobal probressbar (lower right corner)
      globalSSDProgressBar(shiny::Progress$new())
      
      mcdSSD <- cMCD$getMcdSSD()
      
      
      con <- mcdSSD$getAccuarcy()
      desPower <- mcdSSD$getPower()
      
      
      updateProgressBar(
        session=session,
        id=ns("ssdProgressBar"),
        40, 
        title = "Continue sample size determination")
      globalSSDProgressBar()$set(value=0.4, message = "Sample Size Determination", 
                               detail = "Continue sample size determination")
      
      #Open Estimate 'N' tabsetpanel
      updateTabsetPanel(session = session, 
                        inputId = ns("tabsetShowStatModelAndSSD"), 
                        selected = "Estimate 'N'")
      updateTabsetPanel(session = session, 
                        inputId = ns("tabsetDataVisualizationSSD"), 
                        selected = "Estimate 'N' visualization")
      
      #set global seed
      set.seed(mcdSSD$getSeed())
      
      stopValue <- getValueOfStopButton()
      stop_ssd(stopValue)
      
      ssd <- mcdSSD$getSsdObject()

      # cluster <- initSSD_NGoals(strategy="sequential")
      # ssd$intern$cluster <- cluster
      
      
      # prob <- desPower
      # alpha <- prob*(con-2)+1
      # beta <- (1-prob)*(con-2)+1
      # accept_HDI <- bayestestR::hdi(rbeta(1e7, alpha, beta), 0.9)
      # accept_HDIwidth <- accept_HDI$CI_high - accept_HDI$CI_low
      # 
      # i_parallel <- numberOfIterationsPerStep
      # 
      # Incorrect
      # furtherArgs <- list(
      #   i_parallel=i_parallel, 
      #   model_seed = sample(1e5,i_parallel),
      #   accept_HDIwidth = accept_HDIwidth,
      #   accept_HDIConcentration = con
      # )
      # ssd$intern$furtherArgs <- furtherArgs
      
      
      ssd_g(ssd)
      continue_ssd(T)
    })
    
    
    init_ssd_d <- init_ssd %>% debounce(debounceTime10)
    
    #Compile model for ssd
    observe({
      init <- init_ssd_d()
      isolate({
        if(init){
          mcdFormula <- cMCD$getMcdFormula()
          modelSeed <- round(runif(1,-1e7,1e7))
          
          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            10, 
            title = "Compile stan model")
          globalSSDProgressBar()$set(value=0.1, message = "Sample Size Determination", 
                                   detail = "Compile stan model")

          compiledModel <- mcdFormula$buildRstanFormula(modelSeed)
          
          if(is.null(compiledModel)){
            stop_ssd(T)
            return()
          }
          
          #Is maxN valid?
          mcdSSD <- cMCD$getMcdSSD()
          maxN <- mcdSSD$getMaxN()
          checkedN <- cMCD$checkSampleSize(maxN)
          if(!checkedN[[1]]){
            if(checkedN[[2]][1]>=2){
              maxN <- checkedN[[2]][1]
            }else{
              maxN <- checkedN[[2]][2]
            }
          }
          
          l <- list(
            compiledModel=compiledModel,
            maxN=maxN
          )
          
          start_ssd_data(l)
          start_ssd(T)
        }
        init_ssd(F)
      })
    })
    
    #Stop ssd
    observe({
      sVal <- stop_ssd()
      if(is.null(sVal)) return()
      isolate({
        cStopVal <- getValueOfStopButton()
        if(sVal != cStopVal){
          enable(ns("ssdStop"))
          continue_ssd(F)
          start_ssd(F)
          close_ssd(T)
          init_ssd(F)
        }
      })
    })
    
    observeEvent(input[[ns("ssdStop")]], {
      # sVal <- stop_ssd()
      # if(is.null(sVal)) return()
      # 
      # cStopVal <- getValueOfStopButton()
      # if(sVal != cStopVal){
      #   enable(ns("ssdStop"))
      #   continue_ssd(F)
      #   start_ssd(F)
      #   close_ssd(T)
      # }
    })
    
    #begin ssd
    observe({
      start <- start_ssd()
      
      isolate({
        
        cStopVal <- getValueOfStopButton()
        if(is.null(stop_ssd()) || stop_ssd() != cStopVal) return()
        
        if(start){
          ssd_counter(0)
          start_ssd(F)
          
          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            20, 
            title = "Initialize first data sets")
          globalSSDProgressBar()$set(value=0.2, message = "Sample Size Determination", 
                                   detail = "Initialize first data sets")
            
          l <- start_ssd_data()
          
          compiledModel <- l$compiledModel
          maxN <- l$maxN
          minN <- cMCD$getLowestPossibleDataPoints()
          mcdSSD <- cMCD$getMcdSSD()
          goals <- mcdSSD$getGoals(forSSD=T)
          desPower <- mcdSSD$getPower()
          

          con <- mcdSSD$getAccuarcy()

          i_parallel <- numberOfIterationsPerStep
          
          compiled_model_data <- compiledModel$data
          

          #Init first dataset
          mcdResp <- cMCD$getMcdResponse()
          respType <- mcdResp$getType()
          N <- maxN
          initData <- list()
          seed <- round(runif(i_parallel,-1e7,1e7))
          for(i in seq_len(i_parallel)){
            tmpData <- cMCD$getRandomDataset(N, seed[i])
            initData[[i]] <- adjustDataForSSD(tmpData, compiled_model_data)
            #Verify dataset according to integers exceeding the maximum of (2**32-1)
            if(respType=="discrete" && any(initData[[i]][1] >((2**32)-1))){
              showNotification("Your response contains values that exceed the maximum integer value of 2^32-1.",
                               type="error", duration=NULL)
              updateProgressBar(
                session=session,
                id=ns("ssdProgressBar"),
                99, 
                title = "Cancelled")
              globalSSDProgressBar()$set(value=0.99, message = "Sample Size Determination", 
                                         detail = "Cancelled")
              close_ssd(T)
              return()
            }
          }
          

          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            30, 
            title = "Initialize sample size determination")
          globalSSDProgressBar()$set(value=0.3, message = "Sample Size Determination", 
                                   detail = "Initialize sample size determination")
          
          
          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            40, 
            title = "Sample first round of sample size determination")
          globalSSDProgressBar()$set(value=0.4, message = "Sample Size Determination", 
                                   detail = "Sample first round of sample size determination")
          
          
          ssd <- NULL
          try({
            possN <- minN:maxN

            ssd <- startSSDBayas(model = compiledModel, initData = initData, 
                                 powerDesired = desPower, possN = possN, 
                                 goals = goals, con = con, iParallel = i_parallel)
          })

          
          if(is.null(ssd)){
            showNotification("An error occurred during sampling. The operator is notified.",
                             type="error", duration=NULL)
            malfunction_report(code=malfunctionCode()$ssdSampling, msg="startSSD_NGoals ...",
                               type="error", askForReport=T)
            if(localUse) browser()
            updateProgressBar(
              session=session,
              id=ns("ssdProgressBar"),
              100, 
              title = "Cancelled")
            globalSSDProgressBar()$set(value=1, message = "Sample Size Determination", 
                                       detail = "Cancelled")
            close_ssd(T)
            return()
          }

          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            50, 
            title = "Validate first round of sample size determination")
          globalSSDProgressBar()$set(value=0.5, message = "Sample Size Determination", 
                                   detail = "Validate first round of sample size determination")
          
          ssd_g(ssd)
          
          if(ssd$extern$continue){
            continue_ssd(T)
          }else{
            continue_ssd(F)
          }
        }
      })
    })
    
    #plot ssd
    output[[ns("SSDInfoPlot")]] <- renderPlot({
      ssd <- ssd_g()
      maxN <- input[[ns("ssdMaxN")]]
      isolate({
        if(!is.null(ssd) && "bayesianssd" %in% class(ssd)){
          res <- ssd$intern$resultsSSD
          gg <- plotResults(ssd, theme=list(
            main=BAYAS_COLORS$`--modelCreatingPlot-color-values-5`, 
            current=BAYAS_COLORS$`--formula-color-1`, 
            default=BAYAS_COLORS$`--formula-color-5`, 
            mean=BAYAS_COLORS$`--creatingData-colors-1`))
          gg
        }else{
          tmpData <- data.frame(Power=c(0,1), N=c(2,maxN))
          gg <- ggplot(tmpData, aes(y=Power,x=N))
          gg
        }
      })
    })
    
    #summarize ssd
    observe({
      ssd <- ssd_g()
      isolate({
        if(!is.null(ssd)){
          
          bestCandidate <- NULL
          credibleInterval <- NULL
          resultsSSD <- ssd$intern$resultsSSD
          resultsSSDHigh <- resultsSSD[resultsSSD$tendency == 1,]
          if(dim(resultsSSDHigh)[1] > 0){
            bestCandidate <- min(resultsSSDHigh$N)
            mp <- ssd$intern$resultsPowerBinomial[ssd$intern$resultsPowerBinomial$N==bestCandidate,]
            mp_p <- mp$powerMean
            credibleInterval <- c(mp$powerLow, mp$powerHigh)
          } 
          
          #instead
          # text <- printSSD(ssd)
          
          text <- ""
          if(!is.null(bestCandidate)){
            text <- HTML(
              paste0("Potential 'N': <b>", bestCandidate, "</b><br> 90%-credible interval: (",
                     "<b>",formatC(credibleInterval[1], digits=3), "</b> - <b>", formatC(credibleInterval[2], digits=3) ,"</b>) <br>",
                     "Interval width: ", formatC(credibleInterval[2]-credibleInterval[1], digits=3), "<br>",
                     "Number of simulations for this N: ", resultsSSD$i[resultsSSD$N==bestCandidate]))
          }else if(ssd$intern$NMaxTooLow){
            potN <- ssd$extern$N
            if(!is.empty(potN) && is.numeric(potN)){
              if(potN > ssd$intern$maxN*100) potN <- paste0(">",ssd$intern$maxN*100)
            }else{
              potN <- "Unknown"
            }
            text <- HTML(paste0("Potential 'N' candidate: None; 'Max N' reached <br>",
                           "Estimated N greater 'Max N': ", potN))
          }else{
            text <- "Potential 'N' candidate: None"
          }
          
          #summary text
          output[[ns("ssdSummary")]] <- renderUI({
            tags$div(
              style="",
              text
            )
          })
          
          #summary table
          output[[ns("ssdSummaryTable")]] <- renderTable({
            ssd$intern$resultsSSD
          })
        }else{
          #summary text
          output[[ns("ssdSummary")]] <- renderUI({
            tags$div(
              style="",
              ""
            )
          })
          
          #summary table
          output[[ns("ssdSummaryTable")]] <- renderTable({
            NULL
          })
        }
      })
    })

    
    ssd_counter <- reactiveVal(0)
    ssd_d <- ssd_g %>% debounce(debounceTime11)
    
    #iterate ssd
    observe({
      ssd <- ssd_d()
      if(is.null(ssd)) return()
      isolate({

        cStopVal <- getValueOfStopButton()
        if(is.null(stop_ssd())) return()
        if(stop_ssd() != cStopVal){ 
          close_ssd(T)
          return()
        }
        
        
        if(continue_ssd()){
          ssd_counter(ssd_counter()+1)
          
          progressC <- min(ssd_counter(),48)
          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            50+progressC, 
            title = paste0("Sample ",ssd_counter()+1 , ". round of sample size determination"))
          globalSSDProgressBar()$set(value=0.5+(progressC/100), message = "Sample Size Determination", 
                                   detail = paste0("Sample ",ssd_counter()+1 , ". round of sample size determination"))
          
          N <- ssd$extern$N
          
          #check if next N is valid
          checkedN <- cMCD$checkSampleSize(N)
          if(!checkedN[[1]]){
            ssd <- getValidNextNBayas(nLow = checkedN[[2]][1], 
                                      nHigh = checkedN[[2]][2], 
                                      ssd)
          }
          
          compiled_model_data <- ssd$intern$model$data
          

          newSsd <- NULL
          rep <- 0
          repeatSampling <- T
          while(repeatSampling){
            try({
              #new data
              newData <- list()
              seed <- round(runif(ssd$intern$furtherArgs$iParallel,-1e7,1e7))
              for(i in seq_len(ssd$intern$furtherArgs$iParallel)){
                tmpData <- cMCD$getRandomDataset(N, seed[i])
                newData[[i]] <- adjustDataForSSD(tmpData, compiled_model_data)
              }
              
              ssd$intern$furtherArgs$modelSeed <- sample(1e5,ssd$intern$furtherArgs$iParallel)
              
              newSsd <- updateSSDBayas(ssd, newData)
            })
            rep <- rep +1
            if(rep > 4 || !is.null(newSsd)) repeatSampling <- F
          }
          
          
          if(is.null(ssd) || is.null(newSsd)){
            showNotification("An error occurred during sampling. The operator is notified.",
                             type="error", duration=NULL)
            malfunction_report(code=malfunctionCode()$ssdSampling, msg="updateSSDBayas ...",
                               type="error", askForReport=T)
            if(localUse) browser()
            # saveRDS(list(ssd=ssd, newData=newData), paste0(dirname(getwd()), "/SSD/ssd.rds"))
            
            updateProgressBar(
              session=session,
              id=ns("ssdProgressBar"),
              99, 
              title = "Cancelled")
            globalSSDProgressBar()$set(value=0.99, message = "Sample Size Determination", 
                                       detail = "Cancelled")
            close_ssd(T)
            return()
          }
          
          ssd <- newSsd
          
          ssd_g(ssd)
          if(!ssd$extern$continue){
            continue_ssd(F)
            # showNotification(ssd$extern$N, duration=NULL)
            close_ssd(T)
          } 
        }else if(!is.null(ssd)){
          close_ssd(T)
        }
      })
    })

    #ssd finished
    observe({
      close <- close_ssd()
      isolate({
        if(close){
          close_ssd(F)
          
          if(globalSSDProgressBar()$getValue()==1) {
            enable(ns("ssdStop"))
            hide(ns("ssdStop"))
            show(ns("ssdStart"))
            return()
          }
          
          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            99, 
            title = paste0("Close cluster"))
          
          globalSSDProgressBar()$set(value=0.99, message = "Sample Size Determination", 
                                   detail = "Close cluster")
          
          enable(ns("ssdStop"))
          hide(ns("ssdStop"))
          show(ns("ssdStart"))
          
          ssd <- ssd_g()
          

          updateProgressBar(
            session=session,
            id=ns("ssdProgressBar"),
            100, 
            title = "Finished")
          globalSSDProgressBar()$set(value=1, message = "Sample Size Determination", 
                                   detail = "Finished")
          globalSSDProgressBar()$close()
          
          #Write to mcdSSD
          cMCD$getMcdSSD()$setMcdOff()
          cMCD$getMcdSSD()$setSsdObject(ssd)
          
          #Is ssd continuable?
          #TODO
          cMCD$getMcdSSD()$setSsdContinue(T)
          enable(ns("ssdContinue"))
          
        }
      })
    })
        
    #get value of stop button
    getValueOfStopButton <- function(){
      val <- as.integer(input[[ns("ssdStop")]])
      return(val)
    }
    

    #shorten tree to only used parameters
    discardRedundantParametersInTree = function(tree){
      new_tree <- list()
      at <- attributes(tree)
      at$names <- NULL
      attributes(new_tree) <- at
      for(t_i in seq_len(length(tree))){
        t <- tree[[t_i]]
        redundant <- attr(t,"bayasRedundant")
        if(is.logical(redundant) && redundant){
          # tree[[t_i]] <- NULL
        }else if(is.list(t)){
          new_tree <- list.append(list=new_tree, 
                                  element= discardRedundantParametersInTree(t), 
                                  name=names(tree)[[t_i]])
        }else{
          new_tree <- list.append(new_tree, tree[[t_i]], names(tree)[[t_i]])
        }
      }
      return(new_tree)
    }
    
    #Select tree elements 
    #sel: From goal$getParametersA(); goal$getParametersB() passed to rearrangeGoalParameter
    selectParametersInTree = function(tree, sel){
      if(!is.empty(sel)){
        for(t_i in seq_len(length(tree))){
          t <- tree[[t_i]]
          paraId <- attr(t, "bayasParaId")
          subName <- attr(t, "bayasSubName")
          if(is.list(t)){
            tree[[t_i]] <- selectParametersInTree(t, sel)
            if(paraId %in% sel$paraId &&
               is.null(subName) &&
               "" %in% sel$subName[sel$paraId == paraId])
              attr(tree[[t_i]], "stselected") <- T
          }else{
            if(paraId %in% sel$paraId){
              if(subName %in% sel$subName[sel$paraId == paraId])
                attr(tree[[t_i]], "stselected") <- T
            }
          }
        }
      }
      return(tree)
    }

    
    ##########################################
    ############### SSD modal  ############### 
    {
      
      #Tree A
      observeEvent(input[[ns("ssdModalTreeA")]],{
        goal <- goalModal()
        
        treeA <- input[[ns("ssdModalTreeA")]]
        subsA <- cMCD$getParameterSubByTree(treeA)
        
        rm <- c()
        
        subParaIdsA <- list()
        for(a_i in seq_len(length(subsA))){
          a <- subsA[[a_i]]
          if("ModelCreatingDataParameterSub" %in% class(a)){
            if(names(subsA)[a_i] != "") rm <- c(rm, a$getParameter()$getId())
            subParaIdsA <- list.append(subParaIdsA, 
                                       list(paraId=a$getParameter()$getId(), 
                                            subName=names(subsA)[a_i]))
          }else if("ModelCreatingDataParameter" %in% class(a)){
            subParaIdsA <- list.append(subParaIdsA,
                                       list(paraId=a$getId(),
                                            subName=""))
          }
        }
        
        subParaIdsANew <- list()
        for(ss in subParaIdsA){
          if(!(ss$subName == "" && ss$paraId %in% rm)){
            subParaIdsANew <- list.append(subParaIdsANew, ss)
          }
        }
        subParaIdsA <- subParaIdsANew
        
        goal$setParametersA(subParaIdsA, trigger=F)
        goalModal(goal)
      })
      
      #Tree B
      observeEvent(input[[ns("ssdModalTreeB")]],{
        goal <- goalModal()
        
        treeB <- input[[ns("ssdModalTreeB")]]
        subsB <- cMCD$getParameterSubByTree(treeB)
        
        rm <- c()
        
        subParaIdsB <- list()
        for(b_i in seq_len(length(subsB))){
          b <- subsB[[b_i]]
          if("ModelCreatingDataParameterSub" %in% class(b)){
            if(names(subsB)[b_i] != "") rm <- c(rm, b$getParameter()$getId())
            subParaIdsB <- list.append(subParaIdsB, 
                                       list(paraId=b$getParameter()$getId(), 
                                            subName=names(subsB)[b_i]))
          }else if("ModelCreatingDataParameter" %in% class(b)){
            subParaIdsB <- list.append(subParaIdsB,
                                       list(paraId=b$getId(),
                                            subName=""))
          }
        }
        
        subParaIdsBNew <- list()
        for(ss in subParaIdsB){
          if(!(ss$subName == "" && ss$paraId %in% rm)){
            subParaIdsBNew <- list.append(subParaIdsBNew, ss)
          }
        }
        subParaIdsB <- subParaIdsBNew
        
        goal$setParametersB(subParaIdsB, trigger=F)
        goalModal(goal)
      })

      #Name
      observeEvent(input[[ns("ssdModalGoalName")]], {
        #check if name is already in use
        mcdSSD <- cMCD$getMcdSSD()
        goal <- goalModal()
        
        shinyFeedback::hideFeedback(ns("ssdModalGoalName"))
        
        if(!is.null(goal)){
          name <- input[[ns("ssdModalGoalName")]]
          valid <- otherVariableNameIsValid(name, duplicate=F)
          if(valid$valid){
            if(mcdSSD$isGoalNameInUse(name, ignore=goal$getId())){
              valid <- list(valid=F, msg="This name is already in use.")
            }
          }
          if(!(is.null(name) || name == "" || valid$valid)){
            shinyFeedback::showFeedbackDanger(ns("ssdModalGoalName"),
                                              text=valid$msg, 
                                              color=BAYAS_COLORS$`--font-error`, 
                                              icon=NULL) 
            return()
          }
          
          goal$setName(name, trigger=F)
          goalModal(goal)
        }
      })
      
      #Probability mass
      observeEvent(input[[ns("ssdHDI")]], {
        densityMass <- input[[ns("ssdHDI")]]
        goal <- goalModal()
        goal$setHDI(densityMass, trigger=F)
        goalModal(goal)
      })
      
      #Rope / Precision
      observeEvent(input[[ns("ssdSwitchRopePrecision")]], {
        type <- input[[ns("ssdSwitchRopePrecision")]]
        if(type=="rope"){
          show(ns("ropeDiv"))
          hide(ns("precisionDiv"))
        }else{
          hide(ns("ropeDiv"))
          show(ns("precisionDiv"))
        }
        goal <- goalModal()
        goal$setType(type, trigger=F)
        goalModal(goal)
      })
      
      #Rope in-/exclude
      observeEvent(input[[ns("ssdRadioExlude")]], {
        type <- input[[ns("ssdRadioExlude")]]
        goal <- goalModal()
        goal$setRopeExcludeInclude(type)
        goalModal(goal)
      })
      
      #Rope lower
      observeEvent(input[[ns("ssdRopeLower")]], {
        ropeLower <- input[[ns("ssdRopeLower")]]
        goal <- goalModal()
        goal$setRopeLower(ropeLower)
        goalModal(goal)
      })

      #Rope upper
      observeEvent(input[[ns("ssdRopeUpper")]], {
        ropeUpper <- input[[ns("ssdRopeUpper")]]
        goal <- goalModal()
        goal$setRopeUpper(ropeUpper)
        goalModal(goal)
      })

      
      #Precision width
      observeEvent(input[[ns("ssdPrecWidth")]], {
        precWidth <- input[[ns("ssdPrecWidth")]]
        goal <- goalModal()
        goal$setPrecWidth(precWidth)
        goalModal(goal)
      })
      
      
      updateGoalPlot <- reactive({
        #Trees
        treeA <- input[[ns("ssdModalTreeA")]]
        treeB <- input[[ns("ssdModalTreeB")]]
        selA <- shinyTree::get_selected(treeA)
        selB <- shinyTree::get_selected(treeB)
        
        #Goal type
        goalType <- input[[ns("ssdSwitchRopePrecision")]]
        
        #Probability mass
        densityMass <- input[[ns("ssdHDI")]]
        
        #Rope ex-/include
        ropeInEx <- input[[ns("ssdRadioExlude")]]
        
        #Rope lower
        ropeLower <- input[[ns("ssdRopeLower")]]
        
        #Rope upper
        ropeUpper <- input[[ns("ssdRopeUpper")]]
        
        #Precision width
        precWidth <- input[[ns("ssdPrecWidth")]]
        
        list(selA=selA, selB=selB, goalType=goalType, densityMass=densityMass,
             ropeInEx=ropeInEx, ropeLower=ropeLower, ropeUpper=ropeUpper,
             precWidth=precWidth)
      })
      updateGoalPlot_d <- debounce(updateGoalPlot, debounceTime12)
      
      #plot
      output[[ns("ssdModalPlot")]] <- renderPlot({
        ret <- updateGoalPlot_d()
        goal <- goalModal()
        goal$getPlot()
      })
      
      
      #Confirm
      observeEvent(input[[ns("confirmModalSSD")]], {
        mcdSSD <- cMCD$getMcdSSD()
        
        goal <- goalModal()
       
        #Check if selected paras a valid
        if(!goal$getIsValid()){
          showNotification(goal$getInvalidMsg(), type="error")
          return()
        }
        
        removeModal()
        
        goalM <- goal
        l <- goal$getRopeLower()
        u <- goal$getRopeUpper()
        goal$setRopeLower(min(u,l), trigger=F)
        goal$setRopeUpper(max(u,l), trigger=F)
        goal <- mcdSSD$getGoal(selGoalId())
        goal$setGoal(goalM)
      })
      
      #Cancel modal
      observeEvent(input[[ns("cancelModalSSD")]], {
        removeModal()
      })
      
    }
  }
  
  
  
  #adjust data for ssd according to model formula
  adjustDataForSSD <- function(newData, modelData){
    cname <- colnames(modelData)
    for(col in cname){
      newColName <- col
      newColVal <- rep(1, length(newData[[1]]))
      
      if(grepl("\\.", col)){
        
        splitCol <- str_split(col, "\\.\\.")[[1]]
        predNames <- str_split(splitCol[1], "\\.")[[1]]
        attrNames <- str_split(splitCol[2], "\\.")[[1]]
        
        for(p_i in seq_along(predNames)){
          pp <- predNames[[p_i]]
          attr <- attrNames[[p_i]]
          oV <- cMCD$getOtherVariableByName(pp)
          if(!is.null(oV)){
            if(!is.null(oV$getType()) && oV$getType() == "categorical"){
              newColVal[newData[[pp]] != attr] <- 0
            }
          }else{
            if(localUse) browser()
            return(NULL)
          }
        }
        
      }

      
      if(newColName != "" && 
         !newColName %in% colnames(newData)){
        newData[[newColName]] <- newColVal
      }
      
      if(is.numeric(modelData[[col]]))
        newData[[newColName]] <- as.numeric(newData[[newColName]])
    }
    return(newData)
  }
  
  
  
  ##############################################################################
  ############################# Data visualization #############################
  ##############################################################################
  
  {
    oldData <- reactiveVal(NULL)
    observe({
      cMCD$getReactive("data")
      isolate({
        
        data <- cMCD$getData()
        tmpOldData <- oldData()
        #No changes?
        if(!dataEqual(data, tmpOldData)){
          oldData(data)
          
          #Update plot
          visualizeDataPlot()
        }
      })
    })
    
    #Update choices of Y/X-axis/Group by inputs
    observe({
      cMCD$getReactive("responseName")
      cMCD$getReactive("otherVariable")
      cMCD$getReactive("otherVariableName")

      isolate({
        oVs <- cMCD$getOtherVariables()
        xChoices <- list("First"="")
        yChoices <- list("Second"="")
        gBChoices <- list("Group by"="")
        resp <- cMCD$getMcdResponse()
        if(!is.null(resp) && !is.null(resp$getDist())){
          na <- resp$getName()
          if(na=="") na <- "Response"
          xChoices <- list.append(xChoices, 0, na)
          yChoices <- list.append(yChoices, 0, na)          
        }
        
        for(o in oVs){
          xChoices <- list.append(xChoices, o$getId(), o$getName(), extendBySameName = T)
          yChoices <- list.append(yChoices, o$getId(), o$getName(), extendBySameName = T)
          type <- o$getType()
          if(!is.null(type) && type=="categorical"){
            gBChoices <- list.append(gBChoices, o$getId(), o$getName())
          }
        }
        
        visualizeData <- cMCD$getVisualizeData(type=NULL)
        updateSelectizeInput(session, ns("dataVisualizationSelectY"), 
                             choices = yChoices,
                             selected = visualizeData$yAxis)
        updateSelectizeInput(session, ns("dataVisualizationSelectX"), 
                             choices = xChoices,
                             selected = visualizeData$xAxis)
        updateSelectizeInput(session, ns("dataVisualizationSelectGroupBy"), 
                             choices = gBChoices,
                             selected = visualizeData$groupBy)

      })
    })
    
    
    #Update Y/X-axis/Group by inputs
    #Update plot
    observe({
      cMCD$getReactive("visualizeData")
      isolate({
        #Update ui
        oVs <- cMCD$getOtherVariables()
        xChoices <- list("First"="")
        yChoices <- list("Second"="")
        gBChoices <- list("Group by"="")
        resp <- cMCD$getMcdResponse()
        if(!is.null(resp) && !is.null(resp$getDist())){
          na <- resp$getName()
          if(na=="") na <- "Response"
          xChoices <- list.append(xChoices, 0, na, extendBySameName = T)
          yChoices <- list.append(yChoices, 0, na, extendBySameName = T)
        }
        
        for(o in oVs){
          xChoices <- list.append(xChoices, o$getId(), o$getName(), extendBySameName = T)
          yChoices <- list.append(yChoices, o$getId(), o$getName(), extendBySameName = T)
          type <- o$getType()
          if(!is.null(type) && type=="categorical"){
            gBChoices <- list.append(gBChoices, o$getId(), o$getName())
          }
        }
        
        visualizeData <- cMCD$getVisualizeData(type=NULL)
        updateSelectizeInput(session, ns("dataVisualizationSelectY"), 
                             choices = yChoices,
                             selected = visualizeData$yAxis)
        updateSelectizeInput(session, ns("dataVisualizationSelectX"), 
                             choices = xChoices,
                             selected = visualizeData$xAxis)
        updateSelectizeInput(session, ns("dataVisualizationSelectGroupBy"), 
                             choices = gBChoices,
                             selected = visualizeData$groupBy)
        updateSelectizeInput(session, ns("dataVisualizationSelectPlotType"), 
                             selected = visualizeData$plotType)
        
        
        #Update plot
        visualizeDataPlot()
        
      })
    })
    
    #Observe plot inputs
    observeEvent(input[[ns("dataVisualizationSelectY")]], {
      cMCD$setVisualizeData(input[[ns("dataVisualizationSelectY")]], type="yAxis")
    })
    observeEvent(input[[ns("dataVisualizationSelectX")]], ignoreNULL = F,{
      cMCD$setVisualizeData(input[[ns("dataVisualizationSelectX")]], type="xAxis")
    })
    observeEvent(input[[ns("dataVisualizationSelectGroupBy")]], ignoreNULL = F, {
      cMCD$setVisualizeData(input[[ns("dataVisualizationSelectGroupBy")]], type="groupBy")
    })
    observeEvent(input[[ns("dataVisualizationSelectPlotType")]], {
      cMCD$setVisualizeData(input[[ns("dataVisualizationSelectPlotType")]], type="plotType")
    })
    
    
    visualizeDataPlot = function(){
      data <- cMCD$getData()
      vData <- cMCD$getVisualizeData(type=NULL)
      
      yOv <- NULL
      y <- ""
      x <- ""
      groupBy <- c()
      
      if(!is.null(vData$yAxis) && !is.empty(vData$yAxis)){
        if(vData$yAxis==0){
          resp <- cMCD$getMcdResponse()
          y <- resp$getName()
          if(y=="") y <- "Response"
        }else{
          ov <- cMCD$getOtherVariable(vData$yAxis)
          if(!is.null(ov)){
            y <- ov$getName()
            yOv <- ov
          }
        }
      }
      if(!is.null(vData$xAxis) && !is.empty(vData$xAxis)){
        if(vData$xAxis==0){
          resp <- cMCD$getMcdResponse()
          x <- resp$getName()
        }else{
          ov <- cMCD$getOtherVariable(vData$xAxis)
          if(!is.null(ov))
            x <- ov$getName()
        }
      }
      if(y==""){
        output[[ns("dataVisualization")]] <- renderPlot(ggplot())
        return()
      }
      if(!is.null(vData$groupBy)){
        for(id in vData$groupBy){
          ov <- cMCD$getOtherVariable(id)
          if(!is.null(ov))
            groupBy <- c(groupBy, ov$getName())
        }
      }

      gg <- ggplot()
            
      if(vData$plotType == "Points"){
        if(x==""){
          gg <- ggplot(data, aes(y=.data[[y]], x=""))
          gg <- gg + geom_point()
        }else{
          gg <- ggplot(data, aes(x=.data[[x]],y=.data[[y]]))
          gg <- gg + geom_point()
        }
      }else if(vData$plotType == "Histogram"){
        
        statCount <- F 
        
        if(!is.null(yOv)){
          if(yOv$isNumeric()){
            nums <- as.numeric(data[[y]])
            if(all(nums == round(nums))) statCount <- T
          }else{
            statCount <- T
          }
        }else{
          if(all(is.na(data[[y]]))) statCount <- T
        }
          
        gg <- ggplot(data, aes(x=.data[[y]]))
        if(statCount){
          gg <- gg + geom_histogram(stat="count")
        }else{
          gg <- gg + geom_histogram()
        }
      }else if(vData$plotType == "Density"){
        gg <- ggplot(data, aes(x=.data[[y]]))
        gg <- gg + geom_density()
      }

      if(!is.empty(groupBy)){
        gg <- gg + facet_wrap(~interaction(.data[[groupBy]]))
      }
      
      tryCatch({
        gg
      },
      error=function(e){
        if(localUse) browser()
        malfunction_report(code=malfunctionCode()$ggplot, msg="ServerCreateModel: visualizeDataPlot() call",
                           type="error", askForReport=T)
      })
      
      output[[ns("dataVisualization")]] <- renderPlot(gg)
    }
 
  }
  
  
  
  
  
  
  

  
  ##############################################################################
  ################################### Formula ##################################
  ##############################################################################
  {

  triggerSpecialOtherVariables <- reactiveVal(T)
  observe({
    cMCD$getReactive("otherVariableName")
    isolate({
      lastoV <- cMCD$getLastOtherVariableId()
      oV <- cMCD$getOtherVariable(lastoV)
      if(!is.null(oV) && !is.null(oV$getSpecialRole())){
        triggerSpecialOtherVariables(!triggerSpecialOtherVariables())
      }
    })
  })
    
  updateFormula <- reactive({
    name <- cMCD$getReactive("responseName")
    dist <- cMCD$getReactive("responseDist")
    link <- cMCD$getReactive("responseLink")
    para <- cMCD$getReactive("parameter")
    para <- cMCD$getReactive("predictor")
    triggerSpecialOtherVariables()
    list(name=name, dist=dist, link=link, para=para)
  })
  observe({
    res <- debounce(updateFormula,debounceTime13)
    res()
    isolate({
      mcdFormula <- cMCD$getMcdFormula()
      mcdFormula$doTrigger()
    })
  })

  ##Formula
  observe({
    cMCD$getReactive("formula")
    isolate({
      cMCD$getMcdFormula()$updateFormula()
    })
  })
  
  ##Switch between generative and prior 
  observeEvent(input[[ns("formulaSwitchPriorGenerative")]], {
    inp <- input[[ns("formulaSwitchPriorGenerative")]]
    if(!is.null(inp)){
      if(inp == "Generative"){
        cMCD$getMcdFormula()$setMode("Generative")
      }else{
        cMCD$getMcdFormula()$setMode("Prior")
      }
    }
  })
  
  ##Switch between single/multiple predictor line
  observeEvent(input[[ns("formulaSwitchNextLine")]], {
    inp <- input[[ns("formulaSwitchNextLine")]]
    if(!is.null(inp)){
      formula <- cMCD$getMcdFormula()
      if(!is.null(formula)){
        if(inp == "Single"){
          formula$setPredictorLineType("Single")
        }else{
          formula$setPredictorLineType("Multiple")
        }
      }
    }
  })
  
  
  #Update stat. model ui 
  observe({
    cMCD$getMcdFormula()$getReactive("ui")
    isolate({
    
      type <- cMCD$getMcdFormula()$getPredictorLineType()
      mode <- cMCD$getMcdFormula()$getMode()
      
      updateBayasGroupedButtons(session, ns("formulaSwitchNextLine"), selected=type)
      updateBayasGroupedButtons(session, ns("formulaSwitchPriorGenerative"), selected=mode)
      
    })
  })
  
  }


}



##############################################################################
################################### Modules ##################################
##############################################################################


#Only for report items, not for the modal opened by clicking "Report experiment"
planning_reportExperimentServer <- function(id, item) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input[["switchGeneratedData"]], {
        indData <- item$getIndividualData()
        indData$preSel$includeGenData <- input[["switchGeneratedData"]]
        if(input[["switchGeneratedData"]]){
          enable("switchGeneratedDataMinimal")
        }else{
          disable("switchGeneratedDataMinimal")
        }
        item$setIndividualData(indData)
      })
      
      observeEvent(input[["switchSSD"]], {
        indData <- item$getIndividualData()
        indData$preSel$includeSSD <- input[["switchSSD"]]
        if(input[["switchSSD"]]){
          enable("switchSSDResult")
          enable("switchSSDOnlyUsedGoals")
          
          mcdSSD <- cMCD$getMcdSSD()
          ssdObject <- mcdSSD$getSsdObject()
          
          if(!is.null(ssdObject) && !mcdSSD$isMcdOffSame() && input[["switchSSDResult"]]){
            enable("switchSSDOnlyMcdOff")
          }
          
        }else{
          disable("switchSSDResult")
          disable("switchSSDOnlyUsedGoals")
          disable("switchSSDOnlyMcdOff")
        }
        item$setIndividualData(indData)
      })
      
      observeEvent(input[["switchSSDResult"]], {
        indData <- item$getIndividualData()
        indData$preSel$includeSSDResult <- input[["switchSSDResult"]]
        if(input[["switchSSDResult"]]){
          mcdSSD <- cMCD$getMcdSSD()
          ssdObject <- mcdSSD$getSsdObject()
          if(!is.null(ssdObject) && !mcdSSD$isMcdOffSame()){
            enable("switchSSDOnlyMcdOff")
          }
        }else{
          disable("switchSSDOnlyMcdOff")
        }
        item$setIndividualData(indData)
      })
      
      observeEvent(input[["switchSSDOnlyMcdOff"]], {
        indData <- item$getIndividualData()
        indData$preSel$onlySSD <- input[["switchSSDOnlyMcdOff"]]
        item$setIndividualData(indData)
      })
      observeEvent(input[["switchOVOnlyUsed"]], {
        indData <- item$getIndividualData()
        indData$preSel$onlyUsedVars <- input[["switchOVOnlyUsed"]]
        item$setIndividualData(indData)
      })
      observeEvent(input[["switchGeneratedDataMinimal"]], {
        indData <- item$getIndividualData()
        indData$preSel$replaceMinData <- input[["switchGeneratedDataMinimal"]]
        item$setIndividualData(indData)
      })
      observeEvent(input[["switchSSDOnlyUsedGoals"]], {
        indData <- item$getIndividualData()
        indData$preSel$onlyUsedGoals <- input[["switchSSDOnlyUsedGoals"]]
        item$setIndividualData(indData)
      })
    }
  )
}

##############################################################################
################################### Helper ###################################
##############################################################################
{
##Returns for subgroup variables a named list.
#Names: Elements of super groups devided by ":"
#Value: element name for subgroup
nested_list_for_cat_subgroups <<- function(l, new_list=list(), el_name=NULL){
  if(is.list(l)){
    for(el in seq_len(length(l))){
      new_name <- paste0(el_name,":",names(l)[[el]])
      if(is.null(el_name)) new_name <- names(l)[[el]]
      new_list <- nested_list_for_cat_subgroups(l[[el]], new_list, new_name)
    }
  }else{
    val <- l
    l <- paste0(el_name,":",l)
    names(l) <- val
    new_list <- list.append(new_list, l, el_name)
  }
  return(new_list)
}
# values <- list(female=list(small=c("a","b"),medium=c("a","b"),great=c("a","b")),
#                male=list(small=c("a","b"),medium=c("a","b"),great=c("a","b")))
# 
# nested_list_for_cat_subgroups(values)


#Check whether a variable element contains invalid characters e.g. ":"
#returns a 2 element list. list(valid=c(T,F), msg="Invalid msg")
otherVariableElementIsValid <- function(name){
  if(length(name) > 1) {
    warning("otherVariableElementIsValid takes no vectors. Returns false")
    return(list(valid=F, msg="Invalid due to unknwon reason."))
  }
  if(grepl(":",name)) return(list(valid=F, msg="':' are not allowed."))
  return(list(valid=T,msg=""))
}


#Check whether a variable name is invalid (containing e.g. ":", starts with a number)
#returns a 2 element list. list(valid=c(T,F), msg="Invalid msg")
otherVariableNameIsValid <- function(name, resp=F, duplicate=F, ovId=NULL, cMCD=NULL){
  if(is.null(name)) return(list(valid=F, msg="No name"))
  if(length(name) > 1) {
    warning("otherVariableNameIsValid takes no vectors. Returns false")
    return(list(valid=F, msg="Invalid due to unknwon reason."))
  }
  p1 <- grepl(":",name)
  p2 <- grepl("^[a-zöäüA-ZÖÄÜß_-]",name)
  p4 <- name %in% c("N")
  p5 <- grepl("^[a-zöäüA-ZÖÄÜß0-9[:blank:]_-]*$",name)
  
  valid <- T
  msg <- ""
  if(!p5){
    valid <- F
    msg <- "Only letters, numbers, '_' and '-' are allowed."
  }

  if(!p2 && p5){
    valid <- F
    msg <- paste0(msg, "Must start with a letter, '_' or '-'")
  }

  if(!valid){
    return(list(valid=valid,msg=msg))
  }
  
  #Additional check for response var
  if(resp){
    if(name=="_tmp"){
      valid <- F
      msg <- "'_tmp' is not allowed. "
    }
  }
  #Additional check for other variable
  if(!resp){
    p1 <- "Response" == name
    
    if(p1){
      valid <- F
      msg <- "'Response' is not allowed for other variables. "
    }
  }
  
  #If name is a duplicate
  if(duplicate){
    respName <- cMCD$getMcdResponse()
    if(!is.null(respName)) respName <- respName$getName()

    if(!resp && !is.null(respName) && respName == name){
      valid <- F
      msg <- "This name is already in use."
      return(list(valid=valid,msg=msg))
    }
    
    for(ov in cMCD$getOtherVariables()){
      if(is.null(ovId) || ov$getId() != ovId){
        if(ov$getName() == name){
          valid <- F
          msg <- "This name is already in use."
          return(list(valid=valid,msg=msg))
        }
      }
    }
    
  }
  
  return(list(valid=valid,msg=msg))
}


dataEqual = function(d1,d2){
  if(is.null(d1) && is.null(d2)) return(T)
  if(is.null(d1) || is.null(d2)) return(F)

  if(is.empty(d1) && is.empty(d2)) return(T)
  if(is.empty(d1) || is.empty(d2)) return(F)
  
  dim1 <- dim(d1)
  dim2 <- dim(d2)
  if(dim1[1] != dim2[1] || dim1[2] != dim2[2]) return(F)
  
  c1 <- colnames(d1)
  c2 <- colnames(d2)
   
  if(!vectorEqual(c1,c2)) return(F)

  for(col in seq_len(dim1[2])){
    if(!vectorEqual(d1[[col]],d2[[col]])) return(F)
  }
  return(T)
}


}
