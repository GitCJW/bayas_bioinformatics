
init_run_model_function <- function(input, output, session, dataModel, 
                                    global_reportProgressModel){
  
  # ??
  firstAnimation <<- reactiveVal(T)
  
  tmpReportItemValidation <<- NULL
  
  #init empty outputs to disable the loading spinner
  #(The order seems important (for whatever reason), otherwise the spinner don't vanish.)
  output$runModelVariableSummaryTable <- renderTable(NULL)
  output$runModelVariableSummaryPlot <- renderPlot(ggplot())
  output$runModelPlotPPC <- renderPlot(ggplot())
  output$runModelVerbalResult <- renderUI(tags$div())


  # Observe the input name for a model. 
  # Gives a warning, if a model name is already taken, and if the model name is invalid e.g. "", "(Not_fitted)"
  modelNameWarning <- reactiveVal(F)
  observeEvent(input$runModelFitName, {
    checkFitName()
  })
  
  checkFitName = function(){
    models <- dataModel$get.perIterationDataModels()
    model_names = NULL
    if(length(models) != 0) model_names <- sapply(1:length(models), function(i) models[[i]]$get.name())
    if(input$runModelFitName %in% model_names){
      shinyFeedback::showFeedbackDanger("runModelFitName",
                                        text="This model name is already in use", 
                                        color=BAYAS_COLORS$`--font-error`, icon=NULL)
      
      modelNameWarning(T)
    }else if(input$runModelFitName == "" || input$runModelFitName == "(Not_fitted)"){
      shinyFeedback::showFeedbackDanger("runModelFitName",
                                        text="This model name is invalid", 
                                        color=BAYAS_COLORS$`--font-error`, icon=NULL)
      modelNameWarning(T)
    }else{
      shinyFeedback::hideFeedback("runModelFitName")
      modelNameWarning(F)
      return(T)
    }
    return(F)
  }
  
  
  #set seed?
  observeEvent(input$samplingParametersSeedCheck, {
    if(input$samplingParametersSeedCheck){ 
      shinyjs::enable("runModelSeed")
    }else{
      shinyjs::disable("runModelSeed")
    }
  })
  

  label_of_fit_run <<- "Run Fit"
  observeEvent(input$btnRunModelRun, ignoreInit = T, {
    firstAnimation(T)
    
    cPIDM <- dataModel$get.cPerIterationDataModel()
    
    #Sampling parameters set?
    if(!cPIDM$parameters.setted()){
      showNotification("You have some invalid sampling parameters setted.", type="error")
      return()
    }
    
    if(is.null(cPIDM$get.selected_BAYSIS_stan_model())){
      showNotification("Please select a model before run fit.", type = "error")
      return()
    }
    if(modelNameWarning()){
      showNotification("Please select valid model name.", type = "error")
      return()
    }
    #If e.g. mu is empty
    if(!cPIDM$get.selected_BAYSIS_stan_model()$isStanModelRunnable()){
      showNotification("Your model has neither a predictor nor an intercept.", type = "error")
      return()
    }
    
    #If fit name is already in use?
    if(!checkFitName()){
      showNotification("Your fit name is already in use.")
      return()
    }
    
    
    
    if(label_of_fit_run == "Run Fit"){
      label_of_fit_run <<- "Cancel Fit"
      shinyjs::disable(id = "btnRunModelRun")
    }else{
      return()
    }
    
    
    # Update textinput of model name, to prevent warning of double naming
    name <- input$runModelFitName
    t_new <- name
    
    for(i in 0:nchar(name)){
      suppressWarnings(num <- as.numeric(substr(name,nchar(name)-i, nchar(name))))
      if(is.na(num)){
        if(i == 0){ 
          t_new <- paste0(name,0)
        }else{
          num <- as.numeric(substr(name,nchar(name)-(i-1), nchar(name)))
          t_new <- paste0(substr(name,1, nchar(name)-i),num+1)
        }
        break
      }
    }
    
    updateTextInput(session = session, inputId = "runModelFitName", value = t_new)
    

    dMID <- dataModel$getDataModelInputData()

    response <- dMID$getResponseVariable(onlyName=T)

    #Get seed
    seed <- NULL
    if(input$samplingParametersSeedCheck){
      if(!is.null(input$runModelSeed)){
        seed <- input$runModelSeed
        cPIDM$set.seedByUser(T)
      }else{
        showNotification("No valid seed setted. Use random seed.", type="warning")
      }
    }
    if(is.null(seed)){
      cPIDM$set.seedByUser(F)
      withr::with_seed(as.numeric(Sys.time()), {
        seed <- round(runif(1,0,100000))
      })
    }
    if(is.null(seed) && localUse) browser() #withr::with_seed.. not working as expected
    
    #set seed to PIDM
    cPIDM$set.seed(seed)
    
    # Read sampling parameters
    stanParameters <- list(
      iterations = cPIDM$get.number_iterations(),
      chains = cPIDM$get.number_chains(),
      cores = cPIDM$get.number_cores(),
      adapt_delta = cPIDM$get.adapt_delta(),
      max_treedepth = cPIDM$get.max_treedepth(),
      seed = seed
    )

 
    # Redirect output to a file
    console_path <- paste0(dirname(getwd()),"/Console_files/")
    if(!dir.exists(console_path)) dir.create(console_path)
    consoleFile <- paste0(console_path,".console", session$token ,".txt")
    if(!file.exists(consoleFile)) file.create(consoleFile)
    write("",consoleFile, append=F)
    

    consoleFile2 <- paste0(console_path,".console", session$token ,"_2.txt")
    if(!file.exists(consoleFile2)) file.create(consoleFile2)
    write("",consoleFile2, append=F)
    
    consoleFile3 <- paste0(console_path,".console", session$token ,"_3.txt")
    if(!file.exists(consoleFile3)) file.create(consoleFile3)
    write("",consoleFile3, append=F)
    
    
    baysisModel <- cPIDM$get.selected_BAYSIS_stan_model()
    usedVars <- unique(c(baysisModel$get_used_vars(extras=T, response=T)))
    data <- dMID$getLongFormatVariable(usedVars, completeCases=T)
    
    progressBar <- Progress$new(min=0,max=1)
    progressBar$set(value=0, message = 'Prepare sampling ...', detail= "This may take a while")
    
    content <- baysisModel$get_content_for_stan_code(response = response, 
                                                     data = data, 
                                                     stanParameter = stanParameters)

    
    if(is.null(content)){
      label_of_fit_run <<- "Run Fit"
      shinyjs::enable(id = "btnRunModelRun")
      updateTextInput(session = session, inputId = "runModelFitName", value = name)
      progressBar$close()
      return()
    }
    
    
    f <- future({
      sink(consoleFile)
      
      #Calling this directly from the baysis models leads to memory issues,
      #since the baysis object must be loaded in the future.
      #It is also not a solution to save the function call (from the baysis model 'get_code_to_run')
      #in a variable and pass into the future, because the function call variable also increase in the
      #used memory during several fits 
      tC <- tryCatch({
        #TOOD
        #Somehow the output passed to the viewer is not redirected to the consoleFile when using more than 1 core,
        #so that the progressbar doesn't make sense.
        if(!is.null(content$class) && content$class=="brms"){
          sampled_model <- brms::brm(formula = content$formula, data=content$data, verbose = T,
                                     iter = content$stanParameter$iterations, cores = 1, #content$stanParameter$cores, 
                                     control=list(adapt_delta = content$stanParameter$adapt_delta, 
                                                  max_treedepth=content$stanParameter$max_treedepth),
                                     chains = content$stanParameter$chains, family = content$family,
                                     prior=content$prior, 
                                     seed=content$stanParameter$seed)
        }else{
          sampled_model <- rstanarm::stan_glm(formula = content$formula, data=content$data, verbose = T, show_messages = T, open_progress=F,
                                              iter = content$stanParameter$iterations, adapt_delta = content$stanParameter$adapt_delta, cores = 1, #content$stanParameter$cores, 
                                              control=list(max_treedepth=content$stanParameter$max_treedepth),
                                              chains = content$stanParameter$chains, family = content$family,
                                              prior=content$prior, prior_aux=content$prior_aux, prior_intercept=content$prior_intercept,
                                              seed=content$stanParameter$seed)
        }
        T
      },
      error=function(cond){
        print(cond)
        return(F)
      })
      
      sink()
      writeLines("@@@FINISHED@@@",  consoleFile2)
      if(!tC) return(NULL)
      sampled_model
    }, seed=T,  packages = c("rstan","rstanarm"),
    globals = list(consoleFile=consoleFile, consoleFile2=consoleFile2,
                   content=content))
    
    prepareTask(progressBar, consoleFile, consoleFile2, consoleFile3, stanParameters)


    progressBar$set(value=0.9, message = 'Allocate memory ...', detail= "This may take a while")
    ret <- value(f) 
    if(is.null(ret)){
	    if(localUse) browser()
      showNotification("Couldn't fit the model for unknown reasons. The operator is notified.", type="error")
      malfunction_report(code=malfunctionCode()$compareStanModels, msg="unused interaction levels",
                         type="error", askForReport=T)
      
      progressBar$set(value=1)
      progressBar$close()
      label_of_fit_run <<- "Run Fit"
      shinyjs::enable(id = "btnRunModelRun")
      updateTextInput(session = session, inputId = "runModelFitName", value = name)
      return()
    }
    ret <- baysisModel$postprocessing(ret, content)
    sampled_model <- ret[[1]]
    progressBar$set(value=1)
    progressBar$close()
    
    write("", consoleFile, append=F)


    # Validate fitted model and save to iteration data model (and dataModel)
    if(!is.null(sampled_model)){
      cPIDM$set.calculated_stan_object(sampled_model)
      cPIDM$set.time_of_run_fit(Sys.time())
    }else{
      label_of_fit_run <<- "Run Fit"
      shinyjs::enable(id = "btnRunModelRun")
      updateTextInput(session = session, inputId = "runModelFitName", value = name)
      return()
    }

    
    # Save model to datamodel and update selectinput
    cPIDM$setDataModelInputData(dataModel$getDataModelInputData())
    newModel <- cPIDM$getInstance(dataModel$get.pIDM_next_id())
    dataModel$inc.pIDM_next_id()
    newModel$set.name(name)
    
    # Report formula
    pIDM_id <- newModel$get.id()
    
    formulaDiv <- buildStaticFormula(newModel, session)
    reportDiv <- reportType(div=list(ui=formulaDiv))
    
    formulaLatex <- transposeFormulaToPDF(newModel)
    tEnum <- reportTypeEnum()
    
    
    #Add formula element to recommended report progress
    addItem(moduleType = "evaluation",
            dataModel_id=newModel$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM_id, 
            pDIM_name = newModel$get.name(),
            imgFile = paste0("Images/Report/Formula_", GLOBAL_THEME,".png"),
            type=tEnum$formula, object=list(div=reportDiv, latex=formulaLatex), 
            show=F, singleton=T, global_reportProgressModel=global_reportProgressModel,
            recommended=T)
    
    
    dataModel$add.perIterationDataModels(newModel)
    
    dataModel$setSelectedPIDM(name)
    
    
    label_of_fit_run <<- "Run Fit"
    shinyjs::enable(id = "btnRunModelRun")
    # updateActionButton(session = session, inputId = "btnRunModelRun", label = label_of_fit_run)
    
    
    # Switch to 'Overview'
    updateTabsetPanel(session, "runModeltabsetPanel", "Overview")

  })
  
  
  ## stanParameters is a list, containing the number of iterations (iterations) and the number of chains (chains)
  prepareTask <- function(progressBar, consoleFile, consoleFile2, consoleFile3, stanParameters){
    # write("Entering...", consoleFile3, append=T)
    
    old_stat <- ""
    cur <- ""
    status <- ""
    
    flag <- T
      
    timeWaitPre <- 1.098613
    timeWait <- 1.098613
    while(flag){
      write("tick...", consoleFile3, append=T)
      
      #@@@FINISHED@@@

      cur <- tryCatch({
        status <- read.delim(file=consoleFile2, header = F, stringsAsFactors = F)
        # write(status,consoleFile3, append=T)
        cur <- if("@@@FINISHED@@@" == status){
          1
        }else{
          NULL
        }
      },
      error= function(e){
        return(NULL)
      })
      # write(cur,consoleFile3, append=T)
      

      if(!is.null(cur) && cur == 1){
        # write("END", consoleFile3, append=T)
        flag <- F
      }else{
        
        ret <- tryCatch({
          status <- read.delim(file=consoleFile, header = F, stringsAsFactors = F)
          status[length(status[,1]),]
        },
        error= function(e){
          return(NULL)
        })
        
        # write("ret:",consoleFile3, append=T)
        # write(ret,consoleFile3, append=T)
        
        if(is.null(ret)){
          progressBar$set(value = plogis(timeWaitPre)-0.75, message = "Prepare sampling ...", detail="This may take a while")
          # write("a", consoleFile3, append=T)
          timeWaitPre <- timeWaitPre+0.05
        }else{
          if(ret != old_stat){
            old_stat <- ret

            # validate cur
            progress <- validateStanOutput(paste(status), stanParameters)
            if(progress==1){
              flag <- F
              # setProgress(plogis(timeWait), message="Allocate memory ...")
              # progressBar$set(value = plogis(timeWait), message = "Allocate memory ...")
              # write("c", consoleFile3, append=T)
              # timeWait <- timeWait+0.05
            }else{
              # setProgress(progress*0.65+0.25,message="Sample model ...")
              progressBar$set(value = progress*0.5+0.25, message = "Sample model ...", detail="This may take a while")
              # write("b", consoleFile3, append=T)
            }
          }
        }
      }

      Sys.sleep(0.25)
    }
    # write("end", consoleFile3, append=T)
  }
  
  

  ## Select a previous sampled model. That will also update the result view
  observeEvent(input$selectInputRunModelPreviousModels, ignoreInit = T, ignoreNULL=F, {

    shinyjs::removeClass("selectInputRunModelPreviousModels", class="borderColor-primary")
    
    if(is.null(input$selectInputRunModelPreviousModels) || 
       input$selectInputRunModelPreviousModels == ""){
      
      #Remove result view
      output$runModelPlotPPC <- renderPlot(NULL)
      output$runModelVerbalResult <- renderUI(NULL)
      output$runModelVariableSummaryPlot <- renderPlot(NULL)
      output$runModelVariableSummaryTable <- renderDataTable(NULL)
      output$treeMPVars <- renderTree(NULL)
      output$runModelSelectedModelFormula <- renderUI(NULL)
      output$runModelModelValidationTabSQ <- renderDT(NULL)
      output$treePairsVars <- renderTree(NULL)
      output$treePVPVars <- renderTree(NULL)
      output$runModelVariableImpactTable <- renderUI(NULL)
      shinyjs::html("modelPredictionSelectedFitTitle","(No fit selected)")
      modelPredictionSelectedModelFormula <- renderUI(NULL)
      
      return()
    }
    
    shinyjs::disable(id = "btnRunModelRun")

    pDIM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)

    # update Ui
    createModelFitResults(input = input, output = output, session = session, 
                          iterationDataModel = pDIM,
                          global_reportProgressModel=global_reportProgressModel)
    
    output$runModelSelectedModelFormula <- renderUI(
      tags$div(tags$div(id="staticFormula",

        buildStaticFormula(pDIM, session)),
        tags$div(style="text-align:right;",
                 getReportButton("reportFormula", tooltip="Report the current formula")))
      )
    
    dataModel$setSelectedPIDM(input$selectInputRunModelPreviousModels)
    
    shinyjs::enable(id = "btnRunModelRun")
    shinyjs::html("modelPredictionSelectedFitTitle",paste0("\"",input$selectInputRunModelPreviousModels, "\""))
    firstAnimation(F)
  })
  

  
  #Changes in selected perIterationDataModel
  observe({
    dataModel$dependReactiveValue("selectedPIDM")
    
    isolate({
      sel <- dataModel$getSelectedPIDM()
      if(!equal(input$selectInputRunModelPreviousModels, sel)){
        updateSelectInput(session, "selectInputRunModelPreviousModels", selected = sel)
      }
    })
  })
  
  removeAlsoReportedItems <- F
  observeEvent(input$removeCPIDM, {
    #TODO: show warning that also reported stuff of this item will be removed
    if(removeAlsoReportedItems){
      #todo
    }
    
    if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == "" || input$selectInputRunModelPreviousModels == "(Not_fitted)"){
      showNotification("Invalid fitted model to remove.", type="warning")
    }else{
      pIDM <- dataModel$get.perIterationDataModel(name=input$selectInputRunModelPreviousModels)
      dataModel$remove.perIterationDataModel(input$selectInputRunModelPreviousModels)
      
      #remove also reported items
      itemsId <- global_reportProgressModel$getItemsOfPerIterationDataModel(pIDM$get.id())
      global_reportProgressModel$removeItem(ids=itemsId, recommended=F)
      global_reportProgressModel$removeItem(ids=itemsId, recommended=T)
    }
  })
  
  
  #Changes in list of perIterationDataModels
  observe({
    dataModel$dependReactiveValue("perIterationDataModels")
    dataModel$dependReactiveValue("selectedPIDM")
    
    isolate({

      ret <- dataModel$get.perIterationDataModelNames()
      selected <- dataModel$getSelectedPIDM()
      updateSelectInput(session = session, inputId = "selectInputRunModelPreviousModels", 
                        choices = ret$res, selected = selected)
      
    })
  })
  
  
  # Init observer for sample parameters
  observeSampleParameters(input, output, session, dataModel)
  
  #Observe report button for formula
  observeEvent(input$reportFormula, {
    

    pDIM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    pDIM_id <- pDIM$get.id()
    
    formulaDiv <- buildStaticFormula(pDIM, session)
    reportDiv <- reportType(div=list(ui=formulaDiv))
    
    formulaLatex <- transposeFormulaToPDF(pDIM)
    tEnum <- reportTypeEnum()

    #Add formula element to report progress
    addItem(moduleType = "evaluation",
            dataModel_id=pDIM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pDIM_id, 
            pDIM_name = pDIM$get.name(),
            imgFile = paste0("Images/Report/Formula_", GLOBAL_THEME,".png"),
            type=tEnum$formula, object=list(div=reportDiv, latex=formulaLatex), 
            show=T, singleton=T, global_reportProgressModel=global_reportProgressModel)
  })
  
  #Observe report button for model validation
  observeEvent(input$reportModelValidation, {
    
    #Any fit selected?
    if(!is.null(input$selectInputRunModelPreviousModels) && 
       input$selectInputRunModelPreviousModels != ""){
      
      tRIV <- tmpReportItemValidation
      if(is.null(tRIV)){
	    if(localUse) browser()
        showNotification("Something went wrong. The operator is notified.", type="error")
        malfunction_report(code=malfunctionCode()$missingReportItem, msg="missing tmpReportItemValidation",
                           type="error", askForReport=T)
        return()
      }
      tEnum <- reportTypeEnum()
    
      tRIV$object$div <- reportType(div=list(ui=tRIV$object$div))
      
      
      #Add model validation element to report progress
      #pIDM_id=-1 -1 for reported items that are not related to a certain pIDM
      #There are no different dataModels, so that the csv name is used instead
      addItem(moduleType = "evaluation",
              dataModel_id = tRIV$dataModel_id,
              pDIM_id = tRIV$pDIM_id,
              pDIM_name = tRIV$pDIM_name,
              imgFile = tRIV$imgFile,
              type=tEnum$validation, object=tRIV$object,
              singleton=T, show=T, global_reportProgressModel=global_reportProgressModel)
    }
    
  })
  

  
  #Is a model fit selected? Only to highlight report buttons
  observeEvent(input$selectInputRunModelPreviousModels, {
    if(!is.null(input$selectInputRunModelPreviousModels) && 
       input$selectInputRunModelPreviousModels != ""){
      shinyjs::addClass(id = "reportPreviewPPC", "btn-primary")
      shinyjs::addClass(id = "reportModelValidation", "btn-primary")
    }else{
      shinyjs::removeClass(id = "reportPreviewPPC", "btn-primary")
      shinyjs::removeClass(id = "reportModelValidation", "btn-primary")
    }
  })
  
  #Observe report button for preview PPC
  observeEvent(input$reportPreviewPPC, {
    
    #Any fit selected?
    if(!is.null(input$selectInputRunModelPreviousModels) && 
       input$selectInputRunModelPreviousModels != ""){
      
      withProgress(message = 'Adding item to reporting queue',  expr = {
        pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
        plot <- pIDM$get.ppc_plot()
        
        inputName <- pIDM$get.name()
        inputName <- str_replace_all(inputName, " ", "_")
        
        setProgress(value=0.2)
        
        ggsave(paste0(report_folder, "/Thumbnails/ppc_preview_",inputName,".jpg"), 
               plot, device="jpeg", width=100, height=100, units="px", dpi=25)

        tEnum <- reportTypeEnum()
        
        setProgress(value=0.4)
        

        caption <- NULL
        if(pIDM$get.ppc_plot_warnings()) caption <- readTxtFile(paste0(report_folder,"/GeneralTex/PPC_preview_caption.txt"))
        
        latexPlot <- plotToTex(paste0("ppc_preview_",inputName), plot, caption=caption)
        setProgress(value=0.8)
        
        reportDiv <- reportType(div=list(plot=plot))
        
        #Add ppc element to report progress
        #pIDM_id=-1 -1 for reported items that are not related to a certain pIDM
        #There are no different dataModels, so that the csv name is used instead
        addItem(moduleType = "evaluation",
                dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), 
                pDIM_id=pIDM$get.id(),  pDIM_name = pIDM$get.name(),
                imgFile=paste0("Report/Thumbnails/ppc_preview_",inputName,".jpg"), 
                type=tEnum$previewppc, object=list(div=reportDiv, latex=latexPlot), 
                singleton=T, show=T, global_reportProgressModel=global_reportProgressModel)

        setProgress(value=1)
      })
      
    }else{
      showNotification("No plot to report! Fit a model first!", type = "warning")
    }
  })
  
 
  
  # Init observer for MP tab
  observerMPTab(input, output, session, dataModel, global_reportProgressModel)
  
  # Init observer for PPC tab
  observerPPCTab(input, output, session, dataModel, global_reportProgressModel)

  # Init observer for Model validation tab
  observerModelValidationTab(input, output, session, dataModel, global_reportProgressModel)
  
  # Observer of response button in ppc 
  observeEvent(input$runModelLinkUpperLeft, {
    updateTabsetPanel(session, inputId = "runModeltabsetPanel", selected = "Model validation")
  })
  observeEvent(input$runModelLinkUpperRight, {
    updateTabsetPanel(session, inputId = "runModeltabsetPanel", selected = "PPC")
  })
  observeEvent(input$runModelLinkBottomLeft, {
    updateTabsetPanel(session, inputId = "runModeltabsetPanel", selected = "Marginal posteriors")
  })

  ##Footer
  observeEvent(input$btnRunModelBack, {
    updateNavbarPage(session, "navbar", selected = "Model selection")
  }) 
  observeEvent(input$btnRunModelNext, {
    updateNavbarPage(session, "navbar", selected = "Model comparison")
  }) 
  observeEvent(input$btnRunModelNextPrediction, {
    updateNavbarPage(session, "navbar", selected = "Effects / Predictions")
  }) 

}


# Init observer for samples parameters
observeSampleParameters <- function(input, output, session, dataModel){
  
  observeEvent(input$runModelItertations, ignoreNULL = F, {
    cPIDM <- dataModel$get.cPerIterationDataModel()
    
    iter <- input$runModelItertations
    if(!is.null(iter)){
      cPIDM$set.number_iterations(as.numeric(iter))
    }else{
      cPIDM$set.number_iterations(NULL)
    }
  })
  
  observeEvent(input$runModelChains, ignoreNULL = F, {
    cPIDM <- dataModel$get.cPerIterationDataModel()
    chains <- input$runModelChains
    if(!is.null(chains)){
      cPIDM$set.number_chains(as.numeric(chains))
    }else{
      # cPIDM$runModelChains(NULL)
      cPIDM$set.number_chains(NULL)
    }
  })
  
  observeEvent(input$runModelCores, ignoreNULL = F, {
    cPIDM <- dataModel$get.cPerIterationDataModel()
    
    if(localUse){
      cores <- input$runModelCores
      if(!is.null(cores)){
        cPIDM$set.number_cores(as.numeric(cores))
      }else{
        cPIDM$set.number_cores(NULL)
      }
    }
  })
  
  observeEvent(input$runModelAdaptDelta, ignoreNULL = F, {
    cPIDM <- dataModel$get.cPerIterationDataModel()
    
    adaptDelta <- input$runModelAdaptDelta
    if(!is.null(adaptDelta)){
      cPIDM$set.adapt_delta(as.numeric(adaptDelta))
    }else{
      cPIDM$set.adapt_delta(NULL)
    }    
  })
  
  observeEvent(input$runModelMaxTreedepth, ignoreNULL = F, {
    cPIDM <- dataModel$get.cPerIterationDataModel()
    
    maxTree <- input$runModelMaxTreedepth
    if(!is.null(maxTree)){
      cPIDM$set.max_treedepth(as.numeric(maxTree))
    }else{
      cPIDM$set.max_treedepth(NULL)
    }
  })
  
}

# Init observer for Model validation tab
observerModelValidationTab <- function(input, output, session, dataModel, global_reportProgressModel){
  #### Pairs ####
  ns_pairs <- NS("runModelModelValidationTabSQPairsPlot")
  
  #Pairs plot button
  plot_id <- reactiveVal(1)
  observeEvent(input$plotPairs, ignoreInit = T, {
    
    tryCatch({
      res <- c()
      t <- input$treePairsVars
      n_i <- names(t)
      for(i in 1:length(t)){
        if(class(t[[i]])=="list"){
          n_j <- names(t[[i]])
          for(j in 1:length(t[[i]])){
            if(!is.null(attr(t[[i]][[j]],"stselected")) && attr(t[[i]][[j]],"stselected") == "TRUE") res <- c(res, n_j[j])
          }
        }else{
          if(!is.null(attr(t[[i]],"stselected")) && attr(t[[i]],"stselected") == "TRUE") res <- c(res, n_i[i])
        }
      }
      
      #No fit available?
      if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
        showNotification("Please fit a model before.", type="warning")
        return()
      }
      
      
      pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
      
      res <- gsub("<i>||</i>","",res)
      baysisModel <- pIDM$get.selected_BAYSIS_stan_model() 
      res <- baysisModel$matchAuxiliaryNames(res)
      
      #Just select on var? 
      if(length(res) <= 1){
        showNotification("Please select at least 2 formula elements if possible.", type="warning")
        return()
      }
      
      stanfit <- extract_stanfit(pIDM$get.calculated_stan_object())
      
      post <- as.array(stanfit)
      
      gg <- mcmc_pairs(post, pars=res)

      
      plot_id_tmp <- dataModel$getDataModelPlotModel()$getNextId("pairs")
      plot_name <- paste0("Plot ", plot_id_tmp)
      
      # Save plot to data model
      dataModel$getDataModelPlotModel()$add.plot_history("pairs", plotname=plot_name, id=plot_id_tmp, 
                                                         plot=gg, pairsInfo=list(name=pIDM$get.name()))

    },
    error=function(e){
      print(e)
	  if(localUse) browser()
      showNotification("Something went wrong. The operator is notified.", type="error")
      malfunction_report(code=malfunctionCode()$pairsplot, msg="creating pairs plot",
                         type="error", askForReport=T)
    })
    
    
  })
  
  #Change button style depending on existing plots to report/download
  observeEvent(input$runModelModelValidationTabSQPairsPlot, ignoreNULL = F, {
    if(!is.null(input$runModelModelValidationTabSQPairsPlot)){
      shinyjs::addClass(id = "reportPairs", "btn-primary")
      shinyjs::addClass(id = ns_pairs("download"), "btn-primary") 
    }else{
      shinyjs::removeClass(id = "reportPairs", "btn-primary")
      shinyjs::removeClass(id = ns_pairs("download"), "btn-primary")
    }
  })
  
  #Report pairs
  observeEvent(input$reportPairs, {
    
    plot <- dataModel$getDataModelPlotModel()$get.plot_by_name("pairs", input$runModelModelValidationTabSQPairsPlot)
    
    inputName <- input$runModelModelValidationTabSQPairsPlot
    inputName <- str_replace_all(inputName, " ", "_")
    
    ggsave(paste0(report_folder, "/Thumbnails/pairs_",inputName,".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    tEnum <- reportTypeEnum()
    
    #Add element to report progress
    #pDIM_id=-1 -1 for reported items that are not related to a certain pDIM
    #There are no different dataModels, so that the csv name is used instead
    # fileName <- dataModel$getDataModelPlotModel()$get.raw_plot_file_name(input$runModelModelValidationTabSQPairsPlot)

    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)

    reportDiv <- reportType(div=list(plot=plot))
    
    addItem(moduleType = "evaluation",
            dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM$get.id(),  
            pDIM_name = pIDM$get.name(),
            imgFile=paste0("Report/Thumbnails/pairs_",inputName,".jpg"), 
            type=tEnum$pairs, object=list(div=reportDiv, latex=plotToTex(paste0("pairs_",inputName), plot, caption=NULL)), 
            singleton=F, show=T, global_reportProgressModel=global_reportProgressModel)
  })
  
  
  #Remove current pairs plot
  observeEvent(input$removePairs, {
    remove <- input$runModelModelValidationTabSQPairsPlot
    if(is.null(remove) || remove == ""){
      showNotification("There is nothing to close.", type="warning")
      return()
    }
    dataModel$getDataModelPlotModel()$remove.plot_history("pairs",remove)
  })
  
  #Download current pairs plot
  observeEvent(input[[ns_pairs("download")]], {
    
    if(is.null(input$runModelModelValidationTabSQPairsPlot)){
      showNotification("There is no plot to download.", type ="warning")
      return()
    }
    
    showModal(downloadPlotModalUI("modelValidationPairs", input$runModelModelValidationTabSQPairsPlot))
  })
  downloadPlotServer(id="modelValidationPairs", 
                     gInput=input,
                     dataModel=dataModel,
                     plotType="pairs",
                     plotName="runModelModelValidationTabSQPairsPlot")
  
  
  
  shownPairsPlotsIds <- reactiveVal(c())
  #Show plots
  observe({
    dMPM <- dataModel$getDataModelPlotModel()
    dMPM$dependReactiveValue("plot_history_pairs")
    
    isolate({
      
      plots <- dMPM$get.plot_history("pairs")
      
      shownPairsPlotsIds_new <- c()
      add_ids <- c()
      
      for(pp_id in seq_along(plots)){
        pp <- plots[[pp_id]]
        #add
        if(!pp$id %in% shownPairsPlotsIds()){
          
          # Create new tab for new plot in tab panel
          appendTab(
            inputId = "runModelModelValidationTabSQPairsPlot", 
            select = T,
            tab = tabPanel(
              title = paste0("Plot ", pp$id),
              value = paste0("Plot ", pp$id),
              h5(pp$pairsInfo$name, style="text-align:center;"),
              plotOutput(outputId = paste0("runModelModelValidationTabSQPairsPlot", pp$id))
            )
          )
          
          add_ids <- c(add_ids, pp_id)
        }
        shownPairsPlotsIds_new <- c(shownPairsPlotsIds_new, pp$id)
      }
      
      sapply(add_ids, function(i){
        # Render selected type of Plot
        output[[paste0("runModelModelValidationTabSQPairsPlot", plots[[i]]$id)]] <- renderPlot(plots[[i]]$plot)
      })
      
      #remove
      ids <- shownPairsPlotsIds()
      for(id in ids){
        if(!id %in% shownPairsPlotsIds_new){
          selected <- dMPM$get.next_plot("pairs", id=id)
          selected <- paste0("Plot ", selected)
          updateTabsetPanel(session, "runModelModelValidationTabSQPairsPlot", selected = selected)
          removeTab(inputId = "runModelModelValidationTabSQPairsPlot",paste0("Plot ", id))
        }
      }
      
      shownPairsPlotsIds(shownPairsPlotsIds_new)
    })
  })
  
  
  
  #### Prior predictive check ####
  #Observe report button for prior predictive check
  observeEvent(input$reportPriorPC, {
    
    #Any fit selected?
    if(!is.null(input$selectInputRunModelPreviousModels) && 
       input$selectInputRunModelPreviousModels != ""){
      
      pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
      
      plot <- pIDM$get.priorPredictiveCheckPlot()
      
      inputName <- pIDM$get.name()
      
      ggsave(paste0(report_folder, "/Thumbnails/priorpc_",inputName,".jpg"), 
             plot, device="jpeg", width=100, height=100, units="px", dpi=25)
      

      #Add report prior predictive check  to recommended report progress
      tEnum <- reportTypeEnum()
      
      reportDiv <- reportType(div=list(plot=plot))
      
      addItem(moduleType = "evaluation",
              dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM$get.id(), 
              pDIM_name = pIDM$get.name(),
              imgFile = paste0("Report/Thumbnails/priorpc_",inputName,".jpg"),
              type=tEnum$priorpc, object=list(div=reportDiv, latex=plotToTex(paste0("priorpc_",inputName), plot, caption=NULL)), 
              show=T, singleton=T, global_reportProgressModel=global_reportProgressModel)
    }
    
  })
  
  
  #### Prior vs Posterior ####
  ns_pvp <- NS("runModelModelValidationTabSQPVPPlot")
  
  #Prior vs posterior plot button
  plot_id_PVP <- reactiveVal(1)
  observeEvent(input$plotPVP, ignoreInit = T, {
    
    tryCatch({
      
      res <- c()
      t <- input$treePVPVars
      n_i <- names(t)
      for(i in 1:length(t)){
        if(class(t[[i]])=="list"){
          n_j <- names(t[[i]])
          for(j in 1:length(t[[i]])){
            if(!is.null(attr(t[[i]][[j]],"stselected")) && attr(t[[i]][[j]],"stselected") == "TRUE") res <- c(res, n_j[j])
          }
        }else{
          if(!is.null(attr(t[[i]],"stselected")) && attr(t[[i]],"stselected") == "TRUE") res <- c(res, n_i[i])
        }
      }
      
      #No fit available?
      if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
        showNotification("Please fit a model before.", type="warning")
        return()
      }
      
      res <- gsub("<i>||</i>","",res)
      
      #No var selected?
      if(length(res) == 0){
        showNotification("Please select at least 1 formula element.", type="warning")
        return()
      }
      
   
      pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
      sampled_model <- pIDM$get.calculated_stan_object()
      dMID <- pIDM$getDataModelInputData()
      response <- dMID$getResponseVariable(onlyName=T)
      used_opt_terms <- pIDM$get.used_optional_terms()
      baysisModel <- pIDM$get.selected_BAYSIS_stan_model() 

      # Read sampling parameters
      stanParameters <- list(
        iterations = pIDM$get.number_iterations(),
        chains = pIDM$get.number_chains(),
        cores = pIDM$get.number_cores(),
        adapt_delta = pIDM$get.adapt_delta(),
        max_treedepth = pIDM$get.max_treedepth(),
        seed = pIDM$get.seed()
      )
      

      #Replace naming for auxiliary parameters if necessary
      res <- baysisModel$matchAuxiliaryNames(res)

      data <- dMID$getLongFormat()
      
      content <- baysisModel$get_content_for_stan_code(response = response, 
                                                       data = data, 
                                                       stanParameter = stanParameters)


      # assign("sampled_model",sampled_model)
      #Required for the update function. Otherwise the call object in sampled_model will use an older content.
      assign("content", content, envir = .GlobalEnv)
      
      c <- call("posterior_vs_prior", object=sampled_model, pars=res, 
                group_by_parameter = input$groupByParameterPVP)
      gg <- eval(c) + 
        theme(text = element_text(size = 14)) + 
        geom_pointrange(size=1)
      

      plot_id_tmp <- dataModel$getDataModelPlotModel()$getNextId("pvp")
      plot_name <- paste0("Plot ", plot_id_tmp)
      
      # Save plot to data model
      dataModel$getDataModelPlotModel()$add.plot_history("pvp", plotname=plot_name, id=plot_id_tmp, 
                                                         plot=gg, pvpInfo=list(name=pIDM$get.name()))
    },
    error=function(e){
      print(e)
	  if(localUse) browser()
      showNotification("Something went wrong. The operator is notified.", type="error")
      malfunction_report(code=malfunctionCode()$pvpPlot, msg="creating pvp plot",
                         type="error", askForReport=T)
    })
    
  })
  
  #Change button style depending on existing plots to report/download
  observeEvent(input$runModelModelValidationTabSQPVPPlot, ignoreNULL = F, {
    if(!is.null(input$runModelModelValidationTabSQPVPPlot)){
      shinyjs::addClass(id = "reportPVP", "btn-primary")
      shinyjs::addClass(id = ns_pvp("download"), "btn-primary") 
    }else{
      shinyjs::removeClass(id = "reportPVP", "btn-primary")
      shinyjs::removeClass(id = ns_pvp("download"), "btn-primary")
    }
  })
  
  #Report PVP plot
  observeEvent(input$reportPVP, {

    plot <- dataModel$getDataModelPlotModel()$get.plot_by_name("pvp", input$runModelModelValidationTabSQPVPPlot)
    
    inputName <- input$runModelModelValidationTabSQPVPPlot
    inputName <- str_replace_all(inputName, " ", "_")
    
    ggsave(paste0(report_folder, "/Thumbnails/pvp_",inputName,".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    tEnum <- reportTypeEnum()
    
    #Add element to report progress
    #pDIM_id=-1 -1 for reported items that are not related to a certain pDIM
    #There are no different dataModels, so that the csv name is used instead

    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    reportDiv <- reportType(div=list(plot=plot))
    
    addItem(moduleType = "evaluation",
            dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM$get.id(),  
            pDIM_name = pIDM$get.name(),
            imgFile=paste0("Report/Thumbnails/pvp_",inputName,".jpg"), 
            type=tEnum$pvp, object=list(div=reportDiv, latex=plotToTex(paste0("pvp_",inputName), plot, caption=NULL)), 
            singleton=F, show=T, global_reportProgressModel=global_reportProgressModel)
  })
  
  #Remove current PVP plot
  observeEvent(input$removePVP, {
    remove <- input$runModelModelValidationTabSQPVPPlot
    if(is.null(remove) || remove == ""){
      showNotification("There is nothing to close.", type="warning")
      return()
    }
    dataModel$getDataModelPlotModel()$remove.plot_history("pvp",remove)
  })
  
  #Download current PVP plot
  observeEvent(input[[ns_pvp("download")]], {
    
    if(is.null(input$runModelModelValidationTabSQPVPPlot)){
      showNotification("There is no plot to download.", type ="warning")
      return()
    }
    
    showModal(downloadPlotModalUI("modelValidationPVP", input$runModelModelValidationTabSQPVPPlot))
  })
  downloadPlotServer(id="modelValidationPVP", 
                     gInput=input,
                     dataModel=dataModel,
                     plotType="pvp",
                     plotName="runModelModelValidationTabSQPVPPlot")
  
  
  shownPvpPlotsIds <- reactiveVal(c())
  #Show plots
  observe({
    dMPM <- dataModel$getDataModelPlotModel()
    dMPM$dependReactiveValue("plot_history_pvp")
    
    isolate({
      
      plots <- dMPM$get.plot_history("pvp")
      
      shownPvpPlotsIds_new <- c()
      add_ids <- c()
      
      for(pp_id in seq_along(plots)){
        pp <- plots[[pp_id]]
        #add
        if(!pp$id %in% shownPvpPlotsIds()){
          
          # Create new tab for new plot in tab panel
          appendTab(
            inputId = "runModelModelValidationTabSQPVPPlot", 
            select = T,
            tab = tabPanel(
              title = paste0("Plot ", pp$id),
              value = paste0("Plot ", pp$id),
              h5(pp$pvpInfo$name, style="text-align:center;"),
              plotOutput(outputId = paste0("runModelModelValidationTabSQPVPPlot", pp$id))
            )
          )
          
          add_ids <- c(add_ids, pp_id)
        }
        shownPvpPlotsIds_new <- c(shownPvpPlotsIds_new, pp$id)
      }
      
      sapply(add_ids, function(i){
        # Render selected type of Plot
        output[[paste0("runModelModelValidationTabSQPVPPlot", plots[[i]]$id)]] <- renderPlot(plots[[i]]$plot)
      })
      
      #remove
      ids <- shownPvpPlotsIds()
      for(id in ids){
        if(!id %in% shownPvpPlotsIds_new){
          selected <- dMPM$get.next_plot("pvp", id=id)
          selected <- paste0("Plot ", selected)
          updateTabsetPanel(session, "runModelModelValidationTabSQPVPPlot", selected = selected)
          removeTab(inputId = "runModelModelValidationTabSQPVPPlot",paste0("Plot ", id))
        }
      }
      
      shownPvpPlotsIds(shownPvpPlotsIds_new)
    })
  })
  
}


# Init observer for PPC tab
observerPPCTab <- function(input, output, session, dataModel, global_reportProgressModel){

  # "Density overlay", "Interval", "Histogram", "Frequency polygon", "Violin grouped", "Bars"

  observeEvent(input$selectionPPCType, ignoreInit=T, {
    if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
      showNotification("Please fit a model before.", type="warning")
      return()
    }
    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
     
    iter <- pIDM$get.number_iterations()
    chains <- pIDM$get.number_chains()
    maxDraw <- floor(iter/2*chains)
    
    inp <- input$selectionPPCType
    if(inp == "Density overlay" || inp == "Bars"){
      shinyjs::disable(id = "selectionPPCGroup")
      shinyjs::enable(id = "numericPPCDraws")
      updateBayasNumericInput(session, "numericPPCDraws", value = 50, min=1, max=min(200,maxDraw))
      updateSelectizeInput(session, "selectionPPCGroup", selected="")
    }else if(inp == "Histogram" || inp == "Frequency polygon"){
      shinyjs::disable(id = "selectionPPCGroup")
      shinyjs::enable(id = "numericPPCDraws")
      updateBayasNumericInput(session, "numericPPCDraws", value = 8, min=1, max=min(15,maxDraw))
      updateSelectizeInput(session, "selectionPPCGroup", selected="")
    }else if(inp == "Interval"){
      shinyjs::disable(id = "selectionPPCGroup")
      shinyjs::disable(id = "numericPPCDraws")
      updateBayasNumericInput(session, "numericPPCDraws", value = maxDraw, min=1, max=maxDraw)
      updateSelectizeInput(session, "selectionPPCGroup", selected="")
    }else if (inp == "Violin grouped"){
      shinyjs::enable(id = "selectionPPCGroup")
      shinyjs::disable(id = "numericPPCDraws")
      updateBayasNumericInput(session, "numericPPCDraws", value = maxDraw, min=1, max=maxDraw)
    }else{
      stop("A ppc type selected, that is not handled.")
    }
  })
  observeEvent(input$numericPPCDraws, ignoreInit=T, {
    if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
      showNotification("Please fit a model before.", type="warning")
      return()
    }
    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    iter <- pIDM$get.number_iterations()
    chains <- pIDM$get.number_chains()
    maxDraw <- floor(iter/2*chains)
    
    curMaxDraw <- 1
    curDefDraw <- 1
    inp <- input$selectionPPCType
    if(inp == "Density overlay" || inp == "Bars"){
      curMaxDraw <- min(200, maxDraw)
      curDefDraw <- 50
    }else if(inp == "Histogram" || inp == "Frequency polygon"){
      curMaxDraw <- min(15, maxDraw)
      curDefDraw <- 8
    }else if(inp == "Interval" || inp == "Violin grouped"){
      curMaxDraw <- maxDraw
      curDefDraw <- maxDraw
    }
    
    if(!is.numeric(input$numericPPCDraws)){
      updateBayasNumericInput(session, "numericPPCDraws", value = curDefDraw)
      return()
    }else if(input$numericPPCDraws > curMaxDraw){
      showNotification("The number of draws are limited.", type="message")
      updateBayasNumericInput(session, "numericPPCDraws", value = curMaxDraw)
    }else if(input$numericPPCDraws < 1){
      showNotification("The number of draws should be at least 1.", type="message")
      updateBayasNumericInput(session, "numericPPCDraws", value = 1)
    }
  })
  
  #Plot button
  ns_ppc <- NS("FitPagePPCPanelPlot")
  
  plot_id <- reactiveVal(1)
  observeEvent(input$plotPPC, ignoreInit = T, {
    if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
      showNotification("Please fit a model before.", type="warning")
      return()
    }

    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    #fitPageMPPanelPlot
    sampled_model <- pIDM$get.calculated_stan_object()
    dMID <- pIDM$getDataModelInputData()

    data <- dMID$getLongFormat()


    group <- isolate(input$selectionPPCGroup)
    usedVars <- unique(c(pIDM$get.selected_BAYSIS_stan_model()$get_used_vars(extras=T, response=T), group))
    data <- dMID$getLongFormatVariable(usedVars, completeCases=T)
    
    y <- data[[dMID$getResponseVariable(onlyName=T)]]
    

    yrep <- tryCatch({
      pIDM$get.selected_BAYSIS_stan_model()$make_predictions(stanObject=sampled_model,
                                                             data=NULL, draws = input$numericPPCDraws)},
      
      error=function(cond){
        print(cond)
        showNotification("This model uses interaction levels that are not supported by your data and therefore be dropped.
                            Unfortunately, it is not possible to show results yet.", duration = 20, type="error")
        malfunction_report(code=malfunctionCode()$makeStanPredictions, msg="make_predictions on BAYSIS model",
                           type="warning", askForReport=T)
        return(NULL)
      }
    )
    if(is.null(yrep))return()

    
    # "Density overlay", "Interval", "Histogram", "Frequency polygon", "Violin grouped", "Bars"    
    type <- isolate(input$selectionPPCType)
    

    if(type == "Violin grouped"){
      if((is.null(group[1]) || group[1] == "" )){
        showNotification("Please select a variable to group by.", type="warning")
        return()
      }
      dd <- data
      dd_g <- dd[group]
      group_interaction <- interaction(dd_g)
    }
    
    gg <- switch(type,
                 'Density overlay'=bayesplot::ppc_dens_overlay(y, yrep),
                 'Interval'=bayesplot::ppc_intervals(y, yrep),
                 'Frequency polygon'=bayesplot::ppc_freqpoly(y, yrep),
                 'Histogram'=bayesplot::ppc_hist(y, yrep),
                 'Bars'=bayesplot::ppc_bars(y, yrep),
                 'Violin grouped'=bayesplot::ppc_violin_grouped(y, yrep, group = group_interaction, y_draw="both"))

    
    lowerLim <- input$selectionPPCLowerLim
    upperLim <- input$selectionPPCUpperLim
    
    if(could.numeric(lowerLim)) lowerLim <- as.numeric(lowerLim) else lowerLim <- NA
    if(could.numeric(upperLim)) upperLim <- as.numeric(upperLim) else upperLim <- NA
    
    if(!is.na(lowerLim) && !is.na(upperLim) && lowerLim > upperLim){
      showNotification("The lower limit have to be smaller than the upper limit!", type="error")
      return()
    }
    
    if(!is.na(lowerLim) || !is.na(upperLim)){
      if(type %in% c("Interval","Violin grouped")){
        gg <- gg + ylim(lowerLim, upperLim)
      }else{
        gg <- gg + xlim(lowerLim, upperLim)
      }
    } 

    inp <- input$selectionPPCScale
    if(!is.null(inp) && inp != ""){
      if(type %in% c("Interval","Violin grouped")){
        gg <- gg + scale_y_continuous(trans=input$selectionPPCScale)
      }else{
        gg <- gg + scale_x_continuous(trans=input$selectionPPCScale)
      }
    } 
    

    plot_id_tmp <- dataModel$getDataModelPlotModel()$getNextId("ppc")
    plot_name <- paste0("Plot ", plot_id_tmp)
    
    # Save plot to data model
    dataModel$getDataModelPlotModel()$add.plot_history("ppc", plotname=plot_name, id=plot_id_tmp, 
                                                       plot=gg, ppcInfo=list(name=pIDM$get.name()))

  })

  #Change button style depending on existing plots to report/download
  observeEvent(input$FitPagePPCPanelPlot, ignoreNULL = F, {
    if(!is.null(input$FitPagePPCPanelPlot)){
      shinyjs::addClass(id = "reportPPC", "btn-primary")
      shinyjs::addClass(id = ns_ppc("download"), "btn-primary") 
    }else{
      shinyjs::removeClass(id = "reportPPC", "btn-primary")
      shinyjs::removeClass(id = ns_ppc("download"), "btn-primary")
    }
  })
  
  #Report PPC plot
  observeEvent(input$reportPPC, {
    
    plot <- dataModel$getDataModelPlotModel()$get.plot_by_name("ppc", input$FitPagePPCPanelPlot)
    
    inputName <- input$FitPagePPCPanelPlot
    inputName <- str_replace_all(inputName, " ", "_")
    
    ggsave(paste0(report_folder, "/Thumbnails/ppc_",inputName,".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    tEnum <- reportTypeEnum()
    
    #Add element to report progress
    #pDIM_id=-1 -1 for reported items that are not related to a certain pDIM
    #There are no different dataModels, so that the csv name is used instead
    
    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    reportDiv <- reportType(div=list(plot=plot))
    
    addItem(moduleType = "evaluation",
            dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM$get.id(),  
            pDIM_name = pIDM$get.name(),
            imgFile=paste0("Report/Thumbnails/ppc_",inputName,".jpg"), 
            type=tEnum$ppc, object=list(div=reportDiv, latex=plotToTex(paste0("ppc_",inputName), plot, caption=NULL)), 
            singleton=F, show=T, global_reportProgressModel=global_reportProgressModel)
  })
  
  #Remove current plot
  observeEvent(input$removePPC, {
    remove <- input$FitPagePPCPanelPlot
    if(is.null(remove) || remove == ""){
      showNotification("There is nothing to close.", type="warning")
      return()
    }
    selected <- dataModel$getDataModelPlotModel()$get.next_plot("ppc",remove)
    updateTabsetPanel(session, "FitPagePPCPanelPlot", selected = selected)
    removeTab("FitPagePPCPanelPlot", remove)
    dataModel$getDataModelPlotModel()$remove.plot_history("ppc",remove)
    # updateTabsetPanel(session, "FitPagePPCPanelPlot", selected = selected)
  })
   
  
  #Download current PPC plot
  observeEvent(input[[ns_ppc("download")]], {
    
    if(is.null(input$FitPagePPCPanelPlot)){
      showNotification("There is no plot to download.", type ="warning")
      return()
    }
    
    showModal(downloadPlotModalUI("modelValidationPPC", input$FitPagePPCPanelPlot))
  })
  downloadPlotServer(id="modelValidationPPC", 
                     gInput=input,
                     dataModel=dataModel,
                     plotType="ppc",
                     plotName="FitPagePPCPanelPlot")

  shownPPCPlotsIds <- reactiveVal(c())
  #Show plots
  observe({
    dMPM <- dataModel$getDataModelPlotModel()
    dMPM$dependReactiveValue("plot_history_ppc")
    
    isolate({
      
      plots <- dMPM$get.plot_history("ppc")
      
      shownPPCPlotsIds_new <- c()
      add_ids <- c()
      
      for(pp_id in seq_along(plots)){
        pp <- plots[[pp_id]]
        #add
        if(!pp$id %in% shownPPCPlotsIds()){
          
          # Create new tab for new plot in tab panel
          appendTab(
            inputId = "FitPagePPCPanelPlot", 
            select = T,
            tab = tabPanel(
              title = paste0("Plot ", pp$id),
              value = paste0("Plot ", pp$id),
              h5(pp$ppcInfo$name, style="text-align:center;"),
              plotOutput(outputId = paste0("FitPagePPCPanelPlot", pp$id))
            )
          )
          
          add_ids <- c(add_ids, pp_id)
        }
        shownPPCPlotsIds_new <- c(shownPPCPlotsIds_new, pp$id)
      }
      
      sapply(add_ids, function(i){
        # Render selected type of Plot
        output[[paste0("FitPagePPCPanelPlot", plots[[i]]$id)]] <- renderPlot(plots[[i]]$plot)
      })
      
      #remove
      ids <- shownPPCPlotsIds()
      for(id in ids){
        if(!id %in% shownPPCPlotsIds_new){
          selected <- dMPM$get.next_plot("ppc", id=id)
          selected <- paste0("Plot ", selected)
          updateTabsetPanel(session, "FitPagePPCPanelPlot", selected = selected)
          removeTab(inputId = "FitPagePPCPanelPlot",paste0("Plot ", id))
        }
      }
      
      shownPPCPlotsIds(shownPPCPlotsIds_new)
    })
  })
  
}


# Init observer for marginal posterior tab
observerMPTab <- function(input, output, session, dataModel, global_reportProgressModel){
  observeEvent(input$selectionMPType,{
    if(input$selectionMPType == "Intervals"){
      shinyjs::enable(id = "innerHDIValue")
      shinyjs::enable(id = "outerHDIValue")
      shinyjs::enable(id = "selectionMPPointEst")
      updateBayasNumericInput(session, "innerHDIValue", value = 0.5)
      updateBayasNumericInput(session, "outerHDIValue", value = 0.9)
    }else if(input$selectionMPType == "Areas"){
      shinyjs::enable(id = "innerHDIValue")
      shinyjs::enable(id = "outerHDIValue")
      shinyjs::enable(id = "selectionMPPointEst")
      updateBayasNumericInput(session, "innerHDIValue", value = 0.5)
      updateBayasNumericInput(session, "outerHDIValue", value = 1)
    }else{
      shinyjs::disable(id = "innerHDIValue")
      shinyjs::disable(id = "outerHDIValue")
      shinyjs::disable(id = "selectionMPPointEst")
    }
  })
  observeEvent(input$innerHDIValue, {
    if(!is.numeric(input$innerHDIValue)) return()
    if(input$innerHDIValue > input$outerHDIValue){
      showNotification("The inner HDI must be less than the outer HDI.", type="warning")
    }
  })
  observeEvent(input$outerHDIValue, {
    if(!is.numeric(input$outerHDIValue)) return()
    if(input$innerHDIValue > input$outerHDIValue){
      showNotification("The inner HDI must be less than the outer HDI.", type="warning")
    }
  })
  
  
  #Plot button
  ns_mp <- NS("fitPageMPPanelPlot")
  
  plot_id <- reactiveVal(1)
  observeEvent(input$plotMP, ignoreInit = T, {
    res <- c()
    t <- input$treeMPVars
    n_i <- names(t)
    for(i in 1:length(t)){
      if(class(t[[i]])=="list"){
        n_j <- names(t[[i]])
        for(j in 1:length(t[[i]])){
          if(!is.null(attr(t[[i]][[j]],"stselected")) && attr(t[[i]][[j]],"stselected") == "TRUE") res <- c(res, n_j[j])
        }
      }else{
        if(!is.null(attr(t[[i]],"stselected")) && attr(t[[i]],"stselected") == "TRUE") res <- c(res, n_i[i])
      }
    }
    
    #fitPageMPPanelPlot
    if(is.null(input$selectInputRunModelPreviousModels) || input$selectInputRunModelPreviousModels == ""){
      showNotification("Please fit a model before.", type="warning")
      return()
    }
    
    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    
    sampled_model <- pIDM$get.calculated_stan_object()
    posterior <- as.array(sampled_model)
    res <- gsub("<i>||</i>","",res)
    
    baysisModel <- pIDM$get.selected_BAYSIS_stan_model() 
    res <- baysisModel$matchAuxiliaryNames(res)
    
    # c("Intervals","Areas","Density","Density overlay","Histogram","Violin")
    # c("None","Pseudo log")
    type <- isolate(input$selectionMPType)
    scale <- isolate(input$selectionMPTypeXScale)
    inner <- isolate(input$innerHDIValue)
    outer <- isolate(input$outerHDIValue)
    point <- tolower(isolate(input$selectionMPPointEst))

    gg <- switch(type, 
                 Intervals=bayesplot::mcmc_intervals(posterior, pars=res, prob=inner, prob_outer=outer, point_est=point),
                 Areas=bayesplot::mcmc_areas(posterior, pars=res, prob=inner, prob_outer=outer, point_est=point), 
                 Density=bayesplot::mcmc_dens(posterior, pars=res), 
                 'Density overlay'=bayesplot::mcmc_dens_overlay(posterior, pars=res), 
                 Histogram=bayesplot::mcmc_hist(posterior, pars=res), 
                 Violin=bayesplot::mcmc_violin(posterior, pars=res))
    
    if(type != "Violin" && scale=="Pseudo log") gg <- gg + scale_x_continuous(trans=scales::pseudo_log_trans())

    plot_id_tmp <- dataModel$getDataModelPlotModel()$getNextId("mp")
    plot_name <- paste0("Plot ", plot_id_tmp)
    
    # Save plot to data model
    dataModel$getDataModelPlotModel()$add.plot_history("mp", plotname=plot_name, id=plot_id_tmp, 
                                                       plot=gg, mpInfo=list(name=pIDM$get.name(), type=type))
  })
  
  #Change button style depending on existing plots to report/download
  observeEvent(input$fitPageMPPanelPlot, ignoreNULL = F, {
    if(!is.null(input$fitPageMPPanelPlot)){
      shinyjs::addClass(id = "reportMP", "btn-primary")
      shinyjs::addClass(id = ns_mp("download"), "btn-primary") 
    }else{
      shinyjs::removeClass(id = "reportMP", "btn-primary")
      shinyjs::removeClass(id = ns_mp("download"), "btn-primary")
    }
  })
  
  #Report MP plot
  observeEvent(input$reportMP, {
    
    plot <- dataModel$getDataModelPlotModel()$get.plot_by_name("mp", input$fitPageMPPanelPlot)
    
    inputName <- input$fitPageMPPanelPlot
    inputName <- str_replace_all(inputName, " ", "_")
    
    ggsave(paste0(report_folder, "/Thumbnails/mp_",inputName,".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    tEnum <- reportTypeEnum()
    
    #Add element to report progress
    #pDIM_id=-1 -1 for reported items that are not related to a certain pDIM
    #There are no different dataModels, so that the csv name is used instead
    
    pIDM <- dataModel$get.perIterationDataModel(input$selectInputRunModelPreviousModels)
    
    reportDiv <- reportType(div=list(plot=plot))
    
    addItem(moduleType = "evaluation",
            dataModel_id=pIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=pIDM$get.id(),  
            pDIM_name = pIDM$get.name(),
            imgFile=paste0("Report/Thumbnails/mp_",inputName,".jpg"), 
            type=tEnum$mp, object=list(div=reportDiv, latex=plotToTex(paste0("mp_",inputName), plot, caption=NULL)), 
            singleton=F, show=T, global_reportProgressModel=global_reportProgressModel)
  })
  
  
  #Remove current plot
  observeEvent(input$removeMP, {
    remove <- input$fitPageMPPanelPlot
    if(is.null(remove) || remove == ""){
      showNotification("There is nothing to close.", type="warning")
      return()
    }
    dataModel$getDataModelPlotModel()$remove.plot_history("mp",remove)
  })

  #Download current MP plot
  observeEvent(input[[ns_mp("download")]], {
    
    if(is.null(input$fitPageMPPanelPlot)){
      showNotification("There is no plot to download.", type ="warning")
      return()
    }
    
    showModal(downloadPlotModalUI("modelValidationMP", input$fitPageMPPanelPlot))
  })
  downloadPlotServer(id="modelValidationMP", 
                     gInput=input,
                     dataModel=dataModel,
                     plotType="mp",
                     plotName="fitPageMPPanelPlot")
  
  

  shownMPPlotsIds <- reactiveVal(c())
  #Show plots
  observe({
    dMPM <- dataModel$getDataModelPlotModel()
    dMPM$dependReactiveValue("plot_history_mp")
  
    isolate({
      
      plots <- dMPM$get.plot_history("mp")
      
      shownMPPlotsIds_new <- c()
      add_ids <- c()

      for(pp_id in seq_along(plots)){
        pp <- plots[[pp_id]]
        #add
        if(!pp$id %in% shownMPPlotsIds()){
          
          # Create new tab for new plot in tab panel
          appendTab(
            inputId = "fitPageMPPanelPlot", 
            select = T,
            tab = tabPanel(
              title = paste0("Plot ", pp$id),
              value = paste0("Plot ", pp$id),
              h5(pp$mpInfo$name, style="text-align:center;"),
              pp$tabTitle,
              plotOutput(outputId = paste0("fitModelTabMPPanelPlot", pp$id)),
              if(pp$mpInfo$type == "Violin") tags$div("Note: The violin plot shows distributions per Markov chain (4 by default, if not changed under the 'Sampling parameters').") else tags$div()
            )
          )
          
          add_ids <- c(add_ids, pp_id)
        }
        shownMPPlotsIds_new <- c(shownMPPlotsIds_new, pp$id)
      }
      
      sapply(add_ids, function(i){
        # Render selected type of Plot
        output[[paste0("fitModelTabMPPanelPlot", plots[[i]]$id)]] <- renderPlot(plots[[i]]$plot)
      })
      
      #remove
      ids <- shownMPPlotsIds()
      for(id in ids){
        if(!id %in% shownMPPlotsIds_new){
          selected <- dMPM$get.next_plot("mp", id=id)
          selected <- paste0("Plot ", selected)
          updateTabsetPanel(session, "fitPageMPPanelPlot", selected = selected)
          removeTab(inputId = "fitPageMPPanelPlot", paste0("Plot ", id))
        }
      }
      
      shownMPPlotsIds(shownMPPlotsIds_new)
    })
  })
  
}



## Update selectInput of fitted models
# The models are grouped by the used response variable
updateFittedModels <- function(input, output, session, dataModel, 
                               selected = NULL){
  ret <- dataModel$get.perIterationDataModelNames()

  updateSelectInput(session = session, inputId = "selectInputRunModelPreviousModels", choices = ret$res, selected = selected)
  ns_predictionTab <- NS("predictionTab")
  ns_reportTool <- NS("reportTool")
  
  # Update also the dropdown menu of quantification tab
  updateSelectInput(session = session, inputId = ns_predictionTab("selectModelFit"), choices = ret$res, selected = selected)
  retReport <- list("Fits"="")
  
  if(length(ret$res) != 0){
    for(i in length(ret$res)) retReport <- list.append(retReport, ret$res[[i]], names(ret$res)[i])
  }
  updateSelectInput(session = session, inputId = ns_reportTool("filterByFits"), choices = retReport)
}



## stanPara is a list, containing the number of iterations and the number of chains
validateStanOutput <- function(text, stanPara){
  steps <- 1.0 / stanPara$chains / stanPara$iterations
  
  ## Get text index of the first highest chain number (index_second_part)
  matches_indices <- gregexpr("chain [0-9]+", text, ignore.case = T)
  if(matches_indices[[1]][1] == -1) return(0) 
  matches <- regmatches(text,matches_indices)
  greatest_chain <- max(as.numeric(substring(matches[[1]], 7)))
  index_second_part <-  regexpr(paste0("Chain ", greatest_chain), text, ignore.case = T)[1]
  
  
  ## Get heighest iteration number of the string after index_second_part
  sub_text <- substring(text, index_second_part)
  matches_indices_iter <- gregexpr(paste0("[0-9]+ / ", stanPara$iterations), sub_text, ignore.case = T)
  greatest_iter <- 0 
  if(matches_indices_iter[[1]][1] != -1){
    matches_iter <- regmatches(sub_text,matches_indices_iter)
    greatest_iter <- max(as.numeric(regmatches(matches_iter[[1]],regexpr("[0-9]+", matches_iter[[1]]))))
  }
  
  
  ## Calculate progress
  # Extract chain number out of greatest_chain
  greatest_chain
  greatest_iter
  
  return((((greatest_chain-1) * stanPara$iterations) + greatest_iter) * steps)
}


## Creates UI elements and their content for a model fit
createModelFitResults <- function(input, output, session, iterationDataModel,
                                  global_reportProgressModel){
  
  withProgress(message = 'Creating result view', value = 0, expr = {
    
    sampled_model <- iterationDataModel$get.calculated_stan_object()
    dMID <- iterationDataModel$getDataModelInputData()

    usedVars <- unique(c(iterationDataModel$get.selected_BAYSIS_stan_model()$get_used_vars(extras=T, response=T)))
    data <- dMID$getLongFormatVariable(usedVars, completeCases=T)
    response <- dMID$getResponseVariable()
    responseChar <- response$type
    responseName <- response$variable
    cEnum <- characteristicEnum()
    
    if(class(sampled_model)[1] == "stanreg" || class(sampled_model)[1] == "brmsfit"){
      min_draws <- floor(iterationDataModel$get.number_iterations()/2*iterationDataModel$get.number_chains())
      
      
      # Increase progressbar
      setProgress(value = 0.1)
      
      # loo <- rstanarm::loo(sampled_model)
      # Increase progressbar
      # setProgress(value = 0.6)
      
      # pareto_res <- pareto_k_table(loo)
      posterior <- as.array(sampled_model)
      if(class(sampled_model)[1] == "brmsfit"){
        posterior <- as.array(sampled_model)
        d <- dim(posterior)
        posterior <- posterior[1:d[1], 1:d[2], 1:(d[3]-2)]
      }
      
      # PPC Plot view
      # Draw also red shade for ppc discrepancies
      min_draws <- floor(iterationDataModel$get.number_iterations()/2*iterationDataModel$get.number_chains())

      numberDraws <- min(400,min_draws)
      numberSteps <- 100

      
      y <- data[[responseName]]
      
      #adapt numberSteps when discrete data is used
      # if(dMID$getResponseVariable()$type == "Discrete"){
      #   steps <- (max(y) - min(y))+1
      #   numberSteps <- min(steps, numberSteps)
      # }

      y_rep <- NULL
      y_rep <- tryCatch({
        iterationDataModel$get.selected_BAYSIS_stan_model()$make_predictions(stanObject=sampled_model,
                                                                          data=NULL, draws = numberDraws)
      },
      error=function(cond){
        return(NULL)
      })

      if(is.null(y_rep)){
        showNotification("An error has occurred, the data is too large.", type="error")
        return()
      }

      method <- "kde" 
      isDiscrete <- dMID$getResponseVariable()$type == "Discrete"
      res_dev <- calculatePPCDeviation(y=y, y_rep=y_rep, numberSteps=numberSteps, numberDraws=numberDraws,
                                       discrete=isDiscrete, approx.method=method)

      gg <- bayesplot::ppc_dens_overlay(y, y_rep[1:min(100,min_draws),])
      gg <- gg + iterationDataModel$get.selected_BAYSIS_stan_model()$plot_scale(x=T)
      
      thresholdWarning <- plogis(1.5) #zscore of 1.5
      thresholdError <- plogis(2) #zscore of 2
      gg_new <- addPPCDeviationToPlot(gg=gg, res=res_dev, thresholdWarning=thresholdWarning)
      gg <- gg_new
      
      
      output$runModelPlotPPC <- renderPlot(gg)

      # Save ppc plot to pIDM
      iterationDataModel$set.ppc_plot(gg)
      iterationDataModel$set.ppc_plot_warnings(dim(res_dev)[1] > 0)
      
      # Increase progressbar
      setProgress(value = 0.7)

      
      
      # Term Summary view
      output$runModelVariableSummaryPlot <- renderPlot({
        bayesplot::mcmc_intervals(posterior)
      })
      
      output$runModelVariableSummaryTable <- DT::renderDataTable({
        
        post <- posterior_interval(sampled_model, prob = 0.95)
        if("brmsfit" %in% class(sampled_model)){
          var_names <- dimnames(posterior)$variable
          post <- post[1:(dim(post)[1]-2),]
        }else{
          var_names <- dimnames(posterior)$parameters
        }
        
        
        median <- sapply(1:length(var_names), function(i){ median(posterior[,,var_names[i]]) })
        table <- data.frame(stringsAsFactors = F, Variable = var_names, 
                            lesserZero = rep("",length(var_names)), 
                            greaterZero = rep("",length(var_names)), 
                            pi= rep(0,length(var_names)), 
                            sign= rep(0,length(var_names)))
        for(var_name in var_names){
          x0 <- mean(posterior[,, var_name] > 0)
          table[table$Variable == var_name,]$lesserZero <- format(1-x0, digits=3)
          table[table$Variable == var_name,]$greaterZero <- format(x0, digits=3)
          if(x0 > 0.5){
            table[table$Variable == var_name,]$pi <- PI.value(posterior[,, var_name])
            table[table$Variable == var_name,]$sign <- "+"
          }else{
            table[table$Variable == var_name,]$pi <- PI.value(posterior[,, var_name])
            table[table$Variable == var_name,]$sign <- "-"
          }
        }
        colnames(table) <- c("Variables","p(effect<0)","p(effect>0)","pi","sign")
        res <- cbind(first = post[,1], Median = median, last = post[,2], pi = table$pi, sign=table$sign)
        # print1("res",res)
        colnames(res) <- c("2.5%","Median","97.5%","pi","sign")

        datatable(
          res, 
          selection="none",
          options=list(paging=F,searching=F,info=F,ordering=T,
                       columnDefs = list(list(className = "dt-left", targets = "_all")))) %>% 
          formatRound(columns=c('2.5%', 'Median','97.5%'), digits=3) %>% 
          formatRound(columns=c('pi'), digits=2) %>% 
          formatStyle("pi", 
                      color=styleInterval(
                        cuts=c(0.6,0.7,0.8,0.9),
                        values=c(BAYAS_COLORS$`--tb-cell-colors-1`, 
                                 BAYAS_COLORS$`--tb-cell-colors-2`,
                                 BAYAS_COLORS$`--tb-cell-colors-3`,
                                 BAYAS_COLORS$`--tb-cell-colors-4`,
                                 BAYAS_COLORS$`--tb-cell-colors-5`)))
      })

      # Increase progressbar
      setProgress(value = 0.8)

      
      # PPC Tab
      if(responseChar == cEnum$Continuous){
        updateSelectInput(session, "selectionPPCType", choices=c("Density overlay", "Interval", "Histogram", "Frequency polygon", "Violin grouped"),selected="Density overlay")
      }else{
        updateSelectInput(session, "selectionPPCType", choices=c("Density overlay", "Interval", "Histogram", "Frequency polygon", "Violin grouped", "Bars"),selected="Density overlay")
      }
      otherVars <- dMID$getOtherVariables()
      otherVars <- otherVars$variable[otherVars$type == cNum$Categorical]
      updateSelectizeInput(session, "selectionPPCGroup", choices=otherVars)

      
      # Marginal Posterior Tab
      output$treeMPVars <- renderTree({
        getTreeElements(iterationDataModel$get.selected_BAYSIS_stan_model()$getTermCombinationsOfAllModelElements())
      })
      
      
      # Increase progressbar
      setProgress(value = 0.9)
      
      
      # Model validation checklist
      thresholdWarning <- plogis(1.5) #zscore of 1.5
      thresholdError <- plogis(2) #zscore of 2
      initModelValidationPreview(input, output, session, iterationDataModel, res_dev$value,
                                 thresholdWarning=thresholdWarning, thresholdError=thresholdError)
      
      # Report preview ppc
      plot <- iterationDataModel$get.ppc_plot()
      
      inputName <- iterationDataModel$get.name()
      inputName <- str_replace_all(inputName, " ", "_")
  
      subfolder <- paste0(report_folder, "/Thumbnails")
      if(!dir.exists(subfolder)) dir.create(subfolder)
      
      ggsave(paste0(report_folder, "/Thumbnails/ppc_preview_",inputName,".jpg"), 
             plot, device="jpeg", width=100, height=100, units="px", dpi=25)
      tEnum <- reportTypeEnum()
      
      reportDiv <- reportType(div=list(plot=plot))
      
      caption <- NULL
      if(iterationDataModel$get.ppc_plot_warnings()) caption <- readTxtFile(paste0(report_folder,"/GeneralTex/PPC_preview_caption.txt"))
      
      #Add PPC element to report progress
      #pIDM_id=-1 -1 for reported items that are not related to a certain pIDM
      #There are no different dataModels, so that the csv name is used instead
      addItem(moduleType = "evaluation",
              dataModel_id=dMID$getCurrentDataPath(), pDIM_id=iterationDataModel$get.id(),  
              pDIM_name = iterationDataModel$get.name(),
              imgFile=paste0("Report/Thumbnails/ppc_preview_",inputName,".jpg"), 
              type=tEnum$previewppc, object=list(div=reportDiv, latex=plotToTex(paste0("ppc_preview_",inputName), plot, caption=caption)), 
              singleton=T, show=F, global_reportProgressModel=global_reportProgressModel,
              recommended = T)
     
      # Report model validation
      imageOutputFolder <- paste0(report_folder, "/Thumbnails/ppc_modelValidation_",inputName,".png")
      imageOutputFolder2 <- paste0("Report/Thumbnails/ppc_modelValidation_",inputName,".png")
      modelValItem <- modelValidationItem(iterationDataModel, res_dev, imageOutputFolder)

      thresholdWarning <- plogis(1.5) #zscore of 1.5
      thresholdError <- plogis(2) #zscore of 3
      div <- initModelValidationPreview(input, output, session, iterationDataModel, 
                                        res_dev$value, returnDiv = T, thresholdWarning=thresholdWarning, thresholdError=thresholdError)
      
      tmpReportItemValidation <<- list(dataModel_id=dMID$getCurrentDataPath(),
                                       pDIM_id=iterationDataModel$get.id(),
                                       pDIM_name = iterationDataModel$get.name(),
                                       imgFile=imageOutputFolder2,
                                       object=list(div=div, latex=modelValItem, add=list(res_dev=res_dev)))
      
      reportDiv <- reportType(div=list(ui=div))
      
      #Add model validation element to report progress
      #pIDM_id=-1 -1 for reported items that are not related to a certain pIDM
      #There are no different dataModels, so that the csv name is used instead
      addItem(moduleType = "evaluation",
              dataModel_id=dMID$getCurrentDataPath(), pDIM_id=iterationDataModel$get.id(),
              pDIM_name = iterationDataModel$get.name(),
              imgFile=imageOutputFolder2,
              type=tEnum$validation, object=list(div=div, latex=modelValItem, add=list(res_dev=res_dev)),
              singleton=T, show=F, global_reportProgressModel=global_reportProgressModel,
              recommended = T)
      
      # Increase progressbar
      setProgress(value = 1)

    }else{
      print("Not that class")
      print(class(sampled_model))
    }
    
    
    # Model Validation sub tab 'Sampling quantities'
    output$runModelModelValidationTabSQ <- renderDT({
      
      
      n_eff_min <- iterationDataModel$get.number_chains()*100
      n_eff_max <- floor(iterationDataModel$get.number_chains()*iterationDataModel$get.number_iterations()/2)
      
      fit <- extract_stanfit(iterationDataModel$get.calculated_stan_object())
      monitor <- as.data.frame(monitor(fit))
      
      para_names <- names(posterior[1,1,])
      monitor <- monitor[para_names,]
      
      
      ess_b <- monitor$Bulk_ESS
      ess_t <- monitor$Tail_ESS
      rhat <- round(monitor$Rhat,3)
      
      tmp <- data.frame(Parameter=para_names, ESS_bulk = ess_b, ESS_tail = ess_t, R_hat = rhat)
      
      fun_color_range <- colorRampPalette(c(BAYAS_COLORS$`--samplingESS-colors-1`,
                                            BAYAS_COLORS$`--samplingESS-colors-2`)) 
      my_colors_red <- fun_color_range(5)  
      fun_color_range <- colorRampPalette(c(BAYAS_COLORS$`--samplingESS-colors-2`,
                                            BAYAS_COLORS$`--samplingESS-colors-4`)) 
      my_colors_green <- fun_color_range(6)
      cuts_red <- seq(0,n_eff_min,length=6)[2:6]
      cuts_green <- seq(n_eff_min,n_eff_max,length=6)[2:6]
      
      fun_color_range <- colorRampPalette(c(BAYAS_COLORS$`--samplingESS-colors-4`,
                                            BAYAS_COLORS$`--samplingESS-colors-3`,
                                            BAYAS_COLORS$`--samplingESS-colors-2`,
                                            BAYAS_COLORS$`--samplingESS-colors-1`))
      my_colors_rhat <- fun_color_range(4)
      cuts_rhat <- c(1.001,1.01,1.05)
      
      if(is.null(tmp)) return()
      pageLength <- 10
      pageStart <- 0
      datatable(tmp,
                editable=list(target = "cell", disable = list(columns = 0)),
                filter="bottom",
                options=list(ordering=T, lengthMenu=c(5,10,20), pageLength=pageLength, displayStart=pageStart, #, stateSave =T
                             columnDefs = list(list(orderable=TRUE, targets=0)))) %>% formatStyle(
                               'ESS_bulk',
                               color = styleInterval(c(cuts_red,cuts_green), c(my_colors_red, my_colors_green))
                             ) %>% formatStyle(
                               'ESS_tail',
                               color = styleInterval(c(cuts_red,cuts_green), c(my_colors_red, my_colors_green))
                             ) %>% formatStyle(
                               'R_hat',
                               color = styleInterval(cuts_rhat,my_colors_rhat)
                             )
    }, server=F) 
    
    #Model parameters for pairs tree
    output$treePairsVars <- renderTree({
      getTreeElements(iterationDataModel$get.selected_BAYSIS_stan_model()$getTermCombinationsOfAllModelElements())
    })
    
    
    #Model prior predictive check
    cPIDM <- iterationDataModel
    used_opt_terms <- cPIDM$get.used_optional_terms()
    
    # Read sampling parameters
    stanParameters <- list(
      iterations = cPIDM$get.number_iterations(),
      chains = cPIDM$get.number_chains(),
      cores = cPIDM$get.number_cores(),
      adapt_delta = cPIDM$get.adapt_delta(),
      max_treedepth = cPIDM$get.max_treedepth(),
      seed = cPIDM$get.seed()
    )
    baysisModel <- cPIDM$get.selected_BAYSIS_stan_model()
    
    content <- baysisModel$get_content_for_stan_code(response = responseName, 
                                                     data = data, 
                                                     stanParameter = stanParameters)
    
    #Required for the update function. Otherwise the call object in sampled_model will use an older content.
    assign("content", content, envir = .GlobalEnv)
    
    sampled_model <- cPIDM$get.calculated_stan_object()
    model_prior <- NULL
    if(!is.null(content$class) && content$class == "brms"){
      model_prior <- update(sampled_model, sample_prior="only")
    }else{
      model_prior <- update(sampled_model, prior_PD = TRUE, refresh = -1, chains = 4)
    }
    
    y_rep <- cPIDM$get.selected_BAYSIS_stan_model()$make_predictions(
      stanObject=model_prior,
      data=NULL, draws = 50)

    dMID <- iterationDataModel$getDataModelInputData()
    resp_name <- dMID$getResponseVariable(onlyName=T)
    
    y <- data[[resp_name]]

    
    plot <- bayesplot::ppc_dens_overlay(y, y_rep) +
      baysisModel$plot_scale(x=T)

    cPIDM$set.priorPredictiveCheckPlot(plot)
    
    inputName <- iterationDataModel$get.name()
    
    ggsave(paste0(report_folder, "/Thumbnails/priorpc_", inputName, ".jpg"), 
           plot, device="jpeg", width=100, height=100, units="px", dpi=25)
    

    #Add report prior predictive check  to recommended report progress
    tEnum <- reportTypeEnum()
    
    reportDiv <- reportType(div=list(plot=plot))
    
    addItem(moduleType = "evaluation",
            dataModel_id=cPIDM$getDataModelInputData()$getCurrentDataPath(), pDIM_id=cPIDM$get.id(), 
            pDIM_name = cPIDM$get.name(),
            imgFile = paste0("Report/Thumbnails/priorpc_",inputName,".jpg"),
            type=tEnum$priorpc, object=list(div=reportDiv, latex=plotToTex(paste0("priorpc_",inputName), plot, caption=NULL)), 
            show=F, singleton=T, global_reportProgressModel=global_reportProgressModel,
            recommended=T)
    
    output$runModelModelValidationTabPriorPredictiveCheck <- renderPlot({
      
      if(is.null(input$selectInputRunModelPreviousModels)) return(NULL)
      
      plot <- iterationDataModel$get.priorPredictiveCheckPlot()
      if(is.null(plot)){
        removeClass("reportPriorPC", "btn-primary")
      }else{
        addClass("reportPriorPC", "btn-primary")
      }
      plot
    })
    
    
    #Model parameters for pairs tree
    output$treePVPVars <- renderTree({
      getTreeElements(iterationDataModel$get.selected_BAYSIS_stan_model()$getTermCombinationsOfAllModelElements())
    })
    
    
    # Increase progressbar
    setProgress(value = 1)

  })
  
}


initModelValidationPreview <- function(input, output, session, iterationDataModel, ppc_res,
                                       returnDiv=F, thresholdWarning=0.25, thresholdError=0.5){
  
  sampled_model <- iterationDataModel$get.calculated_stan_object()
  # sampled_model <- samp
  
  stanfit <- extract_stanfit(sampled_model)
  
  monitor <- invisible(monitor(stanfit))
  sampler_params <- get_sampler_params(stanfit, inc_warmup = F)
  div_trans <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  div_trans_max <- floor(iterationDataModel$get.number_chains()*iterationDataModel$get.number_iterations()/2)
  treedepth_max <- iterationDataModel$get.max_treedepth()
  treedepth <- sum(sapply(sampler_params, function(x) ifelse(x[, "treedepth__"] == treedepth_max,1,0)))
  

  ## Content for checklist
  # General information
  dMID <- iterationDataModel$getDataModelInputData()
  baysisModel <- iterationDataModel$get.selected_BAYSIS_stan_model()
  usedVars <- unique(c(baysisModel$get_used_vars(extras=T, response=T)))
  data <- dMID$getLongFormatVariable(usedVars, completeCases=T)
  data_full <- dMID$getLongFormatVariable(usedVars, completeCases=F)

  contentMessage0 <- "No warnings about the data"
  check0 <- "check"
  if(dim(data)[1] != dim(data_full)[1]){
    check0 <- "warning"
    contentMessage0 <- "Missing values!"
  }
  
  
  # ESS
  n_eff_min <- iterationDataModel$get.number_chains()*100
  n_eff_bulk <- monitor$Bulk_ESS[monitor$Bulk_ESS < n_eff_min]
  n_eff_tail <- monitor$Tail_ESS[monitor$Tail_ESS < n_eff_min]
  contentMessage1 <- "The ESS for each parameter is OK!"
  check1 <- "check"
  if(length(n_eff_bulk) > 0){
    check1 <- "error"
    contentMessage1 <- "Bulk Effective Samples Size is too low, indicating posterior means and medians may be unreliable."
  }else if(length(n_eff_tail) > 0){
    check1 <- "warning"
    contentMessage1 <- "Tail Effective Samples Size is too low, indicating posterior variances and tail quantiles may be unreliable."
  }

  #Rhat
  rhat <- monitor$Rhat
  rhat_sub <- rhat[c(1:(length(rhat)-2))]
  rhat_sub_err <- rhat[rhat_sub>1.01]
  rhat_sub_err2 <- rhat[rhat_sub>1.1]
  contentMessage2 <- "All rhat values are ok (<1.01)!"
  check2 <- "check"
  if(length(rhat_sub_err2) > 0){ #greater than 1.1
    contentMessage2 <- paste0("There are " , length(rhat_sub_err2), " case(s) where the r_hat value is greater than 1.1!")
    check2 <- "error"
  }else if(length(rhat_sub_err) > 0){
    if(length(rhat_sub_err)/length(rhat_sub) >= 0.1){ # more then 10% from parameter
      contentMessage2 <- paste0("There are " , length(rhat_sub_err), " case(s) where the r_hat value is greater than 1.01!")
      check2 <- "error"
    }else{
      contentMessage2 <- paste0("Some parameters (", length(rhat_sub_err) ,") have a rhat value greater than 1.01!")
      check2 <- "warning"
    }
  }
  
  #Divergent transitions
  contentMessage3 <- "There are no divergences!"
  check3 <- "check"
  if(div_trans/div_trans_max >= 0.01){
    contentMessage3 <- paste0(div_trans, " of ", div_trans_max , " iterations ended with a divergence!")
    check3 <- "error"
  }else if(div_trans/div_trans_max > 0){
    contentMessage3 <- paste0(div_trans, " of ", div_trans_max , " iterations ended with a divergence!")
    check3 <- "warning"
  }
  
  #Treedepth
  contentMessage4 <- "No iteration exceeded the maximum tree depth!"
  check4 <- "check"
  if(treedepth/div_trans_max >= 0.05){
    contentMessage4 <- paste0(treedepth, " of ", div_trans_max , " exceeded the maximum tree depth of ", treedepth_max ,"!")
    check4 <- "error"
  }else if(treedepth/div_trans_max > 0){
    contentMessage4 <- paste0(treedepth, " of ", div_trans_max , " exceeded the maximum tree depth of ", treedepth_max ,"!")
    check4 <- "warning"
  }
  
  #PPC
  contentMessage5 <- "The PPC seems to be OK!"
  check5 <- "check"
  if(length(ppc_res[ppc_res>thresholdError]) > 0){
    contentMessage5 <- "It seems that there are discrepancies within the PPC!"
    check5 <- "error"
  }else if(length(ppc_res[ppc_res>thresholdWarning]) > 0){
    contentMessage5 <- "It seems that there are slight discrepancies within the PPC!"
    check5 <- "warning"
  }
  
  ## Content for question marks
  contentMark0 <- tooltip$checklistGeneralInformation
  contentMark1 <- tooltip$checklistESS
  contentMark2 <- tooltip$checklistRhat
  contentMark3 <- tooltip$checklistDivTrans
  contentMark4 <- tooltip$checklistTreeDepth
  contentMark5 <- tooltip$checklistPPC
  contentMark6 <- tooltip$checklistQuantities
  
  first <- isolate(firstAnimation())

  if(returnDiv) first=F
  
  t0 <- 350
  tStep <- 350
  
  div <- tags$div(
    style="margin-top:15px; margin-left:15px;",
    uiChecklist(check0, "check0", "Data", contentMessage0, "Data", contentMark0, first, t0+(tStep*0)),
    uiChecklist(check1, "check1", "Effective sample size", contentMessage1, "ESS", contentMark1, first, t0+(tStep*1)),
    uiChecklist(check2, "check2", "Convergence (R-hat)", contentMessage2, "R-hat", contentMark2, first, t0+(tStep*2)),
    uiChecklist(check3, "check3", "Divergent transitions", contentMessage3, "Divergent transitions", contentMark3, first, t0+(tStep*3)),
    uiChecklist(check4, "check4", "Tree depth", contentMessage4, "Exceeding max treedepth", contentMark4, first, t0+(tStep*4)),
    uiChecklist(check5, "check5", "Posterior predictive check", contentMessage5, "PPC", contentMark5, first, t0+(tStep*5)),
    uiChecklist("check", "check6", "Model quantities", "Have a look at the 'Marginal posteriors' plot and also at the table 'Summary of marginal posteriors'!", "Quantities",
                contentMark6, first, t0+(tStep*6))
  )
  
  if(returnDiv){
    return(div)
  }else{
    output$runModelVerbalResult <- renderUI({
      
      ret <- tags$div(
        div,
        tags$div(style="margin-top:20px;","For more information go to the ", 
                 actionLink("goToModelValidationTab", "Model validation"), " tab.")
      )
      
      #Better placement?

      delay(t0+(tStep*0),shinyjs::show("check0", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*1),shinyjs::show("check1", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*2),shinyjs::show("check2", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*3),shinyjs::show("check3", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*4),shinyjs::show("check4", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*5),shinyjs::show("check5", anim=T, animType="fade", time=0.5))
      delay(t0+(tStep*6),shinyjs::show("check6", anim=T, animType="fade", time=0.5))
      
      return(ret)
    })
    
    observeEvent(input$goToModelValidationTab, {
      updateTabsetPanel(session, inputId = "runModeltabsetPanel", selected = "Model validation")
    })
  }
  

}


## Tree view for the used mp
getTreeElements <- function(combinationsOfAllModelElements){
  ret <- combinationsOfAllModelElements
  treeInput <- list()
  names <- names(ret)
  for(i in 1:length(ret)){
    suppressWarnings(if(any(is.na(ret[[i]]))){
      treeInput <- list.append(treeInput,structure(NA, stselected=TRUE),names[i])
    }else{
      sublist <- list()
      for(j in ret[[i]]){
        name <- j$selectInput
        sublist <- list.append(sublist, NA, name)
      }
      treeInput <- list.append(treeInput,structure(sublist, stselected=TRUE),names[i])
    })
  }
  return(treeInput)
}



## Calculate ppc deviations
calculatePPCDeviation <- function(y, y_rep, numberSteps, numberDraws, discrete, 
                                  approx.method=c("ecdf", "kde")){
  
  if(discrete){
    numberSteps <- min(numberSteps,(max(c(y,y_rep)-min(c(y,y_rep))+1)))
  }
  steps <- seq(min(c(y,y_rep)), max(c(y,y_rep)), length.out=numberSteps)
  
  cdf_y <- function() return(0)
  if(approx.method=="ecdf"){
    #Empirical Cumulative Distribution Function
    cdf_y <- ecdf(y)
  }else{
    #Kernel Density Estimation
    kde_y <- density(y)
    ckde_y <- cumsum(kde_y$y) * diff(kde_y$x[1:2])
    cdf_y <- approxfun(kde_y$x, ckde_y, yleft=0, yright=1)
  }
  
  fun_reps <- sapply(1:numberDraws, function(i) {
    if(approx.method=="ecdf"){
      #Empirical Cumulative Distribution Function
      cdf_y_rep <- ecdf(y_rep[i,])
    }else{
      #Kernel Density Estimation
      kde_y_rep <- density(y_rep[i,])
      ckde_y_rep <- cumsum(kde_y_rep$y) * diff(kde_y_rep$x[1:2])
      cdf_y_rep <- approxfun(kde_y_rep$x, ckde_y_rep, yleft=0, yright=1)
    }
  })
  
  cdf_y_val <- cdf_y(steps)
  cdf_y_rep_val <- sapply(seq_len(numberDraws), function(i) fun_reps[[i]](steps))
  
  binWidth <- steps[2]-steps[1]
  z_res <- sapply(seq_len(numberSteps-1), function(i){
    numerator <- (((cdf_y_val[i+1]-cdf_y_val[i])/binWidth)-mean((cdf_y_rep_val[i+1,]-cdf_y_rep_val[i,])/binWidth))
    if(numerator == 0) return(0)
    denominator <- sd((cdf_y_rep_val[i+1,]-cdf_y_rep_val[i,])/binWidth)
    if(denominator == 0) return(Inf)
    return(numerator/denominator)
  })
  res_dev <- plogis(abs(z_res))
  res_dev_norm <- (res_dev - 0.5)/ 0.5

  
  # plot(res_dev ~ steps[2:numberSteps])
  # plot(res_dev_norm ~ steps[2:numberSteps])
  
  ret <- data.frame(cuts = steps, value=c(res_dev_norm, 0))
  return(ret)
}

#Decorate plot
addPPCDeviationToPlot <- function(gg, res, thresholdWarning=0.9, adjust=T){

  gg_new <- gg
  
  gg_build <- ggplot_build(gg_new)
  y_max <- max(c(gg_build$data[[1]]$y),gg_build$data[[2]]$y)
  step <- res$cuts[2] - res$cuts[1]
  
  colors <- colorRampPalette(c(BAYAS_COLORS$`--modelCreatingPlot-color-values-4`, 
                               BAYAS_COLORS$`--modelCreatingPlot-color-values-3`))(20)
  
  res_sub <- res[res$value > thresholdWarning,]
  if(adjust){
    res_sub$value_norm <- (res_sub$value-thresholdWarning)/(1-thresholdWarning)
  }
  if(dim(res_sub)[1] > 0){
    res_sub$ymin <- 0
    res_sub$ymax <- y_max
    res_sub$xmin <- res_sub$cuts
    res_sub$xmax <- res_sub$cuts+step
    res_sub$fill <- colors[ceil((res_sub$value_norm)*20)]
    gg_new <- gg_new +
      geom_rect(data = res_sub, mapping=aes(ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax, fill=I(fill)),
                alpha = 0.2)
    
    gg_new <- removeUnnecessaryEnvInPlot(gg_new)
  }

  gg_new
}


