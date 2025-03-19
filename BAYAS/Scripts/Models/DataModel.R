## This is a R6 Class for holding necessary data

DataModel <- R6Class(
  classname = "DataModel", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",

    #DataModelInputData contains all information about the raw input and converted data
    dataModelInputData = NULL,
    
    #DataModelImportData contains all the information about step-by-step long format conversion.
    dataModelImportData = NULL,
    
    #DataModelPlotModel Plot history
    dataModelPlotModel = NULL,
    
    
    # loo objects
    # list of 4 elements
    # 1: path of data of dataModel (used as identifier)
    # 2: list of loo elements
    # 3: result of loo_compare
    # 4: ui table
    loo_result = NULL,
    loo_result_next_id = 0,
    
    
    # Model prediction result tabs
    # Named list of ModelPredictionResultModel objects
    # name is equal with the name from the list
    model_prediction_result_tabs = NULL,
    
    
    pIDM_next_id = 0,
    
    # It stored the first created object of pDIM
    # All other pDIM (fits) are cloned from this one 
    # and will not overwritten!
    # Holds whole information about one "iteration" all input
    # properties after uploading data till run fit
    cPerIterationDataModel = NULL,
    
    # Array of perIterationDataModel
    perIterationDataModels = NULL,
    
    selectedPIDM = NULL,
    
    # ReportProgressModel
    reportProgressModel = NULL,
    
    # In case dataModel is read from local
    load_from_local = F
  ),
  
  
  public = list(
   
    initialize = function(emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$dataModelInputData <- DataModelInputData$new(dataModel = self)
      private$dataModelPlotModel <- DataModelPlotModel$new(dataModel = self)
    },
    
    setDataModelInputData = function(dataModelInputData){
      private$dataModelInputData <- dataModelInputData
    },
    getDataModelInputData = function(){
      return(private$dataModelInputData)
    },
    
    setDataModelImportData = function(dataModelImportData){
      private$dataModelImportData <- dataModelImportData
    },
    getDataModelImportData = function(){
      return(private$dataModelImportData)
    },
    
    setDataModelPlotModel = function(dataModelPlotModel){
      private$dataModelPlotModel <- dataModelPlotModel
    },
    getDataModelPlotModel = function(dataModelPlotModel){
      return(private$dataModelPlotModel)
    },

    set.loo_result = function(x, silent=F){
     private$loo_result <- x
     if(!silent) self$triggerReactiveValue("loo_result")
    },
    get.loo_result = function(){
     return(private$loo_result)
    },
    
    get.loo_result_next_id = function(){
     private$loo_result_next_id <- private$loo_result_next_id+1
     return(private$loo_result_next_id)
    },
    
    get.pIDM_next_id = function(){private$pIDM_next_id},
    inc.pIDM_next_id = function(){private$pIDM_next_id <- private$pIDM_next_id+1},
    
    set.cPerIterationDataModel = function(x){
      private$cPerIterationDataModel <- x
    },
    get.cPerIterationDataModel = function(){
      return(private$cPerIterationDataModel)
    },
    
    set.model_prediction_result_tabs = function(x, silent=F){
     private$model_prediction_result_tabs <- x
     if(!silent) self$triggerReactiveValue("model_prediction_result_tabs")
    },
    add.model_prediction_result_tabs = function(x, name, silent=F){
      private$model_prediction_result_tabs <- list.append(private$model_prediction_result_tabs, x, name)
      if(!silent) self$triggerReactiveValue("model_prediction_result_tabs")
    },
    get.model_prediction_result_tabs = function(){
     return(private$model_prediction_result_tabs)
    },
    get.model_prediction_result_tab = function(name){
     return(private$model_prediction_result_tabs[[name]])
    },
    remove.model_prediction_result_tabs = function(name, silent=F){
      newList <- list()
      for(na in names(private$model_prediction_result_tabs)){
        if(na != name){
          newList <- list.append(newList, private$model_prediction_result_tabs[[na]], na)
        }
      }
      private$model_prediction_result_tabs <- newList
      if(!silent) self$triggerReactiveValue("model_prediction_result_tabs")
    },
    
    set.perIterationDataModels = function(x, silent = F){
     private$perIterationDataModels <- x
     if(!silent) self$triggerReactiveValue("perIterationDataModels")
    },
    add.perIterationDataModels = function(x, index = 0, silent = F){
     name <- x$get.name()
     model_names <- NULL
     if(length(private$perIterationDataModels) != 0) model_names <- sapply(1:length(private$perIterationDataModels), function(i) private$perIterationDataModels[[i]]$get.name())
     index <- match(name, model_names)
       
     if(is.na(index) || index == 0){
       private$perIterationDataModels[[length(private$perIterationDataModels)+1]] <- x
     }else{
       private$perIterationDataModels[[index]] <- x
     }
     if(!silent) self$triggerReactiveValue("perIterationDataModels")
    },
    get.perIterationDataModels = function(){
     return(private$perIterationDataModels)
    },
    get.perIterationDataModelNames = function(){

      res <- list()
      for(model in private$perIterationDataModels){
        dMID <- model$getDataModelInputData()
        
        responseName <- dMID$getResponseVariable(onlyName=T)
        res[[responseName]] <- list.append(res[[responseName]], model$get.name())
      }
      
      return(list(res=res))
    },
    
    get.perIterationDataModelNamesWithHash = function(){

      res <- list(
        name = character(0),
        nameDP = character(0),
        nameHash = character(0),
        displayName  = character(0)
      )
      for(model in private$perIterationDataModels){
        dMID <- model$getDataModelInputData()
        responseName <- dMID$getResponseVariable(onlyName=T)
        data <- dMID$getLongFormatVariable(responseName)
        responseDP <- length(data[[1]])
        responseHash <- rlang::hash(data)
        res$name <- c(res$name, responseName)
        res$nameDP <- c(res$nameDP, paste0(responseName, " (", responseDP, " dp)"))
        res$nameHash <- c(res$nameHash, paste0(responseName, " (", responseDP, " dp) (", responseHash,")"))
        res$hash <- c(res$hash, responseHash)
        res$displayName <-c(res$displayName, responseName)
      }
      
      for(i in seq_along(res$name)){
        name <- res$name
        namesDps <- res$nameDP[res$name == name]
        # hash <- res$hash[res$name == name]
        hashDps <- res$hash[res$nameDP == res$nameDP[i]]
        if(length(namesDps) > 1){
          if(length(unique(hashDps)) > 1){
            res$displayName[i] <- res$nameHash[i]
          }else{
            res$displayName[i] <- res$nameDP[i]
          }
        }
      }
      
      res2 <- list()
      for(model_id in seq_along(private$perIterationDataModels)){
        model <- private$perIterationDataModels[[model_id]]
        res2[[res$displayName[model_id]]] <- list.append(res2[[res$displayName[model_id]]], model$get.name())
      }
      
      return(list(res=res2))
    },
    
    # when response is used, name will be ignored
    get.perIterationDataModel = function(name=NULL, response = NULL, id=NULL){
     if(!is.null(name)){
       index <- NULL
       if(length(private$perIterationDataModels) > 0){
         model_names <- sapply(1:length(private$perIterationDataModels), function(i) private$perIterationDataModels[[i]]$get.name())
         index <- match(name, model_names)
       }
       return(private$perIterationDataModels[[index]])
     }else if(!is.null(response)){
       model_response <- sapply(1:length(private$perIterationDataModels), function(i){
         if(private$perIterationDataModels[[i]]$get.name() != "(Not_fitted)" &&
            private$perIterationDataModels[[i]]$getDataModelInputData()$getResponseVariable(onlyName = T) == response){
           return(private$perIterationDataModels[[i]])
         }else{
           return(NULL)
         }
       })
       model_response[sapply(model_response, is.null)] <- NULL
       return(model_response)
     }else if(!is.null(id)){
       for(model in private$perIterationDataModels){
         if(model$get.id()==id) return(model)
       }
     }
      return(NULL)
    },
    remove.perIterationDataModel = function(name, silent = F){
      model_names <- sapply(1:length(private$perIterationDataModels), function(i) private$perIterationDataModels[[i]]$get.name())
      index <- match(name, model_names)
      private$perIterationDataModels[[index]] <- NULL
      if(length(private$perIterationDataModels) >= index){
        self$setSelectedPIDM(model_names[index+1])
      }else if(length(private$perIterationDataModels) > 0){
        self$setSelectedPIDM(model_names[index-1])
      }
      #Remove also ModelPredictionResultModel
      self$remove.model_prediction_result_tabs(paste0(name, ":MP"))
      self$remove.model_prediction_result_tabs(paste0(name, ":PT"))
      if(!silent) self$triggerReactiveValue("perIterationDataModels")
    },
    
    
    setSelectedPIDM =  function(sel, silent = F){
      private$selectedPIDM <- sel
      if(!silent) self$triggerReactiveValue("selectedPIDM")
    },
    getSelectedPIDM =  function(){
      return(private$selectedPIDM)
    },
    
    set.reportProgressModel = function(model){
     private$reportProgressModel <- model
    },
    get.reportProgressModel = function(){
     return(private$reportProgressModel)
    },
    
    
    set.load_from_local = function(x){
     private$load_from_local <- x
    },
    get.load_from_local = function(){
     return(private$load_from_local)
    },
    
    
    getInstance =  function(){
      if(localUse) browser()
      warning("Not yet implemented 'getInstance' in DataModel")
    },
    
    setInstance =  function(instance){

      private$loo_result <- instance$get.loo_result()
      private$loo_result_next_id <- instance$get.loo_result_next_id()
      private$model_prediction_result_tabs <- instance$get.model_prediction_result_tabs()
      private$pIDM_next_id <- instance$get.pIDM_next_id()
      private$selectedPIDM <- instance$getSelectedPIDM()
      private$load_from_local <- instance$get.load_from_local()
      
      
      #R6 
      private$dataModelInputData$setInstance(instance$getDataModelInputData()$getInstance(dataModel=self))
      private$dataModelPlotModel$setInstance(instance$getDataModelPlotModel()$getInstance(dataModel=self))
      inst <- instance$get.cPerIterationDataModel()$getInstance(id=NULL)
      private$cPerIterationDataModel$setInstance(inst, dataModel=self, removeSelcetedBAYSISModel=T)
      private$reportProgressModel$setInstance(instance$get.reportProgressModel()$getInstance())
      
      private$perIterationDataModels <- list()
      for(aa in instance$get.perIterationDataModels()){
        private$perIterationDataModels <- list.append(private$perIterationDataModels, aa$getInstance(id=NULL))
      }
      
      self$triggerReactiveValues()
    },
    
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        dataModelInputDataState <- NULL
        if(!is.null(private$dataModelInputData)){
          dataModelInputDataState <- private$dataModelInputData$getState(nextUUID)
          nextUUID <- dataModelInputDataState$nextUUID
        }
        dataModelPlotModelState <- NULL
        if(!is.null(private$dataModelPlotModel)){
          dataModelPlotModelState <- private$dataModelPlotModel$getState(nextUUID)
          nextUUID <- dataModelPlotModelState$nextUUID
        }
        cPerIterationDataModelState <- NULL
        if(!is.null(private$cPerIterationDataModel)){
          cPerIterationDataModelState <- private$cPerIterationDataModel$getState(nextUUID)
          nextUUID <- cPerIterationDataModelState$nextUUID
        }
        reportProgressModelState <- NULL
        if(!is.null(private$reportProgressModel)){
          reportProgressModelState <- private$reportProgressModel$getState(nextUUID)
          nextUUID <- reportProgressModelState$nextUUID
        }

        perIterationDataModelsState <- list()
        for(aa in private$perIterationDataModels){
          cState <- aa$getState(nextUUID)
          perIterationDataModelsState <- list.append(perIterationDataModelsState, cState)
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          loo_result = private$loo_result,
          loo_result_next_id = private$loo_result_next_id,
          model_prediction_result_tabs = private$model_prediction_result_tabs,
          pIDM_next_id = private$pIDM_next_id,
          selectedPIDM = private$selectedPIDM,
          load_from_local = private$load_from_local,

          dataModelInputData = dataModelInputDataState,
          dataModelPlotModel = dataModelPlotModelState,
          cPerIterationDataModel = cPerIterationDataModelState,
          perIterationDataModels = perIterationDataModelsState,
          reportProgressModel = reportProgressModelState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'DataModel' = ret
      )
      return(ret)
    },
    setState = function(state){
      check <- super$checkVersion(private$stateVersion, state$stateVersion, stop=T)
      if(check$needsConversion){
        if(check$conversionPossible){
          for(func in check$conversionList){
            state <- do.call(get(func), list(state=state))
          }
        }
      }
      
      private$loo_result = state$loo_result
      private$loo_result_next_id = state$loo_result_next_id
      private$model_prediction_result_tabs = state$model_prediction_result_tabs
      private$pIDM_next_id = state$pIDM_next_id
      private$selectedPIDM = state$selectedPIDM
      private$load_from_local = state$load_from_local
      
      private$dataModelInputData <- state$dataModelInputData
      private$dataModelPlotModel <- state$dataModelPlotModel
      private$cPerIterationDataModel <- state$cPerIterationDataModel
      private$perIterationDataModels <- state$perIterationDataModels
      private$reportProgressModel <- state$reportProgressModel
    },
    resetState = function(){#
      if(!super$resetState()) return()
      if(!is.null(private$dataModelInputData)) private$dataModelInputData$resetState()
      if(!is.null(private$dataModelPlotModel)) private$dataModelPlotModel$resetState()
      if(!is.null(private$cPerIterationDataModel)) private$cPerIterationDataModel$resetState()
      if(!is.null(private$reportProgressModel)) private$reportProgressModel$resetState()
      for(aa in private$perIterationDataModels){
        aa$resetState()
      }
    }
   
  )

)

