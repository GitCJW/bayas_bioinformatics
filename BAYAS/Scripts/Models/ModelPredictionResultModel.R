## This is a R6 Class for holding necessary data for one Tool iteration.

ModelPredictionResultModel <- R6Class(
  
  classname = "ModelPredictionResultModel", 
  inherit = SerializationInterface,
  
   private = list(
     stateVersion = "0.1",
     
     type = c("prediction","effect"),
     
     
     # Name of related model
     fitName = NULL,
     
     #Name of result tab fitName+":MP"/":PT
     resultName = NULL,
     
     tabId = NULL,
     
     catChoices = NULL,
     
     numChoices = NULL,
     
     startValues = NULL,
     
     data = NULL,
     
     catValues = NULL,
     
     usedVarsAdd = NULL,
     
     pIDMId = NULL,
       
     
     # For prediction tabs
     # list of named list
     ## data: data 
     ## index: row index (unique)
     ## values: named list of predictors with their values 
     plot_history = list()
   ),
   
   
   public = list(
     
     initialize = function(type = c("prediction","effect"), fitName, resultName, tabId, 
                           catChoices, numChoices, startValues,
                           catValues, 
                           usedVarsAdd, pIDMId,
                           emptyState = F){
       super$initialize()
       if(emptyState) return()
       
       private$fitName <- fitName
       private$resultName <- resultName
       private$tabId <- tabId
       private$catChoices <- catChoices
       private$numChoices <- numChoices
       private$startValues <- startValues
       
       private$type <- type
       private$catValues <- catValues
       private$usedVarsAdd <- usedVarsAdd
       private$pIDMId <- pIDMId
     },
     
     
     set.fitName = function(name){
       private$fitName <- name
     },
     get.fitName = function(){
       return(private$fitName)
     },
     
     set.resultName = function(name){
       private$resultName <- name
     },
     get.resultName = function(){
       return(private$resultName)
     },
     
     set.tabId = function(id){
       private$tabId <- id
     },
     get.tabId = function(){
       return(private$tabId)
     },
     
     set.catChoices = function(c){
       private$catChoices <- c
     },
     get.catChoices = function(){
       return(private$catChoices)
     },
     
     set.numChoices = function(c){
       private$numChoices <- c
     },
     get.numChoices = function(){
       return(private$numChoices)
     },
     
     set.startValues = function(val){
       private$startValues <- val
     },
     get.startValues = function(){
       return(private$startValues)
     },
     
     set.data = function(data){
       private$data <- data
     },
     get.data = function(){
       return(private$data)
     },
     
     set.type = function(type){
       private$type <- type
     },
     get.type = function(){
       return(private$type)
     },
     
     set.catValues = function(catValues){
       private$catValues <- catValues
     },
     get.catValues = function(){
       return(private$catValues)
     },
     

     set.usedVarsAdd = function(usedVarsAdd){
       private$usedVarsAdd <- usedVarsAdd
     },
     get.usedVarsAdd = function(){
       return(private$usedVarsAdd)
     },
     
     set.pIDMId = function(pIDMId){
       private$pIDMId <- pIDMId
     },
     get.pIDMId = function(){
       return(private$pIDMId)
     },
     
     
     
     set.plot_history = function(hist){
       private$plot_history <- hist
     },
     add.plot_history = function(index, plot){
       plot <- removeUnnecessaryEnvInPlot(plot)
       private$plot_history <- list.append(private$plot_history, plot, index)
     },
     get.plot_history = function(){
       return(private$plot_history)
     },
     next.plot_history.color = function(){
        index <- self$next.plot_history.index()
        colors <- brewer.pal(9,"Set1")
        colors[c(1,2)] <- colors[c(2,1)]
        return(colors[((index-1)%%9)+1])
     },
     #checks, if this combination of values already exists.
     #If so, returns the index, otherwise 0
     duplicate.plot_history = function(values){
        for(i in private$plot_history){
           val <- i$values
           flag <- T
           for(n in names(val)){
              if(val[[n]] != values[[n]]) flag <- F
           }
           if(flag) return(i$index)
        }
        return(0)
     },
     next.plot_history.index = function(){
       if(length(private$plot_history)==0) return(1)
       
       indexes <- c()
       for(i in private$plot_history){
         indexes <- c(indexes, i$index)
       }
       next_index <- setdiff(1:(length(private$plot_history)+1), indexes)[1]
       
      return(next_index)
    },
    
    getInstance =  function(){
      newInstance = ModelPredictionResultModel$new(
        type = private$type,
        fitName=private$fitName, resultName=private$resultName, tabId=private$tabId, 
        catChoices=private$catChoices, numChoices=private$numChoices, startValues=private$startValues,
        catValues=private$catValues, 
        usedVarsAdd=private$usedVarsAdd, pIDMId=private$pIDMId)
      
      newInstance$set.data(private$data)
      newInstance$set.plot_history(private$plot_history)
      return(newInstance)
    },
    
    setInstance = function(instance){
      private$fitName <-  instance$get.fitName()
      private$resultName <- instance$get.resultName()
      private$tabId <- instance$get.tabId()
      private$catChoices <- instance$get.catChoices()
      private$numChoices <- instance$get.numChoices()
      private$startValues <- instance$get.startValues()
      private$data <- instance$get.data()
      private$plot_history <- instance$get.plot_history()
      
      private$type <- instance$get.type()
      private$catValues <- instance$get.catValues()
      private$usedVarsAdd <- instance$get.usedVarsAdd()
      private$pIDMId <- instance$get.pIDMId()
    },
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()

      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)

        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          fitName = private$fitName,
          resultName = private$resultName,
          tabId = private$tabId,
          catChoices = private$catChoices,
          numChoices = private$numChoices,
          startValues = private$startValues,
          data = private$data,
          plot_history = private$plot_history,
          
          type = private$type,
          catValues = private$catValues,
          usedVarsAdd = private$usedVarsAdd,
          pIDMId = private$pIDMId
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelPredictionResultModel' = ret
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
      
      private$fitName <-  state$fitName
      private$resultName <- state$resultName
      private$tabId <- state$tabId
      private$catChoices <- state$catChoices
      private$numChoices <- state$numChoices
      private$startValues <- state$startValues
      private$data <- state$data
      private$plot_history <- state$plot_history
      
      private$type = state$type
      private$catValues = state$catValues
      private$usedVarsAdd = state$usedVarsAdd
      private$pIDMId = state$pIDMId
    },
    resetState = function(){
      if(!super$resetState()) return()
    }

   )                 
)