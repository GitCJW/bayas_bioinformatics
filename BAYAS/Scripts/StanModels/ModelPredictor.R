ModelPredictor <- R6Class(
  classname = "ModelPredictor", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    is.vector = F #Is this predictor a vector, due to factors?
  ),
  
  public = list(
    name = NULL, #For intern, role of this predictor/parameter
    id = NULL, #Unique ID
    display_name = NULL, #Describing name for user
    description = NULL, #Description of the role of this predictor/parameter in the model
    optional = F, #If this predictor/parameter is optional 
    
    characteristics = NULL, #Kind of characteristic this predictor could be
    lower_limit = NULL, #Theoretical lowest value
    upper_limit = NULL, #Theoretical greatest value
    
    numberVar = NULL, #Number of predictors -> 2 e.g. is a interaction term 
    possibleAmount = NULL, #Number of possible amount, 1,2,...,VAR
    same_prio_dist = F, # Use same kind of Prior for each of this predictors
    
    modelParameter = NULL, #Parameter (factor) used for this predictor
    
    userVariable = c(), #User variable that are assigned to this predictor. A vector.
    
    initialize = function(name, id, display_name, description, optional,
                          characteristics, lower_limit, upper_limit,
                          numberVar, possibleAmount, same_prio_dist,
                          modelParameter,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$name <- name
      self$display_name <- display_name
      self$description <- description
      self$optional <- optional
      self$characteristics <- characteristics
      self$lower_limit <- lower_limit
      self$upper_limit <- upper_limit
      self$numberVar <- numberVar
      self$possibleAmount <- possibleAmount
      self$same_prio_dist <- same_prio_dist
      self$modelParameter <- modelParameter
      self$id <- id
      self$modelParameter$setParentPredictor(self)
    },
    
    inc = function(){self$possibleAmount <- self$possibleAmount+1},
    dec = function(){self$possibleAmount <- self$possibleAmount-1},
    
    setParameter = function(modelParameter){
      self$modelParameter <- modelParameter
    },
    
    addVariable = function(Variable){
      self$userVariable <- c(self$userVariable, Variable)
      self$display_name <- self$userVariable[1]
      if(length(self$userVariable) == 1) return()
      for(i in 2:length(self$userVariable)){
        self$display_name <- paste0(self$display_name, "&sdot;", self$userVariable[i])
      }
    },
    
    set.is.vector = function(flag){
      private$is.vector <- flag
      self$modelParameter$set.is.vector(flag)
    },
    
    get.is.vector = function(){
      return(private$is.vector)
    },
    
    
    getInstance =  function(){
      newPara <- NULL
      if(!is.null(self$modelParameter)) newPara <- self$modelParameter$getInstance(NULL)
      newInstance <- ModelPredictor$new(self$name, self$id, self$display_name, self$description, self$optional,
                                    self$characteristics, self$lower_limit, self$upper_limit,
                                    self$numberVar, self$possibleAmount, self$same_prio_dist,
                                    newPara)
      newInstance$userVariable <- self$userVariable
      newInstance$set.is.vector(private$is.vector)
      
      if(!is.null(self$modelParameter)) newPara$setParentPredictor(newInstance)

      return(newInstance)
    },
    
    setInstance = function(instance){

      private$is.vector <- instance$get.is.vector()
      self$name <- instance$name
      self$id <- instance$id
      self$display_name <- instance$display_name
      self$description <- instance$description
      self$optional <- instance$optional
      
      self$characteristics <- instance$characteristics
      self$lower_limit <- instance$lower_limit
      self$upper_limit <- instance$upper_limit
      
      self$numberVar <- instance$numberVar
      self$possibleAmount <- instance$possibleAmount
      self$same_prio_dist <- instance$same_prio_dist
      
      self$userVariable <- instance$userVariable
      
      #R6
      if(!is.null(self$modelParameter)){
        self$modelParameter$setInstance(instance$modelParameter$getInstance(self))
      }else{
        self$modelParameter <- instance$modelParameter$getInstance(self)
      }
      
    },
    
    
    getState = function(uuid){
      
      nextUUID <- uuid
      ret <- list()
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        modelParameterState <- NULL
        if(!is.null(self$modelParameter)){
          modelParameterState <- self$modelParameter$getState(nextUUID)
          nextUUID <- modelParameterState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          is.vector = private$is.vector,
          name = self$name,
          id = self$id,
          display_name = self$display_name,
          description = self$description,
          optional = self$optional,

          characteristics = self$characteristics,
          lower_limit = self$lower_limit,
          upper_limit = self$upper_limit,

          numberVar = self$numberVar,
          possibleAmount = self$possibleAmount,
          same_prio_dist = self$same_prio_dist,

          userVariable = self$userVariable,
        
          #R6
          modelParameter = modelParameterState

        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelPredictor' = ret
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
      
      private$is.vector = state$is.vector
      self$name = state$name
      self$id = state$id
      self$display_name = state$display_name
      self$description = state$description
      self$optional = state$optional
      
      self$characteristics = state$characteristics
      self$lower_limit = state$lower_limit
      self$upper_limit = state$upper_limit
      
      self$numberVar = state$numberVar
      self$possibleAmount = state$possibleAmount
      self$same_prio_dist = state$same_prio_dist
      
      self$userVariable = state$userVariable
      
      #R6
      self$modelParameter = state$modelParameter
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(self$modelParameter)) self$modelParameter$resetState()
    }
  )
)