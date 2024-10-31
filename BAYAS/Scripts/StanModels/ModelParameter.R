ModelParameter <- R6Class(
  classname = "ModelParameter", 
  lock_objects = T,
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    myParentPredictor = NULL #R6
  ),
  
  public = list(
    name = NULL, #For intern, role of this predictor/parameter
    id = NULL, #Unique ID
    modalID = NULL, #unique (sequential) number for the modals variable names
    display_name = NULL, #Describing name for user
    description = NULL, #Description of the role of this predictor/parameter in the model
    optional = F, #If this predictor/parameter is optional 
    visible = F, #If this parameter is shown in the list of optional predictor/parameter. Visible is e.g. intercept.
    type = NULL, #Type of this parameter e.g. term of GLM, auxiliary parameter
    is.vector = F, #Is this parameter a vector, due to factors?
    
    characteristics = NULL, #Kind of characteristic this predictor could be. Only Continuous!
    lower_limit = NULL, #Theoretical lowest value
    upper_limit = NULL, #Theoretical greatest value
    
    possibleAmount = NULL, #Number of possible amount, 1,2,...,VAR
    
    distribution = NULL, # Used distribution for this parameter
    distributions = list(), # named list of distribution, if this parameter is a vector (e.g. factors); self$is.vector = T
    distribution_tmp = NULL, # Used to store the current distribution, to replace the actual, when user does not confirm his changes
    distributions_tmp = NULL, # Same for vector parameters
    possibleDistribution = NULL, #vector
    
    findMe = F,
    
    
    initialize = function(name, id, modalID=NULL, display_name, description, optional, visible, type, 
                          characteristics = cNum$Continuous, lower_limit, upper_limit,
                          possibleAmount, 
                          distributionArgs, possibleDistribution,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$name <- name
      self$display_name <- display_name
      self$description <- description
      self$optional <- optional
      self$visible <- visible
      if(type %in% c("GLMPredictor","AuxParameter")){
        self$type <- type
      }else{
        stop(paste0("Wrong type: ", type))
      }
      self$characteristics <- characteristics
      self$lower_limit <- lower_limit
      self$upper_limit <- upper_limit
      self$possibleAmount <- possibleAmount
      self$distribution <- distributionArgs
      self$possibleDistribution <- possibleDistribution
      self$id <- id
      self$modalID <- modalID
    },
    
    setParentPredictor = function(pred){
      private$myParentPredictor <- pred
    },
    getParentPredictor = function(){
      return(private$myParentPredictor)
    },
    
    inc = function(){self$possibleAmount <- self$possibleAmount+1},
    dec = function(){self$possibleAmount <- self$possibleAmount-1},
    
    setDistribution = function(dist, tmpDist=F){
      if(tmpDist){
        self$distribution_tmp <- dist
      }else{
        self$distribution <- dist
      }
    },
    
    getDistribution = function(){
      newList <- self$possibleDistribution
      names(newList) <- distDisplayName(newList)
      return(newList)
    },
    
    getFullDisplayName = function(){
      if(self$display_name == "b" && self$id >=0){
        if(self$is.vector){
          return(HTML(paste0("<b>b", tags$sub(self$id),"</b>")))
        }else{
          return(HTML(paste0("b", tags$sub(self$id))))
        }
      }else{
        return(HTML(self$display_name))
      }
    },
    
    getFullDisplayNameLatex = function(){
      if(self$display_name == "b" && self$id >=0){
        if(self$is.vector){
          return(paste0("\\underline{b_", self$id,"}"))
        }else{
          return(paste0("b_", self$id))
        }
      }else{
        a <- self$display_name
        a <- str_replace_all(a, "&sigma;", "\\\\sigma")
        a <- str_replace_all(a, "&theta;", "\\\\theta")
        a <- str_replace_all(a, "&Phi;", "\\\\Phi")
        a <- str_replace_all(a, "&lambda;", "\\\\lambda")
        a <- str_replace_all(a, "&kappa;", "\\\\kappa")
        a <- str_replace_all(a, "&alpha;", "\\\\alpha")
        a <- str_replace_all(a, "&beta;", "\\\\beta")
        a <- str_replace_all(a, "&sdot;", "\\\\cdot")
        return(a)
      }
    },
    
    setValue = function(nameDistribution, nameAux, value, tmpValue=F, tmpDist=F){
      if(is.null(nameDistribution)){
        if(tmpDist){
          dist <- self$distribution_tmp
        }else{
          dist <- self$distribution
        }
      }else{
        if(tmpDist){
          dist <- self$distributions_tmp[[nameDistribution]]
        }else{
          dist <- self$distributions_tmp[[nameDistribution]]
        }
      }
      dist$setAuxParameter(nameAux, value, tmpValue)
    },
    
    getValue = function(nameDistribution, tmpValue=F, tmpDist=F){
      if(tmpDist){
        dist <- self$distributions_tmp[[nameDistribution]]
      }else{
        dist <- self$distributions[[nameDistribution]]
      }
      dist$getParameters(tmpValue)
    },
    
    distToTmpDist = function(){
      self$distribution_tmp <- self$distribution$getInstance()
      if(length(self$distributions) >0){
        self$distributions_tmp <- list()
        for(i in seq_along(self$distributions)){
          self$distributions_tmp[[i]] <- self$distributions[[i]]$getInstance()
          self$distributions_tmp[[i]]$element_name <- names(self$distributions)[i]
        }
        names(self$distributions_tmp) <- names(self$distributions)
      }
    },
    
    tmpDistToDist = function(){
      self$distribution <- self$distribution_tmp$getInstance()
      if(length(self$distributions_tmp) >0){
        self$distributions <- list()
        for(i in seq_along(self$distributions_tmp)){
          self$distributions[[i]] <- self$distributions_tmp[[i]]$getInstance()
        } 
        names(self$distributions) <- names(self$distributions_tmp)
      }
    },
    
    distValToTmpVal = function(tmpDist=F){
      if(tmpDist){
        if(!is.null(self$distribution_tmp)) self$distribution_tmp$valToTmpVal()
        if(length(self$distributions_tmp)>0){
          for(dist in self$distributions_tmp){
            dist$valToTmpVal()
          }
        }
      }else{
        if(!is.null(self$distribution)) self$distribution$valToTmpVal()
        if(length(self$distributions)>0){
          for(dist in self$distributions){
            dist$valToTmpVal()
          }
        }
      }
    },
    
    distTmpValToVal = function(tmpDist=F){
      if(tmpDist){
        if(!is.null(self$distribution_tmp)) self$distribution_tmp$tmpValToVal()
        if(length(self$distributions_tmp)>0){
          for(dist in self$distributions_tmp){
            dist$tmpValToVal()
          }
        }
      }else{
        if(!is.null(self$distribution)) self$distribution$tmpValToVal()
        if(length(self$distributions)>0){
          for(dist in self$distributions){
            dist$tmpValToVal()
          }
        }
      }
    },
    
    removeTmpval = function(tmpDist=F){
      if(tmpDist){
        if(!is.null(self$distribution_tmp)) self$distribution_tmp$removeTmpval()
        if(length(self$distributions_tmp)>0){
          for(dist in self$distributions_tmp){
            dist$removeTmpval()
          }
        }
      }else{
        if(!is.null(self$distribution)) self$distribution$removeTmpval()
        if(length(self$distributions)>0){
          for(dist in self$distributions){
            dist$removeTmpval()
          }
        }
      }
    },
    
    set.is.vector = function(flag){
      self$is.vector <- flag
      if(!is.null(self$distribution)) self$distribution$is.vector <- flag
      if(!is.null(self$distribution_tmp)) self$distribution_tmp$is.vector <- flag
      if(!is.null(self$distributions) || length(self$distributions) > 0){
        for(i in self$distributions){
          i$is.vector <- flag
        }
      }
      if(!is.null(self$distributions_tmp) || length(self$distributions_tmp) > 0){
        for(i in self$distributions_tmp){
          i$is.vector <- flag
        }
      }
    },
    
    #For paraProp of 'fixedValues' "distribution
    get.properties = function(){
      return(list(lower_limit=self$lower_limit, 
                  upper_limit=self$upper_limit,
                  discrete=self$characteristics==cEnum$Discrete))
    },

    
    getInstance =  function(myParentPredictor){
      myParentPredictor <- NULL

      newInstance <- ModelParameter$new(self$name, self$id, self$modalID, self$display_name, 
                                    self$description, self$optional, self$visible, self$type, 
                                    self$characteristics, self$lower_limit, self$upper_limit,
                                    self$possibleAmount, 
                                    NULL, self$possibleDistribution)
      
      newInstance$is.vector <- self$is.vector
      newInstance$findMe <- self$findMe
      
      
      #R6
      newInstance$setParentPredictor(myParentPredictor)
      

      # browser()
      # print("call")
      
      if(!is.null(self$distribution)) newInstance$distribution <- self$distribution$getInstance()
      if(!is.null(self$distribution_tmp)) newInstance$distribution_tmp <- self$distribution_tmp$getInstance()
      
      newInstance$distributions <- list()
      for(i in seq_along(self$distributions)){
        dist <- self$distributions[[i]]
        newInstance$distributions <- list.append(newInstance$distributions, dist$getInstance(), names(newInstance$distributions)[i])
      }
      
      newInstance$distributions_tmp <- list()
      for(i in seq_along(self$distributions_tmp)){
        dist <- self$distributions_tmp[[i]]
        newInstance$distributions_tmp <- list.append(newInstance$distributions_tmp, dist$getInstance(), names(newInstance$distributions_tmp)[i])
      }
      
      return(newInstance)
    },
    
    setInstance = function(instance){
      self$name <- instance$name
      self$id <- instance$id
      self$modalID <- instance$modalID
      self$display_name <- instance$display_name
      self$description <- instance$description
      self$optional <- instance$optional
      self$visible <- instance$visible
      self$type <- instance$type
      self$is.vector <- instance$is.vector
      
      self$characteristics <- instance$characteristics
      self$lower_limit <- instance$lower_limit
      self$upper_limit <- instance$upper_limit
      
      self$possibleAmount <- instance$possibleAmount
      
      self$findMe <- instance$findMe
      self$possibleDistribution <- instance$possibleDistribution
      
      #R6
      if(!is.null(private$myParentPredictor)){
        private$myParentPredictor$setInstance(instance$getParentPredictor()$getInstance())
      }else{
        private$myParentPredictor <- instance$getParentPredictor()$getInstance()
      }
      
      if(!is.null(self$distribution)){
        self$distribution$setInstance(instance$distribution$getInstance())
      }else{
        self$distribution <- instance$distribution$getInstance()
      }
      
      if(!is.null(self$distribution_tmp)){
        self$distribution_tmp$setInstance(instance$distribution_tmp$getInstance())
      }else{
        self$distribution_tmp <- instance$distribution_tmp$getInstance()
      }
      
      self$distributions <- list()
      for(i in seq_along(instance$distributions)){
        dist <- instance$distributions[[i]]
        self$distributions <- list.append(self$distributions, dist$getInstance(), names(instance$distributions)[i])
      }
      
      self$distributions_tmp <- list()
      for(i in seq_along(instance$distributions_tmp)){
        dist <- instance$distributions_tmp[[i]]
        self$distributions_tmp <- list.append(self$distributions_tmp, dist$getInstance(), names(instance$distributions_tmp)[i])
      }
    },
    
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)

        myParentPredictorState <- NULL
        if(!is.null(private$myParentPredictor)){
          myParentPredictorState <- private$myParentPredictor$getState(nextUUID)
          nextUUID <- myParentPredictorState$nextUUID
        }
        distributionState <- NULL
        if(!is.null(self$distribution)){
          distributionState <- self$distribution$getState(nextUUID)
          nextUUID <- distributionState$nextUUID
        }
        distribution_tmpState <- NULL
        if(!is.null(self$distribution_tmp)){
          distribution_tmpState <- self$distribution_tmp$getState(nextUUID)
          nextUUID <- distribution_tmpState$nextUUID
        }
        
        distributionsState <- list()
        for(dd_id in seq_along(self$distributions)){
          dd <- self$distributions[[dd_id]]
          cState <- dd$getState(nextUUID)
          distributionsState <- list.append(distributionsState, cState, names(self$distributions)[dd_id])
          nextUUID <- cState$nextUUID
        }
        distributions_tmpState <- list()
        for(dd_id in seq_along(self$distributions_tmp)){
          dd <- self$distributions_tmp[[dd_id]]
          cState <- dd$getState(nextUUID)
          distributions_tmpState <- list.append(distributions_tmpState, cState, names(self$distributions_tmp)[dd_id])
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          name = self$name,
          id = self$id,
          modalID = self$modalID,
          display_name = self$display_name,
          description = self$description,
          optional = self$optional,
          visible = self$visible,
          type = self$type,
          is.vector = self$is.vector,

          characteristics = self$characteristics,
          lower_limit = self$lower_limit,
          upper_limit = self$upper_limit,

          possibleAmount = self$possibleAmount,
          
          findMe = self$findMe,
          possibleDistribution = self$possibleDistribution,
          
          #R6
          myParentPredictor = myParentPredictorState,
          
          distribution = distributionState,
          distribution_tmp = distribution_tmpState,
          distributions = distributionsState,
          distributions_tmp = distributions_tmpState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelParameter' = ret
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
      
      private$uuid = state$uuid
      
      self$name = state$name
      self$id = state$id
      self$modalID = state$modalID
      self$display_name = state$display_name
      self$description = state$description
      self$optional = state$optional
      self$visible = state$visible
      self$type = state$type
      self$is.vector = state$is.vector
      
      self$characteristics = state$characteristics
      self$lower_limit = state$lower_limit
      self$upper_limit = state$upper_limit
      
      self$possibleAmount = state$possibleAmount
    
      self$findMe = state$findMe
      self$possibleDistribution = state$possibleDistribution
      
      #R6
      private$myParentPredictor = state$myParentPredictor
      
      self$distribution = state$distribution
      self$distribution_tmp = state$distribution_tmp
      self$distributions = state$distributions
      self$distributions_tmp = state$distributions_tmp
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$myParentPredictor)) private$myParentPredictor$resetState()
      
      if(!is.null(self$distribution)) self$distribution$resetState()
      if(!is.null(self$distribution_tmp)) self$distribution_tmp$resetState()


      for(dd in self$distributions){
        dd$resetState()
      }
      for(dd in self$distributions_tmp){
        dd$resetState()
      }
    }
    
  )
)




ModalID <- R6Class(
  classname="ModalID",
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    id = 0
  ),
  public = list(
    initialize =  function(emptyState = F){
      super$initialize()
      if(emptyState) return()
    },
    getNext = function(){
      t <- private$id
      private$id <- private$id+1
      return(t)
    },
    setId = function(id){
      private$id <- id
    },
    getId = function(){
      return(private$id)
    },
    
    getInstance =  function(){
      newInstance <- ModalID$new()
      newInstance$setId(private$id)
      return(newInstance)
    },
    
    setInstance = function(instance){
      private$id <- instance$getId()
    },
    
    getState = function(uuid){
      nextUUID <- uuid+1
      ret <- list()
      
      if(self$getUUID() == -1){
        self$setUUID(uuid)

        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          id = private$id
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModalID' = ret
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
      
      private$id <- state$id
    },
    resetState = function(){
      if(!super$resetState()) return()
    }
  )

)