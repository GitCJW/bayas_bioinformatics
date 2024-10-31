ModelCreatingDataOtherVariable <- R6Class(
  classname = "ModelCreatingDataOtherVariable", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    id = NULL,
    mcd = NULL,
    name = NULL,
    type = NULL, #cont, discrete, categorical
    range = NULL, #"positiveGreater", "positive", "percent", etc.
    distName = NULL, #planningDistribtionsEnum(), if type = cont/discrete
    dist = NULL, #ModelCreatingDataOtherVariableDistributionAbstract 
    step = c(1),
    seed = NULL,
    minDatapoints = 1, #for cont and discrete normally 1; for categorical normally the number of categories
    
    dependsOnOV = NULL, #if e.g. a distribution parameter depends on the values of another variable

    #For cat
    categoricalModel = NULL,
    
    
    ##Protected values, that will not cleared by the clear function
    # c("positive","integer")
    constraints = NULL,
    #If this oV is removable 
    removable = T,
    #If this oV is used e.g. as N for (beta-)binom models
    specialRole = NULL
  ),
  
  public = list(
    
    initialize = function(id, mcd, name="unnamed",
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$id <- id
      private$mcd <- mcd
      private$seed <- round(runif(1,-100000,100000))
      private$name <- name
    },
    
    setId = function(id){
      private$id = id
    },
    getId = function(){
      return(private$id)
    },
    
    setMcd = function(mcd){
      private$mcd = mcd
    },
    getMcd = function(mcd){
      return(private$mcd)
    },
    
    setName = function(name, trigger=T){
      if(is.null(name) || str_trim(name)==""){
        name <- "unnamed"
      } 
      name <- private$mcd$getOtherVariableNames(name=name, ignore=private$name)
      
      if(equal0(private$name, name)) return()

      private$name = name
      private$mcd$setHeader(private$id, name)
      if(trigger) private$mcd$doTriggerOtherVariable(private$id, "name") 
    },
    getName = function() return(private$name),
    
    setType = function(type=c("cont","discrete","categorical"), trigger=T){
      match.arg(type)
      if(!is.null(private$type) && private$type != type){
        private$range = NULL
        private$dist = NULL
        private$distName = NULL
      }
      private$type = type
      if(trigger) private$mcd$doTriggerOtherVariable(private$id, "step")
    },
    getType = function() return(private$type),
    
    isNumeric = function(){
      if(!is.null(private$type)){
        if(private$type != "categorical"){
          return(T)
        }else{
          return(private$categoricalModel$isNumeric())
        }
      }
      return(F)
    },

    
    setRange = function(range=c("positiveGreater", "positive", "percent",
                                "unlimited","negGreater","negative",
                                "discretePos","limitedN","bernoulli",
                                "independent", "subgroup", "replacement")){ 
      match.arg(range)
      if(!is.null(private$range) && private$range != range){
        private$dist = NULL
        private$distName = NULL
      }
      private$range = range
    },
    getRange = function() return(private$range),
    
    setDistName = function(dist=unlist(planningDistribtionsEnum("predictor"))){
      match.arg(dist)
      private$distName = dist
      private$mcd$doTriggerOtherVariable(private$id, "dist")
    },
    getDistName = function(){
      return(private$distName)
    },
    setDist = function(dist){
      private$dist = dist
      private$mcd$doTriggerOtherVariable(private$id, "dist")
    },
    getDist = function(){
      return(private$dist)
    },
    

    setDependsOnOV = function(dependsOnOV) return(private$dependsOnOV <- dependsOnOV),
    getDependsOnOV = function() return(private$dependsOnOV),
    
    setMinDatapoints = function(minDatapoints){
      private$minDatapoints = minDatapoints
      private$mcd$doTriggerOtherVariable(private$id, "dist")
    },
    getMinDatapoints = function(){
      return(private$minDatapoints)
    },
    
    setCategoricalModel = function(model){
      private$categoricalModel <- model
      private$mcd$doTriggerOtherVariable(private$id, "dist")
    },
    getCategoricalModel = function(){
      return(private$categoricalModel)
    },
    isRandomizeAvailableForCatagorical = function(top=T, type=c("super","sub")){ #superElement=NULL used?
      ids <- c()
      valid <- T
      
      if(!top && !is.null(private$categoricalModel)){
        dists <- private$categoricalModel$getValuesDistributed()
        if(is.null(dists)) break
        dists_un <- unlist(dists)
        if("explicit" %in% dists_un){
          valid <- F
          ids <- c(ids, private$id)
        }
      }
      #super using explicit distributed freq?
      if("super" %in% type){
        super <- private$dependsOnOV
        for(s in super){
          sOV <- private$mcd$getOtherVariable(s)
          ret <- sOV$isRandomizeAvailableForCatagorical(top=F, type="super")
          if(!ret$valid){
            valid <- F
            ids <- c(ids, ret$ids)
          }
        }
      }
      #subs using explicit distributed freq?
      if("sub" %in% type){
        mcd <- private$mcd
        subs <- mcd$getOtherVariableIdsDependentOnThis(private$id)
        for(s in subs){
          sOV <- private$mcd$getOtherVariable(s)
          ret <- sOV$isRandomizeAvailableForCatagorical(top=F, type="sub")
          if(!ret$valid){
            valid <- F
            ids <- c(ids, ret$ids)
          }
        }
      }
      return(list(valid=valid, ids=ids))
    },
    
    
    getSeed = function() return(private$seed),
    setSeed = function(seed, silent=F){ 
      if(is.numeric(seed) && round(seed) == seed){
        #New: pass to dist
        private$seed <- seed
        if(!is.null(private$dist)){ #} && length(private$dist) > 1){
          private$dist$setSeed(seed)
          if(!silent) private$mcd$doTriggerOtherVariable(private$id, "dist")
        }else if(!is.null(private$type) && 
                 private$type == "categorical"){
          catModel <- private$categoricalModel
          if(!is.null(catModel)){
            catModel$setSeed(seed)
          }
        }
      }
    },
    
    getValues = function(){
      if(self$getType()=="categorical"){
        return(self$getCategoricalModel()$getValuesForNumericDependency())
      }else{
        return(self$getDist()$getValues())
      }
    },
    
    setRandomValues = function(){
      if(is.null(private$type)) return()
      if(private$type != "categorical"){
        dist <- private$dist
        if(!is.null(dist)){
          dp <- private$mcd$getMinDatapointsOfVar(private$id)
          dist$randomValue(dp)
        }
      }
    },
    

    setConstraints = function(cons){
      private$constraints <- cons
    },
    addConstraints = function(cons = c("positive","integer")){
      match.arg(cons)
      private$constraints <- c(private$constraints, cons)
    },
    getConstraints = function(){
      private$constraints
    },
    
    setRemovable = function(r){
      private$removable <- r
    },
    isRemovable = function(){
      private$removable
    },
    
    setSpecialRole = function(role){
      private$specialRole <- role
    },
    getSpecialRole = function(){
      private$specialRole
    },
    
    isInUse = function(){
      if(!is.null(self$getSpecialRole())) return(T)

      preds <- private$mcd$getPredictors()
      for(pred in preds){
        predOvIds <- pred$getOVIds()
        if(private$id %in% predOvIds) return(T)
      }
      
      ovs <- private$mcd$getOtherVariableIdsDependentOnThis(private$id)
      for(ov_id in ovs){
        ov <- private$mcd$getOtherVariable(ov_id)
        if(ov$isInUse()) return(T)
      }
      
      return(F)    
    },
    
    isInUseOnDependendVariable = function(ov){
      dep <- private$mcd$getOtherVariableIdsDependentOnThis(ov)
      for(d in dep){
        ov <- c(ov, self$isInUseOnDependendVariable(d))
      }
      return(ov)
    },
    
    clear = function(){
      private$type = NULL
      private$range = NULL
      private$distName = NULL
      private$dist = NULL
      private$categoricalModel = NULL
      private$minDatapoints = 1
      private$step = c(1)
      private$seed = NULL
      private$dependsOnOV = NULL
    },
    
    addStep = function(step){ 
      private$step = c(private$step, step)
      private$mcd$doTriggerOtherVariable(private$id, "step")
    },
    setStep = function(step){ 
      private$step = step
      private$mcd$doTriggerOtherVariable(private$id, "step")
    },
    setStepBack = function(){
      if(last(private$step) > 1){
        private$step <- head(private$step, -1)
        private$mcd$doTriggerOtherVariable(private$id, "step")
      }
    },
    getLastStep = function() return(last(private$step)),
    getSteps = function() return(private$step),
    
    getInstance = function(){
      new <- ModelCreatingDataOtherVariable$new(id=self$getId(), 
                                                mcd=self$getMcd(), 
                                                name=self$getName())
      new$setType(self$getType())
      new$setRange(self$getRange())
      new$setDistName(self$getDistName())
      new$setDist(self$getDist())
      new$setStep(self$getSteps())
      new$setSeed(self$getSeed())
      new$setMinDatapoints(self$getMinDatapoints())
      
      new$setDependsOnOV(self$getDependsOnOV())
      
      if(!is.null(self$getCategoricalModel())){
        new$setCategoricalModel(self$getCategoricalModel()$getInstance())
      }

      new$setConstraints(self$getConstraints())
      new$setRemovable(self$isRemovable())
      new$setSpecialRole(self$getSpecialRole())

      return(new)
    },
    
    #exact=T: also includes equal id
    compareTo = function(cT, exact=F){
      
      if(!equal0(private$name, cT$getName())) return(F)
      if(!equal0(private$type, cT$getType())) return(F)
      if(!equal0(private$range, cT$getRange())) return(F)
      if(!equal0(private$distName, cT$getDistName())) return(F)
      if(!vectorEqual(private$step, cT$getSteps())) return(F)
      if(!equal0(private$seed, cT$getSeed())) return(F)
      if(!equal0(private$minDatapoints, cT$getMinDatapoints())) return(F)
      
      if(!vectorEqual(private$constraints, cT$getConstraints())) return(F)
      if(!equal0(private$removable, cT$isRemovable())) return(F)
      if(!equal0(private$specialRole, cT$getSpecialRole())) return(F)
      
   
      if(xor(is.null(private$dist), is.null(cT$getDist()))) return(F)
      if(!is.null(private$dist) && !is.null(cT$getDist()) && 
         !private$dist$compareTo(cT$getDist(), exact=exact)) return(F)
      
      if(xor(is.null(private$dependsOnOV), is.null(cT$getDependsOnOV()))) return(F)
      if(!is.null(private$dependsOnOV) && !is.null(cT$getDependsOnOV()) && 
         !private$dependsOnOV$compareTo(cT$getDependsOnOV(), exact=exact)) return(F)
      
      if(xor(is.null(private$categoricalModel), is.null(cT$getCategoricalModel()))) return(F)
      if(!is.null(private$categoricalModel) && !is.null(cT$getCategoricalModel()) && 
         !private$categoricalModel$compareTo(cT$getCategoricalModel(), exact=exact)) return(F)
  
      if(!exact) return(T)
      if(private$id != cT$getId()) return(F)
      
      return(T)
    },
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        mcdState <- NULL
        if(!is.null(private$mcd)){
          mcdState <- private$mcd$getState(nextUUID)
          nextUUID <- mcdState$nextUUID
        }

        categoricalModelState <- NULL
        if(!is.null(private$categoricalModel)){
          categoricalModelState <- private$categoricalModel$getState(nextUUID)
          nextUUID <- categoricalModelState$nextUUID
        }
        
        distState <- NULL
        if(!is.null(private$dist)){
          distState <- private$dist$getState(nextUUID)
          nextUUID <- distState$nextUUID
        }
        
        # distState <- list()
        # for(aa in private$dist){
        #   cState <- aa$getState(nextUUID)
        #   distState <- list.append(distState, cState)
        #   nextUUID <- cState$nextUUID
        # }

        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          id = private$id,
          name = private$name,
          type = private$type,
          range = private$range,
          distName = private$distName,
          step = private$step,
          seed = private$seed,
          minDatapoints = private$minDatapoints,
          dependsOnOV = private$dependsOnOV,
          constraints = private$constraints,
          removable = private$removable,
          specialRole = private$specialRole,
          
          #R6
          mcd = mcdState,
          dist = distState,
          categoricalModel = categoricalModelState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelCreatingDataOtherVariable' = ret
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
      private$name <- state$name
      private$type <- state$type
      private$range <- state$range
      private$distName <- state$distName
      private$step <- state$step
      private$seed <- state$seed
      private$minDatapoints <- state$minDatapoints
      private$dependsOnOV <- state$dependsOnOV
      private$constraints <- state$constraints
      private$removable <- state$removable
      private$specialRole <- state$specialRole
      
      #R6
      private$mcd <- state$mcd
      private$dist <- state$dist
      private$categoricalModel <- state$categoricalModel
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$mdc)) private$mcd$resetState()
      if(!is.null(private$categoricalModel)) private$categoricalModel$resetState()
      if(!is.null(private$dist)) private$dist$resetState()
      # for(aa in private$dist){
      #   aa$resetState()
      # }
    }

  )
)
