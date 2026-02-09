ModelCreatingDataList <- R6Class(
  classname = "ModelCreatingDataList", 
  inherit = ReactiveSerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    mcdList = list(),
    selected = NULL
  ),
  
  public = list(
    
    initialize = function(emptyState = F){
      super$initialize()
      if(emptyState) return()
    },
    
    setMCDList = function(list, silent=F){
      private$mcdList <- list
      if(!silent) self$triggerReactiveValue("mcdList")
    },
    
    getMCDList = function(){
      return(private$mcdList)
    },
    
    getMCD = function(name){
      if(is.null(name)) return(NULL)
      if(name %in% names(private$mcdList)){
        return(private$mcdList[[name]])
      }
      return(NULL)
    },
    
    addMCDList = function(mcd, name, silent=F){
      if(is.null(name)) name <- mcd$getModelName()
      private$mcdList <- list.append(private$mcdList, mcd, name)
      if(!silent) self$triggerReactiveValue("mcdList")
    },
    
    removeMCDList = function(name, silent=F){
      newList <- list()
      for(nameMCD in names(private$mcdList)){
        if(nameMCD != name) newList <- list.append(newList, private$mcdList[[nameMCD]], nameMCD)
      }
      self$setMCDList(newList)
      if(!silent) self$triggerReactiveValue("mcdList")
    },
    
    setSelected = function(selected, silent=F){
      private$selected <- selected
      if(!silent) self$triggerReactiveValue("selected")
    },
    
    getSelected = function(){
      return(private$selected)
    },
    
    getSelectedMcd = function(){
      if(length(names(private$mcdList)) > 0 && 
         !is.null(private$selected) &&
         private$selected %in% names(private$mcdList)){
        return(private$mcdList[[private$selected]])
      }
      return(NULL)
    },
    
    getValidName = function(name){
      names <- names(private$mcdList)
      if(name %in% names){
        while(name %in% names){
          if(endsWith(str_trim(name), ")")){
            hits <- gregexpr("\\([0-9]+\\)", name)[[1]]
            if(length(hits)!=1 || hits > -1){
              index <- last(hits)
              length <- last(attr(hits, "match.length"))
              sub <- substr(name, index+1, (index+length)-2)
              sub_num <- as.numeric(sub)+1
              name <- paste0(substr(name, 0, index-1),
                             "(", sub_num,")") 
            }else{
              name <- paste0(name,"(0)")
            }
          }else{
            name <- paste0(name,"(0)")
          }
        }
      }
      return(name)
    },
    
    getInstance = function(){
      new <- ModelCreatingData$new(private$modelName)
      new$setSelected(private$selected)
      for(mcd_name in names(private$mcdList())){
        mcd <- private$mcdList[[mcd_name]]
        mcdNew <- mcd$getInstance()
        new$addMCDList(mcd=mcdNew, name=mcd_name)
      }
      return(new)
    },
    
    setInstance = function(model){
      self$setSelected(model$getSelected())
      self$setMCDList(list())
      for(mcd_name in names(model$getMCDList())){
        mcd <- model$getMCDList()[[mcd_name]]
        mcdNew <- mcd$getInstance()
        self$addMCDList(mcd=mcdNew, name=mcd_name)
      }
    },
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        mcdListState <- list()
        for(aa_name in names(private$mcdList)){
          aa <- private$mcdList[[aa_name]]
          cState <- aa$getState(nextUUID)
          mcdListState <- list.append(mcdListState, cState, aa_name)
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          selected = private$selected,

          #R6
          mcdList = mcdListState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelCreatingDataList' = ret
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
      private$selected <- state$selected

      #R6
      self$setMCDList(state$mcdList, silent=T)

    },
    resetState = function(){
      if(!super$resetState()) return()
      for(aa in private$mcdList){
        aa$resetState()
      }
    }
  )
)