
ModelCreatingDataSSD <- R6Class(
  classname = "ModelCreatingDataSSD", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.2",
    
    mcd = NULL,
    goalId = 0,
    goals = list(),
    
    power = 0.8,
    maxN = 2,
    accuarcy = 200,
    seed = 1,
    
    ssdObject = NULL, #startSSD_NGoals(...)
    ssdContinue = F,
    
    #After a completed ssd run, a persistent mcd will be stored in mcdOff.
    #So that changes in the mcd (response, predictors, parameters) do not affect this.
    mcdOff = NULL
  ),
  
  public = list(
    
    initialize = function(mcd, goalId=0,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$mcd <- mcd
      private$goalId <- goalId
    },
    
    setMcd = function(mcd){
      private$mcd <- mcd
    },
    
    getMcd = function(){
      return(private$mcd)
    },
    
    getGoalId = function(){
      return(private$goalId)
    },
    
    createGoal = function(){
      private$goalId <- private$goalId+1
      name <- self$getGoalNames(name="")
      newGoal <- SSDGoal$new(private$goalId, self)
      newGoal$setName(name)
      return(newGoal)
    },
    
    addGoal = function(goal){
      private$goals <- list.append(private$goals, goal)
      self$doTrigger()
    },
    
    removeGoal = function(goalId){
      newList <- list()
      for(l in private$goals){
        if(l$getId() != goalId)
          newList <- list.append(newList, l)
      }
      private$goals <- newList
      self$doTrigger()
    },
    
    getGoals = function(forSSD=F, used=F){
      if(forSSD){
        newGoals <- list()
        for(g in private$goals){
          if(!g$getInUse()){
            next
          }
          parasA <- g$getParametersA(asFormulaNames=T)
          parasB <- g$getParametersB(asFormulaNames=T)
          
          parasA[parasA=="b_(Intercept)"] <- "b_Intercept"
          parasB[parasB=="b_(Intercept)"] <- "b_Intercept"

          newEntry <- createGoal(
            parametersA = parasA, parametersB = parasB,
            goalType = g$getType(), ci = g$getHDI(), ropeType = g$getRopeExcludeInclude(),
            ropeLower = g$getRopeLower(), ropeUpper = g$getRopeUpper(),
            ropeExclusive = T, precisionWidth = g$getPrecWidth())

          newGoals <- list.append(newGoals, newEntry)
        }
        return(newGoals)
      }else{
        if(used){
          gg <- private$goals
          gUsed <- list()
          for(g in gg){
            if(g$getInUse()) gUsed <- list.append(gUsed,g)
          }
          return(gUsed)
        }else{
          return(private$goals)
        }
        
      }
    },
    getGoal = function(id){
      if(is.empty(id)) return(NULL)
      for(g in private$goals){
        if(g$getId()==id) return(g)
      }
    },
    
    setPower = function(pwr){
      private$power <- pwr
    },
    getPower = function(){
      return(private$power)
    },
    
    setMaxN = function(maxN){
      private$maxN <- maxN
    },
    getMaxN = function(){
      return(private$maxN)
    },
    
    setAccuarcy = function(accuarcy){
      private$accuarcy <- accuarcy
    },
    getAccuarcy = function(){
      return(private$accuarcy)
    },
    
    setSeed = function(seed){
      private$seed <- seed
    },
    getSeed = function(){
      return(private$seed)
    },
    
    setSsdObject = function(ssd){
      private$ssdObject <- ssd
    },
    
    getSsdObject = function(){
      return(private$ssdObject)
    },
    
    setSsdContinue = function(continue){
      private$ssdContinue <- continue
    },
    
    getSsdContinue = function(){
      return(private$ssdContinue)
    },
    
    setMcdOff = function(mcdOff=NULL, asIt=F){
      if(asIt){
        private$mcdOff <- mcdOff
        return()
      }
      if(is.null(mcdOff)){
        private$mcdOff <- private$mcd$getInstance()
      }else{
        private$mcdOff <- mcdOff
      }
    },
    
    getMcdOff = function(){
      return(private$mcdOff)
    },
    
    isMcdOffSame = function(){
      if(xor(is.null(private$mcdOff), is.null(private$mcd))) return(F)
      if(is.null(private$mcdOff) && is.null(private$mcd)) return(T)
      return(private$mcd$compareTo(private$mcdOff))
    },
    
    #returns goal names
    #if name is setted, returns name if available (not in use)
    #or an alternative name (adding "_" recursively)
    getGoalNames = function(name=NULL, ignore=c()){
      names <- c()
      for(g in private$goals){
        names <- c(names, g$getName())
      }
      if(is.null(name)){
        return(names)
      }else{
        names <- names[!names %in% ignore]
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
        return(name)
      }
    },
    
    isGoalNameInUse = function(name, ignore=NULL){
      names <- c()
      for(g in private$goals){
        if(is.null(ignore) || ignore != g$getId())
          names <- c(names, g$getName())
      }
      return(name %in% names)
    },
    
    verifyGoals = function(){
      for(g in private$goals){
        g$checkIfValid()
      }
    },
    
    #Verify if each used goal is valid
    isValid = function(){
      self$verifyGoals()
      for(g in private$goals){
        if(g$getInUse() && !g$getIsValid()) return(F)
      }
      return(T)
    },
    
    doTrigger = function(){
      private$mcd$doTriggerSSDGoals()
    },
    
    getInstance = function(mcd=private$mcd){
      new <- ModelCreatingDataSSD$new(mcd=mcd, goalId=private$goalId)
      for(g in private$goals){
       new$addGoal(g$getInstance(new))
      }
      new$setPower(private$power)
      new$setMaxN(private$maxN)
      new$setAccuarcy(private$accuarcy)
      new$setSeed(private$seed)
      new$setSsdObject(private$ssdObject)
      new$setSsdContinue(private$ssdContinue)
      new$setMcdOff(private$mcdOff, asIt=T)
      return(new)
    },
    
    setInstance = function(){
      if(localUse) browser()
    },
    
    
    compareTo = function(cT, exact=F){

      if(private$goalId != cT$getGoalId()) return(F)
      if(length(private$goals) != length(cT$getGoals())){
        for(i in seq_along(private$goals)){
          if(!private$goals[[i]]$compareTo(cT$getGoals()[[i]])) return(F)
        }
      }
 
      if(private$power != cT$getPower()) return(F)
      if(private$maxN != cT$getMaxN()) return(F)
      if(private$accuarcy != cT$getAccuarcy()) return(F)
      if(private$seed != cT$getSeed()) return(F)
      
      
      if(!exact) return(T)
      
      # ssdObject = NULL
      
      
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
        mcdOffState <- NULL
        if(!is.null(private$mcdOff)){
          mcdOffState <- private$mcdOff$getState(nextUUID)
          nextUUID <- mcdOffState$nextUUID
        }
        
        goalsState <- list()
        for(aa in private$goals){
          cState <- aa$getState(nextUUID)
          goalsState <- list.append(goalsState, cState)
          nextUUID <- cState$nextUUID
        }

        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          goalId = private$goalId,
          power = private$power,
          maxN = private$maxN,
          accuarcy = private$accuarcy,
          seed = private$seed,
          ssdObject = private$ssdObject,
          ssdContinue = private$ssdContinue,
          
          mcd = mcdState,
          mcdOff = mcdOffState,
          goals = goalsState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelCreatingDataSSD' = ret
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
      
      private$goalId <- state$goalId
      private$power <- state$power
      private$maxN <- state$maxN
      private$accuarcy <- state$accuarcy
      private$seed <- state$seed
      private$ssdObject <- state$ssdObject
      private$ssdContinue <- state$ssdContinue
      
      private$mcd <- state$mcd
      private$mcdOff <- state$mcdOff
      private$goals <- state$goals
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$mcd)) private$mcd$resetState()
      if(!is.null(private$mcdOff)) private$mcdOff$resetState()
      for(aa in private$goals){
        aa$resetState()
      }
    }
  )
)





SSDGoal <- R6Class(
  classname = "SSDGoal", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    
    id=NULL,
    mcdSSD=NULL,
    
    inUse=T,
    isValid=F,
    invalidMsg="No parameters defined",
    
    name="",
    nameSetted=F,
    
    parametersA=list(),
    parametersB=list(),
    
    type = "rope", #rope or precision
    hdi = 0.95,
    
    precWidth = 1,
    
    ropeExclude = "exclude",
    ropeLower = -1,
    ropeUpper = 1,
    
    trigger = T
    
  ),
  
  public = list(
    
    initialize = function(id, mcdSSD,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$id <- id
      private$mcdSSD <- mcdSSD
    },
    
    setId = function(id, trigger=T){
      private$id <- id
      if(trigger) self$doTrigger()
    },
    getId = function(){
      return(private$id)
    },
    
    setMcdSSD = function(mcdSSD, trigger=T){
      private$mcdSSD <- mcdSSD
      if(trigger) self$doTrigger()
    },
    getMcdSSD = function(){
      return(private$mcdSSD)
    },
    
    setInUse = function(flag, trigger=T){
      if(private$inUse == flag) return()
      private$inUse <- flag
      if(trigger) self$doTrigger()
    },
    getInUse = function(){
      return(private$inUse)
    },
    
    # check if this goal is no longer valid due to removed or redundant used parameters
    checkIfValid = function(){
      #If no parameter is selected
      if(is.empty(private$parametersA) &&
         is.empty(private$parametersB)){
        msg <- "No parameters defined."
        self$setInvalidMsg(msg)
        self$setIsValid(F)
        return()
      }
  
      #if parameters are selected that are nor longer available
      invalidParas <- list()
      for(p in private$parametersA){
        mcd <- private$mcdSSD$getMcd()
        para <- mcd$getParameter(p$paraId)
        if(!is.null(para)){
          if(p$subName == ""){
            # ssub <- para$getSingleSub()
            # if(is.null(ssub)) invalidParas <- list.append(invalidParas, p)
          }else{
            sub <- para$getSubs()[[p$subName]]
            if(is.null(sub) || sub$getAmIRedundant()){
              invalidParas <- list.append(invalidParas, p)
            }
          }
        }else{
          invalidParas <- list.append(invalidParas, p)
        }
      }
      for(p in private$parametersB){
        mcd <- private$mcdSSD$getMcd()
        para <- mcd$getParameter(p$paraId)
        if(!is.null(para)){
          if(p$subName == ""){
            # ssub <- para$getSingleSub()
            # if(is.null(ssub)) invalidParas <- list.append(invalidParas, p)
          }else{
            sub <- para$getSubs()[[p$subName]]
            if(is.null(sub) || sub$getAmIRedundant()){
              invalidParas <- list.append(invalidParas, p)
            }
          }
        }else{
          invalidParas <- list.append(invalidParas, p)
        }
      }
      
      #create invalid message
      if(!is.empty(invalidParas)){
        msg <- "Some of the used parameters are no longer available or redundant."
        self$setIsValid(F)
        self$setInvalidMsg(msg)
        return()
      }
      
      #If the same parameters for A and B are selected
      if(!is.empty(private$parametersA) && !is.empty(private$parametersB) &&
         length(private$parametersA) == length(private$parametersB)){
        reA <- self$getParametersA(twoListFormat=T)
        reB <- self$getParametersB(twoListFormat=T)
        for(pId in reA$paraId){
          elA <- reA$subName[reA$paraId==pId]
          elB <- reB$subName[reB$paraId==pId]
          if(length(elA) == length(elB) &&
             all(elA %in% elB) && all(elB %in% elA)){
            self$setIsValid(F)
            self$setInvalidMsg("The two lists should not contain identical parameters.")
            return()
          }
        }
      }
      
      self$setIsValid(T)
      self$setInvalidMsg("")
    },
    
    setIsValid = function(flag, trigger=T){
      if(private$isValid == flag) return()
      private$isValid <- flag
      if(trigger) self$doTrigger()
    },
    getIsValid = function(){
      return(private$isValid)
    },
    
    setInvalidMsg = function(msg, trigger=T){
      private$invalidMsg <- msg
      if(trigger) self$doTrigger()
    },
    getInvalidMsg = function(){
      return(private$invalidMsg)
    },
    
    setName = function(name, trigger=T, ignoreCheck=F){
      self$setNameSetted(T)
      if(ignoreCheck){
        private$name <- name
        if(trigger) self$doTrigger()
        return()
      }
      if(is.null(name) || str_trim(name)==""){
        name <- "unnamed"
        self$setNameSetted(F)
      } 
      name <- private$mcdSSD$getGoalNames(name=name, ignore=private$name)
      
      if(equal0(private$name, name)) return()
      
      if(private$name == name) return()
      private$name <- name
      if(trigger) self$doTrigger()
    },
    getName = function(){
      return(private$name)
    },
    
    setNameSetted = function(flag){
      private$nameSetted <- flag
    },
    getNameSetted = function(){
      return(private$nameSetted)
    },
    
    setParametersA = function(paras, trigger=T){
      private$parametersA <- paras
      self$checkIfValid()
      if(trigger) self$doTrigger()
    },
    setParametersB = function(paras, trigger=T){
      private$parametersB <- paras
      self$checkIfValid()
      if(trigger) self$doTrigger()
    },
    addParametersA = function(para, trigger=T){
      private$parametersA <- list.append(private$parametersA, para)
      if(trigger) self$doTrigger()
    },
    addParametersB = function(para, trigger=T){
      private$parametersB <- list.append(private$parametersB, para)
      if(trigger) self$doTrigger()
    },
    getParametersA = function(twoListFormat=F, asFormulaNames=F){
      return(self$getParameter("a", twoListFormat=twoListFormat,
                               asFormulaNames=asFormulaNames))
    },
    getParametersB = function(twoListFormat=F, asFormulaNames=F){
      return(self$getParameter("b", twoListFormat=twoListFormat,
                               asFormulaNames=asFormulaNames))
    },
    # twoListFomat: from list(list(paraId,subName),...) to
    #list(paraId=c(), subName=c())
    getParameter = function(type=c("a","b"), twoListFormat, asFormulaNames){

      para <- NULL
      if(type=="a"){
        para <- private$parametersA
      }else if(type=="b"){
        para <- private$parametersB
      }else{
        return(NULL)
      }
      if(twoListFormat || asFormulaNames){
        paraIds <- c()
        subNames <- c()
        formulaNames <- c()
        for(s in para){
          paraIds <- c(paraIds, s$paraId)
          subNames <- c(subNames, s$subName)
          if(asFormulaNames){
            paraObj <- private$mcdSSD$getMcd()$getParameter(s$paraId)
            
            #For non auxiliar parameters (predictors, intercetp, etc.)
            if(!is.null(paraObj$getPredId())){
              
              predObj <- private$mcdSSD$getMcd()$getPredictor(paraObj$getPredId())
              predName <- predObj$getName()
              sPredName <- str_split(predName, ":")[[1]]
              sSubName <- str_split(s$subName, ":")[[1]]
              
              
              # parameter <- private$mcd$getParameterOfPredictor(pred[1])
              # predName <- predictor$getName()
              # subs <- parameter$getSubs() 
              # subNames <- names(subs)
              
              newName <- ""
              if(!(length(sSubName)==1 && sSubName[[1]] == predName)){
                for(s_i in seq_along(sSubName)){
                  if(length(sSubName)>1 || sSubName[[1]] != ""){
                    newName <- paste0(predName,"..",sSubName[[s_i]])
                  }else{
                    newName <- predName
                  }
                  newName <- gsub(":",".",newName)
                }
              }
              newName <- paste0("b_",newName)
              
            }else{
              newName <- paste0(paraObj$asLatex())
            }
           
            
            # newName <- paste0("b_",paste0(paste0(sPredName,sSubName), collapse=":"))
            formulaNames <- c(formulaNames, newName)
          }
        }
        if(asFormulaNames){
          return(formulaNames)
        }else{
          return(list(paraId=paraIds, subName=subNames))
        }
      }else{
        return(para)
      }
    },
    
    setType = function(type, trigger=T){
      private$type = type
      if(trigger) self$doTrigger()
    },
    getType = function(){
      return(private$type)
    },
    
    setHDI = function(hdi, trigger=T){
      private$hdi = hdi
      if(trigger) self$doTrigger()
    },
    getHDI = function(){
      return(private$hdi)
    },

    setPrecWidth = function(precWidth, trigger=T){
      private$precWidth = precWidth
      if(trigger) self$doTrigger()
    },
    getPrecWidth = function(){
      return(private$precWidth)
    },


    setRopeExcludeInclude = function(ropeExclude, trigger=T){
      private$ropeExclude = ropeExclude
      if(trigger) self$doTrigger()
    },
    getRopeExcludeInclude = function(){
      return(private$ropeExclude)
    },
    setRopeLower = function(ropeLower, trigger=T){
      private$ropeLower = ropeLower
      if(trigger) self$doTrigger()
    },
    getRopeLower = function(){
      return(private$ropeLower)
    },
    setRopeUpper = function(ropeUpper, trigger=T){
      private$ropeUpper = ropeUpper
      if(trigger) self$doTrigger()
    },
    getRopeUpper = function(){
      return(private$ropeUpper)
    },
    
    setTrigger = function(trigger){
      private$trigger <- trigger
    },
    getTrigger = function(){
      return(private$trigger)
    },

    getPlot = function(){
      mcd <- private$mcdSSD$getMcd()
      
      trigger <- self$getTrigger()
      self$setTrigger(F)
      self$checkIfValid()
      self$setTrigger(trigger)
      if(!self$getIsValid()) return(ggplot())
      
      paraA <- self$getParametersA()
      paraB <- self$getParametersB()
      
      sumUpAGen <- rep(0,1e4)
      sumUpAPrior <- rep(0,1e4)
      for(pA in paraA){
        para <- mcd$getParameter(pA$paraId)
        if(pA$subName == ""){
          if(length(para$getSubs()) == 1){
            sub <- para$getSubs()[[1]]
          }else{
            if(localUse) browser()
          }
        }else{
          sub <- para$getSubs()[[pA$subName]]
        }
       
        # if(is.null(pA$subName) || pA$subName == ""){
        #   sub <- para$getSingleSub()
        #   if(is.null(sub)) next
        # }
        dist <- sub$getValueDistribution()
        val <- dist$randomValue(1e4)
        sumUpAGen <- sumUpAGen+val
        dist <- sub$getPrior()
        val <- dist$randomValue(1e4)
        sumUpAPrior <- sumUpAPrior+val
      }
      sumUpBGen <- rep(0,1e4)
      sumUpBPrior <- rep(0,1e4)
      for(pB in paraB){
        para <- mcd$getParameter(pB$paraId)
        if(pB$subName == ""){
          if(length(para$getSubs()) == 1){
            sub <- para$getSubs()[[1]]
          }else{
            if(localUse) browser()
          }
        }else{
          sub <- para$getSubs()[[pB$subName]]
        }
        # if(is.null(pB$subName) || pB$subName == ""){
        #   sub <- para$getSingleSub()
        #   if(is.null(sub)) next
        # }
        dist <- sub$getValueDistribution()
        val <- dist$randomValue(1e4)
        sumUpBGen <- sumUpBGen+val
        dist <- sub$getPrior()
        val <- dist$randomValue(1e4)
        sumUpBPrior <- sumUpBPrior+val
      }

      genData <- sumUpAGen-sumUpBGen
      priorData <- sumUpAPrior-sumUpBPrior
      
      postData <- (genData+priorData)/2
      
      densGen <- density(genData)
      densPrior <- density(priorData)
      densPost <- density(postData)
      
      limits <- bayestestR::hdi(genData, ci=private$hdi)
      genData <- genData[genData>=limits$CI_low & genData<=limits$CI_high]
      limits <- bayestestR::hdi(priorData, ci=private$hdi)
      priorData <- priorData[priorData>=limits$CI_low & priorData<=limits$CI_high]
      limits <- bayestestR::hdi(postData, ci=private$hdi)
      postData <- postData[postData>=limits$CI_low & postData<=limits$CI_high]
      
      genBins <- min(50, length(unique(genData)))
      priorBins <- min(50, length(unique(priorData)))
      
      bins <- max(genBins, priorBins)
    

      colGen <- "lightblue" #Only used to identify, not to colorize
      colPrior <- "orange" #Only used to identify, not to colorize
      
      
      gg <- ggplot()
      if(private$type=="rope"){
        
        color <- BAYAS_COLORS$`--modelCreatingPlotROPE-color-values-1`
        if(private$ropeExclude == "include") color <- BAYAS_COLORS$`--modelCreatingPlotROPE-color-values-2`
        
        gg <- gg + 
          geom_histogram(data=data.frame(x=genData, col=colGen),
                         aes(x=x, fill=colGen),
                         bins=bins,
                         alpha=0.5) + 
          geom_histogram(data=data.frame(x=priorData, col=colPrior),
                         aes(x=x, fill=colPrior),
                         bins=bins,
                         alpha=0.5) +
          scale_fill_manual(breaks = c("grey","blue","red", "orange","lightblue"), #Only used to identify, not to colorize
                            values=c(BAYAS_COLORS$`--modelCreatingPlot-color-values-1`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-2`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-3`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-4`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-5`),
                            name="Model", label=c("","","","Inference","Data generation"))
        
        ggb <- ggplot_build(gg)
        ggb_data <- ggb$data
        cc1 <- ggb_data[[1]]$count
        cc2 <- ggb_data[[2]]$count
        
        gg <- gg +
          geom_rect(aes(xmin = private$ropeLower, xmax = private$ropeUpper,
                        ymin = 0, ymax = max(cc1, cc2)*1.1), alpha=0.3,
                    color=color, fill=color)
        
        gg <- gg + xlab("") + ylab("") +
          scale_y_continuous(breaks=NULL)  +
          theme(legend.title = element_text(size=16),
                legend.text = element_text(size=14))

      }else if(private$type=="precision"){

        fake_y <- postData
        
        limits <- eti(fake_y, ci=private$hdi, verbose=F)
        
        if(is.na(limits$CI_low)){
          if(private$hdi < 0.5){
            limits$CI_low <- -0.001
          }else{
            limits$CI_low <- min(fake_y)
          }
        }
        if(is.na(limits$CI_high)){
          if(private$hdi < 0.5){
            limits$CI_high <- 0.001
          }else{
            limits$CI_high <- max(fake_y)
          }
        } 
        
        fakeData <- data.frame(
          x = fake_y,
          col="lightblue" #BAYAS_COLORS$`--modelCreatingPlot-color-1`
        )
        fakeData$col[fakeData$x <= limits$CI_low] <- "grey" #BAYAS_COLORS$`--modelCreatingPlot-color-2`
        fakeData$col[fakeData$x >= limits$CI_high] <- "grey" #BAYAS_COLORS$`--modelCreatingPlot-color-2`
        bins <- 50
        
        breaks <- seq(from=min(fake_y), to=max(fake_y), length=bins-2)
        breaks <- c(breaks, limits$CI_low, limits$CI_high)
        breaks <- unique(breaks)
        
        gg <- ggplot()
        gg <- gg + 
          geom_histogram(data=fakeData,
                         aes(x=x, fill=col, color=col),
                         breaks=breaks,
                         alpha=0.5) + 
          scale_fill_manual(breaks = c("grey","blue","red", "orange","lightblue"), #Only used to identify, not to the colorize
                            values=c(BAYAS_COLORS$`--modelCreatingPlot-color-values-1`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-2`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-3`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-4`,
                                     BAYAS_COLORS$`--modelCreatingPlot-color-values-5`)) + 
          scale_color_manual(breaks = c("grey","blue","red", "orange","lightblue"), #Only used to identify, not to the colorize
                             values=c(BAYAS_COLORS$`--modelCreatingPlot-color-values-1`,
                                      BAYAS_COLORS$`--modelCreatingPlot-color-values-2`,
                                      BAYAS_COLORS$`--modelCreatingPlot-color-values-3`,
                                      BAYAS_COLORS$`--modelCreatingPlot-color-values-4`,
                                      BAYAS_COLORS$`--modelCreatingPlot-color-values-5`))

        
        ggb <- ggplot_build(gg)
        ggb_data <- ggb$data
        cc1 <- ggb_data[[1]]$count
        height <- max(cc1) * 0.1
        
        gg <- gg +
          geom_segment(aes(x = limits$CI_low, y = height, 
                           xend = limits$CI_high, yend = height),
                       linewidth=2) +
          annotate("text", x = mean(fake_y), y=height*0.5, size = 5, 
                   label=paste0("\u2264 ", private$precWidth))
          
        gg <- gg + ylab("") + 
          scale_y_continuous(breaks=NULL) +
          theme(legend.position = "none")

        gg
      }
      gg
    },
    
    doTrigger = function(){
      if(private$trigger)
        private$mcdSSD$doTrigger()
    },
    
    getInstance = function(mcdSSD=NULL){
      if(is.null(mcdSSD)) mcdSSD <- private$mcdSSD
      new <- SSDGoal$new(private$id, mcdSSD)
      new$setParametersA(private$parametersA, trigger=F)
      new$setParametersB(private$parametersB, trigger=F)
      new$setInUse(self$getInUse(), trigger=F)
      new$setIsValid(private$isValid, trigger=F)
      new$setInvalidMsg(private$invalidMsg, trigger=F)
      new$setName(private$name, trigger=F, ignoreCheck=T)
      new$setNameSetted(private$nameSetted)
      new$setType(private$type, trigger=F)
      new$setHDI(private$hdi, trigger=F)
      new$setPrecWidth(private$precWidth, trigger=F)
      new$setRopeExcludeInclude(private$ropeExclude, trigger=F)
      new$setRopeLower(private$ropeLower, trigger=F)
      new$setRopeUpper(private$ropeUpper, trigger=F)
      return(new)
    },
    
    setGoal = function(goal){

      private$inUse <- goal$getInUse()
      private$isValid <- goal$getIsValid()
      private$invalidMsg <- goal$getInvalidMsg()
      
      private$name <- goal$getName()
      private$nameSetted <- goal$getNameSetted()
      
      private$parametersA <- goal$getParametersA()
      private$parametersB <- goal$getParametersB()
      
      private$type <- goal$getType()
      private$hdi <- goal$getHDI()
      
      private$precWidth <- goal$getPrecWidth()
      
      private$ropeExclude <- goal$getRopeExcludeInclude()
      private$ropeLower <- goal$getRopeLower()
      private$ropeUpper <- goal$getRopeUpper()
      
      self$doTrigger()
    },
    
    
    compareTo = function(cT){

      if(private$id != cT$getId()) return(F)
      
      if(private$inUse != cT$getInUse()) return(F)
      if(private$isValid != cT$getIsValid()) return(F)
      if(private$name != cT$getName()) return(F)
      if(private$nameSetted != cT$getNameSetted()) return(F)
      if(private$type != cT$getType()) return(F)
      if(private$hdi != cT$getHDI()) return(F)
      if(private$precWidth != cT$getPrecWidth()) return(F)
      if(private$ropeExclude != cT$getRopeExclude()) return(F)
      if(private$ropeLower != cT$getRopeLower()) return(F)
      if(private$ropeUpper != cT$getRopeUpper()) return(F)
      

      if(length(private$parametersA) != length(cT$getParametersA)) return(F)
      
      #Para A
      reA <- self$getParametersA(twoListFormat=T)
      reCTA <- cT$getParametersA(twoListFormat=T)
      for(pId in reA$paraId){
        elA <- reA$subName[reA$paraId==pId]
        elCTA <- reCTA$subName[reCTA$paraId==pId]
        if(length(elA) == length(elCTA) &&
           all(elA %in% elCTA) && all(elCTA %in% elA)){
          return(F)
        }
      }
      
      #Para B
      reB <- self$getParametersB(twoListFormat=T)
      reCTB <- cT$getParametersB(twoListFormat=T)
      for(pId in reB$paraId){
        elB <- reB$subName[reB$paraId==pId]
        elCTB <- reCTB$subName[reCTB$paraId==pId]
        if(length(elB) == length(elCTB) &&
           all(elB %in% elCTB) && all(elCTB %in% elB)){
          return(F)
        }
      }
  
      return(T)
    },
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
  
        mcdSSDState <- NULL
        if(!is.null(private$mcdSSD)){
          mcdSSDState <- private$mcdSSD$getState(nextUUID)
          nextUUID <- mcdSSDState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,

          id = private$id,
          
          inUse = private$inUse,
          isValid = private$isValid,
          invalidMsg = private$invalidMsg,
          name = private$name,
          nameSetted = private$nameSetted,
          type  =  private$type, 
          hdi  =  private$hdi,
          precWidth  =  private$precWidth,
          ropeExclude  =  private$ropeExclude,
          ropeLower  =  private$ropeLower,
          ropeUpper  =  private$ropeUpper,
          trigger  =  private$trigger,
          parametersA = private$parametersA,
          parametersB = private$parametersB,
          
          #R6
          mcdSSD = mcdSSDState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'SSDGoal' = ret
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
      
      private$inUse <- state$inUse
      private$isValid <- state$isValid
      private$invalidMsg <- state$invalidMsg
      private$name <- state$name
      private$nameSetted <- state$nameSetted
      private$type  <-  state$type
      private$hdi  <-  state$hdi
      private$precWidth  <-  state$precWidth
      private$ropeExclude  <-  state$ropeExclude
      private$ropeLower  <-  state$ropeLower
      private$ropeUpper  <-  state$ropeUpper
      private$trigger  <-  state$trigger
      private$parametersA <- state$parametersA
      private$parametersB <- state$parametersB
      
      #R6
      private$mcdSSD <- state$mcdSSD
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$mcdSSD)) private$mcdSSD$resetState()
    }
  )
)
