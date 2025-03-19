
#Can be any kind of parameter; slope, dummy, intercept or aux
ModelCreatingDataParameter <- R6Class(
  classname = "ModelCreatingDataParameter", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    id = NULL,
    mcd = NULL,
    name = NULL, #can be html
    type = NULL, #c("intercept","predictor", "aux", "fixed")
    formulaName = NULL, #e.g. Normal@sigma, Beta@alpha, etc. (only used for type=="aux")
    
    constraints=list(), #named list: lowerLimit=x1; upperLimit=x2
    
    predId = NULL, #id of predictor, if type=="predictor"
    subs = list(), # named list. names have to be unique and correspond to predictors (element) naming
    
    useSameDistForAllSubs = F,
    priority = 1
  ),
  
  public = list(
    
    initialize = function(id, mcd, formulaName, predId=NULL,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$id <- id
      private$mcd <- mcd
      private$predId <- predId
      private$formulaName <- formulaName
    },
    
    setId = function(id, silent=F){
      private$id <- id
      if(!silent) private$mcd$doTriggerParameter(private$id)
    },
    getId = function(){
      return(private$id)
    },
    
    setMcd = function(mcd){
      private$mcd <- mcd
    },
    getMcd = function(){
      return(private$mcd)
    },
    
    setName = function(name, predName=F, silent=F){
      change <- F
      if(predName){
        pred <- private$mcd$getPredictor(private$predId)
        if(!is.null(pred)) {
          name <- pred$getName()
          if(!equal0(private$name, HTML(paste0("b<sub>",name,"</sub>")))) change <- T
          private$name <- HTML(paste0("b<sub>",name,"</sub>"))
          
        }
      }else{
        if(!equal0(private$name, name)) change <- T
        private$name <- name
      }
      if(!silent && change) private$mcd$doTriggerParameter(private$id)
    },
    getName = function(){
      return(private$name)
    },
    
    setType = function(type, silent=F){
      change <- F
      if(!equal0(private$type,type)) change <- T
      private$type <- type
      if(!silent && change) private$mcd$doTriggerParameter(private$id)
    },
    getType = function(){
      return(private$type)
    },
    
    setFormulaName = function(formulaName){
      private$formulaName <- formulaName
    },
    getFormulaName = function(){
      return(private$formulaName)
    },
    
    setConstraints = function(constraints, silent=F){
      private$constraints <- constraints
      if(!silent) private$mcd$doTriggerParameter(private$id)
    },
    addConstraint = function(name, value, silent=F){
      private$constraints <- list.append(private$constraints, value, name)
      if(!silent) private$mcd$doTriggerParameter(private$id)
    },
    getConstraints = function(){
      return(private$constraints)
    },
    
    setPredId = function(predId){
      private$predId <- predId
    },
    getPredId = function(){
      return(private$predId)
    },
    
    createSub = function(){
      ModelCreatingDataParameterSub$new(self)
    },
    
    setSubs = function(subs, silent=F){
      private$subs <- subs
      if(!silent) private$mcd$doTriggerParameter(private$id)
    },
    addSub = function(sub, name, silent=F){
      private$subs <- list.append(private$subs, sub, name)
      if(!silent) private$mcd$doTriggerParameter(private$id)
    },
    getSubs = function(){
      return(private$subs)
    },
    getSub = function(sub){
      if(sub %in% names(private$subs)){
        return(private$subs[[sub]])
      }
      return(NULL)
    },
    getSingleSub = function(){
      if(length(private$subs)==1 &&
         names(private$subs) == ""){
        return(private$subs[[1]])
      }
      return(NULL)
    },
    removeSub = function(subName){
      subNames <- names(private$subs)
      index <- match(subName, subNames)
      private$subs <- private$subs[-index]
      private$mcd$doTriggerParameter(private$id)
    },
    
    hasOnlyFixedValueSubs = function(type=c("generative","prior")){
      for(sub in private$subs){
        dist <- sub$getValueDistribution()
        if(type=="prior"){
          dist <- sub$getPrior()
        }
        
        if(dist$getName() != "FixedValue") return(F)
      }
      return(T)
    },
    
    isVector = function(){
      return(length(private$subs) > 1)
    },
    
    setUseSameDistForAllSubs = function(same){
      private$useSameDistForAllSubs <- same
    },
    
    getUseSameDistForAllSubs = function(){
      return(private$useSameDistForAllSubs)
    },
    
    setPriority = function(priority, silent=F){
      change <- F
      if(!equal0(private$priority, priority)) change <- T
      private$priority <- priority
      if(!silent && change) private$mcd$doTriggerParameter(private$id)
    },
    getPriority = function(){
      return(private$priority)
    },
    
    getAmIRedundant = function(){
      for(s in private$subs){
        if(!s$getAmIRedundant()) return(F)
      }
      return(T)
    },
    
    asLatex = function(){
      
      name <- private$formulaName
      
      latexName <- str_split(name, "@")[[1]][2]
      
      return(latexName)
    },
    
    doTrigger = function(){
      private$mcd$doTriggerParameter(private$id)
    },
    
    
    #if silent = TRUE, trigger parameter will not fire
    getInstance = function(mcd=NULL,silent=F){
      if(is.null(mcd)) mcd <- private$mcd
      new <- ModelCreatingDataParameter$new(private$id, mcd, private$formulaName)
      new$setName(private$name, silent=silent)
      new$setType(private$type, silent=silent)
      new$setPredId(private$predId)
      new$setConstraints(private$constraints, silent=silent)
      newSubs <- list()
      for(s_i in seq_len(length(private$subs))){
        s <- private$subs[[s_i]]
        newSubs <- list.append(newSubs, s$getInstance(new, silent=silent), 
                               names(private$subs)[[s_i]])
      }
      new$setSubs(newSubs, silent=silent) 
      new$setPriority(private$priority, silent=silent)
      new$setUseSameDistForAllSubs(private$useSameDistForAllSubs)
      return(new)
    },
    
    compareTo = function(cT, exact=F){

      if(private$name != cT$getName()) return(F)
      if(private$type != cT$getType()) return(F)
      if(private$formulaName != cT$getFormulaName()) return(F)
      if(private$useSameDistForAllSubs != cT$getUseSameDistForAllSubs()) return(F)
      if(private$priority != cT$getPriority()) return(F)
      
      if(!equal0(private$predId, cT$getPredId())) return(F)
      
      
      if(xor(is.null(private$constraints), is.null(cT$getConstraints()))) return(F)
      if(!is.null(private$constraints) && !is.null(cT$getConstraints())){
        if(length(private$constraints) != length(cT$getConstraints())) return(F)
        if(!vectorEqual(names(private$constraints), names(cT$getConstraints()))) return(F)
        unA <- unlist(private$constraints)
        unB <- unlist(cT$getConstraints())
        if(!vectorEqual(unA, unB)) return(F)
      }
      
      if(xor(is.null(private$subs), is.null(cT$getSubs()))) return(F)
      if(!is.null(private$subs) && !is.null(cT$getSubs())){
        if(length(private$subs) != length(cT$getSubs())) return(F)
        if(!vectorEqual(names(private$subs), names(cT$getSubs()))) return(F)
        for(i in seq_along(private$subs)){
          if(!private$subs[[i]]$compareTo(cT$getSubs()[[i]], exact=exact)) return(F)
        }
      }

      
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
        
        mcdResponseState <- NULL
        if(!is.null(private$mcdResponse)){
          mcdResponseState <- private$mcdResponse$getState(nextUUID)
          nextUUID <- mcdResponseState$nextUUID
        }

        subsState <- list()
        subNames <- names(private$subs)
        for(aa_index in seq_along(private$subs)){
          aa <- private$subs[[aa_index]]
          aa_name <- subNames[[aa_index]]
          cState <- aa$getState(nextUUID)
          subsState <- list.append(subsState, cState, aa_name)
          nextUUID <- cState$nextUUID
        }

        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          id = private$id,
          name = private$name,
          type = private$type,
          formulaName = private$formulaName,
          constraints = private$constraints,
          predId = private$predId,
          
          useSameDistForAllSubs = private$useSameDistForAllSubs,
          priority = private$priority,
          
          #R6
          mcd = mcdResponseState,
          subs = subsState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelCreatingDataParameter' = ret
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
      
      private$id = state$id
      private$name = state$name
      private$type = state$type
      private$formulaName = state$formulaName
      private$constraints = state$constraints
      private$predId = state$predId
      
      private$useSameDistForAllSubs = state$useSameDistForAllSubs
      private$priority = state$priority
      
      #R6
      private$mcd = state$mcd
      private$subs = state$subs
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$mdc)) private$mcd$resetState()
      for(aa in private$subs){
        aa$resetState()
      }
    }
  )
)


#Used as a sub model for parameters
#a dummy (categorical) parameter can have more than one ...ParameterSub
ModelCreatingDataParameterSub <- R6Class(
  classname = "ModelCreatingDataParameterSub", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    parameter = NULL, 
    valueDistribution = NULL, #ModelCreatingDataDistribution
    prior = NULL, #ModelCreatingDataDistribution
    amIRedundant = F,
    redundant = list(comb=list(), equal=list())
  ),
  
  public = list(
    
    initialize = function(parameter,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      private$parameter <- parameter
    },
    
    getParameter = function(){
      return(private$parameter)
    },
    
    setValueDistribution = function(dist, silent=F){
      private$valueDistribution <- dist
      if(!silent) private$parameter$doTrigger()
    },
    getValueDistribution = function(){
      return(private$valueDistribution)
    },
    
    setPrior = function(prior, silent=F){
      private$prior <- prior
      if(!silent) private$parameter$doTrigger()
    },
    getPrior = function(){
      return(private$prior)
    },
    
    setAmIRedundant = function(flag, silent=F){
      change <- F
      if(!equal0(private$amIRedundant, flag)) change <- T
      private$amIRedundant <- flag
      if(!silent && change) private$parameter$doTrigger()
    },
    getAmIRedundant = function(){
      return(private$amIRedundant)
    },
    
    setRedundant = function(redundant, silent=F){
      private$redundant <- redundant
      # if(!silent) private$parameter$doTrigger()
    },
    getRedundant = function(){
      return(private$redundant)
    },
    
    getPlot = function(){
      genDist <- private$valueDistribution
      priorDist <- private$prior
      
      genData <- genDist$randomValue(1e4, F)
      priorData <- priorDist$randomValue(1e4, F)

      cons <- private$parameter$getConstraints()
      ll <- cons$lowerLimit
      ul <- cons$upperLimit
      if(is.null(ll)) ll <- -Inf
      if(is.null(ul)) ul <- Inf
      # breaks <- c(-Inf,ll, ul, Inf)
      breaks <- c(ll, ul, Inf)
      
 
      limits <- bayestestR::hdi(genData, ci=0.99)
      limits$CI_low <- max(limits$CI_low, round(ll,100))
      limits$CI_high <- min(limits$CI_high, ul)
      genData <- genData[genData>=limits$CI_low & genData<=limits$CI_high]
      limits <- bayestestR::hdi(priorData, ci=0.99)
      limits$CI_low <- max(limits$CI_low, round(ll,100))
      limits$CI_high <- min(limits$CI_high, ul)
      priorData <- priorData[priorData>=limits$CI_low & priorData<=limits$CI_high]
      
      genBins <- min(50, length(unique(genData)))
      priorBins <- min(50, length(unique(priorData)))
      
      bins <- max(genBins, priorBins)


      colGen <- genData>=ll&genData<=ul
      colGen[colGen==T] <- BAYAS_COLORS$`--formula-color-1`
      colGen[colGen==F] <- BAYAS_COLORS$`--formula-color-2`
      
      colPrior <- priorData>=ll&priorData<=ul
      colPrior[colPrior==T] <- BAYAS_COLORS$`--formula-color-3`
      colPrior[colPrior==F] <- BAYAS_COLORS$`--formula-color-2`

      gg <- ggplot()
      gg <- gg + 
        geom_histogram(data=data.frame(x=genData, col=colGen),
                       aes(x=x, fill=col),
                       bins=bins,
                       alpha=0.5) + 
        geom_histogram(data=data.frame(x=priorData, col=colPrior),
                       aes(x=x, fill=col),
                       bins=bins,
                       alpha=0.5) +
        scale_fill_manual(breaks = c(BAYAS_COLORS$`--formula-color-2`,
                                     BAYAS_COLORS$`--formula-color-4`,
                                     BAYAS_COLORS$`--formula-color-5`,
                                     BAYAS_COLORS$`--formula-color-3`,
                                     BAYAS_COLORS$`--formula-color-1`),
                          values=c(BAYAS_COLORS$`--modelCreatingPlot-color-values-1`,
                                   BAYAS_COLORS$`--modelCreatingPlot-color-values-2`,
                                   BAYAS_COLORS$`--modelCreatingPlot-color-values-3`,
                                   BAYAS_COLORS$`--modelCreatingPlot-color-values-4`,
                                   BAYAS_COLORS$`--modelCreatingPlot-color-values-5`))

      gg <- gg + xlab("") + ylab("") + 
        scale_y_continuous(breaks=NULL) +
        theme(legend.position = "none")
     
      gg
    },
    
    
    asLatex = function(){
      if(localUse) browser()
      
      name <- private$name
      
      latexName <- name
      
      
    },
    
    getInstance = function(parameter=NULL, silent=F){
      if(is.null(parameter)) parameter <- private$parameter
      new <- ModelCreatingDataParameterSub$new(parameter) # private$parameter 
      new$setValueDistribution(private$valueDistribution$getInstance(), silent=silent)
      new$setPrior(private$prior$getInstance(), silent=silent)
      new$setAmIRedundant(private$amIRedundant, silent=silent)
      new$setRedundant(private$redundant, silent=silent)
      return(new)
    },
    
    compareTo = function(cT, exact=F){

      if(private$amIRedundant != cT$getAmIRedundant()) return(F)
      if(!vectorEqual(private$redundant$comb, cT$getRedundant()$comb)) return(F)
      if(!vectorEqual(private$redundant$equal, cT$getRedundant()$equal)) return(F)
      
      if(xor(is.null(private$valueDistribution), is.null(cT$getValueDistribution()))) return(F)
      if(!is.null(private$valueDistribution) && !is.null(cT$getValueDistribution())){
        if(!private$valueDistribution$compareTo(cT$getValueDistribution(), exact=exact)) return(F)
      }
      
      if(xor(is.null(private$prior), is.null(cT$getPrior()))) return(F)
      if(!is.null(private$prior) && !is.null(cT$getPrior())){
        if(!private$prior$compareTo(cT$getPrior(), exact=exact)) return(F)
      }
      
      if(!exact) return(T)
    },
    
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)

        parameterState <- NULL
        if(!is.null(private$parameter)){
          parameterState <- private$parameter$getState(nextUUID)
          nextUUID <- parameterState$nextUUID
        }
        
        valueDistributionState <- NULL
        if(!is.null(private$valueDistribution)){
          valueDistributionState <- private$valueDistribution$getState(nextUUID)
          nextUUID <- valueDistributionState$nextUUID
        }
        
        priorState <- NULL
        if(!is.null(private$prior)){
          priorState <- private$prior$getState(nextUUID)
          nextUUID <- priorState$nextUUID
        }
        
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          amIRedundant = private$amIRedundant,
          redundant = private$redundant,
          
          #R6
          parameter = parameterState,
          valueDistribution = valueDistributionState,
          prior = priorState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelCreatingDataParameterSub' = ret
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
      
      private$amIRedundant <- state$amIRedundant
      private$redundant <- state$redundant
      
      #R6
      private$parameter <- state$parameter
      private$valueDistribution <- state$valueDistribution
      private$prior <- state$prior
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(private$parameter)) private$parameter$resetState()
      if(!is.null(private$valueDistribution)) private$valueDistribution$resetState()
      if(!is.null(private$prior)) private$prior$resetState()
    }
    
  )
)


getParametersPossiblePriorDistributions <- function(type=c("predictor","aux"), 
                                                    class=c("generation","inference"),
                                                    modelName, linkName){

  d <- planningDistribtionsEnum("all")
  dResp <- planningDistribtionsEnum("response")
  dLink <- planningLinkEnum()
  

  if(class=="generation"){
    if(type %in% c("predictor","intercept","fixed")){
      ret <- list('Choose...'="",
                  'Single value'=d$FixedValue,
                  'Continuous unbound (recommended)' = c('Normal'=d$Normal,
                                           'Cauchy'=d$Cauchy,
                                           'Logistic'=d$Logistic,
                                           'Uniform'=d$Uniform,
                                           'Student\'s t'=d$Student_t),
                  'Continuous positive (not recommended)' = c('Log-Normal'=d$Log_Normal,
                                            'Gamma'=d$Gamma,
                                            'Exponential'=d$Exponential,
                                            'Inverse-Gaussian'=d$Inverse_Gaussian,
                                            'F'=d$F,
                                            'Chi-squared'=d$Chi_squared_non_1,
                                            'Weibull'=d$Weibull))
      
      if(modelName %in% c(dResp$Negative_Binomial, dResp$Poisson) &&
         linkName %in% c(dLink$identity)){
        names(ret)[3] <- 'Continuous unbound (not recommended)'
        names(ret)[4] <- 'Continuous unbound (recommended)'
      }
    }else if(type == "aux"){
      ret <- list('Choose...'="",
                  'Single value'=d$FixedValue,
                  'Continuous unbound (truncated at 0)' = c('Normal'=d$Normal,
                                           'Cauchy'=d$Cauchy,
                                           'Logistic'=d$Logistic,
                                           'Uniform'=d$Uniform,
                                           'Student\'s t'=d$Student_t),
                  'Continuous positive' = c('Log-Normal'=d$Log_Normal,
                                            'Gamma'=d$Gamma,
                                            'Exponential'=d$Exponential,
                                            'Inverse-Gaussian'=d$Inverse_Gaussian,
                                            'F'=d$F,
                                            'Chi-squared'=d$Chi_squared_non_1,
                                            'Weibull'=d$Weibull))
    }
  }else if(class=="inference"){
    if(type == "predictor"){
      ret <- list('Choose...'="",
                  'Continuous unbound (recommended)' = c('Normal'=d$Normal,
                                           'Cauchy'=d$Cauchy,
                                           'Logistic'=d$Logistic,
                                           'Uniform'=d$Uniform,
                                           'Student\'s t'=d$Student_t),
                  'Continuous positive (not recommended)' = c('Log-Normal'=d$Log_Normal,
                                            'Gamma'=d$Gamma,
                                            'Exponential'=d$Exponential,
                                            'Chi-squared'=d$Chi_squared_non_1,
                                            'Weibull'=d$Weibull))
      if(modelName %in% c(dResp$Negative_Binomial, dResp$Poisson) &&
         linkName %in% c(dLink$identity)){
        names(ret)[2] <- 'Continuous unbound (not recommended)'
        names(ret)[3] <- 'Continuous unbound (recommended)'
      }
    }else if(type == "aux"){
      ret <- list('Choose...'="",
                  'Continuous unbound (truncated at 0)' = c('Normal'=d$Normal,
                                           'Cauchy'=d$Cauchy,
                                           'Logistic'=d$Logistic,
                                           'Uniform'=d$Uniform,
                                           'Student\'s t'=d$Student_t),
                  'Continuous positive' = c('Log-Normal'=d$Log_Normal,
                                            'Gamma'=d$Gamma,
                                            'Exponential'=d$Exponential,
                                            'Chi-squared'=d$Chi_squared_non_1,
                                            'Weibull'=d$Weibull))
    }
  }

  
  return(ret)
}

