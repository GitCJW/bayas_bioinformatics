################################################################################
####################### Distributions for other predictor ###################### 
################################################################################
greaterZero <- .Machine$double.xmin

planningDistribtionsEnum <- function(type=c("response", "predictor", "all")){
  l <- list(Beta="Beta", Cauchy="Cauchy", 
            Chi_squared_non_1 ="Chi_squared_non_1", Chi_squared_1="Chi_squared_1", 
            Exponential="Exponential", F="F", Gamma="Gamma", Inverse_Gaussian="Inverse_Gaussian",
            Log_Normal="Log_Normal", Logistic="Logistic", Normal="Normal",
            Student_t="Student_t", Horseshoe="Horseshoe", Uniform="Uniform", Weibull="Weibull",
            Bernoulli="Bernoulli", Beta_Binomial="Beta_Binomial", Binomial="Binomial",
            Geometric="Geometric", Hypergeometric="Hypergeometric", Negative_Binomial="Negative_Binomial",
            Poisson="Poisson", FixedValue="FixedValue")
  
  l_sorted <- c(l$Log_Normal, l$Gamma, l$Inverse_Gaussian, l$F, l$Chi_squared_1,
                l$Exponential, l$Chi_squared_non_1, l$Weibull,
                l$Beta,
                l$Normal, l$Cauchy, l$Logistic, l$Uniform, l$Student_t, l$Horseshoe,
                l$Poisson, l$Negative_Binomial, l$Geometric, l$Hypergeometric,
                l$Binomial, l$Beta_Binomial,
                l$Bernoulli, 
                l$FixedValue)
  
  names(l_sorted) <-   names(l)[match(l_sorted,unlist(l))] 
  
  if(type=="response"){
    l_sorted <- l_sorted[c(1,2,3,
                           6,
                           9,
                           10,12,
                           16,17,
                           20,21,
                           22)]
  }else if(type=="predictor"){
    l_sorted <- l_sorted[c(1:14,16:22)]
  }
  return(as.list(l_sorted))
}


enum <- planningDistribtionsEnum("all")

ModelCreatingDataOtherVariableDistributionFactory <- function(dist = planningDistribtionsEnum(),
                                                              seed=NULL){
  if(dist == enum$Log_Normal){
    return(OVDLogNormal$new(seed))
  }else if(dist == enum$Gamma){
    return(OVDGamma$new(seed))
  }else if(dist == enum$Inverse_Gaussian){
    return(OVDInverse_Gaussian$new(seed))
  }else if(dist == enum$F){
    return(OVDF$new(seed))
  }else if(dist == enum$Chi_squared_1){
    return(OVDChi_squared_1$new(seed))
  }else if(dist == enum$Exponential){
    return(OVDExponential$new(seed))
  }else if(dist == enum$Chi_squared_non_1){
    return(OVDChi_squared_non_1$new(seed))
  }else if(dist == enum$Weibull){
    return(OVDWeibull$new(seed))
  }else if(dist == enum$Beta){
    return(OVDBeta$new(seed))
  }else if(dist == enum$Normal){
    return(OVDNormal$new(seed))
  }else if(dist == enum$Cauchy){
    return(OVDCauchy$new(seed))
  }else if(dist == enum$Logistic){
    return(OVDLogistic$new(seed))
  }else if(dist == enum$Uniform){
    return(OVDUniform$new(seed))
  }else if(dist == enum$Student_t){
    return(OVDStudent_t$new(seed))
  }else if(dist == enum$Poisson){
    return(OVDPoisson$new(seed))
  }else if(dist == enum$Negative_Binomial){
    return(OVDNegative_Binomial$new(seed))
  }else if(dist == enum$Geometric){
    return(OVDGeometric$new(seed))
  }else if(dist == enum$Hypergeometric){
    return(OVDHypergeometric$new(seed))
  }else if(dist == enum$Binomial){
    return(OVDBinomial$new(seed))
  }else if(dist == enum$Beta_Binomial){
    return(OVDBeta_Binomial$new(seed))
  }else if(dist == enum$Bernoulli){
    return(OVDBernoulli$new(seed))
  }else if(dist == enum$Horseshoe){
    return(OVDHorseshoe$new(seed))
  }else if(dist == enum$FixedValue){
    return(OVDFixedValue$new(seed))
  }
}


#Proper initial priors for distribution parameters
getModelCreatingDataDistributionParameterPrior = function(dist=pDistEnum, glm=T, para){
  if(is.null(dist)) return(list())
  match.arg(dist)
  ret <- list()
  if(dist==pDistEnum$Log_Normal){
    if(para==2){
      newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
      return(newDist)
    }else{
      return(NULL)
    }
  }else if(dist==pDistEnum$Gamma){
    if(glm){
      if(para==2){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Inverse_Gaussian){
    if(para==2){
      newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
      return(newDist)
    }else{
      return(NULL)
    }
  }else if(dist==pDistEnum$F){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Chi_squared_1){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para==1){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Exponential){
    if(glm){
      return(NULL)
    }else{
      if(para==1){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Chi_squared_non_1){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para==1){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Weibull){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Beta){
    if(glm){
      if(para %in% c(2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Normal){
    if(para %in% c(2)){
      newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
      return(newDist)
    }else{
      return(NULL)
    }
  }else if(dist==pDistEnum$Cauchy){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Logistic){
    if(para %in% c(2)){
      newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
      return(newDist)
    }else{
      return(NULL)
    }
  }else if(dist==pDistEnum$Uniform){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Student_t){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(2,3)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Poisson){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`), "mean",extendBySameName=T)
    }else{
      if(para %in% c(1)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Negative_Binomial){
    if(glm){
      if(para %in% c(2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Geometric){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Hypergeometric){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      if(para %in% c(1,2)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    }
  }else if(dist==pDistEnum$Binomial){
    #1: fixPara
    if(para %in% c(1)){
      # newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
      # return(newDist)
      return(NULL)
    }else{
      return(NULL)
    }
    
  }else if(dist==pDistEnum$Beta_Binomial){
    if(glm){
      #1: fixPara
      if(para %in% c(1)){
        # newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        # return(newDist)
        return(NULL)
      }else if(para %in% c(3)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
      
    }else{
      #1: fixPara
      if(para %in% c(1)){
        # newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        # return(newDist)
        return(NULL)
      }else if(para %in% c(2,3)){
        newDist <- ModelCreatingDataOtherVariableDistributionFactory(planningDistribtionsEnum("predictor")$Exponential)
        return(newDist)
      }else{
        return(NULL)
      }
    } 
  }else if(dist==pDistEnum$Bernoulli){
    return(NULL)
  }
  return(ret)
}


#Distributions used in glms (or similar) that use alternative parameters
getDistributionUsedForGLMUseAltParameters = function(distName){
  dists <- c(enum$Gamma, enum$Exponential, enum$Beta, enum$Negative_Binomial, enum$Beta_Binomial,
             enum$Poisson)
  return(distName %in% dists)
}


{
  #Abstract
  {
    ModelCreatingDataOtherVariableDistributionAbstract <- R6Class(
      classname = "ModelCreatingDataOtherVariableDistributionAbstract", 
      inherit = SerializationInterface,
      
      private=list(
        stateVersion = "0.1",
        name=NULL,
        parameter=list(), #DistributionParameter ?
        values=NULL,
        seed=NULL,
        alternative=F,
        alternativeParameter=list(),
        useAlternative=F,
        negateValues=F,
        min=-Inf,
        max=Inf
      ),
      
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize()
          if(emptyState) return()
          
          if(is.null(seed))
            seed <- round(runif(1,-10000,10000))
          private$seed <- seed
        },
        setParameter = function(parameter){
          private$parameter <- parameter
        },
        getParameter = function() return(private$parameter),
        setValues = function(values) private$values <- values,
        getValues = function(formatted=F) {
          if(formatted){
            return(format(private$values,digits=3))
          }else{
            return(private$values)
          }
        },
        getName = function() return(private$name),
        getSeed = function() return(private$seed),
        setSeed = function(seed){
          if(length(seed) > 1 && localUse) browser()
          if(is.numeric(seed)){
            if(all(seed==round(seed)) &&
               seed <= .Machine$integer.max &&
               seed >= -.Machine$integer.max){
              private$seed <- seed
              return()
            }
          }
          if(localUse) browser()
        },
        getMin = function(){ 
          if(private$negateValues){
            return(private$max*-1)
          }
          return(private$min)
        },
        getMax = function(){ 
          if(private$negateValues){
            return(private$min*-1)
          }
          return(private$max)
        },
        hasAlternative = function() return(private$alternative),
        setAlternativeParameter = function(parameter){
          private$alternativeParameter <- parameter
        },
        getAlternativeParameter = function() return(private$alternativeParameter),
        
        setUseAlternative = function(flag) private$useAlternative <- flag,
        getUseAlternative = function() return(private$useAlternative),
        
        #id: name or index
        setParameterValue = function(id, val, alternative=F, variable=F){
          if(!is.numeric(id)){
            n <- names(private$parameter)
            if(alternative) n <- names(private$alternativeParameter)
            if(id %in% n){
              id <- match(id, n)
            }else{
              if(localUse) browser()
              return()
            }
          }
          if(alternative){
            if(variable){
              private$alternativeParameter[[id]]$setOtherVariableId(val)
            }else if(id <= length(private$alternativeParameter) &&
                     private$alternativeParameter[[id]]$isValidInput(val)[[1]]){
              private$alternativeParameter[[id]]$value <- val
            }
          }else{
            if(variable){
              private$parameter[[id]]$setOtherVariableId(val)
            }else if(id <= length(private$parameter) &&
                     private$parameter[[id]]$isValidInput(val)[[1]]){
              private$parameter[[id]]$value <- val
            }
          }
        },
        setParameterValueToDefault = function(id, alternative=F){
          if(!is.numeric(id)){
            n <- names(private$parameter)
            if(alternative) n <- names(private$alternativeParameter)
            if(id %in% n){
              id <- match(id, n)
            }else{
              return()
            }
          }
          if(alternative){
            if(id <= length(private$alternativeParameter)){
              private$alternativeParameter[[id]]$value <- private$alternativeParameter[[id]]$default_val
            }
          }else{
            if(id <= length(private$parameter)){
              private$parameter[[id]]$value <- private$parameter[[id]]$default_val
            }
          }
        },
        
        setParameterType = function(id, type = c("value","variable"), alternative=F){
          match.arg(type)
          if(!is.numeric(id)){
            n <- names(private$parameter)
            if(alternative) n <- names(private$alternativeParameter)
            if(id %in% n){
              id <- match(id, n)
            }else{
              return()
            }
          }
          if(alternative){
            if(id <= length(private$alternativeParameter))
              private$alternativeParameter[[id]]$setType(type)
          }else{
            if(id <= length(private$parameter))
              private$parameter[[id]]$setType(type)
          }
        },
        
        dependsOnOtherVariable = function(alternative){
          dep <- c()
          if(!alternative){
            for(i in private$parameter){
              dep <- c(dep, i$getOtherVariableId())
            }
          }else{
            for(i in private$alternativeParameter){
              dep <- c(dep, i$getOtherVariableId())
            }
          }
          return(dep)
        },
        
        setNegateValues = function(flag) private$negateValues <- flag,
        getNegateValues = function() return(private$negateValues),
        
        randomValue = function(N, internalAssigned=T){},
        
        plotValues = function(values=NULL, values2=NULL){
    
          if(is.null(values)){
            values <- self$randomValue(1e4, F)
          }
          bins <- min(30, max(2,length(unique(values))))
          binWidth <- (max(values)-min(values))/(bins-1)
          if(binWidth == 0 ) binWidth <- 1
          
          gg <- ggplot() + 
            geom_histogram(data=data.frame(x=values), 
                           aes(x=x), 
                           binwidth=binWidth,
                           alpha=0.9) + 
            xlab("") + ylab("") + 
            scale_y_continuous(breaks=NULL)
          
          if(!is.null(values2)){
            if(length(values2)<(length(values)/10)){
              values2 <- rep(values2, floor((length(values)/10)/length(values2)))
            }
            
            gg <- gg  + 
              geom_histogram(data=data.frame(x=values2), 
                             aes(x=x), 
                             binwidth=binWidth,
                             fill="lightblue",
                             alpha=0.8)
            
            
          }
          gg
        },
        
        #overridden by concrete classes
        getAsStanSyntax = function(){},
        
        #overridden by concrete classes
        getAsLatexSyntax = function(mcd=NULL){},

        
        transform = function(toAlt=T){},
        
        getInstance = function(){
          new <- ModelCreatingDataOtherVariableDistributionFactory(private$name)
          new$setValues(private$values)
          new$setSeed(private$seed)
          para <- list()
          paraNames <- names(private$parameter)
          for(pi in seq_along(private$parameter)){
            p <- private$parameter[[pi]]
            para <- list.append(para, p$getInstance(), paraNames[pi])
          }
          new$setParameter(para)
          
          para <- list()
          paraNames <- names(private$parameter)
          if(private$alternative){
            paraNames <- names(private$alternativeParameter)
            for(pi in seq_len(length(private$alternativeParameter))){
              p <- private$alternativeParameter[[pi]]
              para <- list.append(para, p$getInstance(), paraNames[pi])
            }
            new$setAlternativeParameter(para)
          }
          new$setUseAlternative(private$useAlternative)
          new$setNegateValues(private$negateValues)
          return(new)
        },
        
        compareTo = function(cT, exact=F){

          if(private$name != cT$getName()) return(F)
          if(private$min != cT$getMin()) return(F)
          if(private$max != cT$getMax()) return(F)
          
          if(private$alternative != cT$hasAlternative()) return(F)
          if(private$useAlternative != cT$getUseAlternative()) return(F)
          if(private$negateValues != cT$getNegateValues()) return(F)
          
          
          para <- private$parameter
          paraCT <- cT$getParameter()
          if(private$useAlternative){
            para <- private$alternativeParameter
            paraCT <- cT$getAlternativeParameter()
          }

          if(xor(is.null(para), is.null(paraCT))) return(F)
          if(!is.null(para) && !is.null(paraCT)){
            if(length(para) != length(paraCT)) return(F)
            if(!vectorEqual(names(para), names(paraCT))) return(F)
            for(i in seq_along(para)){
              if(!para[[i]]$compareTo(paraCT[[i]], exact = exact)) return(F)
            }
          }
          
          if(!exact) return(T)
          
          if(private$seed != cT$getSeed()) return(F)
          if(!vectorEqual(private$values, cT$getValues())) return(F)
          
          return(T)
        },
        
        getState = function(uuid){
          nextUUID <- uuid
          ret <- list()
          
          if(self$getUUID() == -1){
            
            nextUUID <- nextUUID + 1
            self$setUUID(uuid)
            
            parameterState <- list()
            parameterNames <- names(private$parameter)
            for(aa_index in seq_along(private$parameter)){
              aa <- private$parameter[[aa_index]]
              aa_name <- parameterNames[[aa_index]]
              cState <- aa$getState(nextUUID)
              parameterState <- list.append(parameterState, cState, aa_name)
              nextUUID <- cState$nextUUID
            }
            alternativeParameterState <- list()
            alternativePparameterNames <- names(private$alternativeParameter)
            for(aa_index in seq_along(private$alternativeParameter)){
              aa <- private$alternativeParameter[[aa_index]]
              aa_name <- alternativePparameterNames[[aa_index]]
              cState <- aa$getState(nextUUID)
              alternativeParameterState <- list.append(alternativeParameterState, cState, aa_name)
              nextUUID <- cState$nextUUID
            }
            
            ret <- list(
              uuid = uuid, 
              stateVersion = private$stateVersion,
              
              name=private$name,
              values=private$values,
              seed=private$seed,
              alternative=private$alternative,
              useAlternative=private$useAlternative,
              negateValues=private$negateValues,
              
              #R6
              parameter=parameterState,
              alternativeParameter=alternativeParameterState
            )
          }
          
          ret <- list(
            uuid = self$getUUID(),
            nextUUID = nextUUID,
            'ModelCreatingDataOtherVariableDistributionAbstract' = ret
          )
          names(ret)[3] <- class(self)[1]
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
          
          private$name <- state$name
          private$values <- state$values
          private$seed <- state$seed
          private$alternative <- state$alternative
          private$useAlternative <- state$useAlternative
          private$negateValues <- state$negateValues
          
          #R6
          private$parameter <- state$parameter
          private$alternativeParameter <- state$alternativeParameter
        },
        resetState = function(){
          if(!super$resetState()) return()
          for(aa in private$parameter){
            aa$resetState()
          }
          for(aa in private$alternativeParameter){
            aa$resetState()
          }
        }
        
      )
    )
  }
  #Log_Normal
  {
    OVDLogNormal <- R6Class(
      classname = "OVDLogNormal", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Log_Normal,
                   stateVersion = "0.2",
                   min=greaterZero, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=0, default_val=0, min_val=-Inf, 
                                          max_val=Inf, discrete=F)
          sigma <- DistributionParameter$new(name="sigma", display_name="Standard deviation", 
                                             description = "The standard deviation",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(mu=mu, sigma=sigma)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rlnorm(N, private$parameter$mu$value, private$parameter$sigma$value)
            val[val < greaterZero] <- greaterZero
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("lognormal(", private$parameter$mu$value, ",", private$parameter$sigma$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          mu <- private$parameter$mu$value
          sigma <- private$parameter$sigma$value
          
          depMu <- private$parameter$mu$getOtherVariableId()
          depSigma <- private$parameter$sigma$getOtherVariableId()
          
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            mu <- ov$getName()
          }
          if(!is.null(depSigma)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depSigma)
            sigma <- ov$getName()
          }
          
          return(paste0("\\text{Log-normal}(", mu, " , ", sigma, ")"))
        }
      )
    )
  }
  #Gamma
  {
    OVDGamma <- R6Class(
      classname = "OVDGamma", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Gamma,
                   alternative=T,
                   min=greaterZero, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          kappa <- DistributionParameter$new(name="kappa", display_name="Shape", 
                                             description = "The shape value", 
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          Theta <- DistributionParameter$new(name="Theta", display_name="Rate", 
                                             description = "The rate deviation",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(kappa=kappa, Theta=Theta)
          
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=1, default_val=1, min_val=greaterZero, 
                                          max_val=Inf, discrete=F, isAlt=T)
          shape <- DistributionParameter$new(name="shape", display_name="Shape", 
                                             description = "The shape deviation",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F, isAlt=T)
          private$alternativeParameter <- list(mu=mu, shape=shape)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rgamma(N, shape=private$parameter$kappa$value, rate=private$parameter$Theta$value)
            val[val < greaterZero] <- greaterZero
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("gamma(", private$parameter$kappa$value, ",", private$parameter$Theta$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          kappa <- private$parameter$kappa$value
          Theta <- private$parameter$Theta$value
          
          depKappa <- private$parameter$kappa$getOtherVariableId()
          depTheta <- private$parameter$Theta$getOtherVariableId()
          
          if(!is.null(depKappa)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depKappa)
            kappa <- ov$getName()
          }
          if(!is.null(depTheta)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depTheta)
            Theta <- ov$getName()
          }
          
          return(paste0("\\text{Gamma}(", kappa, " , ", Theta, ")"))
        },
        
        transform = function(toAlt=T){
          if(toAlt){
            kappa <- private$parameter$kappa$value
            Theta <- private$parameter$Theta$value
            private$alternativeParameter$mu$value <- kappa/Theta
            private$alternativeParameter$shape$value <- kappa
          }else{
            mu <- private$alternativeParameter$mu$value
            shape <- private$alternativeParameter$shape$value
            private$parameter$kappa$value <- shape
            private$parameter$Theta$value <- private$parameter$kappa$value/mu
          }
        }
      )
    )
  }
  #Inverse_Gaussian
  {
    OVDInverse_Gaussian <- R6Class(
      classname = "OVDInverse_Gaussian", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Inverse_Gaussian,
                   min=greaterZero, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=1, default_val=1, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          shape <- DistributionParameter$new(name="shape", display_name="Shape", 
                                             description = "The shape",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(mu=mu, shape=shape)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rinv_gaussian(N, private$parameter$mu$value, private$parameter$shape$value)
            val[val < greaterZero] <- greaterZero
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          warning("Not implemented: getAsStanSyntax in OVDInverse_Gaussian")
          return(NULL)
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          warning("Not implemented: getAsLatexSyntax in OVDInverse_Gaussian")
        }
      )
    )
  }
  #F
  {
    OVDF <- R6Class(
      classname = "OVDF", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$F,
                   min=greaterZero, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          d1 <- DistributionParameter$new(name="d1", display_name="d<sub>1</sub>", 
                                          description = "The first degrees of freedom", 
                                          value=10, default_val=10, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          d2 <- DistributionParameter$new(name="d2", display_name="d<sub>2</sub>", 
                                          description = "The second degrees of freedom",
                                          value=10, default_val=10, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          private$parameter <- list(d1=d1, d2=d2)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rf(N, private$parameter$d1$value, private$parameter$d2$value)
            val[val < greaterZero] <- greaterZero
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          warning("Not implemented: getAsStanSyntax in OVDF")
          return(NULL)
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          warning("Not implemented: getAsLatexSyntax in OVDF")
        }
      )
    )
  }
  #Chi_squared_1
  {
    OVDChi_squared_1 <- R6Class(
      classname = "OVDChi_squared_1", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Chi_squared_1,
                   min=greaterZero, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          df <- DistributionParameter$new(name="df", display_name="Degrees of freedom", 
                                          description = "Degrees of freedom", 
                                          value=1, default_val=1, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          private$parameter <- list(df=df)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rchisq(N, private$parameter$df$value)
            val[val < greaterZero] <- greaterZero
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("chi_square(", private$parameter$df$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          df <- private$parameter$df$value

          depDf <- private$parameter$df$getOtherVariableId()

          if(!is.null(depDf)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depDf)
            df <- ov$getName()
          }

          return(paste0("\\text{Chi\\_square}(", df,")"))
        }
      )
    )
  }
  #Exponential
  {
    OVDExponential <- R6Class(
      classname = "OVDExponential", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Exponential,
                   alternative=T,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          lambda <- DistributionParameter$new(name="lambda", display_name="rate", 
                                              description = "The rate value", 
                                              value=1, default_val=1, min_val=greaterZero, 
                                              max_val=Inf, discrete=F)
          private$parameter <- list(lambda=lambda)
          
          mu <- DistributionParameter$new(name="mu", display_name="mean", 
                                              description = "The mean", 
                                              value=1, default_val=1, min_val=greaterZero, 
                                              max_val=Inf, discrete=F)
          private$alternativeParameter <- list(mu=mu)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rexp(N, private$parameter$lambda$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("exponential(", private$parameter$lambda$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){

          lambda <- private$parameter$lambda$value
          depLambda <- private$parameter$lambda$getOtherVariableId()

          if(!is.null(depLambda)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depLambda)
            lambda <- ov$getName()
          }

          return(paste0("\\text{Exponential}(", lambda,")"))
        },
        
        transform = function(toAlt=T){
          if(toAlt){
            private$alternativeParameter$mu$value <- 1/private$parameter$lambda$value
          }else{
            private$parameter$lambda$value <- 1/private$alternativeParameter$mu$value
          }
        }
      )
    )
  }
  #Chi_squared_non_1
  {
    OVDChi_squared_non_1 <- R6Class(
      classname = "OVDChi_squared_non_1", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Chi_squared_non_1,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          df <- DistributionParameter$new(name="df", display_name="Degrees of freedom", 
                                          description = "Degrees of freedom", 
                                          value=2, default_val=2, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          private$parameter <- list(df=df)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rchisq(N, private$parameter$df$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("chi_square(", private$parameter$df$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){          

          df <- private$parameter$df$value
          depDf <- private$parameter$df$getOtherVariableId()

          if(!is.null(depDf)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depDf)
            df <- ov$getName()
          }

          return(paste0("\\text{Chi\\_square}(", df, ")"))
        }
      )
    )
  }
  #Weibull
  {
    OVDWeibull <- R6Class(
      classname = "OVDWeibull", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Weibull,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          lambda <- DistributionParameter$new(name="lambda", display_name="Scale", 
                                              description = "The scale value", 
                                              value=1, default_val=1, min_val=greaterZero, 
                                              max_val=Inf, discrete=F)
          kappa <- DistributionParameter$new(name="kappa", display_name="Shape", 
                                             description = "The shape value",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(lambda=lambda, kappa=kappa)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rweibull(N, scale=private$parameter$lambda$value, shape=private$parameter$kappa$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("weibull(", private$parameter$lambda$value, ",", private$parameter$kappa$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          kappa <- private$parameter$kappa$value
          lambda <- private$parameter$lambda$value
          
          depKappa <- private$parameter$kappa$getOtherVariableId()
          depLambda <- private$parameter$lambda$getOtherVariableId()
          
          if(!is.null(depKappa)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depKappa)
            kappa <- ov$getName()
          }
          if(!is.null(depLambda)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depLambda)
            lambda <- ov$getName()
          }
          
          return(paste0("\\text{Weibull}(", lambda, " , ", kappa,")"))
        }
      )
    )
  }
  #Normal
  {
    OVDNormal <- R6Class(
      classname = "OVDNormal", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Normal,
                   stateVersion = "0.2",
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=0, default_val=0, min_val=-Inf, 
                                          max_val=Inf, discrete=F)
          sigma <- DistributionParameter$new(name="sigma", display_name="Standard deviation", 
                                             description = "The standard deviation",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(mu=mu, sigma=sigma)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rnorm(N, private$parameter$mu$value, private$parameter$sigma$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("normal(", private$parameter$mu$value, ",", private$parameter$sigma$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          mu <- private$parameter$mu$value
          sigma <- private$parameter$sigma$value
          
          depMu <- private$parameter$mu$getOtherVariableId()
          depSigma <- private$parameter$sigma$getOtherVariableId()
          
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            mu <- ov$getName()
          }
          if(!is.null(depSigma)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depSigma)
            sigma <- ov$getName()
          }
          
          return(paste0("\\text{Normal}(", mu, " , ", sigma, ")"))
        }
      )
    )
  }
  #Cauchy
  {
    OVDCauchy <- R6Class(
      classname = "OVDCauchy", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Cauchy,
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          location <- DistributionParameter$new(name="location", display_name="Location", 
                                                description = "The location value", 
                                                value=0, default_val=0, min_val=-Inf, 
                                                max_val=Inf, discrete=F)
          scale <- DistributionParameter$new(name="scale", display_name="Scale", 
                                             description = "The scale",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(location=location, scale=scale)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rcauchy(N, private$parameter$location$value, private$parameter$scale$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("cauchy(", private$parameter$location$value, ",", private$parameter$scale$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          location <- private$parameter$location$value
          scale <- private$parameter$scale$value
          
          depLocation <- private$parameter$location$getOtherVariableId()
          depScale <- private$parameter$scale$getOtherVariableId()
          
          if(!is.null(depLocation)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depLocation)
            location <- ov$getName()
          }
          if(!is.null(depScale)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depScale)
            scale <- ov$getName()
          }
          
          return(paste0("\\text{Cauchy}(", location, " , ", scale,")"))
        }
      )
    )
  }
  #Beta
  {
    OVDBeta <- R6Class(
      classname = "OVDBeta", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Beta,
                   alternative=T,
                   min=0, max=1),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          alpha <- DistributionParameter$new(name="alpha", display_name="#Successes", 
                                             description = "The 'number of successes'", 
                                             value=2, default_val=2, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          beta <- DistributionParameter$new(name="beta", display_name="#Failures", 
                                            description = "The 'number of failures'",
                                            value=2, default_val=2, min_val=greaterZero, 
                                            max_val=Inf, discrete=F)
          private$parameter <- list(alpha=alpha, beta=beta)
          
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value (0-1)", 
                                          value=0.5, default_val=0.5, min_val=greaterZero, 
                                          max_val=1-greaterZero, discrete=F, isAlt=T)
          phi <- DistributionParameter$new(name="phi", display_name="Dispersion", 
                                                  description = "The shape deviation",
                                                  value=4, default_val=4, min_val=greaterZero, 
                                                  max_val=Inf, discrete=F, isAlt=T)
          private$alternativeParameter <- list(mu=mu, phi=phi)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rbeta(N, shape1=private$parameter$alpha$value, shape2=private$parameter$beta$value)
          })
          val[val==1] <- 0.999999999999999
          val[val==0] <- 0.000000000000001
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("beta(", private$parameter$alpha$value, ",", private$parameter$beta$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          alpha <- private$parameter$alpha$value
          beta <- private$parameter$beta$value
          
          depAlpha <- private$parameter$alpha$getOtherVariableId()
          depBeta <- private$parameter$beta$getOtherVariableId()
          
          if(!is.null(depAlpha)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depAlpha)
            alpha <- ov$getName()
          }
          if(!is.null(depBeta)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depBeta)
            beta <- ov$getName()
          }
          
          return(paste0("\\text{Beta}(", alpha, " , ", beta,")"))
        },
        transform = function(toAlt=T){
          if(toAlt){
            alpha <- private$parameter$alpha$value
            beta <- private$parameter$beta$value
            private$alternativeParameter$mu$value <- alpha/(alpha+beta)
            private$alternativeParameter$phi$value <- alpha+beta
          }else{
            mu <- private$alternativeParameter$mu$value
            phi <- private$alternativeParameter$phi$value
            private$parameter$alpha$value <- mu*phi
            private$parameter$beta$value <- (1-mu)*phi
          }
        }
      )
    )
  }
  #Logistic
  {
    OVDLogistic <- R6Class(
      classname = "OVDLogistic", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Logistic,
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=0, default_val=0, min_val=-Inf, 
                                          max_val=Inf, discrete=F)
          scale <- DistributionParameter$new(name="scale", display_name="Scale", 
                                             description = "The variance",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          private$parameter <- list(mu=mu, scale=scale)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rlogis(N, private$parameter$mu$value, private$parameter$scale$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("logistic(", private$parameter$mu$value, ",", private$parameter$scale$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){

          mu <- private$parameter$mu$value
          scale <- private$parameter$scale$value
          
          depMu <- private$parameter$mu$getOtherVariableId()
          depScale <- private$parameter$scale$getOtherVariableId()
          
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            mu <- ov$getName()
          }
          if(!is.null(depScale)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depScale)
            scale <- ov$getName()
          }
          
          return(paste0("\\text{Logistic}(", mu, " , ", scale,")"))
        }
      )
    )
  }
  #Uniform
  {
    OVDUniform <- R6Class(
      classname = "OVDUniform", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Uniform,
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          min <- DistributionParameter$new(name="min", display_name="Min", 
                                           description = "The min value", 
                                           value=-2, default_val=-2, min_val=-Inf, 
                                           max_val=Inf, discrete=F)
          max <- DistributionParameter$new(name="max", display_name="Max", 
                                           description = "The max",
                                           value=2, default_val=2, min_val=-Inf, 
                                           max_val=Inf, discrete=F)
          private$parameter <- list(min=min, max=max)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- runif(N, private$parameter$min$value, private$parameter$max$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("uniform(", private$parameter$min$value, ",", private$parameter$max$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          
          min <- private$parameter$min$value
          max <- private$parameter$max$value
          
          depMin <- private$parameter$min$getOtherVariableId()
          depMax <- private$parameter$max$getOtherVariableId()
          
          if(!is.null(depMin)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMin)
            min <- ov$getName()
          }
          if(!is.null(depMax)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMax)
            max <- ov$getName()
          }
          
          return(paste0("\\text{Uniform}(", min, " , ", max,")"))
        }
      )
    )
  }
  #Student_t
  {
    OVDStudent_t <- R6Class(
      classname = "OVDStudent_t", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Student_t,
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          df <- DistributionParameter$new(name="df", display_name="Degrees of freedom", 
                                          description = "Degrees of freedom", 
                                          value=3, default_val=3, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "Mean", 
                                          value=0, default_val=0, min_val=-Inf, 
                                          max_val=Inf, discrete=F)
          sigma <- DistributionParameter$new(name="sigma", display_name="Standard deviation", 
                                          description = "The variance (squared scale).", 
                                          value=2.5, default_val=2.5, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          
          private$parameter <- list(df=df, mu=mu, sigma=sigma)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rt(N, df=private$parameter$df$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("student_t(", private$parameter$df$value,",", 
          private$parameter$mu$value, ",", private$parameter$sigma$value, ")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){

          df <- private$parameter$df$value
          mu <- private$parameter$mu$value
          sigma <- private$parameter$sigma$value
          
          depDf <- private$parameter$df$getOtherVariableId()
          depMu <- private$parameter$mu$getOtherVariableId()
          depSigma <- private$parameter$sigma$getOtherVariableId()
          
          if(!is.null(depDf)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depDf)
            df <- ov$getName()
          }
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            df <- ov$getName()
          }
          if(!is.null(depSigma)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depSigma)
            df <- ov$getName()
          }
          
          return(paste0("\\text{Student_t}(", df, " , ", mu, " , ", sigma, ")"))
        }
      )
    )
  }
  #Poisson
  {
    OVDPoisson <- R6Class(
      classname = "OVDPoisson", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Poisson,
                   alternative=T,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          lambda <- DistributionParameter$new(name="lambda", display_name="Rate", 
                                              description = "The rate (also mean and variance) value", 
                                              value=5, default_val=5, min_val=greaterZero, 
                                              max_val=Inf, discrete=F)
          private$parameter <- list(lambda=lambda)
          
          mu <- DistributionParameter$new(name="mu", display_name="mean", 
                                          description = "The rate (also mean and variance) value", 
                                          value=5, default_val=5, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          private$alternativeParameter <- list(mu=mu)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rpois(N, private$parameter$lambda$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("poisson(", private$parameter$lambda$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          lambda <- private$parameter$lambda$value

          depLambda <- private$parameter$lambda$getOtherVariableId()

          if(!is.null(depLambda)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depLambda)
            lambda <- ov$getName()
          }
          
          return(paste0("\\text{Poisson}(", lambda, ")"))
        },

        transform = function(toAlt=T){
          if(toAlt){
            private$alternativeParameter$mu$value <- private$parameter$lambda$value
          }else{
            private$parameter$lambda$value <- private$alternativeParameter$mu$value 
          }
        }
      )
    )
  }
  #Negative_Binomial
  {
    OVDNegative_Binomial <- R6Class(
      classname = "OVDNegative_Binomial", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Negative_Binomial,
                   alternative=T,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          successes <- DistributionParameter$new(name="successes", display_name="#Successes", 
                                                 description = "The mean value", 
                                                 value=10, default_val=10, min_val=greaterZero, 
                                                 max_val=Inf, discrete=F)
          prob <- DistributionParameter$new(name="prob", display_name="Probability", 
                                            description = "The variance",
                                            value=0.5, default_val=0.5, min_val=0, 
                                            max_val=1, discrete=F)
          private$parameter <- list(successes=successes, prob=prob)
          
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value", 
                                          value=10, default_val=10, min_val=greaterZero, 
                                          max_val=Inf, discrete=F, isAlt=T)
          shape <- DistributionParameter$new(name="shape", display_name="Overdispersion", 
                                           description = "The overdispersion",
                                           value=20, default_val=20, min_val=greaterZero, 
                                           max_val=Inf, discrete=F, isAlt=T)
          private$alternativeParameter <- list(mu=mu, shape=shape)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rnbinom(N, size=private$parameter$successes$value, 
                          prob=private$parameter$prob$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("neg_binomial_2(", private$alternativeParameter$mu$value, ",", private$parameter$shape$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          mu <- private$parameter$mu$value
          shape <- private$parameter$shape$value
          
          depMu <- private$parameter$mu$getOtherVariableId()
          depShape <- private$parameter$shape$getOtherVariableId()
          
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            mu <- ov$getName()
          }
          if(!is.null(depShape)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depShape)
            shape <- ov$getName()
          }
          
          return(paste0("\\text{Neg\\_binomial\\_2}(", mu, " , ", shape,")"))
        },
        transform = function(toAlt=T){
          if(toAlt){
            successes <- private$parameter$successes$value
            prob <- private$parameter$prob$value
            var <- (successes*(1-prob))/prob**2
            private$alternativeParameter$mu$value <- (successes*(1-prob))/prob
            private$alternativeParameter$shape$value <- (mu**2)/(var-mu)
          }else{
            mu <- private$alternativeParameter$mu$value
            shape <- private$alternativeParameter$shape$value
            var <- mu + mu^2/shape
            private$parameter$successes$value <- mu**2/(var-mu)
            private$parameter$prob$value <- mu/var
          }
        }
      )
    )
  }
  #Geometric
  {
    OVDGeometric <- R6Class(
      classname = "OVDGeometric", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Geometric,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          prob <- DistributionParameter$new(name="prob", display_name="Probability", 
                                            description = "The probability",
                                            value=0.5, default_val=0.5, min_val=greaterZero, 
                                            max_val=1, discrete=F)
          private$parameter <- list(prob=prob)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rgeom(N, private$parameter$prob$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          #geometric is not implemented in stan, but is a special case of the negative binomial
          prob <- private$parameter$prob$value
          theta <- prob/(1+prob)
          return(paste0("neg_binomial(1,", theta,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          prob <- private$parameter$prob$value
          
          depProb <- private$parameter$prob$getOtherVariableId()
          
          if(!is.null(depProb)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depProb)
            prob <- ov$getName()
          }
          return(paste0("\\text{Neg\\_binomial}(", prob,")"))
        }
      )
    )
  }
  #Hypergeometric
  {
    OVDHypergeometric <- R6Class(
      classname = "OVDHypergeometric", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Hypergeometric,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          m <- DistributionParameter$new(name="m", display_name="#'Black' balls", 
                                         description = "The number of black balls in the urn",
                                         value=10, default_val=10, min_val=0, 
                                         max_val=Inf, discrete=T)
          
          n <- DistributionParameter$new(name="n", display_name="#'White' balls", 
                                         description = "The number of white balls in the urn",
                                         value=5, default_val=5, min_val=0, 
                                         max_val=Inf, discrete=T)
          
          k <- DistributionParameter$new(name="k", display_name="#Draws", 
                                         description = "The number of draws",
                                         value=5, default_val=5, min_val=0, 
                                         max_val=Inf, discrete=T)
          private$parameter <- list(m=m,n=n,k=k)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rhyper(N, private$parameter$m$value, private$parameter$n$value, 
                          private$parameter$k$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("hypergeometric(", private$parameter$k$value, ",", 
                        private$parameter$m$value, ",", private$parameter$n$value ,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          k <- private$parameter$k$value
          m <- private$parameter$m$value
          n <- private$parameter$n$value
          
          depK <- private$parameter$k$getOtherVariableId()
          depM <- private$parameter$m$getOtherVariableId()
          depN <- private$parameter$n$getOtherVariableId()
          
          if(!is.null(depK)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depK)
            k <- ov$getName()
          }
          if(!is.null(depM)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depM)
            m <- ov$getName()
          }
          if(!is.null(depN)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depN)
            m <- ov$getName()
          }
          return(paste0("\\text{Hypergeometric}(", k, " , ", m, " , ", n,")"))
        }
      )
    )
  }
  #Binomial
  {
    OVDBinomial <- R6Class(
      classname = "OVDBinomial", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Binomial,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          N <- DistributionParameter$new(name="N", display_name="Population size", 
                                         description = "The population size",
                                         value=20, default_val=20, min_val=1, 
                                         max_val=Inf, discrete=T)
          
          prob <- DistributionParameter$new(name="prob", display_name="Probability", 
                                            description = "The probability of success",
                                            value=.5, default_val=.5, min_val=0, 
                                            max_val=1, discrete=F)
          
          private$parameter <- list(N=N,prob=prob)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rbinom(N, private$parameter$N$value, private$parameter$prob$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("binomial(", private$alternativeParameter$N$value, ",", 
                        private$parameter$prob$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          N <- private$parameter$N$value
          prob <- private$parameter$prob$value
          
          depN <- private$parameter$N$getOtherVariableId()
          depProb <- private$parameter$prob$getOtherVariableId()
          
          if(!is.null(depN)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depN)
            N <- ov$getName()
          }
          if(!is.null(depProb)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depProb)
            prob <- ov$getName()
          }
          
          return(paste0("\\text{Binomial}(", N, " , ", prob,")"))
        }
      )
    )
  }
  #Beta_Binomial
  {
    OVDBeta_Binomial <- R6Class(
      classname = "OVDBeta_Binomial", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Beta_Binomial,
                   alternative=T,
                   min=0, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          N <- DistributionParameter$new(name="N", display_name="Population size", 
                                         description = "The population size",
                                         value=20, default_val=20, min_val=1, 
                                         max_val=Inf, discrete=T)
          
          alpha <- DistributionParameter$new(name="alpha", display_name="#Successes", 
                                             description = "The 'number of successes'", 
                                             value=2, default_val=2, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          beta <- DistributionParameter$new(name="beta", display_name="#Failures", 
                                            description = "The 'number of failures'",
                                            value=2, default_val=2, min_val=greaterZero, 
                                            max_val=Inf, discrete=F)
          private$parameter <- list(N=N, alpha=alpha, beta=beta)
          
          # N2 <- DistributionParameter$new(name="N", display_name="Population size", 
          #                                 description = "The population size",
          #                                 value=20, default_val=20, min_val=1, 
          #                                 max_val=Inf, discrete=T)
          
          mu <- DistributionParameter$new(name="mu", display_name="Mean", 
                                          description = "The mean value (0-1)", 
                                          value=0.5, default_val=0.5, min_val=greaterZero, 
                                          max_val=1-greaterZero, discrete=F, isAlt=T)
          phi <- DistributionParameter$new(name="phi", display_name="Dispersion", 
                                           description = "The shape deviation",
                                           value=4, default_val=4, min_val=greaterZero, 
                                           max_val=Inf, discrete=F, isAlt=T)
          private$alternativeParameter <- list(N=N, mu=mu, phi=phi)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rbeta_binomial(N, private$alternativeParameter$N$value,
                                  private$alternativeParameter$mu$value, 
                                  private$alternativeParameter$phi$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("neg_binomial_2(", private$alternativeParameter$mu$value, 
                        ",", private$parameter$phi$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          mu <- private$parameter$mu$value
          phi <- private$parameter$phi$value
          
          depMu <- private$parameter$mu$getOtherVariableId()
          depPhi <- private$parameter$phi$getOtherVariableId()
          
          if(!is.null(depMu)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depMu)
            mu <- ov$getName()
          }
          if(!is.null(depPhi)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depPhi)
            phi <- ov$getName()
          }
          
          return(paste0("\\text{Neg\\_binomial\\_2}(", mu, " , ", phi,")"))
        },
        transform = function(toAlt=T){
          if(toAlt){
            alpha <- private$parameter$alpha$value
            beta <- private$parameter$beta$value
            private$alternativeParameter$mu$value <- alpha/(alpha+beta)
            private$alternativeParameter$phi$value <- alpha+beta
          }else{
            mu <- private$alternativeParameter$mu$value
            phi <- private$alternativeParameter$phi$value
            private$parameter$alpha$value <- mu*phi
            private$parameter$beta$value <- (1-mu)*phi
          }
        }
      )
    )
  }
  #Bernoulli
  {
    OVDBernoulli <- R6Class(
      classname = "OVDBernoulli", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Bernoulli,
                   min=0, max=1),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          prob <- DistributionParameter$new(name="prob", display_name="Probability", 
                                            description = "The probability of success",
                                            value=.5, default_val=.5, min_val=0, 
                                            max_val=1, discrete=F)
          
          private$parameter <- list(prob=prob)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rbinom(N, 1, private$parameter$prob$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("bernoulli(", private$alternativeParameter$prob$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          
          prob <- private$parameter$prob$value

          depProb <- private$parameter$prob$getOtherVariableId()

          if(!is.null(depProb)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depProb)
            prob <- ov$getName()
          }
          
          return(paste0("\\text{Bernoulli}(",prob, ")"))
        }
      )
    )
  }
  #Horseshoe
  {
    OVDHorseshoe <- R6Class(
      classname = "OVDHorseshoe", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$Horseshoe,
                   min=-Inf, max=Inf),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)

          df <- DistributionParameter$new(name="df", display_name="df", 
                                          description = "Local shrinkage degrees of freedom", 
                                          value=4, default_val=4, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          global_df <- DistributionParameter$new(name="global_df", display_name="Global shrinkage degrees of freedom", 
                                             description = "Global shrinkage degrees of freedom",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          global_scale <- DistributionParameter$new(name="global_scale", display_name="Global shrinkage scale", 
                                          description = "Global shrinkage scale", 
                                          value=1, default_val=1, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          slab_df <- DistributionParameter$new(name="slab_df", display_name="Slab degrees of freedom", 
                                             description = "Slab degrees of freedom",
                                             value=1, default_val=1, min_val=greaterZero, 
                                             max_val=Inf, discrete=F)
          slab_scale <- DistributionParameter$new(name="slab_scale", display_name="Slab scale", 
                                          description = "Slab scale", 
                                          value=3, default_val=3, min_val=greaterZero, 
                                          max_val=Inf, discrete=F)
          private$parameter <- list(df=df, global_df=global_df, global_scale=global_scale,
                                    slab_df=slab_df, slab_scale=slab_scale)
        },
        randomValue = function(N, internalAssigned=T){
          withr::with_seed(private$seed, {
            val <- rhs(N, private$parameter$df$value, private$parameter$global_df$value,
                       private$parameter$global_scale$value, private$parameter$slab_df$value,
                       private$parameter$slab_scale$value)
          })
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(paste0("horseshoe(", private$parameter$df$value, ",", private$parameter$global_df$value, ",",
                        private$parameter$global_scale$value, ",", private$parameter$slab_df$value, ",",
                        private$parameter$slab_scale$value,")"))
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          if(localUse) browser()
          warning("Not used, not tested.")
          
          df <- private$parameter$df$value
          global_df <- private$parameter$global_df$value
          global_scale <- private$parameter$global_scale$value
          slab_df <- private$parameter$slab_df$value
          slab_scale <- private$parameter$slab_scale$value
          
          depDf <- private$parameter$df$getOtherVariableId()
          depGlobal_df <- private$parameter$global_df$getOtherVariableId()
          depGlobal_scale <- private$parameter$global_scale$getOtherVariableId()
          depSlab_df <- private$parameter$slab_df$getOtherVariableId()
          depSlab_scale <- private$parameter$slab_scale$getOtherVariableId()
          
          if(!is.null(depDf)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depDf)
            df <- ov$getName()
          }
          if(!is.null(depGlobal_df)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depGlobal_df)
            global_df <- ov$getName()
          }
          if(!is.null(depGlobal_scale)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depGlobal_scale)
            global_scale <- ov$getName()
          }
          if(!is.null(depSlab_df)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depSlab_df)
            slab_df <- ov$getName()
          }
          if(!is.null(depSlab_scale)){
            if(is.null(mcd) && localUse) browser()#Should not be null
            ov <- mcd$getOtherVariable(depSlab_scale)
            slab_scale <- ov$getName()
          }
          return(paste0("\\text{Horseshoe}(", df, " , ", global_df, " , ", global_scale, " , ", slab_df, " , ", slab_scale, ")"))
        }
      )
    )
  }
  #Fixed single value
  {
    OVDFixedValue <- R6Class(
      classname = "OVDFixedValue", 
      inherit = ModelCreatingDataOtherVariableDistributionAbstract,
      
      private=list(name=enum$FixedValue,
                   min=0, max=1),
      public=list(
        initialize = function(seed=NULL,
                              emptyState = F){
          super$initialize(seed, emptyState=emptyState)
          value <- DistributionParameter$new(name="value", display_name="Value", 
                                            description = "The value",
                                            value=1, default_val=1, min_val=-Inf, 
                                            max_val=Inf, discrete=F)
          
          private$parameter <- list(value=value)
        },
        randomValue = function(N, internalAssigned=T){
          val <- rep(private$parameter$value$value,N)
          if(private$negateValues) val <- val*-1
          if(internalAssigned) private$values <- val
          return(val)
        },
        getAsStanSyntax = function(){
          return(private$parameter$value$value)
        },
        
        getAsLatexSyntax = function(mcd=NULL){
          return(private$parameter$value$value)
        }
      )
    )
  }
}



################################################################################
##################### Categorical model for other predictor ####################
################################################################################
{
  ModelCreatingDataOtherVariableCategorical <- R6Class(
    classname = "ModelCreatingDataOtherVariableCategorical", 
    inherit = SerializationInterface,
    
    private = list(
      stateVersion = "0.1",
      type="", #'independent' 'subgroup' 'replacement'
      
      #Only used when type=='subgroup'
      subgroupExplicit=F,
      upperGroupElements=NULL,
      subgroupOf=NULL, #other variable id
      subgroupOfNames=NULL, #other variable name(s) c(outermost:"sex", ..., innermost(subgroup):"size")
      
      #Only used when type=='replacement'
      replaceValuesOf=NULL, #other variable id
      replaceValues=NULL, #list of (2 element) vectors list([[1]] = c(valueToReplaced, replaceValue), [[2]]= c(...), ...)
      
      #if type=='subgroup' all following parameters are named lists
      #names are the elements of the super group(s) 
      values=NULL, #values e.g. c("female", "male") or list(female=c("a","b"), male=c("a","c"))
      valuesDistributed="equally", #c("equally","random","explicit")
      valuesFrequency=1, #how values are distributed e.g. c(1,1)
      
      capped=T,
      randomize=F,
      randomParameter=10, #for "random"
      seed=round(runif(1,-10000,10000)),
      
      
      getInternalParameter = function(para, super){
        super <- self$splitSuperElement(super)
        if(!is.list(para) || length(super)==0) return(NULL)
        if(length(super)>1){
          return(private$getInternalParameter(para[[first(super)]], last(super,n=-1)))
        }else{
          return(para[[first(super)]])
        }
      },
      setInternalParameter = function(para, super, val){
        super <- self$splitSuperElement(super)
        if(is.null(val)) val <- ""
        if(is.list(para)){
          if(length(super) == 0){
            para <- val
          }else{
            para[[super[1]]] <- private$setInternalParameter(para[[super[1]]], super[-1], val)
          }
        }else{
          if(length(super) == 0){
            para <- val
          }else{
            para <- list(private$setInternalParameter(list(), super[-1], val))
            names(para) <- super[1]
          }
        }
        return(para)
      },
      setAllInternalParameter = function(para, val){
        el <- private$upperGroupElements
        if(is.list(el)) el <- unlist(nested_list_for_cat_subgroups(private$upperGroupElements))
        for(e in el){
          para <- private$setInternalParameter(para, e, val)
        }
        return(para)
      },
      removeInternalParameter = function(para, super){
        if(is.null(super) || super =="") return(para)
        if(!is.list(para)) return(para)
        
        new_para <- list()
        
        super <- self$splitSuperElement(super)
      }
    ),
    
    public = list(
      
      initialize = function(seed,
                            emptyState = F){
        super$initialize()
        if(emptyState) return()
        private$seed <- seed
      },
      
      setType = function(type, superElement=NULL, asIt=F){
        if(asIt){
          private$type <- type
          return()
        }
        tmp <- private$type
        private$type <- type
        if(tmp =="independent" && type=="subgroup"){
          self$initAsSubgroup()
        }else if(tmp =="subgroup" && type=="independent"){
          self$initAsSubgroup(T)
        }else if(type != tmp){
          self$clear()
        }
      },
      getType = function(superElement=NULL){
        if(is.null(superElement)){
          return(private$type)
        }else{
          return(private$getInternalParameter(private$type, superElement))
        }
      },
      
      setValues = function(values, superElement=NULL, asIt=F){
        if(asIt){
          private$values <- values
          return()
        }
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$values <- private$setAllInternalParameter(private$values, values)
            if(!is.null(private$valuesDistributed) &&
               unlist(private$valuesDistributed)[[1]]=="equally") 
              private$valuesFrequency <- private$setAllInternalParameter(private$valuesFrequency, rep(1,length(values)))
          }else{
            private$values <- values
            if(!is.null(private$valuesDistributed) &&
               private$valuesDistributed=="equally") 
              private$valuesFrequency <- rep(1,length(values))
          }
        }else{
          private$values <- private$setInternalParameter(private$values, superElement, values)
          if(!is.null(private$valuesDistributed) &&
             private$getInternalParameter(private$valuesDistributed, superElement)=="equally") 
            private$valuesFrequency <- private$setInternalParameter(private$valuesFrequency, superElement, rep(1,length(values)))
        }
      },
      #dp:datapoints or batches: replicate (values*valuesFreq)
      getValues = function(dp=NULL, batches=NULL, superElement=NULL){
        if(!is.null(dp) && is.numeric(dp)){
          if(private$type=="subgroup"){
            val <- self$getValuesSubset()
            fac <- ceiling(dp/length(val))
            val <- rep(val,fac)
            return(val[1:dp])
          }else{
            freq <- private$valuesFrequency
            if(private$valuesDistributed == "equally") freq <- rep(1,length(private$values))
            val <- rep(private$values, freq)
            return(rep(val,length=dp))
          }
        }else if(!is.null(batches) && is.numeric(batches)){
          if(private$type=="subgroup"){
            val <- self$getValuesSubset()
            return(rep(val,batches))
          }else{
            freq <- private$valuesFrequency
            if(private$valuesDistributed == "equally") freq <- rep(1,length(private$values))
            val <- rep(private$values, freq)
            return(rep(val,batches))
          }
        }else{
          if(is.null(superElement)){
            return(private$values)
          }else{
            return(private$getInternalParameter(private$values, superElement))
          }
        }
      },
      
      #just the (right number of) elements of the values (leafs)
      getValuesSubset = function(){
        totalVal <- c()
        for(e in private$upperGroupElements){
          val <- private$getInternalParameter(private$values,e)
          freq <- private$getInternalParameter(private$valuesFrequency,e)
          val <- rep(val, freq)
          totalVal <- c(totalVal, val)
        }
        return(totalVal)
      },
      
      #This function is used for distributions that have a parameter dependency to
      #this oV. For example the mu parameter of a normal distribution depends on this.
      #This is only valid, if this oV values are all numeric.
      #Used only for preview table/plot within oV modal
      getValuesForNumericDependency = function(){
        if(private$type == "independent"){
          values <- as.numeric(private$values)
          if(any(is.na(values)) || any(is.empty(values))){
            stop("Some values are no numerics!") #TODO if never happened, replace with warning
          }
          return(values)
        }else if(private$type == "subgroup"){
          values <- as.numeric(unlist(private$values))
          if(any(is.na(values)) || any(is.empty(values))){
            stop("Some values are no numerics!") #TODO if never happened, replace with warning
          }
          return(values)
        }else if(private$type == "replacement"){
          rep <- c()
          for(r in private$replaceValues){
            if(r[2] == ""){
              rep <- c(rep, r[1])
            }else{
              rep <- c(rep, r[2])
            }
          }
          values <- as.numeric(rep)
          if(any(is.na(values)) || any(is.empty(values))){
            stop("Some values are no numerics!") #TODO if never happened, replace with warning
          }
          return(values)
        }
      },
      
      getValuesForFactors = function(values = NULL, fac=c()){
        
        if(is.null(values)) values <- private$values
        if(is.list(values)){
          for(val in values){
            fac <- self$getValuesForFactors(val, fac)
          }
        }else{
          fac <- c(fac, setdiff(values,fac))
        }
        
        return(fac)
      },
      
      #reorders the values (only for subgroups) if explicitly values are used 
      #sex: "m","f" -> cat (subgroup) m: "a","b"; f: "b","a" -> f: "a", "b"
      reorderValues = function(){
        ordered <- self$reorderValue(self$getValues(), fac=c())
        self$setValues(values=ordered[[1]], asIt=T)
      },
      
      reorderValue = function(values, fac){
        orderedValues <- values
        if(is.list(values)){
          for(v_i in seq_len(length(values))){
            val <- values[[v_i]]
            newOrder <- self$reorderValue(val, fac)
            values[[v_i]] <- newOrder[[1]]
            fac <- newOrder[[2]]
          }
          orderedValues <- values
        }else{
          val <- unique(values)
          if(!all(val %in% fac)){
            fac <- c(fac, setdiff(val, fac))
          }
          orderedValues <- values[order(match(values, fac))]
        }
        return(list(orderedValues,fac))
      },
      
      getLeafOfFirstElement = function(data){
        while(is.list(data)){
          data <- data[[1]]
        }
        return(data)
      },
      
      setValuesDistributed = function(valuesDistributed=c("equally","explicit"),
                                      superElement=NULL, asIt=F){
        if(asIt){
          private$valuesDistributed <- valuesDistributed
          return()
        }
        if(is.null(valuesDistributed)) return()
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$valuesDistributed <- private$setAllInternalParameter(private$valuesDistributed, valuesDistributed)
            if(!is.null(private$valuesDistributed) &&
               unlist(private$valuesDistributed)[[1]]=="equally") 
              private$valuesFrequency <- private$setAllInternalParameter(private$valuesFrequency, rep(1,length(self$getLeafOfFirstElement(private$values))))
          }else{
            private$valuesDistributed <- valuesDistributed
            if(!is.null(private$valuesDistributed) &&
               private$valuesDistributed=="equally") 
              private$valuesFrequency <- rep(1,length(private$values))
          }
        }else{
          private$valuesDistributed <- private$setInternalParameter(private$valuesDistributed, superElement, valuesDistributed)
          if(!is.null(private$valuesDistributed) &&
             private$getInternalParameter(private$valuesDistributed, superElement)=="equally") 
            private$valuesFrequency <- private$setInternalParameter(private$valuesFrequency, superElement, rep(1,length(self$getLeafOfFirstElement(private$values))))
        }
      },
      
      getValuesDistributed = function(superElement=NULL){
        if(is.null(superElement)){
          return(private$valuesDistributed)
        }else{
          return(private$getInternalParameter(private$valuesDistributed, superElement))
        }
      },
      
      setValuesFrequency = function(valuesFrequency, superElement=NULL,
                                    asIt=F){
        if(asIt){
          private$valuesFrequency <- valuesFrequency
          return()
        }
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$valuesFrequency <- private$setAllInternalParameter(private$valuesFrequency, valuesFrequency)
          }else{
            private$valuesFrequency <- valuesFrequency
          }
        }else{
          private$valuesFrequency <- private$setInternalParameter(private$valuesFrequency, superElement, valuesFrequency)
        }
      },
      getValuesFrequency= function(superElement=NULL){
        if(is.null(superElement)){
          return(private$valuesFrequency)
        }else{
          return(private$getInternalParameter(private$valuesFrequency, superElement))
        }
      },
      
      setCapped = function(flag, superElement=NULL, asIt=F){ 
        if(asIt){
          private$capped <- flag
          return()
        }
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$capped <- private$setAllInternalParameter(private$capped, flag)
          }else{
            private$capped <- flag
          }
        }else{
          private$capped <- private$setInternalParameter(private$capped, superElement, flag)
        }
      },
      getCapped = function(superElement=NULL){ 
        if(is.null(superElement)){
          return(private$capped)
        }else{
          return(private$getInternalParameter(private$capped, superElement))
        }
      },
      
      setRandomize = function(flag, superElement=NULL, asIt=F){ 
        if(asIt){
          private$randomize <- flag
          return()
        }
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$randomize <- private$setAllInternalParameter(private$randomize, flag)
          }else{
            private$randomize <- flag
          }
        }else{
          private$randomize <- private$setInternalParameter(private$randomize, superElement, flag)
        }
      },
      getRandomize = function(superElement=NULL){ 
        if(is.null(superElement)){
          return(private$randomize)
        }else{
          return(private$getInternalParameter(private$randomize, superElement))
        }
      },

      
      setRandomParameter = function(randomParameter, superElement=NULL, asIt=F){
        if(asIt){
          private$randomParameter <- randomParameter
          return()
        }
        if(!is.numeric(randomParameter) || randomParameter <0) return()
        if(randomParameter==0) randomParameter <- .Machine$double.xmin
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$randomParameter <- private$setAllInternalParameter(private$randomParameter, randomParameter)
          }else{
            private$randomParameter <- randomParameter
          }
        }else{
          private$randomParameter <- private$setInternalParameter(private$randomParameter, superElement, randomParameter)
        }
      },
      getRandomParameter = function(superElement=NULL){
        if(is.null(superElement)){
          return(private$randomParameter)
        }else{
          return(private$getInternalParameter(private$randomParameter, superElement))
        }
      },
      
      setSubgroupOf = function(subgroupOf){
        private$subgroupOf <- subgroupOf
      },
      getSubgroupOf = function(){ 
        return(private$subgroupOf)
      },
      
      setSubgroupOfNames = function(subgroupOfNames, new=F){
        if(new){
          private$subgroupOfNames <- subgroupOfNames
        }else{
          private$subgroupOfNames <- subgroupOfNames
        }
      },
      getSubgroupOfNames = function(){ 
        return(private$subgroupOfNames)
      },
      addSubgroupOfNames = function(name, reverse=F){
        if(reverse){
          private$subgroupOfNames <- c(name, private$subgroupOfNames)
        }else{
          private$subgroupOfNames <- c(private$subgroupOfNames, name)
        }
      },
      
      setUpperGroupElements = function(upperGroupElements, new=F){
        el_old <- private$upperGroupElements
        if(is.list(el_old)) el_old <- unlist(nested_list_for_cat_subgroups(private$upperGroupElements))
        
        el_new <- upperGroupElements
        if(is.list(el_new)) el_new <- unlist(nested_list_for_cat_subgroups(upperGroupElements))
        
        if(is.null(el_old) && is.null(el_new)) return()
        if(!is.null(el_old) && !is.null(el_new)){
          if(length(el_old) == length(el_new)){
            flag <- T
            for(i in seq_len(length(el_old))){
              if(el_old[[i]] != el_new[[i]]){
                flag <- F
                break
              }
            }
            if(flag) return()
          }
        }
        
        if(new){
          el_old <- private$upperGroupElements
          if(is.list(el_old)) el_old <- unlist(nested_list_for_cat_subgroups(private$upperGroupElements))
          valuesDistributed_old <- private$valuesDistributed
          values_old <- private$values
          valuesFrequency_old <- private$valuesFrequency
          capped_old <- private$capped
          randomize_old <- private$randomize
          randomParameter_old <- private$randomParameter
          seed_old <- private$seed
          
          subgroupOf_old <- self$getSubgroupOf()
          subgroupOfNames_old <- self$getSubgroupOfNames()
          subgroupExplicit_old <- self$getSubgroupExplicit()
          
          self$clear()
          
          private$subgroupOf <- subgroupOf_old
          private$subgroupOfNames <- subgroupOfNames_old
          private$upperGroupElements <- upperGroupElements
          private$subgroupExplicit <- subgroupExplicit_old
          
          el <- upperGroupElements
          if(is.list(el)) el <- unlist(nested_list_for_cat_subgroups(private$upperGroupElements))
          
          
          self$setValuesDistributed("equally",NULL)
          self$setValues(NULL, NULL)
          self$setValuesFrequency(1, NULL)
          self$setCapped(T, NULL)
          self$setRandomize(F, NULL)
          self$setRandomParameter(0, NULL)
          self$setSeed(0,NULL)
          
          e_intersect <- intersect(el,el_old)
          
          for(e in e_intersect){
            private$valuesDistributed <- private$setInternalParameter(private$valuesDistributed, e, private$getInternalParameter(valuesDistributed_old,e))
            private$values <- private$setInternalParameter(private$values, e, private$getInternalParameter(values_old,e))
            private$valuesFrequency <- private$setInternalParameter(private$valuesFrequency, e, private$getInternalParameter(valuesFrequency_old,e))
            private$capped <- private$setInternalParameter(private$capped, e, private$getInternalParameter(capped_old,e))
            private$randomize <- private$setInternalParameter(private$randomize, e, private$getInternalParameter(randomize_old,e))
            private$randomParameter <- private$setInternalParameter(private$randomParameter, e, private$getInternalParameter(randomParameter_old,e))
            se <- private$setInternalParameter(private$seed, e, private$getInternalParameter(seed_old,e))
            self$setSeed(se, asIt=T)
          }
          
          
          if(!private$subgroupExplicit && !is.empty(el_old)){
            el_ref <- el_old[[1]]
            e_new <- setdiff(el, el_old)
            for(e in e_new){
              private$valuesDistributed <- private$setInternalParameter(private$valuesDistributed, e, private$getInternalParameter(valuesDistributed_old,el_ref))
              private$values <- private$setInternalParameter(private$values, e, private$getInternalParameter(values_old,el_ref))
              private$valuesFrequency <- private$setInternalParameter(private$valuesFrequency, e, private$getInternalParameter(valuesFrequency_old,el_ref))
              private$capped <- private$setInternalParameter(private$capped, e, private$getInternalParameter(capped_old,el_ref))
              private$randomize <- private$setInternalParameter(private$randomize, e, private$getInternalParameter(randomize_old,el_ref))
              private$randomParameter <- private$setInternalParameter(private$randomParameter, e, private$getInternalParameter(randomParameter_old,el_ref))
              se <- private$setInternalParameter(private$seed, e, private$getInternalParameter(seed_old,el_ref))
              self$setSeed(se, asIt=T)
            }
          }
          
        }else{
          private$upperGroupElements <- upperGroupElements
        }
      },
      getUpperGroupElements = function(){ 
        return(private$upperGroupElements)
      },
      
      setSubgroupExplicit = function(flag){
        private$subgroupExplicit <- flag
      },
      getSubgroupExplicit = function(){
        return(private$subgroupExplicit)
      },
      
      setReplaceValuesOf = function(replaceValuesOf, asIt=F){
        if(asIt){
          private$replaceValuesOf <- replaceValuesOf
          return()
        }
        private$replaceValuesOf <- replaceValuesOf
      },
      getReplaceValuesOf = function(){
        return(private$replaceValuesOf)
      },
      
      setReplaceValues = function(replaceValues, el=NULL, asIt=F){
        if(asIt){
          private$replaceValues <- replaceValues
          return()
        }
        if(!is.null(el)){
          for(i in seq_len(length(private$replaceValues))){
            if(private$replaceValues[[i]][1]==el){
              private$replaceValues[[i]] <- c(private$replaceValues[[i]][1],replaceValues)
              return()
            }
          }
          private$replaceValues <- list.append(private$replaceValues, c(el,replaceValues))
        }else{
          private$replaceValues <- replaceValues
        }
      },
      #replaceValue: c(valueToReplaced, replaceValue)
      addReplaceValues = function(replaceValue){
        private$replaceValues <- list.append(private$replaceValues, replaceValue)
      },
      getReplaceValues = function(el=NULL){
        if(!is.null(el)){
          for(e in private$replaceValues){
            if(e[[1]] == el) return(e)
          }
          return(NULL)
        }else{
          return(private$replaceValues)
        }
      },
      removeReplaceValue = function(el){
        if(!is.null(el)){
          newList <- list()
          for(e in private$replaceValues){
            if(e[[1]] != el) newList <- list.append(newList, e)
          }
          private$replaceValues <- newList
        }
      },
      
      setSeed = function(seed, superElement=NULL, asIt=F){
        if(asIt){
          private$seed <- seed
          return()
        }
        if(!is.numeric(seed)) return()
        if(seed != round(seed)) return()
        if(seed < -.Machine$integer.max || seed > .Machine$integer.max) return()
        if(is.null(superElement)){
          if(private$type=='subgroup'){
            private$seed <- private$setAllInternalParameter(private$seed, seed)
          }else{
            private$seed <- seed
          }
        }else{
          private$seed <- private$setInternalParameter(private$seed, superElement, seed)
        }
      },
      getSeed = function(superElement=NULL){ 
        if(is.null(superElement)){
          return(private$seed)
        }else{
          return(private$getInternalParameter(private$seed, superElement))
        }
      },
      
      isValid = function(valuesDistributed=NULL, superElement=NULL, constraints=NULL){
        
        ##No supergroup selected; replacement constraints violated
        if(private$type=="replacement"){
          if(is.null(private$replaceValuesOf) || private$replaceValuesOf == ""){
            return(list(valid=F, message="Please select a variable to be replaced."))
          }else{
            if("integer" %in% constraints){
              if(!self$isInteger()){
                return(list(valid=F, message="Only integer values are allowed!"))
              }
            }
            if("positive" %in% constraints){
              if(!self$isPositive()){
                return(list(valid=F, message="Only positive values are allowed!"))
              }
            }
            return(list(valid=T, message=""))
          }
        }else if(private$type=="subgroup"){
          if(is.null(private$subgroupOf) || private$subgroupOf == ""){
            return(list(valid=F, message="Please select a super group"))
          }
        }

        
        ##For subgroups: if constraints are violated
        #if is.null(superElement)  && private$type=='subgroup' && private$subgroupExplicit
        #--> call isValid by iterating through superElement
        if(is.null(superElement) && 
           private$type=='subgroup'){ #&& private$subgroupExplicit
          
          el <- private$upperGroupElements
          if(is.list(el)) el <- unlist(nested_list_for_cat_subgroups(private$upperGroupElements))
          
          for(e in el){
            ret <- self$isValid(valuesDistributed=valuesDistributed, superElement=e,
                                constraints=constraints)
            if(!ret$valid) return(ret)
          }
          if("integer" %in% constraints){
            if(!self$isInteger()){
              return(list(valid=F, message="Only integer values are allowed!"))
            }
          }
          if("positive" %in% constraints){
            if(!self$isPositive()){
              return(list(valid=F, message="Only positive values are allowed!"))
            }
          }
          return(list(valid=T, message=""))
        }
        
        valuesFrequency <- private$valuesFrequency
        values <- private$values
        randomParameter <- private$randomParameter
        
        if(private$type=="subgroup" && is.null(private$subgroupOf)){
          return(list(valid=F, message="No 'subgroup of' setted"))
        } 
        
        if(is.null(superElement)){
          if(is.null(valuesDistributed)){
            if(private$type=='subgroup'){
              valuesDistributed <- self$getLeafOfFirstElement(private$valuesDistributed) #just take the first one, since all have to be equal
              if(!is.null(private$valuesFrequency)) valuesFrequency <- self$getLeafOfFirstElement(private$valuesFrequency) #just take the first one, since all have to be equal
              if(!is.null(private$values)) values <- self$getLeafOfFirstElement(private$values) #just take the first one, since all have to be equal
              if(!is.null(private$randomParameter)) randomParameter <- private$randomParameter[[1]] #just take the first one, since all have to be equal
            }else{
              valuesDistributed <- private$valuesDistributed
            }
          }
        }else{
          if(is.null(valuesDistributed))
            valuesDistributed <- private$getInternalParameter(private$valuesDistributed, superElement)
          valuesFrequency <- private$getInternalParameter(private$valuesFrequency, superElement)
          values <- private$getInternalParameter(private$values, superElement)
          randomParameter <- private$getInternalParameter(private$randomParameter, superElement)
        }
        
        ret <- list(valid=T, message="")
        
        
        if(valuesDistributed == "explicit"){
          if(is.null(valuesFrequency) || length(valuesFrequency)==0){
            ret$valid <- F
            ret$message <- "Frequencies have to be given"
          }else if(any(is.na(as.numeric(valuesFrequency)))){
            ret$valid <- F
            ret$message <- "All elements have to be numeric"
          }else if(all(round(as.numeric(valuesFrequency))!=as.numeric(valuesFrequency))){
            ret$valid <- F
            ret$message <- "Frequencies have to be integer values"
          }else if(length(valuesFrequency) != length(values)){
            ret$valid <- F
            ret$message <- "Number of values and frequencies have to be equal."
          }else if(any(as.numeric(valuesFrequency) > 10 | as.numeric(valuesFrequency) < 1)){
            ret$valid <- F
            ret$message <- "Frequencies have to be in a range of [1-10]"
          }
        }else if(valuesDistributed == "random"){
          para <- randomParameter
          if(!is.numeric(para) || para < 0){
            ret$valid <- F
            ret$message <- "Random parameter have to be numeric and greater 0!"
          }
        }else{
          val <- values
          val_s <- unique(val)
          # if(length(val_s)<2){
          #   ret$valid <- F
          #   ret$message <- "There have to be at least two different values"
          #   if(private$type=="subgroup"){
          #     ret$message <- paste0(ret$message, " for each element")
          #   }
          # }else 
            if(any(val_s=="")){
            ret$valid <- F
            ret$message <- "Empty elements are not allowed"
          }
        }
        if("integer" %in% constraints){
          if(!self$isInteger()){
            return(list(valid=F, message="Only integer values are allowed!"))
          }
        }
        if("positive" %in% constraints){
          if(!self$isPositive()){
            return(list(valid=F, message="Only positive values are allowed!"))
          }
        }

        return(ret)
      },
      
      #returns true if all values are numeric
      isNumeric = function(superElement=NULL){
        values <- c()
        if(private$type=="replacement"){
          rep <- c()
          for(r in private$replaceValues){
            if(r[2] == ""){
              rep <- c(rep, r[1])
            }else{
              rep <- c(rep, r[2])
            }
          }
          suppressWarnings(values <- as.numeric(rep))
          if(any(is.na(values)) || any(is.empty(values))){
            return(F)
          }
          return(T)
        }else{
          values <- private$values
          if(!is.null(superElement)){
            values <- private$getInternalParameter(private$values, superElement)
          }else{
            values <- unlist(values)
          }
        }
        return(all(!is.na(suppressWarnings(as.numeric(values)))))
      },
      
      #returns true if all values are integers
      isInteger = function(superElement=NULL){
        values <- c()
        if(private$type=="replacement"){
          rep <- c()
          for(r in private$replaceValues){
            if(r[2] == ""){
              rep <- c(rep, r[1])
            }else{
              rep <- c(rep, r[2])
            }
          }
          values <- as.numeric(rep)
          if(any(is.na(values)) || any(is.empty(values))){
            return(F)
          }
          return(all(values==round(values)))
        }else{
          values <- private$values
          if(!is.null(superElement)){
            values <- private$getInternalParameter(private$values, superElement)
          }else{
            values <- unlist(values)
          }
        }
        if(all(!is.na(suppressWarnings(as.numeric(values))))){
          return(all(as.numeric(values)==round(as.numeric(values))))
        }else{
          return(F)
        }
      },
      
      #returns true if all values are positive
      isPositive = function(superElement=NULL){
        values <- c()
        if(private$type=="replacement"){
          rep <- c()
          for(r in private$replaceValues){
            if(r[2] == ""){
              rep <- c(rep, r[1])
            }else{
              rep <- c(rep, r[2])
            }
          }
          values <- as.numeric(rep)
          if(any(is.na(values)) || any(is.empty(values))){
            return(F)
          }
          return(all(values>=0))
        }else{
          values <- private$values
          if(!is.null(superElement)){
            values <- private$getInternalParameter(private$values, superElement)
          }else{
            values <- unlist(values)
          }
        }
        if(all(!is.na(suppressWarnings(as.numeric(values))))){
          return(all(as.numeric(values)>=0))
        }else{
          return(F)
        }
      },
      
      #Returns the dataset of replace values of original oV
      getReplacedValues = function(replaceOfoV, df=NULL){
        if(is.null(df)){
          replaceOfoVCatModel <- replaceOfoV$getCategoricalModel()
          df <- replaceOfoVCatModel$getTableData(replaceOfoV$getName())
        }else{
          replaceOfoVCatModel <- replaceOfoV$getCategoricalModel()
          df_alt <- replaceOfoVCatModel$getTableData(replaceOfoV$getName())
          n <- names(df_alt)
          df <- df[,n, drop=F]
          df <- df[complete.cases(df),,drop=F]
        }
        
        n <- names(df)
        for(na in n){
          df[[na]] <- as.character(df[[na]])
        }
        df <- cbind(df, df[[dim(df)[2]]])
        
        repl <- private$replaceValues
        
        for(i in repl){
          if(i[[2]]=="")next
          df_copy <- df
          super <- str_split(i[[1]], ":")[[1]]
          for(j_index in seq_len(length(super))){
            j <- super[j_index]
            df_copy[df_copy[[j_index]]!=j,(j_index:dim(df_copy)[2])] <- ""
          }
          newCol <- df_copy[[dim(df_copy)[2]]]
          df[newCol != "",dim(df)[[2]]] <- i[[2]]
        }
        return(df[[dim(df)[2]]])
      },
      
      getReplacementTable = function(replaceOfoV, name, filter=T){
        
        dt <- datatable(data.frame())
        replaceOfoVCatModel <- replaceOfoV$getCategoricalModel()
        valid <- replaceOfoVCatModel$isValid()
        
        if(valid$valid){
          df <- replaceOfoVCatModel$getTableData(replaceOfoV$getName())
          
          n <- names(df)
          df <- cbind(df, df[[dim(df)[2]]])
          names(df) <- c(n, name)
          
          df[[dim(df)[2]]] <- self$getReplacedValues(replaceOfoV)
          
          if(filter){
            dt <- datatable(df, filter="top", rownames=F,
                            options=list(paging=F,searching=T, info=F, ordering=T, sDom  = '<"top">lrt<"bottom">ip'))
          }else{
            dt <- datatable(df, rownames=F,
                            options=list(paging=F,searching=F, info=F, ordering=T))
          }
          
          dt <- dt %>%
            formatStyle(colnames(df), lineHeight="50%")
          
          return(dt)
        }else{
          return(dt)
        }
      },
      
      getTable = function(varName, filter=T){
        
        dt <- datatable(data.frame())
        
        ret <- self$isValid()
        # if(!ret$valid) return(dt)
        
        df <- self$getTableData(varName)
        
        if(filter){
          dt <- datatable(df, filter="top", rownames=F,
                          options=list(paging=F,searching=T, info=F, ordering=T, sDom  = '<"top">lrt<"bottom">ip'))
        }else{
          dt <- datatable(df, rownames=F,
                          options=list(paging=F,searching=F, info=F, ordering=T))
        }
        
        dt <- dt %>%
          formatStyle(colnames(df), lineHeight="50%")
        
        return(dt)
      },
      
      getTableData = function(varName){
        
        df <- data.frame()
        if(private$type=="subgroup"){
          
          df <- self$getTableDataSubgroup(varName=varName, data=private$values, 
                                          freq=private$valuesFrequency)
          
        }else{
          val <- private$values
          if(is.null(val)) val <- ""
          if(private$valuesDistributed == "equally"){
            df <- data.frame(val)
          }else if(private$valuesDistributed == "explicit"){
            if(any(is.na(private$valuesFrequency))){
              private$valuesFrequency <- rep(1, length(val))
            }
            freq <- private$valuesFrequency
            freq <- freq[1:max(1,length(val))]
            df <- data.frame(rep(val, freq))
          }
          if(dim(df)[2] == length(varName))
            names(df) <- varName
        }
        return(df)
      },
      
      #returns the dataset for type=="subgroup"
      getTableDataSubgroup = function(varName, data, freq, depth=1, name=NULL){
        df <- data.frame()
        if(is.list(data)){
          for(n in names(data)){
            df_depth <- self$getTableDataSubgroup(varName, data[[n]], freq[[n]], 
                                                  depth+1, paste(c(name,n),collapse=":"))
            
            df_depth <- cbind(new=n,df_depth)
            new.n <- names(df_depth)
            new.n[1] <- private$subgroupOfNames[depth]
            names(df_depth) <- new.n
            if(is.empty(df)){
              df <- df_depth
            }else{
              df <- rbind(df,df_depth)
            }
          }
        }else{
          if(any(is.na(freq))){
            freq <- rep(1, length(data))
          }
          freq <- freq[1:max(1,length(data))]
          df <- data.frame(rep(data, freq))
          names(df) <- varName
        }
        return(df)
      },
      
      
      initAsSubgroup = function(reverse=F){
        
        if(reverse){
          
          val <- private$values
          dist <- private$valuesDistributed
          freq <- private$valuesFrequency
          while(is.list(val)){
            val <- val[[1]]
            dist <- dist[[1]]
            freq <- freq[[1]]
          }
          private$values <- val
          private$valuesDistributed <- dist
          private$valuesFrequency <- freq
          private$capped <- if(!is.null(private$capped) && length(private$capped) >0) unlist(private$capped)[[1]] else T
          private$randomize <- if(!is.null(private$randomize) && length(private$randomize) >0) unlist(private$randomize)[[1]] else F
          private$randomParameter <- if(!is.null(private$randomParameter) && length(private$randomParameter) >0) unlist(private$randomParameter)[[1]] else 10
          se <- if(!is.null(private$seed) && length(private$seed) >0) unlist(private$seed)[[1]] else 0
          self$setSeed(se, asIt=T)
          private$subgroupOf <- NULL
          private$subgroupOfNames <- NULL
        }else{
          
          values <- private$values
          valuesDistributed <- private$valuesDistributed
          if(!valuesDistributed %in% c("equally", "explicit")) valuesDistributed <- "equally"
          valuesFrequency <- private$valuesFrequency
          
          capped <- private$capped
          randomize <- private$randomize
          randomParameter <- private$randomParameter
          seed <- private$seed
          
          private$values <- private$setAllInternalParameter(private$values,values)
          private$valuesDistributed <- private$setAllInternalParameter(private$valuesDistributed,valuesDistributed)
          private$valuesFrequency <- private$setAllInternalParameter(private$valuesFrequency,valuesFrequency)
          
          private$capped <- private$setAllInternalParameter(private$capped,capped)
          private$randomize <- private$setAllInternalParameter(private$randomize,randomize)
          private$randomParameter <- private$setAllInternalParameter(private$randomParameter,randomParameter)
          se <- private$setAllInternalParameter(private$seed,seed)
          self$setSeed(se, asIt=T)
        }
      },
      
      splitSuperElement = function(superElement){
        if(length(superElement)==1){
          superElement <- str_split(superElement, ":")[[1]]
        }
        return(superElement)
      },
      
      clear = function(){
        private$subgroupExplicit=F
        private$upperGroupElements=NULL
        private$subgroupOf=NULL
        private$subgroupOfNames=NULL
        
        private$replaceValuesOf=NULL
        private$replaceValues=NULL
        
        private$values=NULL
        private$valuesDistributed="equally"
        private$valuesFrequency=1 
        
        private$capped=T
        private$randomize=F
        private$randomParameter=10
        private$seed=round(runif(1,-10000,10000))
      },
      
      getInstance = function(){
        new <- ModelCreatingDataOtherVariableCategorical$new(private$seed)
        new$setType(private$type)
        new$setValues(private$values, asIt=T)
        new$setValuesDistributed(private$valuesDistributed, asIt=T)
        new$setValuesFrequency(private$valuesFrequency, asIt=T)
        new$setRandomParameter(private$randomParameter, asIt=T)
        new$setSubgroupOf(private$subgroupOf)
        new$setReplaceValuesOf(private$replaceValuesOf, asIt=T)
        new$setReplaceValues(private$replaceValues, asIt=T)
        new$setRandomize(private$randomize, asIt=T)
        new$setSeed(private$seed, asIt=T)
        new$setCapped(private$capped, asIt=T)
        new$setSubgroupExplicit(private$subgroupExplicit)
        new$setUpperGroupElements(private$upperGroupElements)
        new$setSubgroupOfNames(private$subgroupOfNames)
        return(new)
      },
      
      compareTo = function(cT, exact=F){

        if(private$type != cT$getType()) return(F)


        if(private$subgroupExplicit != cT$getSubgroupExplicit()) return(F)
        if(xor(is.null(private$upperGroupElements), is.null(cT$getUpperGroupElements()))){
          return(F)
        }else if(!is.null(private$upperGroupElements) && !is.null(cT$getUpperGroupElements())){
          if(localUse)browser()
        }
        if(xor(is.null(private$subgroupOf), is.null(cT$getSubgroupOf()))){
          return(F)
        }else if(!is.null(private$subgroupOf) && !is.null(cT$getSubgroupOf())){
          if(localUse)browser()
        }
        if(xor(is.null(private$subgroupOfNames), is.null(cT$getSubgroupOfNames()))){
          return(F)
        }else if(!is.null(private$subgroupOfNames) && !is.null(cT$getSubgroupOfNames())){
          if(localUse)browser()
        }

        
        if(xor(is.null(private$replaceValuesOf), is.null(cT$getReplaceValuesOf()))){
          return(F)
        }else if(!is.null(private$replaceValuesOf) && !is.null(cT$getReplaceValuesOf())){
          if(localUse)browser()
        }
        if(xor(is.null(private$replaceValues), is.null(cT$getReplaceValues()))){
          return(F)
        }else if(!is.null(private$replaceValues) && !is.null(cT$getReplaceValues())){
          if(localUse)browser()
        }
        
        
        # #if type=='subgroup' all following parameters are named lists
        # #names are the elements of the super group(s) 
        if(xor(is.null(private$values), is.null(cT$getValues()))){
          return(F)
        }else if(!is.null(private$values) && !is.null(cT$getValues())){
          unA <- unlist(private$values)
          unB <- unlist(cT$getValues())
          if(!vectorEqual(unA, unB)) return(F)
        }
        if(xor(is.null(private$valuesDistributed), is.null(cT$getValuesDistributed()))){
          return(F)
        }else if(!is.null(private$valuesDistributed) && !is.null(cT$getValuesDistributed())){
          unA <- unlist(private$valuesDistributed)
          unB <- unlist(cT$getValuesDistributed())
          if(!vectorEqual(unA, unB)) return(F)
        }
        if(xor(is.null(private$valuesFrequency), is.null(cT$getValuesFrequency()))){
          return(F)
        }else if(!is.null(private$valuesFrequency) && !is.null(cT$getValuesFrequency())){
          unA <- unlist(private$valuesFrequency)
          unB <- unlist(cT$getValuesFrequency())
          if(!vectorEqual(unA, unB)) return(F)
        }
        

        #g
        if(private$capped != cT$getCapped()) return(F)
        if(private$randomize != cT$getRandomize()) return(F)
        if(private$randomParameter != cT$getRandomParameter()) return(F)
        if(private$seed != cT$getSeed()) return(F)
        

        return(T)
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
            
            type=private$type,
            subgroupExplicit=private$subgroupExplicit,
            upperGroupElements=private$upperGroupElements,
            subgroupOf=private$subgroupOf, 
            subgroupOfNames=private$subgroupOfNames, 
            replaceValuesOf=private$replaceValuesOf, 
            replaceValues=private$replaceValues,
            values=private$values, 
            valuesDistributed=private$valuesDistributed, 
            valuesFrequency=private$valuesFrequency, 
            capped=private$capped,
            randomize=private$randomize,
            randomParameter=private$randomParameter, 
            seed=private$seed
          )
        }
        
        ret <- list(
          uuid = self$getUUID(),
          nextUUID = nextUUID,
          'ModelCreatingDataOtherVariableCategorical' = ret
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
        
        private$type <- state$type
        private$subgroupExplicit <- state$subgroupExplicit
        private$upperGroupElements <- state$upperGroupElement
        private$subgroupOf <- state$subgroupOf
        private$subgroupOfNames <- state$subgroupOfNames
        private$replaceValuesOf <- state$replaceValuesOf
        private$replaceValues <- state$replaceValues
        private$values <- state$values
        private$valuesDistributed <- state$valuesDistributed
        private$valuesFrequency <- state$valuesFrequency
        private$capped <- state$capped
        private$randomize <- state$randomize
        private$randomParameter <- state$randomParameter
        private$seed <- state$seed
      },
      resetState = function(){
        if(!super$resetState()) return()
      }
    )
  )
}

