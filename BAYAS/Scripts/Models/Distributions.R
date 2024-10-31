printDist <<- F
greaterZero <- .Machine$double.xmin

#All possible distributions and also "FixedValues", which is no distribution, 
#but set fixed values to parameters. Used e.g. for binomials N.
distributionEnum <- function(){
  list(Normal = "Normal", Lognormal = "Lognormal", Exponential = "Exponential", Binomial = "Binomial", 
       Gamma = "Gamma", StudentT = "StudentT", Beta = "Beta", Cauchy = "Cauchy",
       HalfStudentT = "HalfStudentT", HalfNormal = "HalfNormal", HalfCauchy = "HalfCauchy",
       Poisson = "Poisson", NegBinom = "NegBinom", FixedValues = "Fixed values", FixedBinomialN = "FixedBinomialN")
}

distDisplayName <- function(name=NULL){
  dEnum <- distributionEnum()
  df <- data.frame(name = unlist(dEnum), displayName = unlist(dEnum))
  df[df$name==dEnum$FixedBinomialN,]$displayName <- dEnum$FixedValues
  if(is.null(name)){
    return(df)
  }else{
    return(df[name,]$displayName)
  }
}

#Dist is a String coming from one of the distributionEnum()
# @paraProp (Parameter properties) is just needed for the 'FixedValues' "Distribution", 
# because the fixed Values dist. have to know the parameters type and also the boundaries.
FactoryDistribution <- function(dist, name, display_name=NULL, description=NULL, adjustable, is.vector, paraProp=NULL){
  dEnum <- distributionEnum()
  if(dist == dEnum$Normal){
    return(NormalDistribution$new(name, dist, display_name, description, adjustable, is.vector))
  }else if(dist == dEnum$Lognormal){
    return(LognormalDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Log-normal distribution",display_name), 
                                       description=ifelse(is.null(description),"The Log-normal distribution is strictly positive.",description), adjustable, is.vector))
  }else if(dist == dEnum$Exponential){
    return(ExponentialDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Exponential distribution",display_name), 
                                       description=ifelse(is.null(description),"The exponential distribution is strictly positive.",description), adjustable, is.vector))
  }else if(dist == dEnum$Binomial){
    if(adjustable) { 
      malfunction_report(code=malfunctionCode()$distributions, msg="The Binomial is not adjustable",
                         type="error")
      stop("The Binomial is not adjustable")
    }
    return(BinomialDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Binomial distribution",display_name), 
                                       description=ifelse(is.null(description),"The Binomial distribution is used for non negative integers.",description), F, is.vector))
  }else if(dist == dEnum$StudentT){
    return(StudentTDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Student t-distribution",display_name), 
                                    description=ifelse(is.null(description),"The Student t-distribution covers a range from -Inf to Inf.",description), adjustable, is.vector))
  }else if(dist == dEnum$HalfStudentT){
    return(HalfStudentTDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Half Student t-distribution",display_name), 
                                    description=ifelse(is.null(description),"The Half Student t-distribution covers a range from 0 to Inf.",description), adjustable, is.vector))
  }else if(dist == dEnum$Cauchy){
    return(CauchyDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Cauchy distribution",display_name), 
                                    description=ifelse(is.null(description),"The Cauchy distribution covers a range from -Inf to Inf.",description), adjustable, is.vector))
  }else if(dist == dEnum$Gamma){
    return(GammaDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Gamma distribution",display_name), 
                                      description=ifelse(is.null(description),"The Gamma distribution covers a range from 0 to Inf.",description), adjustable, is.vector))
  }else if(dist == dEnum$HalfNormal){
    return(HalfNormalDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Half-Normal distribution",display_name), 
                                  description=ifelse(is.null(description),"The half-Normal distribution covers a range from 0 (&mu;) to Inf.",description), adjustable, is.vector))
  }else if(dist == dEnum$Poisson){
    return(PoissonDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Poisson distribution",display_name), 
                                      description=ifelse(is.null(description),"The Poisson distribution covers a range from 0 to Inf.",description), F, is.vector))
  }else if(dist == dEnum$NegBinom){
    return(NegBinomDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Negative-binomial distribution",display_name), 
                                   description=ifelse(is.null(description),"The Negative-binomial distribution covers a range from 0 to Inf.",description), F, is.vector))
  }else if(dist == dEnum$Beta){
    return(BetaDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Beta distribution",display_name), 
                                    description=ifelse(is.null(description),"The Beta distribution covers a range from >0 to <1",description), F, is.vector))
  }else if(dist == dEnum$HalfCauchy){
    return(HalfCauchyDistribution$new(name, dist, display_name=ifelse(is.null(display_name),"Half-cauchy distribution",display_name), 
                                description=ifelse(is.null(description),"The Half-cauchy distribution covers a range from 0 to Inf",description), adjustable, is.vector))
  }else if(dist == dEnum$FixedValues){
    return(FixedValues$new(name, dist, display_name=ifelse(is.null(display_name),"Fixed values",display_name), 
                                      description=ifelse(is.null(description),"Use single or vectorized fixed data.",description), F, is.vector,
                           paraProp=paraProp))
  }else if(dist == dEnum$FixedBinomialN){
    return(FixedBinomialN$new(name, dist, display_name=ifelse(is.null(display_name),"Fixed values",display_name), 
                           description=ifelse(is.null(description),"Use single or vectorized fixed data.",description), F, is.vector,
                           paraProp=paraProp))
  }else{
    malfunction_report(code=malfunctionCode()$distributions, msg=paste0("Non valid distribution: ", dist),
                       type="error")
    stop(paste0("Non valid distribution: ", dist))
  }

}

AbstractDistribution <- R6Class(
  classname = "AbstractDistribution", 
  inherit = SerializationInterface,
  lock_objects = T,
                    
  private = list(
    stateVersion = "0.1"
  ),
      
  public = list(
    name = NULL, #For intern, role of this predictor/parameter
    element_name = NULL, #Name of a variable that this distribution belongs to
    id = runif(1,0,1),
    dist_name = NULL, #Distribution name e.g. "normal"
    display_name = NULL, #Describing name for user
    description = NULL, #Description of the role of this predictor/parameter in the model
    dataY = NULL, #data of y response variable, should be a vector 
    dataX = NULL, #data of x, could be a data.frame due to interaction or a vector
    is.gaussian = F, #Normal GLM?
    is.intercept = F, #is this distribution related to a intercept?
    is.regCoef = F, #is this distribution related to a regression coefficient (no auxiliary parameter like sigma of the Gaussian noise)
    auxParameter = list(), #List of DistributionParameters
    adjustable = F, #Are the parameters of the distribution adjustable by the data?
    fixedValue = F, #If this is no distribution, but a fixed value that can have an assignment of a value or user variable
    dEnum=NULL,
    is.vector = F, #if this distribution is part of a vector. Just for display issues
    vectorId = "", #if this distribution is part of a vector, this is unique name within this vector
    paraProp = NULL, # Just needed for the fixedValues "distribution"
    
    
    
    initialize = function(name, dist_name, display_name=NULL, description=NULL, 
                          adjustable, is.vector, paraProp=NULL,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$name <- name
      self$dist_name <- dist_name
      self$display_name <- display_name
      self$description <- description
      self$adjustable <- adjustable
      self$is.vector <- is.vector
      self$paraProp <- paraProp
      self$setDefaultAuxParameter()
      self$setInitVals()
    },

    setInitVals = function(){},
    
    setAuxParameter = function(name, value, tmpValue = F){
      namelist <- sapply(1:length(self$auxParameter), function(i){self$auxParameter[[i]]$name})
      if(!name %in% namelist){
        malfunction_report(code=malfunctionCode()$distributions, msg=paste0(name ," is no valid parameter of ", self$name),
                           type="error")
        stop(paste0(name ," is no valid parameter of ", self$name))
      }else{
        for(i in self$auxParameter){
          if(i$name == name){
            # if(i$min_val <= value && i$max_val >= value){
              if(tmpValue){
                i$tmp_value <- value
              }else{
                i$value <- value
              }
            # }else{
            #   stop(paste0(value ," is not in range of ", i$name))
            # } 
          }
        }
      }
    },
    
    getValueOf = function(name, tmpValue = F){
      for(i in self$auxParameter){
        if(i$name == name){
          if(tmpValue){
            if(is.na(i$tmp_value) || is.null(i$tmp_value)){ 
              # warning("tmp_value is na. Returning value instead.")
              return(i$value)
            }else{
              return(i$tmp_value)
            }
          }else{
            return(i$value)
          }
        }
      }
    },
    
    getParameterNames = function(){
      names <- c()
      for(i in self$auxParameter){
        names <- c(names, i$name)
      }
      return(names)
    },
    
    setTmpValues = function(){
      for(i in self$auxParameter){
        if(!is.na(i$tmp_value)) i$value <- i$tmp_value
      }
    },
    
    setDefaultAuxParameter = function(){},
    
    plotMe = function(){},
    
    getPrior = function(brms=F){},
    
    
    adjustedParameters = function(){},
    
    #Returns a named list
    myAdustedParameters = function(){
      return(self$adjustedParameters())
    },
    
    adjustMyself = function(tmpValue=F){
      if(!self$adjustable) return()
      res <- self$adjustedParameters()
      names <- names(res)
      for(i in 1:length(res)){
        self$setAuxParameter(names[i], res[[names[i]]],tmpValue=tmpValue)
      }
    },
    
    getParameters = function(tmpValue = F){
      names <- self$getParameterNames()
      ret <- list()
      for(name in names){
        ret[[name]] <- self$getValueOf(name,tmpValue)
      }
      return(ret)
    },
    
    setProperties = function(is.gaussian, is.intercept, is.regCoef){
      self$is.gaussian = is.gaussian
      self$is.intercept = is.intercept
      self$is.regCoef = is.regCoef
    },
    
    setData = function(x,y){
      self$dataY <- y
      self$dataX <- x
    },
    
    setDataAndProperties = function(x=NULL, dataModel, para){

      
      dMID <- dataModel$getDataModelInputData()
      response <- dMID$getResponseVariable(onlyName = T)
      y <- dMID$getLongFormatVariable(response, completeCases=T)[[1]]
      
      baysisModel <- dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()
      y <- tryCatch({      
        baysisModel$transformResponse(y)
        },
        error=function(cond){
          showNotification(paste0("Some values of your response \"",response,"\" can not be logarithmized. Therefore this model is not suitable!"),
                           type = "error")
          y
        },
        warning=function(cond){
          removeNotification(123)
          showNotification(paste0("Some values of your response \"",response,"\" can not be logarithmized. Therefore this model is not suitable!"),
                           type = "error")
          y
        }
      )
      pred_var <- para$getParentPredictor()$userVariable
      if(is.null(x) && !is.null(pred_var)){
        # x <- dataModel$get.user_cleaned_input_data()[pred_var]
        x <- dMID$getLongFormatVariable(pred_var, completeCases=T)
      }
      is.gaussian <- ifelse(dataModel$get.cPerIterationDataModel()$get.selected_BAYSIS_stan_model()$is.gaussian,T,F)
      is.regCoef <- ifelse(para$name == "RegCoef",T,F)
      is.intercept <- ifelse(para$name == "Intercept",T,F)
      
      #Interaction? Workaround 
      if(length(x[1,])>1) {
        new_x <- rep(1,length(x[1,]))
        for(i in 1:length(x[1,])){
          new_x <- new_x * x[,i]
        }
        x <- new_x
      }
      if(is.data.frame(x)) x <- x[,1]
      
      self$is.gaussian <- is.gaussian
      self$is.intercept <- is.intercept
      self$is.regCoef <- is.regCoef
      self$setData(x,y)
    },
    
    amIUsingAdjustedValues = function(tmpValue=F){
      adjustedVal <- self$myAdustedParameters()
      flag <-T
      for(auxPara in self$auxParameter){
        val <- self$getValueOf(auxPara$name, tmpValue)
        if(!quasiEqual(val, adjustedVal[[auxPara$name]])) flag <- F
      }
      return(flag)
    },
    
    tmpValToVal = function(){
      for(auxPara in self$auxParameter){
        auxPara$tmpValToVal()
      }
    },
    
    valToTmpVal = function(){
      for(auxPara in self$auxParameter){
        auxPara$valToTmpVal()
      }
    },
    
    removeTmpval = function(){
      for(auxPara in self$auxParameter){
        auxPara$removeTmpval()
      }
    },
    
    getFormula = function(rounded=T, index=NULL){return(NULL)}, #Default rounded to 2 digits
    
    getFormulaLatex = function(rounded=T, index=NULL){return(NULL)}, #Default rounded to 2 digits
    
    getAuxParametersLatex = function(index=NULL){return(NULL)}, 
    
    
    getEqualOrTilde = function(){
      if(self$fixedValue){
        return("=")
      }else{
        return("~")
      }
    },
    
    #Especially of fixed value distributions
    #users_variables comes from the variable 'users_variables' of PerIterationDataModel
    whichUserVarFitsToThisParameter = function(users_variables){
      pp <- self$paraProp
      uv <- users_variables
      cNum <- characteristicEnum()
      
      if(pp$discrete){
        uv <- uv[uv$type == cNum$Discrete,]
      }else{
        uv <- uv[uv$type == cNum$Continuous,]
      }
      uv <- uv[compareLimits(pp$lower_limit,uv$lower) & compareLimits(pp$upper_limit, uv$upper),]
      
      return(c(uv$variable))
    },
    

    getInstance = function(){
      
      newInstance <- FactoryDistribution(dist=self$dEnum, name=self$name, 
                                  display_name=self$display_name, description=self$description, 
                                  adjustable=self$adjustable, self$is.vector, self$paraProp)
      newInstance$auxParameter <- list()
      for(aP in self$auxParameter){
        newInstance$auxParameter <- list.append(newInstance$auxParameter, aP$getInstance())
      }
      newInstance$setProperties(self$is.gaussian, self$is.intercept, self$is.regCoef)
      newInstance$setData(self$dataX,self$dataY)
      newInstance$vectorId <- self$vectorId
      newInstance$id <- runif(1,1,2)
      newInstance$element_name <- self$element_name
      newInstance$paraProp <- self$paraProp
      newInstance$fixedValue <- self$fixedValue
      return(newInstance)
    },
    
    setInstance = function(instance){
      if(localUse) browser()
      
      self$name <- instance$name
      self$element_name <- instance$element_name
      self$id <- instance$id
      self$dist_name <- instance$dist_name
      self$display_name <- instance$display_name
      self$description <- instance$description
      self$dataY <- instance$dataY
      self$dataX <- instance$dataX
      self$is.gaussian <- instance$is.gaussian
      self$is.intercept <- instance$is.intercept
      self$is.regCoef <- instance$is.regCoef
      self$adjustable <- instance$adjustable
      self$fixedValue <- instance$fixedValue
      self$dEnum <- instance$dEnum
      self$is.vector <- instance$is.vector
      self$vectorId <- instance$vectorId
      self$paraProp <- instance$paraProp
      
      self$auxParameter <- list()
      for(aP in instance$auxParameter){
        self$auxParameter <- list.append(self$auxParameter, aP$getInstance())
      }
      
    },
    
    getState = function(uuid){
      nextUUID <- uuid+1
      ret <- list()

      if(self$getUUID() == -1){
        self$setUUID(uuid)

        auxParameterState <- list()
 
        for(aa in self$auxParameter){
          cState <- aa$getState(nextUUID)
          auxParameterState <- list.append(auxParameterState, cState)
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          name = self$name, 
          element_name = self$element_name, 
          id = self$id,
          dist_name = self$dist_name, 
          display_name = self$display_name, 
          description = self$description, 
          dataY = self$dataY, 
          dataX = self$dataX, 
          is.gaussian = self$is.gaussian, 
          is.intercept = self$is.intercept, 
          is.regCoef = self$is.regCoef, 
          adjustable = self$adjustable, 
          fixedValue = self$fixedValue, 
          dEnum=self$dEnum,
          is.vector = self$is.vector, 
          vectorId = self$vectorId, 
          paraProp = self$paraProp, 
          
          auxParameter = auxParameterState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'AbstractDistribution' = ret
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
      
      self$name <- state$name
      self$element_name <- state$element_name
      self$id <- state$id
      self$dist_name <- state$dist_name
      self$display_name <- state$display_name
      self$description <- state$description
      self$dataY <- state$dataY
      self$dataX <- state$dataX
      self$is.gaussian <- state$is.gaussian
      self$is.intercept <- state$is.intercept
      self$is.regCoef <- state$is.regCoef
      self$adjustable <- state$adjustable
      self$fixedValue <- state$fixedValue
      self$dEnum<-state$dEnum
      self$is.vector <- state$is.vector
      self$vectorId <- state$vectorId
      self$paraProp <- state$paraProp
      
      self$auxParameter <- state$auxParameter
    },
    resetState = function(){
      if(!super$resetState()) return()
      for(aa in self$auxParameter){
        aa$resetState()
      }
    }
  )
)

NormalDistribution <- R6Class(classname = "NormalDistribution", inherit = AbstractDistribution,
  public = list(
    dEnum=distributionEnum()$Normal,
    setInitVals = function(){
      if(is.null(self$display_name)){
        self$display_name <- "Normal distribution"
      }
      if(is.null(self$description)){
        self$description <- "Normal distribution covers a range from -Inf to +Inf"
      } 
    },
    setDefaultAuxParameter = function(){
      self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
      self$auxParameter[[2]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The standard deviation.", value=10, default_val=10,min_val=greaterZero, max_val=Inf)
    },
    plotMe = function(values){ #1:mu, 2:sigma
      tmpMu <- self$getValueOf("mu")
      tmpSigma <- self$getValueOf("sigma")
      if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
      if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpSigma <- values[2]
      min <- qnorm(0.0005,tmpMu,tmpSigma)
      max <- qnorm(0.9995,tmpMu,tmpSigma)
      x <- seq(from=min, to=max, length=1000)
      y <- dnorm(x,tmpMu,tmpSigma)
      data <- data.frame(x=x,y=y)
      ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
    },
    # adjustedParameters = function(y, x=NULL, is.gaussian, is.regCoef, is.intercept){
    adjustedParameters = function(){
      x = self$dataX
      y = self$dataY
      if(printDist) print("adjusted by normal")
      res <- list(mu=NA,sigma=NA)
      factor <- ifelse(self$is.gaussian,sd(y),1)
      #Intercept
      if(self$is.intercept){
        res$mu <- ifelse(self$is.gaussian,mean(y),0)
        res$sigma <- 2.5*factor
      }else if(self$is.regCoef){
        res$mu <- 0
        if(length(unique(x))==1){
          res$sigma <- 2.5*factor
        # }else if(length(unique(x)) == 2){
        #   res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
        }else{
          res$sigma <- (2.5*factor)/sd(x)          
        }
      }else{ #Auxiliary parameter e.g. sigma 
        if(self$is.gaussian){
          malfunction_report(code=malfunctionCode()$distributions, msg="Distribution.R --> A non regCoef or intercept have a normal prior in a gaussian GLM.",
                             type="error")
          stop("Something went wrong. Distribution.R --> A non regCoef or intercept have a normal prior in a gaussian GLM.")
        }else{
          res$mu <- 0
          res$sigma <- 1
          # stop("Still empty. Distribution.R") #When will be a normal distribution used for an auxiliary parameter?
        }
      }
      # print0("sigma: ", res$sigma)
      return(res)
    },
    getPrior = function(brms=F){
      if(brms){
        p <- brms::set_prior(paste0("normal(",self$auxParameter[[1]]$value,",",self$auxParameter[[2]]$value,")"))
        return(p)
      }else{
        return(normal(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value,autoscale=F))
      }
    },
    getFormula = function(rounded=T, index=NULL){
      tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
      tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
      if(self$is.vector){
        return(paste0(tags$span("Normal", class="formulaDistribution"),
                      " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))      
      }else{
        return(paste0(tags$span("Normal", class="formulaDistribution"),
               " ( ",tmp_mu," , ",tmp_sigma," )"))
      }
    },
    getFormulaLatex = function(rounded=T, index=NULL){
      tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
      tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
      if(self$is.vector){
        param1 <- list(name="\\mu",index=index, vector=self$is.vector)
        param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
        return(distToLatex("Normal",list(param1,param2)))
      }else{
        param1 <- list(name=tmp_mu,index=NULL, vector=F)
        param2 <- list(name=tmp_sigma,index=NULL, vector=F)
        return(distToLatex("Normal",list(param1,param2)))
      }
    },
    getAuxParametersLatex = function(index=NULL){
      tmp_mu <- self$getValueOf("mu")
      tmp_sigma <- self$getValueOf("sigma")
      if(self$is.vector){
        param1 <- list(name="\\mu",index=index, vector=self$is.vector)
        param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
        return(distAuxToLatex(list(param1,param2)))
      }else{
        param1 <- list(name=tmp_mu,index=NULL, vector=F)
        param2 <- list(name=tmp_sigma,index=NULL, vector=F)
        return(distAuxToLatex(list(param1,param2)))
      }
    }
  )
)

LogNormalDistribution <- R6Class(classname = "LogNormalDistribution", inherit = AbstractDistribution,
                                 public = list(
                                   dEnum=distributionEnum()$Lognormal,
                                   setInitVals = function(){
                                     if(is.null(self$display_name)){
                                       self$display_name <- "Lognormal distribution"
                                     }
                                     if(is.null(self$description)){
                                       self$description <- "Lognormal distribution covers a range from >0 to +Inf"
                                     } 
                                   },
                                   setDefaultAuxParameter = function(){
                                     self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
                                     self$auxParameter[[2]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The standard deviation.", value=2, default_val=2,min_val=greaterZero, max_val=Inf)
                                   },
                                   plotMe = function(values){ #1:mu, 2:sigma
                                     tmpMu <- self$getValueOf("mu")
                                     tmpSigma <- self$getValueOf("sigma")
                                     if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
                                     if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpSigma <- values[2]
                                     min <- qlnorm(0.01,tmpMu,tmpSigma)
                                     max <- qlnorm(0.89,tmpMu,tmpSigma)
                                     x <- seq(from=min, to=max, length=1000)
                                     y <- dlnorm(x,tmpMu,tmpSigma)
                                     data <- data.frame(x=x,y=y)
                                     ggplot(data, aes(x=x,y=y)) + geom_line() + scale_x_continuous(trans = 'pseudo_log') + scale_y_continuous(breaks = NULL)
                                   },
                                   adjustedParameters = function(){
                                     x = self$dataX
                                     y = self$dataY
                                     if(printDist) print("adjusted by lognormal")
                                     res <- list(mu=NA,sigma=NA)
                                     factor <- ifelse(self$is.gaussian,sd(y),1)
                                     #Intercept
                                     if(self$is.intercept){
                                       res$mu <- 0
                                       res$sigma <- 2*factor
                                     }else if(self$is.regCoef){
                                       res$mu <- 0
                                       if(length(unique(x))==1){
                                         res$sigma <- 1.5*factor
                                       }else if(length(unique(x)) == 2){
                                         res$sigma <- (1.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                       }else{
                                         res$sigma <- (1.5*factor)/sd(x)          
                                       }
                                     }else{ #Auxiliary parameter e.g. sigma 
                                       if(self$is.gaussian){
                                         malfunction_report(code=malfunctionCode()$distributions, msg="Distribution.R --> A non regCoef or intercept have a normal prior in a gaussian GLM.",
                                                            type="error")
                                         stop("Something went wrong. Distribution.R --> A non regCoef or intercept have a normal prior in a gaussian GLM.")
                                       }else{
                                         malfunction_report(code=malfunctionCode()$distributions, msg="Distribution.R --> A non regCoef or intercept have a normal prior in a gaussian GLM.",
                                                            type="error")
                                         stop("Still empty. Distribution.R") #When will a normal distribution will be used for an auxiliary parameter?
                                       }
                                     }
                                     return(res)
                                   },
                                   getPrior = function(brms=F){
                                     stop("Not yet implemented")
                                   },
                                   getFormula = function(rounded=T, index=NULL){
                                     tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                     tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                     if(self$is.vector){
                                       return(paste0(tags$span("Log-Normal", class="formulaDistribution"),
                                                     " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))    
                                     }else{
                                       return(paste0(tags$span("Log-Normal", class="formulaDistribution"),
                                                     " ( ",tmp_mu," , ",tmp_sigma," )"))
                                     }
                                   },
                                   getFormulaLatex = function(rounded=T, index=NULL){
                                     tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                     tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                     if(self$is.vector){
                                       param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                       param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                       return(distToLatex("Log-Normal",list(param1,param2)))
                                     }else{
                                       param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                       param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                       return(distToLatex("Log-Normal",list(param1,param2)))
                                     }
                                   },
                                   getAuxParametersLatex = function(index=NULL){
                                     tmp_mu <- self$getValueOf("mu")
                                     tmp_sigma <- self$getValueOf("sigma")
                                     if(self$is.vector){
                                       param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                       param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                       return(distAuxToLatex(list(param1,param2)))
                                     }else{
                                       param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                       param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                       return(distAuxToLatex(list(param1,param2)))
                                     }
                                   }
                                 )
)

ExponentialDistribution <- R6Class(classname = "ExponentialDistribution", inherit = AbstractDistribution,
  public = list(
    dEnum=distributionEnum()$Exponential,
    setInitVals = function(){
      if(is.null(self$display_name)){
        self$display_name <- "Exponential distribution"
      }
      if(is.null(self$description)){
        self$description <- "The exponential distribution is strictly positive"
      } 
    },
    setDefaultAuxParameter = function(){
      self$auxParameter[[1]] <- DistributionParameter$new(name = "lambda", display_name="&lambda;", description="The rate.", value=0.1, default_val=0.1,min_val=greaterZero, max_val=Inf)
    },
    plotMe = function(values){ #1: lambda
      tmpLambda <- self$getValueOf("lambda")
      if(!is.null(values[1])){
        if(is.numeric(values[1]) && !is.na(values[1]) && is.numeric(values[1])){
          tmpLambda <- values[1]
          if(values[1] < 1e-20) tmpLambda <- 1e-20
        }
      }
      min <- 1e-20
      max <- qexp(0.99, tmpLambda)
      x <- seq(from=min, to=max, length=1000)
      y <- dexp(x,tmpLambda)
      data <- data.frame(x=x,y=y)
      ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
    },
    adjustedParameters = function(){
      y = self$dataY
      if(printDist) print("adjusted by exponential")
      res <- list(lambda=NA)
      factor <- ifelse(self$is.gaussian,sd(y),1)
      #Intercept
      if(self$is.intercept){
        malfunction_report(code=malfunctionCode()$distributions, msg="Adjustment of parameter for the exponential distribution for an intercept.",
                           type="error")
        stop("Not implemented. Adjustment of parameter for the exponential distribution for an intercept.")
      }else if(self$is.regCoef){
        malfunction_report(code=malfunctionCode()$distributions, msg="Adjustment of parameter for the exponential distribution for a regression coefficient.",
                           type="error")
        stop("Not implemented. Adjustment of parameter for the exponential distribution for a regression coefficient.")
      }else{ #Auxiliary parameter e.g. sigma 
        if(self$is.gaussian){
          res$lambda <- 1/(1*factor)
        }else{
          res$lambda <- 1/(1*factor)
          # stop("Not implemented. Adjustment of parameter for the exponential distribution for a aux coefficient, that is not for the gaussian glm.")
        }
      }
      return(res)
    },
    getPrior = function(brms=F){
      if(brms){
        p <- brms::set_prior(paste0("exponential(",self$auxParameter[[1]]$value,")"))
        return(p)
      }else{
        return(rstanarm::exponential(self$auxParameter[[1]]$value,autoscale=F))
      }
    },
    getFormula = function(rounded=T, index=NULL){
      tmp_lambda <- ifelse(rounded,round(self$getValueOf("lambda"),2), self$getValueOf("lambda"))
      if(self$is.vector){
        return(paste0(tags$span("Exponential", class="formulaDistribution"),
                      " ( ", "<b>&lambda;",tags$sub(index) ,"</b>)"))   
      }else{
        return(paste0(tags$span("Exponential", class="formulaDistribution"),
                      " ( ",tmp_lambda," )"))
      }
    },
    getFormulaLatex = function(rounded=T, index=NULL){
      tmp_lambda <- ifelse(rounded,round(self$getValueOf("lambda"),2), self$getValueOf("lambda"))
      if(self$is.vector){
        param1 <- list(name="\\lambda",index=index, vector=self$is.vector)
        return(distToLatex("Exponential",list(param1)))
      }else{
        param1 <- list(name=tmp_lambda,index=NULL, vector=F)
        return(distToLatex("Exponential",list(param1)))
      }
    },
    getAuxParametersLatex = function(index=NULL){
      tmp_lambda <- self$getValueOf("lambda")
      if(self$is.vector){
        param1 <- list(name="\\lambda",index=index, vector=self$is.vector)
        return(distAuxToLatex(list(param1)))
      }else{
        param1 <- list(name=tmp_lambda,index=NULL, vector=F)
        return(distAuxToLatex(list(param1)))
      }
    }
  )
)

BetaDistribution <- R6Class(classname = "BetaDistribution", inherit = AbstractDistribution,
                              public = list(
                                dEnum=distributionEnum()$Beta,
                                setInitVals = function(){
                                  if(is.null(self$display_name)){
                                    self$display_name <- "Beta distribution"
                                  }
                                  if(is.null(self$description)){
                                    self$description <- "Beta distribution covers a range from >0 to <1"
                                  } 
                                },
                                setDefaultAuxParameter = function(){
                                  self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean.", value=0.5, default_val=0.5,min_val=0, max_val=1)
                                  self$auxParameter[[2]] <- DistributionParameter$new(name = "kappa", display_name="&kappa;", description="The (Wright's genetic) distance.", value=4, default_val=4,min_val=greaterZero, max_val=Inf)
                                },
                                plotMe = function(values){ #1:mu, 2:kappa
                                  tmpMu <- self$getValueOf("mu")
                                  tmpKappa <- self$getValueOf("sigma")
                                  if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
                                  if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpKappa <- values[2]
                                  tmpAlpha <- tmpMu*tmpKappa
                                  tmpBeta <- (1-tmpMu)*tmpKappa
                                  min <- qbeta(0.005, tmpAlpha,tmpBeta)
                                  max <- qbeta(0.995, tmpAlpha,tmpBeta)
                                  x <- seq(from=min, to=max, length=1000)
                                  y <- dbeta(x,tmpAlpha,tmpBeta)
                                  data <- data.frame(x=x,y=y)
                                  ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                },
                                adjustedParameters = function(){
                                  x = self$dataX
                                  y = self$dataY
                                  if(printDist) print("adjusted by beta")
                                  malfunction_report(code=malfunctionCode()$distributions, msg="Beta distribution is not adjustable!",
                                                     type="error")
                                  stop("Beta distribution is not adjustable!")
                                },
                                getPrior = function(brms=F){
                                  malfunction_report(code=malfunctionCode()$distributions, msg="Not yet implemented. BetaDistribution",
                                                     type="error")
                                  stop("Not yet implemented")
                                },
                                getFormula = function(rounded=T, index=NULL){
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_kappa <- ifelse(rounded,round(self$getValueOf("kappa"),2), self$getValueOf("kappa"))
                                  if(self$is.vector){
                                    return(paste0(tags$span("Beta", class="formulaDistribution"),
                                                  " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&kappa;", tags$sub(index),"</b> )"))     
                                  }else{
                                    return(paste0(tags$span("Beta", class="formulaDistribution"),
                                                  " ( ",tmp_mu," , ",tmp_kappa," )"))  
                                  }
                                },
                                getFormulaLatex = function(rounded=T, index=NULL){
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_kappa <- ifelse(rounded,round(self$getValueOf("kappa"),2), self$getValueOf("kappa"))
                                  if(self$is.vector){
                                    param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\kappa",index=index, vector=self$is.vector)
                                    return(distToLatex("Beta",list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_kappa,index=NULL, vector=F)
                                    return(distToLatex("Beta",list(param1,param2)))
                                  }
                                },
                                getAuxParametersLatex = function(index=NULL){
                                  tmp_mu <- self$getValueOf("mu")
                                  tmp_kappa <- self$getValueOf("kappa")
                                  if(self$is.vector){
                                    param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\kappa",index=index, vector=self$is.vector)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_kappa,index=NULL, vector=F)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }
                                }
                              )
)


BinomialDistribution <- R6Class(classname = "BinomialDistribution", inherit = AbstractDistribution,
                                 public = list(
                                   dEnum=distributionEnum()$Binomial,
                                   setInitVals = function(){
                                     if(is.null(self$display_name)){
                                       self$display_name <- "Binomial distribution"
                                     }
                                     if(is.null(self$description)){
                                       self$description <- "Binomial distribution covers integers from 0 to N (+Inf)"
                                     } 
                                   },
                                   setDefaultAuxParameter = function(){
                                     self$auxParameter[[1]] <- DistributionParameter$new(name = "N", display_name="<i>n</i>", description="Number of trials.", value=100, default_val=100,min_val=0, max_val=Inf, discrete = T)
                                     self$auxParameter[[2]] <- DistributionParameter$new(name = "p", display_name="<i>p</i>", description="Success probability.", value=0.5, default_val=0.5,min_val=0, max_val=1)
                                   },
                                   plotMe = function(values){ #1:N, 2:p
                                     tmpN <- self$getValueOf("N")
                                     tmpP <- self$getValueOf("p")
                                     if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpN <- values[1]
                                     if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpP <- values[2]
                                     min <- 0
                                     max <- tmpN
                                     x <- c(min:max)
                                     y <- dbinom(x,tmpN,tmpP)
                                     data <- data.frame(x=x,y=y)
                                     ggplot(data) + geom_bar(aes(x=x,y=y),stat="identity")
                                   },
                                   adjustedParameters = function(){
                                     x = self$dataX
                                     y = self$dataY
                                     if(printDist) print("adjusted by lognormal")
                                     malfunction_report(code=malfunctionCode()$distributions, msg="Binomial distribution is not adjustable!",
                                                        type="error")
                                     stop("Binomial distribution is not adjustable!")
                                   },
                                   getPrior = function(brms=F){
                                     malfunction_report(code=malfunctionCode()$distributions, msg="Not yet implemented. BinomialDistribution",
                                                        type="error")
                                     stop("Not yet implemented")
                                   },
                                   getFormula = function(rounded=T, index=NULL){
                                     tmp_N <- ifelse(rounded,round(self$getValueOf("N"),2), self$getValueOf("N"))
                                     tmp_p <- ifelse(rounded,round(self$getValueOf("p"),2), self$getValueOf("p"))
                                     if(self$is.vector){
                                       return(paste0(tags$span("Binomial", class="formulaDistribution"),
                                                     " ( ", "<b>N",tags$sub(index) ,"</b> , <b>", "p", tags$sub(index),"</b> )"))     
                                     }else{
                                       return(paste0(tags$span("Binomial", class="formulaDistribution"),
                                                     " ( ",tmp_N," , ",tmp_p," )"))  
                                     }
                                   },
                                   getFormulaLatex = function(rounded=T, index=NULL){
                                     tmp_N <- ifelse(rounded,round(self$getValueOf("N"),2), self$getValueOf("N"))
                                     tmp_p <- ifelse(rounded,round(self$getValueOf("p"),2), self$getValueOf("p"))
                                     if(self$is.vector){
                                       param1 <- list(name="N",index=index, vector=self$is.vector)
                                       param2 <- list(name="p",index=index, vector=self$is.vector)
                                       return(distToLatex("Binomial",list(param1,param2)))
                                     }else{
                                       param1 <- list(name=tmp_N,index=NULL, vector=F)
                                       param2 <- list(name=tmp_p,index=NULL, vector=F)
                                       return(distToLatex("Binomial",list(param1,param2)))
                                     }
                                   },
                                   getAuxParametersLatex = function(index=NULL){
                                     tmp_N <- self$getValueOf("N")
                                     tmp_p <- self$getValueOf("p")
                                     if(self$is.vector){
                                       param1 <- list(name="N",index=index, vector=self$is.vector)
                                       param2 <- list(name="p",index=index, vector=self$is.vector)
                                       return(distAuxToLatex(list(param1,param2)))
                                     }else{
                                       param1 <- list(name=tmp_N,index=NULL, vector=F)
                                       param2 <- list(name=tmp_p,index=NULL, vector=F)
                                       return(distAuxToLatex(list(param1,param2)))
                                     }
                                   }
                                 )
)

PoissonDistribution <- R6Class(classname = "PoissonDistribution", inherit = AbstractDistribution,
                                public = list(
                                  dEnum=distributionEnum()$Poisson,
                                  setInitVals = function(){
                                    if(is.null(self$display_name)){
                                      self$display_name <- "Poisson distribution"
                                    }
                                    if(is.null(self$description)){
                                      self$description <- "Poisson distribution covers integers from 0 to Inf"
                                    } 
                                  },
                                  setDefaultAuxParameter = function(){
                                    self$auxParameter[[1]] <- DistributionParameter$new(name = "lambda", display_name="&lambda;", description="lambda controls the mean and variance. (&lambda; = &mu; = var)", value=10, default_val=10,min_val=greaterZero, max_val=Inf, discrete = T)
                                  },
                                  plotMe = function(values){ #1:lambda
                                    tmpLambda <- self$getValueOf("N")
                                    if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpLambda <- values[1]
                                    min <- qpois(0.005,tmpLambda)
                                    max <- qpois(0.995,tmpLambda)
                                    x <- c(min:max)
                                    y <- dpois(x,tmpLambda)
                                    data <- data.frame(x=x,y=y)
                                    ggplot(data) + geom_bar(aes(x=x,y=y),stat="identity")
                                  },
                                  adjustedParameters = function(){
                                    x = self$dataX
                                    y = self$dataY
                                    if(printDist) print("adjusted by poisson")
                                    malfunction_report(code=malfunctionCode()$distributions, msg="Poisson distribution is not adjustable!",
                                                       type="error")
                                    stop("Poisson distribution is not adjustable!")
                                  },
                                  getPrior = function(brms=F){
                                    malfunction_report(code=malfunctionCode()$distributions, msg="Not yet implemented Poisson.",
                                                       type="error")
                                    stop("Not yet implemented")
                                  },
                                  getFormula = function(rounded=T, index=NULL){
                                    tmp_lambda <- ifelse(rounded,round(self$getValueOf("lambda"),2), self$getValueOf("lambda"))

                                    if(self$is.vector){
                                      return(paste0(tags$span("Poisson", class="formulaDistribution"),
                                                    " ( ", "<b>&lambda;",tags$sub(index) ,"</b> )"))      
                                    }else{
                                      return(paste0(tags$span("Poisson", class="formulaDistribution"),
                                                    " ( ",tmp_lambda," )"))
                                    }
                                  },
                                  getFormulaLatex = function(rounded=T, index=NULL){
                                    tmp_lambda <- ifelse(rounded,round(self$getValueOf("lambda"),2), self$getValueOf("lambda"))
                                    if(self$is.vector){
                                      warning("This distribution (poisson) includes vectors that are not highlighted as such..")
                                      
                                      return(paste0("\\text{Poisson}(\\lambda_",index,")"))
                                    }else{
                                      return(paste0("\\text{Poisson}(",tmp_lambda ,")"))
                                    }
                                    if(self$is.vector){
                                      param1 <- list(name="\\lambda",index=index, vector=self$is.vector)
                                      return(distToLatex("Poisson",list(param1)))
                                    }else{
                                      param1 <- list(name=tmp_lambda,index=NULL, vector=F)
                                      return(distToLatex("Poisson",list(param1)))
                                    }
                                  },
                                  getAuxParametersLatex = function(index=NULL){
                                    tmp_lambda <- self$getValueOf("lambda")
                                    if(self$is.vector){
                                      warning("This distribution (poisson) includes vectors that are not highlighted as such..")
                                      
                                      return(paste0("\\text{Poisson}(\\lambda_",index,")"))
                                    }else{
                                      return(paste0("\\text{Poisson}(",tmp_lambda ,")"))
                                    }
                                    if(self$is.vector){
                                      param1 <- list(name="\\lambda",index=index, vector=self$is.vector)
                                      return(distAuxToLatex(list(param1)))
                                    }else{
                                      param1 <- list(name=tmp_lambda,index=NULL, vector=F)
                                      return(distAuxToLatex(list(param1)))
                                    }
                                  }
                                )
)

NegBinomDistribution <- R6Class(classname = "NegBinomDistribution", inherit = AbstractDistribution,
                               public = list(
                                 dEnum=distributionEnum()$NegBinom,
                                 setInitVals = function(){
                                   if(is.null(self$display_name)){
                                     self$display_name <- "Negative binomial distribution"
                                   }
                                   if(is.null(self$description)){
                                     self$description <- "Negative binomial distribution covers integers from 0 to Inf"
                                   } 
                                 },
                                 setDefaultAuxParameter = function(){
                                   self$auxParameter[[1]] <- DistributionParameter$new(name = "Mean", display_name="&mu;", description="The mean.", value=10, default_val=10,min_val=greaterZero, max_val=Inf, discrete = T)
                                   self$auxParameter[[2]] <- DistributionParameter$new(name = "Dispersion", display_name="&Phi;", description="The dispersion.", value=2, default_val=2,min_val=greaterZero, max_val=Inf)
                                 },
                                 plotMe = function(values){ #1:Mean, 2:Dispersion
                                   tmpMu <- self$getValueOf("Mean")
                                   tmpDis <- self$getValueOf("Dispersion")
                                   if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
                                   if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpDis <- values[2]
                                   min <- qnbinom(0.005,size=tmpDis,mu=tmpMu)
                                   max <- qnbinom(0.995,size=tmpDis,mu=tmpMu)
                                   x <- c(min:max)
                                   y <- dnbinom(x,size=tmpDis,mu=tmpMu)
                                   data <- data.frame(x=x,y=y)
                                   ggplot(data) + geom_bar(aes(x=x,y=y),stat="identity")
                                 },
                                 adjustedParameters = function(){
                                   x = self$dataX
                                   y = self$dataY
                                   if(printDist) print("adjusted by lognormal")
                                   malfunction_report(code=malfunctionCode()$distributions, msg="Negative Binomial distribution is not adjustable!",
                                                      type="error")
                                   stop("Binomial distribution is not adjustable!")
                                 },
                                 getPrior = function(brms=F){
                                   malfunction_report(code=malfunctionCode()$distributions, msg="Not yet implemented NegBinomDistribution",
                                                      type="error")
                                   stop("Not yet implemented")
                                 },
                                 getFormula = function(rounded=T, index=NULL){
                                   tmp_Mu <- ifelse(rounded,round(self$getValueOf("Mean"),2), self$getValueOf("Mean"))
                                   tmp_Dis <- ifelse(rounded,round(self$getValueOf("Dispersion"),2), self$getValueOf("Dispersion"))
                                   if(self$is.vector){
                                     return(paste0(tags$span("Neg-Binomial", class="formulaDistribution"),
                                                   " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&Phi;", tags$sub(index),"</b> )"))      
                                   }else{
                                     return(paste0(tags$span("Neg-Binomial", class="formulaDistribution"),
                                                   " ( ",tmp_Mu," , ",tmp_Dis," )"))
                                   }
                                 },
                                 getFormulaLatex = function(rounded=T, index=NULL){
                                   tmp_Mu <- ifelse(rounded,round(self$getValueOf("Mean"),2), self$getValueOf("Mean"))
                                   tmp_Dis <- ifelse(rounded,round(self$getValueOf("Dispersion"),2), self$getValueOf("Dispersion"))
                                   if(self$is.vector){
                                     param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                     param2 <- list(name="\\phi",index=index, vector=self$is.vector)
                                     return(distToLatex("Neg-Binomial",list(param1,param2)))
                                   }else{
                                     param1 <- list(name=tmp_Mu,index=NULL, vector=F)
                                     param2 <- list(name=tmp_Dis,index=NULL, vector=F)
                                     return(distToLatex("Neg-Binomial",list(param1,param2)))
                                   }
                                 },
                                 getAuxParametersLatex = function(index=NULL){
                                   tmp_Mu <- self$getValueOf("Mean")
                                   tmp_Dis <- self$getValueOf("Dispersion")
                                   if(self$is.vector){
                                     param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                     param2 <- list(name="\\phi",index=index, vector=self$is.vector)
                                     return(distAuxToLatex(list(param1,param2)))
                                   }else{
                                     param1 <- list(name=tmp_Mu,index=NULL, vector=F)
                                     param2 <- list(name=tmp_Dis,index=NULL, vector=F)
                                     return(distAuxToLatex(list(param1,param2)))
                                   }
                                 }
                               )
)


StudentTDistribution <- R6Class(classname = "StudentTDistribution", inherit = AbstractDistribution,
                              public = list(
                                dEnum=distributionEnum()$StudentT,
                                setInitVals = function(){
                                  if(is.null(self$display_name)){
                                    self$display_name <- "Student's t-distribution"
                                  }
                                  if(is.null(self$description)){
                                    self$description <- "Student's t-distribution covers a range from -Inf to +Inf"
                                  } 
                                },
                                setDefaultAuxParameter = function(){
                                  self$auxParameter[[1]] <- DistributionParameter$new(name = "nu", display_name="&nu;", description="The degree of freedom.", value=3, default_val=3,min_val=greaterZero, max_val=Inf)
                                  self$auxParameter[[2]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
                                  self$auxParameter[[3]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The variance (squared scale).", value=2.5, default_val=2.5,min_val=greaterZero, max_val=Inf)
                                },
                                plotMe = function(values){ #1:nu, 2:mu, 3:sigma
                                  tmpNu <- self$getValueOf("nu")
                                  tmpMu <- self$getValueOf("mu")
                                  tmpSigma <- self$getValueOf("sigma")
                                  if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpNu <- values[1]
                                  if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpMu <- values[2]
                                  if(!is.null(values[3]) && !is.na(values[3]) && is.numeric(values[3])) tmpSigma <- values[3]
                                  min <- qst(0.01,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                  max <- qst(0.99,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                  if(min == Inf || min == -Inf || max == Inf || max == -Inf){
                                    return(ggplot())
                                  }
                                  x <- seq(from=min, to=max, length=1000)
                                  y <- dst(x,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                  data <- data.frame(x=x,y=y)
                                  ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                },
                                adjustedParameters = function(){
                                  x = self$dataX
                                  y = self$dataY
                                  if(printDist) print("adjusted by student-t")
                                  res <- list(nu=NA,mu=NA,sigma=NA)
                                  factor <- ifelse(self$is.gaussian,sd(y),1)
                                  #Intercept
                                  res$nu <- 3
                                  if(self$is.intercept){
                                    res$mu <- 0
                                    res$sigma <- 10*factor
                                  }else if(self$is.regCoef){
                                    res$mu <- 0
                                    if(length(unique(x))==1){
                                      res$sigma <- 2.5*factor
                                    }else if(length(unique(x)) == 2){
                                      res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                    }else{
                                      res$sigma <- (2.5*factor)/sd(x)          
                                    }
                                  }else{ #Auxiliary parameter e.g. sigma 
                                    if(self$is.gaussian){
                                      malfunction_report(code=malfunctionCode()$distributions, msg="Distribution.R --> A non regCoef or intercept have a student-t prior in a gaussian GLM.",
                                                         type="error")
                                      stop("Something went wrong. Distribution.R --> A non regCoef or intercept have a student-t prior in a gaussian GLM.")
                                    }else{
                                      malfunction_report(code=malfunctionCode()$distributions, msg="Still empty. Distribution.R",
                                                         type="error")
                                      stop("Still empty. Distribution.R") #When will a normal distribution will be used for an auxiliary parameter?
                                    }
                                  }
                                  return(res)
                                },
                                getPrior = function(brms=F){
                                  if(brms){
                                    p <- brms::set_prior(paste0("student_t(",self$auxParameter[[1]]$value,",",self$auxParameter[[2]]$value,",",self$auxParameter[[3]]$value,")"))
                                    return(p)
                                  }else{
                                    return(student_t(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value,self$auxParameter[[3]]$value, autoscale=F))
                                  }
                                },
                                getFormula = function(rounded=T, index=NULL){
                                  tmp_nu <- ifelse(rounded,round(self$getValueOf("nu"),2), self$getValueOf("nu"))
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    return(paste0(tags$span("StudentT", class="formulaDistribution"),
                                                  " ( ", "<b>&nu;",tags$sub(index) ,"</b> , <b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))      
                                  }else{
                                    return(paste0(tags$span("StudentT", class="formulaDistribution"),
                                                  " ( ",tmp_nu," , ",tmp_mu," , ",tmp_sigma," )"))
                                  }
                                },
                                getFormulaLatex = function(rounded=T, index=NULL){
                                  tmp_nu <- ifelse(rounded,round(self$getValueOf("nu"),2), self$getValueOf("nu"))
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    param1 <- list(name="\\nu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param3 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distToLatex("StudentT",list(param1,param2,param3)))
                                  }else{
                                    param1 <- list(name=tmp_nu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param3 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distToLatex("StudentT",list(param1,param2,param3)))
                                  }
                                },
                                getAuxParametersLatex = function(index=NULL){
                                  tmp_nu <- self$getValueOf("nu")
                                  tmp_mu <- self$getValueOf("mu")
                                  tmp_sigma <- self$getValueOf("sigma")
                                  if(self$is.vector){
                                    param1 <- list(name="\\nu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param3 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distAuxToLatex(list(param1,param2,param3)))
                                  }else{
                                    param1 <- list(name=tmp_nu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param3 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distAuxToLatex(list(param1,param2,param3)))
                                  }
                                }
                              )
)

HalfStudentTDistribution <- R6Class(classname = "HalfStudentT", inherit = AbstractDistribution,
                                public = list(
                                  dEnum=distributionEnum()$HalfStudentT,
                                  setInitVals = function(){
                                    if(is.null(self$display_name)){
                                      self$display_name <- "Half-Student's t-distribution"
                                    }
                                    if(is.null(self$description)){
                                      self$description <- "Half-Student's t-distribution covers a range from 0 to +Inf"
                                    } 
                                  },
                                  setDefaultAuxParameter = function(){
                                    self$auxParameter[[1]] <- DistributionParameter$new(name = "nu", display_name="&nu;", description="The degree of freedom.", value=3, default_val=3,min_val=greaterZero, max_val=Inf)
                                    self$auxParameter[[2]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
                                    self$auxParameter[[3]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The variance (squared scale).", value=2.5, default_val=2.5,min_val=greaterZero, max_val=Inf)
                                  },
                                  plotMe = function(values){ #1:nu, 2:mu, 3:sigma
                                    tmpNu <- self$getValueOf("nu")
                                    tmpMu <- self$getValueOf("mu")
                                    tmpSigma <- self$getValueOf("sigma")
                                    if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpNu <- values[1]
                                    if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpMu <- values[2]
                                    if(!is.null(values[3]) && !is.na(values[3]) && is.numeric(values[3])) tmpSigma <- values[3]
                                    min <- qst(0.01,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                    max <- qst(0.99,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                    x <- seq(from=min, to=max, length=1000)
                                    y <- dst(x,nu=tmpNu,mu=tmpMu,sigma=tmpSigma)
                                    data <- data.frame(x=x,y=y)
                                    ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)

                                  },
                                  adjustedParameters = function(){
                                    x = self$dataX
                                    y = self$dataY
                                    if(printDist) print("adjusted by student-t")
                                    res <- list(nu=NA,mu=NA,sigma=NA)
                                    factor <- ifelse(self$is.gaussian,sd(y),1)
                                    #Intercept
                                    res$nu <- 3
                                    if(self$is.intercept){
                                      res$mu <- 0
                                      res$sigma <- 10*factor
                                    }else if(self$is.regCoef){
                                      res$mu <- 0
                                      if(length(unique(x))==1){
                                        res$sigma <- 2.5*factor
                                      }else if(length(unique(x)) == 2){
                                        res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                      }else{
                                        res$sigma <- (2.5*factor)/sd(x)          
                                      }
                                    }else{ #Auxiliary parameter e.g. sigma 
                                      if(length(unique(x))==1){
                                        res$sigma <- 2.5*factor
                                      }else if(length(unique(x)) == 2){
                                        res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                      }else{
                                        res$sigma <- (2.5*factor)/sd(x)          
                                      }
                                    }
                                    return(res)
                                  },
                                  getPrior = function(brms=F){
                                    warning("This function should only be used in a context, where this function is used as a half-student-t.")
                                    if(brms){
                                      p <- brms::set_prior(paste0("student_t(",self$auxParameter[[1]]$value,",",
                                                              self$auxParameter[[2]]$value,",",
                                                              self$auxParameter[[3]]$value,")"),
                                                       lb=0)
                                      
                                      return(p)
                                    }else{
                                      return(student_t(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value,self$auxParameter[[3]]$value, autoscale=F))
                                    }
                                  },
                                  getFormula = function(rounded=T, index=NULL){
                                    tmp_nu <- ifelse(rounded,round(self$getValueOf("nu"),2), self$getValueOf("nu"))
                                    tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                    tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                    if(self$is.vector){
                                      return(paste0(tags$span("Half-StudentT", class="formulaDistribution"),
                                                    " ( ", "<b>&nu;",tags$sub(index) ,"</b> , <b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))      
                                    }else{
                                      return(paste0(tags$span("Half-StudentT", class="formulaDistribution"),
                                                    " ( ",tmp_nu," , ",tmp_mu," , ",tmp_sigma," )"))
                                    }
                                  },
                                  getFormulaLatex = function(rounded=T, index=NULL){
                                    tmp_nu <- ifelse(rounded,round(self$getValueOf("nu"),2), self$getValueOf("nu"))
                                    tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                    tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                    if(self$is.vector){
                                      param1 <- list(name="\\nu",index=index, vector=self$is.vector)
                                      param2 <- list(name="\\mu",index=index, vector=self$is.vector)
                                      param3 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                      return(distToLatex("Half-StudentT",list(param1,param2,param3)))
                                    }else{
                                      param1 <- list(name=tmp_nu,index=NULL, vector=F)
                                      param2 <- list(name=tmp_mu,index=NULL, vector=F)
                                      param3 <- list(name=tmp_sigma,index=NULL, vector=F)
                                      return(distToLatex("Half-StudentT",list(param1,param2,param3)))
                                    }
                                  },
                                  getAuxParametersLatex = function(index=NULL){
                                    tmp_nu <- self$getValueOf("nu")
                                    tmp_mu <- self$getValueOf("mu")
                                    tmp_sigma <- self$getValueOf("sigma")
                                    if(self$is.vector){
                                      param1 <- list(name="\\nu",index=index, vector=self$is.vector)
                                      param2 <- list(name="\\mu",index=index, vector=self$is.vector)
                                      param3 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                      return(distAuxToLatex(list(param1,param2,param3)))
                                    }else{
                                      param1 <- list(name=tmp_nu,index=NULL, vector=F)
                                      param2 <- list(name=tmp_mu,index=NULL, vector=F)
                                      param3 <- list(name=tmp_sigma,index=NULL, vector=F)
                                      return(distAuxToLatex(list(param1,param2,param3)))
                                    }
                                  }
                                )
)


CauchyDistribution <- R6Class(classname = "CauchyDistribution", inherit = AbstractDistribution,
                                public = list(
                                  dEnum=distributionEnum()$Cauchy,
                                  setInitVals = function(){
                                    if(is.null(self$display_name)){
                                      self$display_name <- "Cauchy distribution"
                                    }
                                    if(is.null(self$description)){
                                      self$description <- "Cauchy distribution covers a range from -Inf to +Inf"
                                    } 
                                  },
                                  setDefaultAuxParameter = function(){
                                    self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
                                    self$auxParameter[[2]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The variance (scale).", value=10, default_val=10,min_val=greaterZero, max_val=Inf)
                                  },
                                  plotMe = function(values){ #1:mu, 2:sigma
                                    tmpMu <- self$getValueOf("mu")
                                    tmpSigma <- self$getValueOf("sigma")
                                    if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
                                    if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpSigma <- values[2]
                                    min <- qcauchy(0.05,location=tmpMu,scale=tmpSigma)
                                    max <- qcauchy(0.95,location=tmpMu,scale=tmpSigma)
                                    x <- seq(from=min, to=max, length=1000)
                                    y <- dcauchy(x,location=tmpMu,scale=tmpSigma)
                                    data <- data.frame(x=x,y=y)
                                    ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                  },
                                  adjustedParameters = function(){
                                    x = self$dataX
                                    y = self$dataY
                                    if(printDist) print("adjusted by cauchy")
                                    res <- list(mu=NA,sigma=NA)
                                    factor <- ifelse(self$is.gaussian,sd(y),1)
                                    #Intercept
                                    if(self$is.intercept){
                                      res$mu <- 0
                                      res$sigma <- 10*factor
                                    }else if(self$is.regCoef){
                                      res$mu <- 0
                                      if(length(unique(x))==1){
                                        res$sigma <- 2.5*factor
                                      }else if(length(unique(x)) == 2){
                                        res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                      }else{
                                        res$sigma <- (2.5*factor)/sd(x)          
                                      }
                                    }else{ #Auxiliary parameter e.g. sigma 
                                      if(self$is.gaussian){
                                        malfunction_report(code=malfunctionCode()$distributions, msg="Distribution.R --> A non regCoef or intercept have a student-t prior in a gaussian GLM.",
                                                           type="error")
                                        stop("Something went wrong. Distribution.R --> A non regCoef or intercept have a Cauchy prior in a gaussian GLM.")
                                      }else{
                                        malfunction_report(code=malfunctionCode()$distributions, msg="Still empty. Distribution.R",
                                                           type="error")
                                        stop("Still empty. Distribution.R") #When will a normal distribution will be used for an auxiliary parameter?
                                      }
                                    }
                                    return(res)
                                  },
                                  getPrior = function(brms=F){
                                    if(brms){
                                      p <- brms::set_prior(paste0("cauchy(",self$auxParameter[[1]]$value,",",
                                                                  self$auxParameter[[2]]$value,")"))
                                      return(p)
                                    }else{
                                      return(cauchy(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value, autoscale=F))
                                    }
                                  },
                                  getFormula = function(rounded=T, index=NULL){
                                    tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                    tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                    if(self$is.vector){
                                      return(paste0(tags$span("Cauchy", class="formulaDistribution"),
                                                    " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))      
                                    }else{
                                      return(paste0(tags$span("Cauchy", class="formulaDistribution"),
                                                    " ( ",tmp_mu," , ",tmp_sigma," )"))
                                    }
                                  },
                                  getFormulaLatex = function(rounded=T, index=NULL){
                                    tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                    tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                    if(self$is.vector){
                                      param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                      param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                      return(distToLatex("Cauchy",list(param1,param2)))
                                    }else{
                                      param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                      param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                      return(distToLatex("Cauchy",list(param1,param2)))
                                    }
                                  },
                                  getAuxParametersLatex = function(index=NULL){
                                    tmp_mu <- self$getValueOf("mu")
                                    tmp_sigma <- self$getValueOf("sigma")
                                    if(self$is.vector){
                                      param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                      param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                      return(distAuxToLatex(list(param1,param2)))
                                    }else{
                                      param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                      param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                      return(distAuxToLatex(list(param1,param2)))
                                    }
                                  }
                                )
)

HalfCauchyDistribution <- R6Class(classname = "HalfCauchyDistribution", inherit = AbstractDistribution,
                              public = list(
                                dEnum=distributionEnum()$HalfCauchy,
                                setInitVals = function(){
                                  if(is.null(self$display_name)){
                                    self$display_name <- "Half-cauchy distribution"
                                  }
                                  if(is.null(self$description)){
                                    self$description <- "Half-cauchy distribution covers a range from 0 to +Inf"
                                  } 
                                },
                                setDefaultAuxParameter = function(){
                                  # fit<-rstanarm::stan_glm("y~1", data=data.frame(y=rnorm(100)), prior_intercept=cauchy(-1,5), prior_aux=cauchy(0,5))
                                  # self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=0, max_val=Inf)
                                  self$auxParameter[[1]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The variance (scale).", value=10, default_val=10,min_val=greaterZero, max_val=Inf)
                                },
                                plotMe = function(values){#1:sigma
                                  tmpSigma <- self$getValueOf("sigma")
                                  if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpSigma <- values[1]
                                  min <- 0
                                  max <- qhalfcauchy(0.95,scale=tmpSigma)
                                  x <- seq(from=min, to=max, length=1000)
                                  y <- dhalfcauchy(x,scale=tmpSigma)
                                  data <- data.frame(x=x,y=y)
                                  ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                },
                                adjustedParameters = function(){
                                  x = self$dataX
                                  y = self$dataY
                                  if(printDist) print("adjusted by half-cauchy")
                                  res <- list(sigma=NA)
                                  factor <- ifelse(self$is.gaussian,sd(y),1)
                                  #Intercept
                                  if(self$is.intercept){
                                    res$sigma <- 10*factor
                                  }else if(self$is.regCoef){
                                    if(length(unique(x))==1){
                                      res$sigma <- 2.5*factor
                                    }else if(length(unique(x)) == 2){
                                      res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                    }else{
                                      res$sigma <- (2.5*factor)/sd(x)          
                                    }
                                  }else{ #Auxiliary parameter e.g. sigma 
                                    res$sigma <- 2.5*factor
                                  }
                                  return(res)
                                },
                                getPrior = function(brms=F){                                  
                                  # return(cauchy(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value, autoscale=F))
                                  warning("This function should only be used in a context, where this function is used as a half-student-t.")
                                  if(brms){
                                    p <- brms::set_prior(paste0("cauchy(",0,",",
                                                                self$auxParameter[[2]]$value,")"),
                                                         lb=0)
                                    return(p)
                                  }else{
                                    return(cauchy(0,self$auxParameter[[1]]$value, autoscale=F))
                                  }
                                },
                                getFormula = function(rounded=T, index=NULL){
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    return(paste0(tags$span("Half-Cauchy", class="formulaDistribution"),
                                                  " ( 0 ,", "<b>&sigma;",tags$sub(index) ,"</b> )"))      
                                  }else{
                                    return(paste0(tags$span("Half-Cauchy", class="formulaDistribution"),
                                                  " ( 0 , ",tmp_sigma," )"))
                                  }
                                },
                                getFormulaLatex = function(rounded=T, index=NULL){
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    param1 <- list(name="0",index=NULL, vector=F)
                                    param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distToLatex("Half-Cauchy",list(param1,param2)))
                                  }else{
                                    param1 <- list(name=0,index=NULL, vector=F)
                                    param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distToLatex("Half-Cauchy",list(param1,param2)))
                                  }
                                },
                                getAuxParametersLatex = function(index=NULL){
                                  tmp_sigma <- self$getValueOf("sigma")
                                  if(self$is.vector){
                                    param1 <- list(name="0",index=NULL, vector=F)
                                    param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }else{
                                    param1 <- list(name=0,index=NULL, vector=F)
                                    param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }
                                }
                              )
)

GammaDistribution <- R6Class(classname = "GammaDistribution", inherit = AbstractDistribution,
                              public = list(
                                dEnum=distributionEnum()$Gamma,
                                setInitVals = function(){
                                  if(is.null(self$display_name)){
                                    self$display_name <- "Gamma distribution"
                                  }
                                  if(is.null(self$description)){
                                    self$description <- "Gamma distribution covers a range from 0 to +Inf"
                                  } 
                                },
                                setDefaultAuxParameter = function(){
                                  self$auxParameter[[1]] <- DistributionParameter$new(name = "shape", display_name="&alpha;", description="The shape parameter.", value=2, default_val=2,min_val=greaterZero, max_val=Inf)
                                  self$auxParameter[[2]] <- DistributionParameter$new(name = "rate", display_name="&beta;", description="The rate parameter.", value=2, default_val=2,min_val=greaterZero, max_val=Inf)
                                },
                                plotMe = function(values){ #1:shape, 2:rate
                                  tmpShape <- self$getValueOf("shape")
                                  tmpRate <- self$getValueOf("rate")
                                  if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpShape <- values[1]
                                  if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpRate <- values[2]
                                  min <- qgamma(0,shape=tmpShape,rate=tmpRate)
                                  max <- qgamma(0.99,shape=tmpShape,rate=tmpRate)
                                  x <- seq(from=min, to=max, length=1000)
                                  y <- dgamma(x,shape=tmpShape,rate=tmpRate)
                                  data <- data.frame(x=x,y=y)
                                  ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                },
                                adjustedParameters = function(){
                                  x = self$dataX
                                  y = self$dataY
                                  if(printDist) print("adjusted by gamma")
                                  malfunction_report(code=malfunctionCode()$distributions, msg="adjustedParameters not implemented GammaDistribution",
                                                     type="error")
                                  stop("Not implemented!")
                                  
                                  
                                },
                                getPrior = function(brms=F){
                                  malfunction_report(code=malfunctionCode()$distributions, msg="Not yet implemented GammaDistribution",
                                                     type="error")
                                  stop("Not yet implemented")
                                },
                                getFormula = function(rounded=T, index=NULL){
                                  tmp_shape <- ifelse(rounded,round(self$getValueOf("shape"),2), self$getValueOf("shape"))
                                  tmp_rate <- ifelse(rounded,round(self$getValueOf("rate"),2), self$getValueOf("rate"))
                                  if(self$is.vector){
                                    return(paste0(tags$span("Gamma", class="formulaDistribution"),
                                                  " ( ", "<b>&alpha;",tags$sub(index) ,"</b> , <b>", "&beta;", tags$sub(index),"</b> )"))      
                                  }else{
                                    return(paste0(tags$span("Gamma", class="formulaDistribution"),
                                                  " ( ",tmp_shape," , ",tmp_rate," )"))
                                  }
                                },
                                getFormulaLatex = function(rounded=T, index=NULL){
                                  tmp_shape <- ifelse(rounded,round(self$getValueOf("shape"),2), self$getValueOf("shape"))
                                  tmp_rate <- ifelse(rounded,round(self$getValueOf("rate"),2), self$getValueOf("rate"))
                                  if(self$is.vector){
                                    param1 <- list(name="\\alpha",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\beta",index=index, vector=self$is.vector)
                                    return(distToLatex("Gamma",list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_shape,index=NULL, vector=F)
                                    param2 <- list(name=tmp_rate,index=NULL, vector=F)
                                    return(distToLatex("Gamma",list(param1,param2)))
                                  }
                                },
                                getAuxParametersLatex = function(index=NULL){
                                  tmp_shape <- self$getValueOf("shape")
                                  tmp_rate <- self$getValueOf("rate")
                                  if(self$is.vector){
                                    param1 <- list(name="\\alpha",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\beta",index=index, vector=self$is.vector)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_shape,index=NULL, vector=F)
                                    param2 <- list(name=tmp_rate,index=NULL, vector=F)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }
                                }
                              )
)

HalfNormalDistribution <- R6Class(classname = "HalfNormalDistribution", inherit = AbstractDistribution,
                              public = list(
                                dEnum=distributionEnum()$HalfNormal,
                                setInitVals = function(){
                                  if(is.null(self$display_name)){
                                    self$display_name <- "Half-Normal distribution"
                                  }
                                  if(is.null(self$description)){
                                    self$description <- "Half-Normal distribution covers a range from 0 to +Inf"
                                  } 
                                },
                                setDefaultAuxParameter = function(){
                                  self$auxParameter[[1]] <- DistributionParameter$new(name = "mu", display_name="&mu;", description="The mean (location).", value=0, default_val=0,min_val=-Inf, max_val=Inf)
                                  self$auxParameter[[2]] <- DistributionParameter$new(name = "sigma", display_name="&sigma;", description="The variance (squared scale).", value=10, default_val=10,min_val=greaterZero, max_val=Inf)
                                },
                                plotMe = function(values){ #1:mu, 2:sigma
                                  tmpMu <- self$getValueOf("mu")
                                  tmpSigma <- self$getValueOf("sigma")
                                  if(!is.null(values[1]) && !is.na(values[1]) && is.numeric(values[1])) tmpMu <- values[1]
                                  if(!is.null(values[2]) && !is.na(values[2]) && is.numeric(values[2])) tmpSigma <- values[2]
                                  min <- qhalfnorm(0.01,mean=tmpMu,sd=tmpSigma)
                                  max <- qhalfnorm(0.99,mean=tmpMu,sd=tmpSigma)
                                  x <- seq(from=min, to=max, length=1000)
                                  y <- dhalfnorm(x,mean=tmpMu,tmpSigma)
                                  data <- data.frame(x=x,y=y)
                                  ggplot(data, aes(x=x,y=y)) + geom_line() + scale_y_continuous(breaks = NULL)
                                },
                                adjustedParameters = function(){
                                  x = self$dataX
                                  y = self$dataY
                                  if(printDist) print("adjusted by half-normal")
                                  res <- list(mu=NA,sigma=NA)
                                  factor <- ifelse(self$is.gaussian,sd(y),1)
                                  #Intercept
                                  if(self$is.intercept){
                                    res$mu <- 0
                                    res$sigma <- 10*factor
                                  }else if(self$is.regCoef){
                                    res$mu <- 0
                                    if(length(unique(x))==1){
                                      res$sigma <- 2.5*factor
                                    }else if(length(unique(x)) == 2){
                                      res$sigma <- (2.5*factor)/(abs(unique(x)[2]-unique(x)[1]))
                                    }else{
                                      res$sigma <- (2.5*factor)/sd(x)          
                                    }
                                  }else{ #Auxiliary parameter e.g. sigma 
                                    res$mu <- 0
                                    res$sigma <- 2.5*factor
                                  }
                                  return(res)
                                },
                                getPrior = function(brms=F){
                                  warning("This function should only be used in a context, where this function is used as a half-normal.")
                                  if(brms){
                                    p <- brms::set_prior(paste0("normal(",self$auxParameter[[1]]$value,",",
                                                                self$auxParameter[[2]]$value,")"),
                                                         lb=0)
                                    return(p)
                                  }else{
                                    return(normal(self$auxParameter[[1]]$value,self$auxParameter[[2]]$value, autoscale=F))
                                  }
                                },
                                getFormula = function(rounded=T, index=NULL){
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    return(paste0(tags$span("Half-Normal", class="formulaDistribution"),
                                                  " ( ", "<b>&mu;",tags$sub(index) ,"</b> , <b>", "&sigma;", tags$sub(index),"</b> )"))      
                                  }else{
                                    return(paste0(tags$span("Half-Normal", class="formulaDistribution"),
                                                  " ( ",tmp_mu," , ",tmp_sigma," )"))
                                  }
                                },
                                getFormulaLatex = function(rounded=T, index=NULL){
                                  tmp_mu <- ifelse(rounded,round(self$getValueOf("mu"),2), self$getValueOf("mu"))
                                  tmp_sigma <- ifelse(rounded,round(self$getValueOf("sigma"),2), self$getValueOf("sigma"))
                                  if(self$is.vector){
                                    param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distToLatex("Half-Normal",list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distToLatex("Half-Normal",list(param1,param2)))
                                  }
                                },
                                getAuxParametersLatex = function(index=NULL){
                                  tmp_mu <- self$getValueOf("mu")
                                  tmp_sigma <- self$getValueOf("sigma")
                                  if(self$is.vector){
                                    param1 <- list(name="\\mu",index=index, vector=self$is.vector)
                                    param2 <- list(name="\\sigma",index=index, vector=self$is.vector)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }else{
                                    param1 <- list(name=tmp_mu,index=NULL, vector=F)
                                    param2 <- list(name=tmp_sigma,index=NULL, vector=F)
                                    return(distAuxToLatex(list(param1,param2)))
                                  }
                                }
                              )
)

FixedValues <- R6Class(classname = "FixedValues", inherit = AbstractDistribution,
                                  public = list(
                                    dEnum=distributionEnum()$FixedValues,
                                    setInitVals = function(){
                                      self$fixedValue <- T
                                      if(is.null(self$display_name)){
                                        self$display_name <- "Fixed values"
                                      }
                                      if(is.null(self$description)){
                                        self$description <- "Use single fixed value or a variable containing fixed values."
                                      } 
                                    },
                                    setDefaultAuxParameter = function(){
                                      para <- self$paraProp
                                      if(is.null(para)) return()
                                      lower <- 0
                                      upper <- 1
                                      discrete <- F
                                      if(para$lower_limit=="-INF" || para$lower_limit=="VAR"){
                                        lower <- -Inf
                                      }else if(para$lower_limit==">0"){
                                        lower <- greaterZero
                                      }else if(para$lower_limit=="0"){
                                        lower <- 0
                                      }
                                      if(para$upper_limit=="INF" || para$upper_limit=="VAR"){
                                        upper <- Inf
                                      }else if(para$upper_limit=="1"){
                                        upper <- 1
                                      }
                                      self$auxParameter[[1]] <- DistributionParameter$new(name = "Value", display_name=para$display_name, description="The fixed value", value=1, default_val=1,min_val=lower, max_val=upper, discrete)
                                    },
                                    plotMe = function(values){ #1:Value, 
                                      x <- c(1:length(values))
                                      y <- self$getValueOf("Value")
                                      data <- data.frame(x=x,y=y)
                                      ggplot(data, aes(x=x,y=y)) 
                                    },
                                    adjustedParameters = function(){
                                      print("No adjustment possible, since this is a fixed value. Returning 1.")
                                      return(list(Value=1))
                                    },
                                    getPrior = function(brms=F){
                                      print("No prior, since this is a fixed value.")
                                    },
                                    getFormula = function(rounded=T, index=NULL){
                                      tmp_values <- ifelse(rounded,round(self$getValueOf("Value"),2), self$getValueOf("Value"))
                                      if(self$is.vector){
                                        return(paste0(tags$span(tmp_values)))
                                      }else{
                                        return(paste0(tags$span(tmp_values)))
                                      }
                                    },
                                    getFormulaLatex = function(rounded=T, index=NULL){
                                      tmp_values <- ifelse(rounded,round(self$getValueOf("Value"),2), self$getValueOf("Value"))
                                      if(self$is.vector){
                                        return(paste0(tmp_values))
                                      }else{
                                        return(paste0(tmp_values))
                                      }
                                    },
                                    getAuxParametersLatex = function(index=NULL){
                                      tmp_values <- self$getValueOf("Value")
                                      if(self$is.vector){
                                        return(list(tmp_values))
                                      }else{
                                        return(list(tmp_values))
                                      }
                                    }
                                  )
)


FixedBinomialN <- R6Class(classname = "FixedBinomialN", inherit = AbstractDistribution,
                       public = list(
                         dEnum=distributionEnum()$FixedBinomialN,
                         setInitVals = function(){
                           self$fixedValue <- T
                           if(is.null(self$display_name)){
                             self$display_name <- "Fixed values"
                           }
                           if(is.null(self$description)){
                             self$description <- "Use single fixed value or a variable containing fixed values."
                           } 
                         },
                         setDefaultAuxParameter = function(){
                           para <- self$paraProp
                           if(is.null(para)) return()
                           lower <- 1
                           upper <- Inf
                           discrete <- T
                           # if(para$lower_limit=="-INF" || para$lower_limit=="VAR"){
                           #   lower <- -Inf
                           # }else if(para$lower_limit==">0"){
                           #   lower <- greaterZero
                           # }else if(para$lower_limit=="0"){
                           #   lower <- 0
                           # }
                           # if(para$upper_limit=="INF" || para$upper_limit=="VAR"){
                           #   upper <- Inf
                           # }else if(para$upper_limit=="1"){
                           #   upper <- 1
                           # }
                           self$auxParameter[[1]] <- DistributionParameter$new(name = "Value", display_name=para$display_name, description="The fixed value", value="?", default_val="?",min_val=lower, max_val=upper, discrete)
                         },
                         plotMe = function(values){ #1:Value, 
                           x <- c(1:length(values))
                           y <- self$getValueOf("Value")
                           data <- data.frame(x=x,y=y)
                           ggplot(data, aes(x=x,y=y)) 
                         },
                         adjustedParameters = function(){
                           return(list(Value=NA))
                           return(list(Value=max(1,max(self$dataY))))
                         },
                         getPrior = function(brms=F){
                           print("No prior, since this is a fixed value.")
                         },
                         getFormula = function(rounded=T, index=NULL){
                           tmp_values <- self$getValueOf("Value")
                           if(is.numeric(tmp_values)){
                             tmp_values <- ifelse(rounded,round(self$getValueOf("Value"),2), self$getValueOf("Value"))
                           }
                           if(self$is.vector){
                             return(paste0(tags$span(tmp_values)))
                           }else{
                             return(paste0(tags$span(tmp_values)))
                           }
                         },
                         getFormulaLatex = function(rounded=T, index=NULL){
                           tmp_values <- self$getValueOf("Value")
                           if(is.numeric(tmp_values)){
                             tmp_values <- ifelse(rounded,round(self$getValueOf("Value"),2), self$getValueOf("Value"))
                           }
                           if(self$is.vector){
                             return(paste0(tmp_values))
                           }else{
                             return(paste0(tmp_values))
                           }
                         },
                         getAuxParametersLatex = function(index=NULL){
                           tmp_values <- self$getValueOf("Value")
                           if(is.numeric(tmp_values)){
                             tmp_values <- self$getValueOf("Value")
                           }
                           if(self$is.vector){
                             return(list(tmp_values))
                           }else{
                             return(list(tmp_values))
                           }
                         }
                       )
)



# "auxiliary" parameters for the distributions, e.g. mu and sigma for normal distribution
DistributionParameter <- R6Class(
  classname = "DistributionParameter",
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1",
    #For Planning
    type = "value", #value / variable
    otherVariableId = NULL,
    isAlt = F #not used
  ),
                                 
                                 
  public = list(
    
    name = "",
    display_name = "",
    description = "",
    value = NA,
    default_val = NA,
    tmp_value = NA,
    min_val = NA,
    max_val = NA,
    discrete = F,

    
    initialize = function(name, display_name, description, value, default_val, 
                          min_val, max_val, discrete=F, type="value", otherVariableId=NULL,
                          isAlt=F,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$name <- name
      self$display_name <- display_name
      self$description <- description
      self$value <- value
      self$default_val <- default_val
      self$min_val <- min_val
      self$max_val <- max_val
      self$discrete <- discrete
      private$type <- type
      private$otherVariableId <- otherVariableId
      private$isAlt <- isAlt
    },
    
    validInput = function(x){
      if(is.numeric(x)){
        if(self$discrete) x <- round(x)
        if(x < self$min_val) return(self$min_val)
        if(x > self$max_val) return(self$max_val)
        return(x)
      }else{
        return(self$default_val)
      }
    },
    
    #opt should be a vector of elements that would be ok, e.g. user variable names (:strings)
    #returns a list of two elements. First a boolean if its a valid input and second a message of invalidity
    isValidInput = function(x, opt=NULL){
      ret <- list()
      ret[[1]] <- T
      x_all <- x
      for(x in x_all){
        if(is.na(x) || is.null(x)){
          ret[[1]] <- F
          if(self$discrete){
            ret[[2]] <- "This parameter must be a discrete value "
          }else{
            ret[[2]] <- "This parameter must be a value "
          }
          ret[[2]] <- paste0(ret[[2]], "in a range of ", self$min_val, " to ", self$max_val, ".")
        }else if(is.numeric(x)){
          if(self$discrete && round(x)!=x){
            ret[[1]] <- F
            ret[[2]] <- "This parameter must be discrete."
          }
          if(x < self$min_val){
            ret[[1]] <- F
            ret[[2]] <- paste0("This parameter must be greater ", self$min_val, ".")
          } 
          if(x > self$max_val){
            ret[[1]] <- F
            ret[[2]] <- paste0("This parameter must be smaller ", self$max_val, ".")
          }
        }
      }

      if(all(x %in% opt)) ret[[1]] <- T
      return(ret)
    },
    
    #Returns the tmp value
    #Returns the value if the tmp value is null/na
    getTmpVal = function(){
      if(is.na(self$tmp_value) || is.null(self$tmp_value)){
        return(self$value)
      }else{
        return(self$tmp_value)
      }
    },
    
    tmpValToVal = function(){
      if(!is.na(self$tmp_value) && !is.null(self$tmp_value)) self$value = self$tmp_value
    },
    
    valToTmpVal = function(){
      self$tmp_value = self$value
    },
    
    removeTmpval = function(){
      self$tmp_value = NA
    },
    
    getValueFormatted = function(){
      return(format(self$value,digits=3))
    },
    
    getType = function() return(private$type),
    setType = function(type){
      private$type <- type
      if(type=="value"){
        private$otherVariableId <- NULL
      }
    },
    
    getOtherVariableId = function() return(private$otherVariableId),
    setOtherVariableId = function(otherVariableId){ 
      private$otherVariableId <- otherVariableId
    },
    
    getIsAlt = function(){
      return(private$isAlt)
    },
    setIsAlt = function(isAlt){
      private$isAlt <- isAlt
    },
    
    
    getInstance = function(){
      instance <- DistributionParameter$new(name=self$name, display_name=self$display_name, 
                                            description=self$description, value=self$value, 
                                            default_val=self$default_val, min_val=self$min_val,
                                            max_val=self$max_val, discrete=self$discrete,
                                            type=private$type, otherVariableId=private$otherVariableId,
                                            isAlt=private$isAlt)
      return(instance)
    },
    
    setInstance = function(instance){

      private$type = instance$getType()
      private$otherVariableId = instance$getOtherVariableId()
      private$isAlt = instance$getIsAlt()
      
      self$name = instance$name
      self$display_name = instance$display_name
      self$description = instance$description
      self$value = instance$value
      self$default_val = instance$default_val
      self$tmp_value = instance$tmp_value
      self$min_val = instance$min_val
      self$max_val = instance$max_val
      self$discrete = instance$discrete
      
    },
    
    compareTo = function(cT, exact=F){

      if(!equal0(private$type, cT$getType())) return(F)
      if(!equal0(private$otherVariableId, cT$getOtherVariableId())) return(F)
      if(!equal0(private$isAlt, cT$getIsAlt())) return(F)
      
      if(!equal0(self$name, cT$name)) return(F)
      if(!equal0(self$display_name, cT$display_name)) return(F)
      if(!equal0(self$description, cT$description)) return(F)
      if(!equal0(self$value, cT$value)) return(F)
      if(!equal0(self$default_val, cT$default_val)) return(F)
      if(!equal0(self$tmp_value, cT$tmp_value)) return(F)
      if(!equal0(self$min_val, cT$min_val)) return(F)
      if(!equal0(self$max_val, cT$max_val)) return(F)
      if(!equal0(self$discrete, cT$discrete)) return(F)
      
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
          
          type = private$type,
          otherVariableId = private$otherVariableId,
          isAlt = private$isAlt,
          
          name = self$name,
          display_name = self$display_name,
          description = self$description,
          value = self$value,
          default_val = self$default_val,
          tmp_value = self$tmp_value,
          min_val = self$min_val,
          max_val = self$max_val,
          discrete = self$discrete
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'DistributionParameter' = ret
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
      
      private$type = state$type
      private$otherVariableId = state$otherVariableId
      private$isAlt = state$isAlt
      
      self$name = state$name
      self$display_name = state$display_name
      self$description = state$description
      self$value = state$value
      self$default_val = state$default_val
      self$tmp_value = state$tmp_value
      self$min_val = state$min_val
      self$max_val = state$max_val
      self$discrete = state$discrete
    },
    resetState = function(){
      if(!super$resetState()) return()
    }
  )                     
                                 
)


#dist: Name of distribution, e.g. "Normal"
#params: list of parameters as a list(name="mu",index=1,vector=T)
distToLatex <- function(dist, params){
  tex <- paste0("\\text{", dist, "}(")
  for(p in params){
    if(p$vector) tex <- paste0(tex, "\\underline{")
    tex <- paste0(tex, p$name)
    if(!is.null(p$index)) tex <- paste0(tex, "_{",p$index,"}")
    if(p$vector) tex <- paste0(tex, "}")
    tex <- paste0(tex, ", ")
  }
  tex <- paste0(substr(tex,1,nchar(tex)-2), ")")
  return(tex)
}

distAuxToLatex <- function(params){
  ret <- list()
  for(p in params){
    tex <- ""
    if(p$vector) tex <- "\\underline{"
    tex <- paste0(tex, p$name)
    if(!is.null(p$index)) tex <- paste0(tex, "_{",p$index,"}")
    if(p$vector) tex <- paste0(tex, "}")
    ret <- list.append(ret, tex)
  }
  return(ret)
}


#density and quantile function for half-normal
dhalfnorm <- function (x, mean = 0,sd = 1, log = FALSE) {
  dens <- log(2) + dnorm(x, mean = mean, sd = sd, 
                         log = TRUE)
  if (log == FALSE) 
    dens <- exp(dens)
  return(dens)
}
qhalfnorm <- function (p, mean = 0,sd = 1, lower.tail = TRUE, log.p = FALSE) {
  p <- as.vector(p)
  sd <- as.vector(sd)
  if (any(p < 0) || any(p > 1)) 
    stop("p must be in [0,1].")
  if (any(sd <= 0)) 
    stop("The sd parameter must be positive.")
  NN <- max(length(p), length(sd))
  p <- rep(p, len = NN)
  sd <- rep(sd, len = NN)
  if (log.p == TRUE) 
    p <- exp(p)
  if (lower.tail == FALSE) 
    p <- 1 - p
  q <- qnorm((p + 1)/2, mean = mean, sd = sd)
  return(q)
}



