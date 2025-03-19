## In diesem Skript werden alle Stanmodelle deklaiert, die im Tool verwendet werden k?nnen.
## Jedes Modell muss von der Oberklase erben.
# library(R6)
# library(dplyr)
# library(futile.logger)

cNum <- characteristicEnum()

## instantiate concrete stan model classes
get_all_stan_models <- function(){
  a <- GLMGaussianStanModel$new()
  b <- GLMPoissonStanModel$new()
  c <- GLMBernoulliStanModel$new()
  d <- GLMBinomialStanModel$new()
  e <- GLMNegBinomialStanModel$new()
  f <- GLMGammaStanModel$new()
  g <- GLMInverseGaussianStanModel$new()
  h <- GLMLogNormalStanModel$new()
  i <- GLMBetaStanModel$new()
  j <- GLMExponentialStanModel$new()
  vector_stan_models <- c(a,b,c,d,e,f,g,h,i,j)
  
  return(vector_stan_models)
}


# Declare abstract stan model
BAYSISAbstractModel <- R6Class(
  classname = "AbstractStanModel", 
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.2"
  ),
  
  public = list(

    #unique id for 'stan' models
    id = NULL,
    display_name = NULL,
    description = NULL,
    is.discrete = NULL,
    
    
    #For parameter adjustment. Normal models behaviour is slight differnt. 
    #This var will be set TRUE for normal and log-normal distribution.
    #The log-normal is a simple normal model where the outcome will logarithmized.
    is.gaussian = F,

    
    #(R6) The PerIterationDataModel
    myPerIterationDataModel = NULL,
    
    # If the model will be run, this varaible holds the description text of the result.
    # Contains several information about the model fit like n_eff, rhat, pareto estimation, values of variables and their impact.
    result_description = NULL,
    
    
    # Model properties for users variable that are applicable.
    # Also used for filter method.
    modelProperties = list(must=list(), opt=list()),
    

    # integer, or VAR if the number of co-variable are independent
    number_co_var = NULL,
    number_optional_co_var = NULL,


    # Matrix of:
    # Columns: characteristic, lower limit, upper limit, description
    # Rows: Predictor, and each other
    # d: discrete, c: continuous
    parameters = data.frame(stringsAsFactors = F

    ),

    # This variable hold the optional parameters. This could be any type of variable with any constraint. 
    # If there are no constraint, use 'VAR'.
    # The number of Variable (numberVar) are used to take the number of selectInputs.
    # The name of the data frames will show in the selectInput, when adding a new term.
    # PossibleAmount hold the number of times this kind of term can be added. Use 'VAR' if there are no limit.
    optional_values = NULL,
    
    
    # Returns the formula of this model with certain parameters 
    get_formula = function(){return(NULL)},
    
    # Used ids for predictors
    id_used = c(),
    
    # Stores the ModelParameter initialized in concrete classes
    parameterLineElements = NULL, # e.g. self$get_para_order(c("Intercept", "RegCoef","Sigma"))
    
    
    # Which parameters and predictors are available
    avail_predictor = list(),
    avail_parameter = list(),
    
    # Which parameters and predictors are used (added by user, or default)
    used_predictor = list(), #Predictor also contains parameters
    used_parameter = list(),
    
    
    initialize = function(emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$init_para_pred()
      self$default_para_pred()
    },
    
    
    # Get smallest free id
    get_id = function(){
      i <- 0
      while(T){
        i <- i+1
        if(!i %in% self$id_used)return(i)
      }
    },
    
    # Add id to id_used
    add_id = function(i){
      if(i %in% self$id_used) stop("Id is already in use!")
      self$id_used <- c(self$id_used,i)
    },
    
    # Remove used id from list
    remove_id = function(i){
      self$id_used <- self$id_used[self$id_used!=i]
    },

    
    init_para_pred = function(){},
    default_para_pred = function(){},
    
    # Before adding a predictor, the predictor have to be created
    create_predictor_by_name = function(name){
      pred <- self$avail_predictor[[name]]$getInstance()
      pred$modelParameter$distribution <- pred$modelParameter$distribution$getInstance()
      return(pred)
    },

    #If the user add a predictor
    #Each predictor has also its related parameter, which will be added too.
    add_predictor = function(pred){
      if("ModelPredictor" %in% class(pred)){
        self$used_predictor <- list.append(self$used_predictor, pred)
        #Edit number of possible amount
        name <- pred$name
        for(i in self$avail_predictor){
          if(i$name == name) i$dec()
        }
        #Add also parameter
        self$add_parameter(pred$modelParameter)
        #After adding the modelparameter it get a new id that will also be passed to its predictor
        pred$id <- pred$modelParameter$id
      }else{
        warning("Not a predictor!")
      }
    },
    
    #Remove also the corresponding parameter
    remove_predictor = function(pred){
      id <- pred$id
      if("ModelPredictor" %in% class(pred)){
        #Remove para_pred
        for(i in seq_along(self$used_predictor)){
          obj <- self$used_predictor[[i]]
          if(obj$id == id){
            self$used_predictor[[i]] <- NULL
            break
          }
        }
        #Edit number of possible amount
        name <- pred$name
        for(i in self$avail_predictor){
          if(i$name == name){ 
            i$inc() 
            }
        }
        #Remove the distributions
        # self$remove_distributions_of_parameter(pred$modelParameter)
        #Remove also corresponding parameter
        self$remove_parameter(pred$modelParameter)
      }else{
        warning("Not a predictor!")
      }
      # self$remove_id(id)
    },
    
    get_predictor_of_type = function(pred_type){
      pred_list <- list()
      index <- 1
      for(i in self$used_predictor){
        if(i$name == pred_type){
          pred_list[[index]] <- i
          index <- index+1
        }
      }
      if(length(pred_list)==0){
        return(NULL)
      }else{
        return(pred_list)
      }
    },
    
    get_parameter_of_type = function(para_type){
      para_list <- list()
      index <- 1
      for(i in self$used_parameter){
        if(i$name == para_type){
          para_list[[index]] <- i
          index <- index+1
        }
      }
      if(length(para_list)==0){
        return(NULL)
      }else{
        return(para_list)
      }
    },
    
    #If the user add a parameter
    add_parameter = function(para){
      if(is.null(para$getParentPredictor()) &&
         para$display_name =="b") stop("null!!!")
      
      new_id <- para$id
      if(new_id > 0){
        new_id <- self$get_id()
      }
      para$id <- new_id
      self$add_id(new_id)
      if("ModelParameter" %in% class(para)){
        self$used_parameter <- list.append(self$used_parameter, para)
        #Edit number of possible amount
        name <- para$name
        for(i in self$avail_parameter){
          if(i$name == name) i$dec()
        }
      }else{ 
        warning("Not a parameter!")
      }
    },
    
    remove_parameter = function(para){
      id <- para$id
      if("ModelParameter" %in% class(para)){
        #Remove para
        for(i in seq_along(self$used_parameter)){
          obj <- self$used_parameter[[i]]
          if(obj$id == id){
            self$used_parameter[[i]] <- NULL
            break
          }
        }
        #Edit number of possible amount
        name <- para$name
        for(i in self$avail_parameter){
          if(i$name == name) i$inc()
        }
      }else{
        warning("Not a parameter!")
      }
      self$remove_id(id)
    },
    
    remove_distributions_of_parameter = function(parameter){

    },
    
    reindex_predictors = function(){
      for(i in self$used_predictor){
        next_id <- self$get_id()
        if(i$id > next_id){
          self$remove_id(i$id)
          i$id <- next_id
          i$modelParameter$id <- next_id
          self$add_id(next_id)
        }
      }
    },
    
    new_formula = function(response, predictor){return(NULL)},
    
    
    
    #Had to be overriden by concrete baysis stan model
    #Return false if the stan model should not be run (no predictor (also no intercept) selected)
    #Return true otherwise
    isStanModelRunnable = function(){
      stop("Not implemented for this baysis stan model!")
    },
    
    
    #Returns the r formula
    r_formula = function(){
      dMId <- self$myPerIterationDataModel$getDataModelInputData()
      response <- dMId$getResponseVariable(onlyName=T)
      formula <- paste0(response," ~ -1+")
      for(pred in self$get_predictor_line()){
        if(pred$name == "Intercept") formula <- paste0(response," ~ 1")
      }
      for(pred in self$get_predictor_line()){
        if(pred$name != "Intercept"){
          for(var in pred$userVariable){
            formula <- paste0(formula , var , ":")
          }
          formula <- substr(formula,1,nchar(formula)-1)
        } 
        formula <- paste0(formula, "+")
      }
      formula <- substr(formula,1,nchar(formula)-1)
      return(formula)
    },
    
    #Returns used user variables
    #extras are e.g. number of trials 'N' of the Binomial 
    get_used_vars = function(extras=F, response=F){
      dMId <- self$myPerIterationDataModel$getDataModelInputData()
      responseName <- dMId$getResponseVariable(onlyName=T)
      vars <- c()
      for(pred in self$get_predictor_line()){
        if(pred$name != "Intercept"){
          for(var in pred$userVariable){
            vars <- c(vars,var)
          }
        } 
      }
      if(extras) {
        add <- self$get_used_vars_additional()
        vars <- c(vars, add$var)
      }
      if(response) vars <- c(vars, responseName)
      return(vars)
    },
    
    #Returns additional variables (e.g. N of binomial dist)
    #Overwritten by concrete classes
    get_used_vars_additional = function(){
      return(NULL)
    },
    
    #Returns if the predictor is used within the formula
    is_predictor_used = function(){
      for(pred in self$get_predictor_line()){
        if(pred$name == "Intercept"){
          return(T)
        } 
      }
      return(F)
    },
    

    # Returns a list of parameter elements in order of given string elements
    get_para_order = function(elements){
      list <- list()
      for(order in elements){
        for(element in self$used_parameter){
          if(element$name == order){
            list <- list.append(list, element)
          }
        }
      }
      return(list) 
    },
    
    
    #Returns the predictor that is related to the parameter
    myParentPredictor = function(parameter){
      stop("Deprecated, BaysisStanModels.R --> myParentPredictor")
      parameter$findMe <- T
      for(i in self$used_predictor){
        if(i$modelParameter$findMe){
          parameter$findMe <- F
          return(i)
        }
      }
      parameter$findMe <- F
      return(NULL)
    },
    
    # Returns the parameter of the predictor line (in right order)
    # Predictor and its parameter for the predictor line (mu = b0 + b1*x ...)
    # Each element is in its own div to remove it easily by a right click 
    get_predictor_line = function(){
      res <- list()
      # index <- 1
      for(predictor in self$used_predictor){
        if(predictor$name == "Intercept"){
          # res[[index]] <- predictor
          # index <- index+1
          res <- list.append(res,predictor)
        }
      }
      for(predictor in self$used_predictor){
        if(predictor$name == "Predictor"){
          # res[[index]] <- predictor
          # index <- index+1
          res <- list.append(res,predictor)
        }
      }
      return(res)
    },
    
    #Adjust each (aux)parameter of distribution of each used model parameter (predictor) based on users input
    adjustDistributionParameters = function(tmpValue=F, tmpDist=F){
      #iterate over each used predictor
      for(para in self$used_parameter){
        self$adjustDistributionParametersOfParameter(parameter=para, tmpValue=tmpValue, tmpDist=tmpDist)
      }
    },
    
    #Adjust each (aux)parameter of distribution of predictor based on users input
    adjustDistributionParametersOfPredictor = function(predictor, tmpValue=F, tmpDist=F){
      self$adjustDistributionParametersOfParameter(predictor$modelParameter, tmpValue=tmpValue, tmpDist=tmpDist)
    },
    
    #Adjust each (aux)parameter of distribution of parameter based on users input
    adjustDistributionParametersOfParameter = function(parameter, tmpValue=F, tmpDist=F){
      if(parameter$is.vector){
        self$adjustDistributionParametersOfVector(parameter, tmpValue=tmpValue, tmpDist=tmpDist)
      }else{
        if(tmpDist){
          dist <- parameter$distribution_tmp
        }else{
          dist <- parameter$distribution
        }
        user_var <- parameter$getParentPredictor()$userVariable
        
        x <- NULL
        if(!is.null(user_var)){
          dMId <- self$myPerIterationDataModel$getDataModelInputData()
          x <- dMId$getLongFormatVariable(user_var, completeCases=T)
        }

        dist$setDataAndProperties(x=x, perIterationDataModel=self$myPerIterationDataModel, para=parameter)
        dist$adjustMyself(tmpValue=tmpValue)
      }
    },
    
    adjustDistributionParametersOfVector = function(parameter, tmpValue=F, tmpDist=F){
      predictor <- parameter$getParentPredictor()
      varsElementList <- self$getTermCombinationsOfPredictorListOfList(predictor)
      
      dMId <- self$myPerIterationDataModel$getDataModelInputData()
      y <- dMId$getResponseVariable(onlyName=T)
      
      data <- dMId$getLongFormatVariable(c(y,self$get_used_vars()), completeCases=T)

      
      for(el in varsElementList){
        data_t <- data[c(y,el$vars)]
        
        #Handle interaction data 
        for(v_i in 1:length(el$vars)){
          vars <- el$vars[v_i]
          varEl <- el$varElements[v_i]
          #if var is conti, nothing is changed
          if(is.na(varEl))next
          data_t[[vars]] <- ifelse(data_t[[vars]]==varEl,1,0)
        }
        x <- rep(1,length(data_t[,1]))
        for(v_i in 1:length(el$vars)){
          x <- x*data_t[[el$vars[v_i]]]
        }
        
        if(tmpDist){
          if(is.null(parameter$distributions_tmp[[el$string]]) || 
             (parameter$distributions_tmp[[el$string]]$dist_name != parameter$distribution_tmp$dist_name)){
            dist <- parameter$distribution_tmp$getInstance()
          }else{
            dist <- parameter$distributions_tmp[[el$string]]
          } 
        }else{
          if(is.null(parameter$distributions[[el$string]]) || 
             (parameter$distributions[[el$string]]$dist_name != parameter$distribution$dist_name)){
            dist <- parameter$distribution$getInstance()
          }else{
            dist <- parameter$distributions[[el$string]]
          } 
        }
        
        #and adjust
        dist$setDataAndProperties(x=data.frame(x=x), perIterationDataModel=self$myPerIterationDataModel, para=parameter)
        dist$adjustMyself(tmpValue=tmpValue)
        
        if(tmpDist){
          parameter$distributions_tmp[[el$string]] <- dist
          parameter$distributions_tmp[[el$string]]$element_name <- el$string
        }else{
          parameter$distributions[[el$string]] <- dist
          parameter$distributions[[el$string]]$element_name <- el$string
        }
      }
    },
    
    #Just a single distribution of a vector element
    adjustDistributionParameterOfVector = function(parameter, elementName, tmpValue=F, tmpDist=F){
      if(tmpDist){
        parameter$distributions_tmp[[elementName]] <- self$getAdjustedDistributionParameterOfVector(parameter, elementName, tmpValue=tmpValue, tmpDist=tmpDist)
        parameter$distributions_tmp[[elementName]]$element_name <- elementName
      }else{
        parameter$distributions[[elementName]] <- self$getAdjustedDistributionParameterOfVector(parameter, elementName, tmpValue=tmpValue, tmpDist=tmpDist)
        parameter$distributions[[elementName]]$element_name <- elementName
      }
    },
    
    #Just a single distribution of a vector element
    getAdjustedDistributionParameterOfVector = function(parameter, elementName, tmpValue=F, tmpDist=F){
      # predictor <- self$myParentPredictor(parameter)
      predictor <- parameter$getParentPredictor()
      varsElementList <- self$getTermCombinationsOfPredictorListOfList(predictor)

      dMId <- self$myPerIterationDataModel$getDataModelInputData()
      y <- dMId$getResponseVariable(onlyName=T)
      
      data <- dMId$getLongFormatVariable(c(y,self$get_used_vars()), completeCases=T)
      
      for(el in varsElementList){
        if(el$string != elementName) next;
        data_t <- data[c(y,el$vars)]
        
        #Handle interaction data 
        for(v_i in 1:length(el$vars)){
          vars <- el$vars[v_i]
          varEl <- el$varElements[v_i]
          #if var is conti, nothing is changed
          if(is.na(varEl))next
          data_t[[vars]] <- ifelse(data_t[[vars]]==varEl,1,0)
        }
        x <- rep(1,length(data_t[,1]))
        for(v_i in 1:length(el$vars)){
          x <- x*data_t[[el$vars[v_i]]]
        }
        
        if(tmpDist){
          if(is.null(parameter$distributions_tmp[[el$string]]) || 
             (parameter$distributions_tmp[[el$string]]$dist_name != parameter$distribution_tmp$dist_name)){
            dist <- parameter$distribution_tmp$getInstance()
          }else{
            dist <- parameter$distributions_tmp[[el$string]]
          } 
        }else{
          if(is.null(parameter$distributions[[el$string]]) || 
             (parameter$distributions[[el$string]]$dist_name != parameter$distribution$dist_name)){
            dist <- parameter$distribution$getInstance()
          }else{
            dist <- parameter$distributions[[el$string]]
          } 
        }
        
        #and adjust 
        dist$setDataAndProperties(x=data.frame(x=x), perIterationDataModel=self$myPerIterationDataModel, para=parameter) 
        dist$adjustMyself(tmpValue=tmpValue)
        
        return(dist)
        break;
      }
    },
    
    
    #Edit the elements of vector predictors
    revisePredictors = function(){
      preds <- self$used_predictor
      for(i in preds){
        dist_names <- names(i$modelParameter$distributions)
        new_dist_names <- as.vector(unlist(self$getTermCombinationsOfPredictorList(i)))
        for(element in new_dist_names){
          #if this element is not part of yet, create it
          if(!element %in% dist_names){
            i$modelParameter$distributions[[element]] <- self$getAdjustedDistributionParameterOfVector(i$modelParameter, element, tmpValue=F, tmpDist=F)
            i$modelParameter$distributions[[element]]$element_name <- element
          }
        }
        #remove the other elements from the parameter
        for(elements in new_dist_names){
          if(!element %in% new_dist_names){
            i$modelParameter$distributions[[element]] <- NULL
            i$modelParameter$distributions_tmp[[element]] <- NULL
          } 
        }
      }
    },
    
    
    #returns a sub list of @getTermCombinationsOfPredictorListOfList
    #that contains a named list, with names equal to the selectInput field with the content
    #of the string field.
    getTermCombinationsOfPredictorList = function(pred){
      retList <- self$getTermCombinationsOfPredictorListOfList(pred)
      ret <- list()
      for(el in retList){
        ret[[el$selectInput]] <- el$string
      }
      return(ret)
    },
    
    #Implemented in concrete classes
    getTermCombinationsOfAllModelElements = function(withoutAux=F){
      stop("not yet implemented")
    },
    
    #TODO: interaction of conti+cat -> varElements should be NA
    #returns a named list of elements that are again a list of 4 entries
    #string: Aa:Ba
    #vars: A,B
    #varElements: a,a
    #selectInput: A<i>a</i>:B<i>a</i>
    getTermCombinationsOfPredictorListOfList = function(pred){
      vars <- pred$userVariable
      comb <- self$getTermCombinationsOfPredictor(pred)
      ft <- terms(formula(self$r_formula()), allowDotAsName=T)
      attr <- attributes(ft)
      term.labels <- attr$term.labels
      right.split <- ""
      
      ret <- list()
      
      for(label in term.labels){
        splits <- strsplit(label,":")[[1]]
        if(length(splits) == length(vars)){
          if(all(vars %in% splits)){
            right.split <- splits
            break;
          }
        }
      }
      
      for(c in comb){
        ret_element <- list()
        ret_element[["string"]] <- c
        if(length(vars)>1){
          single_c <- strsplit(c,":")[[1]]
          name <- ""
          ret_varsele <- c()
          for(c_index in 1:length(single_c)){
            c_i <- single_c[c_index]
            ret_varsele <- c(ret_varsele, substr(c_i,nchar(right.split[c_index])+1,nchar(c_i)))
            name <- paste0(name, right.split[c_index], "<i>", substr(c_i,nchar(right.split[c_index])+1,nchar(c_i)) , "</i>:")
          }
          name <- substr(name,1,nchar(name)-1)
          ret_element[["vars"]] <- right.split
          ret_element[["varElements"]] <- ifelse(ret_varsele == "",NA,ret_varsele)
          ret_element[["selectInput"]] <- name
          ret[[c]] <- ret_element
        }else{
          ret_element[["vars"]] <- vars
          ret_element[["varElements"]] <- ifelse(substr(c,nchar(vars)+1,nchar(c)) == "",NA,substr(c,nchar(vars)+1,nchar(c)))
          ret_element[["selectInput"]] <- paste0(substr(c,1,nchar(vars)), "<i>", substr(c,nchar(vars)+1,nchar(c)) ,"</i>")
          ret[[c]] <- ret_element
        }
      }
      return(ret)
    },
    
    getTermCombinationsOfPredictor = function(pred){
      #data
      dMId <- self$myPerIterationDataModel$getDataModelInputData()

      y <- dMId$getResponseVariable(onlyName=T)
      dat <- dMId$getLongFormatVariable(c(y,self$get_used_vars()), completeCases=T)
    
      #get current formula
      ft <- terms(formula(self$r_formula()), allowDotAsName=T)
      #create vars from predictor
      vars <- pred$userVariable
      
      # dMID$getInputProperty(vars, type="type")
      
      # Returns by a given formula and variable names all possible interaction terms
      # E.g. y~ x1+x2+x1:x2 and vars=c("x1","x2) ; x1 and x2 categorial var with "A","B"
      # Return: x1A:x2A, x1B:x2B (and depending on data also: ) x1A:x2B, x1B:x2A
      return(formula_combination(ft, dat, vars))
    },
    
    
    #Get explanation of certain models
    getExplanation = function(response){
      
      return(NULL)
      
    },
    
    # Run the stan code with given parameters
    run_stan_code = function(){return(NULL)},
    
    #Get priors
    getPrior = function(type = c("intercept","aux","predictor"), element_name = NULL){
      stop("Not yet implemented for this concrete baysis stan model.")
    },
    
    #cast prior list to single distribution with vectors of distribution parameters
    getPriorAsSingleVector = function(prior_list, brms=F){
  
      if(brms){
        #Special priors like horseshoe?
        if(prior_list[[1]]$dist %in% c("hs")){
          df <- data.frame(matrix(unlist(prior_list), ncol=length(prior_list), byrow=F))
          row_identical <- apply(df, 1, function(x) length(unique(x)) == 1)
          if(!all(row_identical) && localUse) browser()
          return(prior_list[[1]])
        }
        return(prior_lists)
      }else{
        if(length(prior_list)==0){
          return(NULL)
        }else if(length(prior_list)==1){
          return(prior_list[[1]])
        }else {
          
          #Special priors like horseshoe?
          if(prior_list[[1]]$dist %in% c("hs")){
            df <- data.frame(matrix(unlist(prior_list), ncol=length(prior_list), byrow=F))
            row_identical <- apply(df, 1, function(x) length(unique(x)) == 1)
            if(!all(row_identical) && localUse) browser()
            return(prior_list[[1]])
          }
          
          ret <- prior_list[[1]]
          namesAuxParas <- names(ret)
          for(e_i in 2:length(prior_list)){
            e <- prior_list[[e_i]]
            for(nAuxPara in namesAuxParas){
              ret[[nAuxPara]] <- c(ret[[nAuxPara]],e[[nAuxPara]])
            }
          }
          ret$dist <- ret$dist[[1]]
          return(ret)
        }
      }

    },
    
    regular_description = function(combination=F, intercept=T){
      pre <- "The distribution shows uncertainty of the effect of the predictor"
      if(combination) pre <- paste0(pre, " combination")
      if(intercept){
        pre <- paste0(pre, ", which is an intercept.")
      }else{
        pre <- paste0(pre, ", which is a slope.")
      }
      post <-"In case of a true effect (and an optimal fit) the distribution is 
      one sided according to the dashed line (pi values are nearly 0 and 1). 
      If the effect is unclear, both pi values are around 0.5. 
      Often a pi(+ or -) value of  <0.05 is advisable."
      return(list(pre=pre,post=post))
    },
    
    effects_description = function(type=c("slope","group")){return("No description available for this model.")},
    
    #Should be overwritten by concretes
    matchAuxiliaryNames = function(name){
      return(name)
    },
    
    #Overwritten by concrete class
    postprocessing = function(stan_result, content){
      stop("Function: 'postprocessing' not implemented")
    },
    
    #Overwritten by concrete class
    get_x_intercept = function(transform, single){
      stop("Function: 'get_x_intercept' not implemented")
    },
    
    #Overwritten by concrete class
    transform_post = function(postA, postB=NULL, transform=F){
      stop("Function: 'transform_post' not implemented")
    },
    
    #Not overwritten by concrete class
    #But extended in lognormal
    #EDIT: Overwritten in beta, due to errors (https://github.com/stan-dev/rstanarm/issues/407)
    make_predictions = function(stanObject, data, mean=F, draws=NULL){
      stanfit <- extract_stanfit(stanObject)
      
      nDraws <- dim(as.matrix(stanfit))[1]
      if(!is.null(draws)) draws <- min(draws, nDraws)
      
      if(mean){
        if(is.null(data)){
          if(is.null(draws)) draws <- 100
          return(posterior_epred(stanObject, draws=draws, ndraws=draws))
        }else if(dim(data)[1]==0){
          return(posterior_epred(stanObject)[,1,drop=F])
        }else{
          return(posterior_epred(stanObject, newdata=data))
        }
      }else{
        if(is.null(data)){
          if(is.null(draws)) draws <- 100
          return(posterior_predict(stanObject, draws=draws, ndraws=draws))
        }else if(dim(data)[1]==0){
          return(posterior_predict(stanObject)[,1,drop=F])
        }else{
          return(posterior_predict(stanObject, newdata=data))
        }
      }
    },
    
    #Overwritten by concrete class
    get_density = function(stanObject, data, mean=F){
      stop("Function: 'get_density' not implemented")
    },
    
    #Overwritten by concrete class
    get_overlap = function(stanObject, data1, data2, mean=F){
      stop("Function: 'get_overlap' not implemented")
    },
    
    
    #For some 'stan' models it is necessary to transform the response variable.
    #E.g the binomial takes as the response just the proportion of success rather than the total count of successes.
    #For other 'stan' models, just pass y back
    transformResponse = function(y){
      return(y)
    },
    
    inverseTransformResponse = function(y){
      return(y)
    },
    
    jacobianCorrection = function(x,...){
      return(x)
    },
    
    #Implemented in concrete classes if necessary
    plot_scale = function(x=F,y=F){
      return(NULL)
    },
    
    # This function will be used for the unit test. It returns each necessarily variable.
    get_my_vars = function(){
      vars <- list(self$id, self$display_name, self$number_co_var, self$number_optional_co_var, self$parameters, self$optional_values,
                   self$get_formula, self$run_stan_code)
      return(vars)
    },

    # Contains description, that will be used by each explicit baysis stan model
    result_description_pre = "Check for Markov chain sampling problems:<br>",
    
    result_description_post = "<p>Use the 'posterior predictive check' (PPC, upper right) 
      to check whether the fitted model is consistent with measured response. <br>
      'Marginal posteriors' (below) gives medians of the coefficients associated with each predictor,
      and intervals indicating the uncertainty of these coefficients. <br>
      'Direction of effects' (lower right) shows in which direction the response variable changes if we change a predictor.</p>",

    
    getInstance =  function(myPerIterationDataModel){

      concreteClass <- class(self)[1]
      newInstance <- get(concreteClass)$new()
      
      newInstance$id <- self$id
      newInstance$display_name <- self$display_name
      newInstance$description <- self$description
      newInstance$is.discrete <- self$is.discrete
      newInstance$is.gaussian <- self$is.gaussian
      newInstance$result_description <- self$result_description
      newInstance$number_co_var <- self$number_co_var
      newInstance$number_optional_co_var <- self$number_optional_co_var
      newInstance$parameters <- self$parameters
      newInstance$optional_values <- self$optional_values
      newInstance$id_used <- self$id_used
      
      
      #R6
      newInstance$myPerIterationDataModel <- myPerIterationDataModel

      
      #R6 ModelParameter
      newInstance$parameterLineElements <- list()
      for(i in self$parameterLineElements){
        newInstance$parameterLineElements <- list.append(newInstance$parameterLineElements, i$getInstance())
      }
      
      #R6 ModelProp
      newInstance$modelProperties <- list(must=list(), opt=list())
      for(i in self$modelProperties$opt){
        newInstance$modelProperties$opt <- list.append(newInstance$modelProperties$opt, i$getInstance())
      }
      for(i in self$modelProperties$must){
        newInstance$modelProperties$must <- list.append(newInstance$modelProperties$must, i$getInstance())
      }

      newInstance$avail_predictor <- list()
      for(aa_id in seq_along(self$avail_predictor)){
        aa <- self$avail_predictor[[aa_id]]
        newInstance$avail_predictor <- list.append(newInstance$avail_predictor, aa$getInstance(), names(self$avail_predictor)[aa_id])
      }
      newInstance$avail_parameter <- list()
      for(aa_id in seq_along(self$avail_parameter)){
        aa <- self$avail_parameter[[aa_id]]
        newInstance$avail_parameter <- list.append(newInstance$avail_parameter, aa$getInstance(NULL), names(self$avail_parameter)[aa_id])
      }

      
      newInstance$used_parameter <- list()
      newInstance$used_predictor <- list()
      for(aa in self$used_predictor){
        aa_inst <- aa$getInstance()
        newInstance$used_predictor <- list.append(newInstance$used_predictor, aa_inst)
        newInstance$used_parameter <- list.append(newInstance$used_parameter, aa_inst$modelParameter)
      }
      
      for(aa in self$used_parameter){
        if(is.null(aa$getParentPredictor())) 
          newInstance$used_parameter <- list.append(newInstance$used_parameter, aa$getInstance(NULL))
      }


      return(newInstance)
    },
    
    setInstance = function(instance){

      self$id <- instance$id
      self$display_name <- instance$display_name
      self$description <- instance$description
      self$is.discrete <- instance$is.discrete
      self$is.gaussian <- instance$is.gaussian
      self$result_description <- instance$result_description
      self$number_co_var <- instance$number_co_var
      self$number_optional_co_var <- instance$number_optional_co_var
      self$parameters <- instance$parameters
      self$optional_values <- instance$optional_values
      self$id_used <- instance$id_used

      #R6
      self$myPerIterationDataModel <- instance$myPerIterationDataModel

      self$parameterLineElements <- instance$parameterLineElements
      self$modelProperties <- instance$modelProperties
      self$avail_parameter <- instance$avail_parameter
      self$avail_predictor <- instance$avail_predictor

      self$used_predictor <- instance$used_predictor
      self$used_parameter <- instance$used_parameter
    },
    
  
    getState = function(uuid){
      nextUUID <- uuid
      ret <- list()
      
      if(self$getUUID() == -1){
        nextUUID <- nextUUID + 1
        self$setUUID(uuid)
        
        myPerIterationDataModelState <- NULL
        if(!is.null(self$myPerIterationDataModel)){
          myPerIterationDataModelState <- self$myPerIterationDataModel$getState(nextUUID)
          nextUUID <- myPerIterationDataModelState$nextUUID
        }
        
        parameterLineElementsState <- list()
        for(aa in self$parameterLineElements){
          cState <- aa$getState(nextUUID)
          parameterLineElementsState <- list.append(parameterLineElementsState, cState)
          nextUUID <- cState$nextUUID
        }
        
        modelPropertiesState <- list(must=list(), opt=list())
        for(aa in self$modelProperties$must){
          cState <- aa$getState(nextUUID)
          modelPropertiesState$must <- list.append(modelPropertiesState$must, cState)
          nextUUID <- cState$nextUUID
        }
        for(aa in self$modelProperties$opt){
          cState <- aa$getState(nextUUID)
          modelPropertiesState$opt <- list.append(modelPropertiesState$opt, cState)
          nextUUID <- cState$nextUUID
        }
        
        avail_parameterState <- list()
        for(aa_id in seq_along(self$avail_parameter)){
          aa <- self$avail_parameter[[aa_id]]
          cState <- aa$getState(nextUUID)
          avail_parameterState <- list.append(avail_parameterState, cState, names(self$avail_parameter)[aa_id])
          nextUUID <- cState$nextUUID
        }
        
        avail_predictorState <- list()
        for(aa_id in seq_along(self$avail_predictor)){
          aa <- self$avail_predictor[[aa_id]]
          cState <- aa$getState(nextUUID)
          avail_predictorState <- list.append(avail_predictorState, cState, names(self$avail_predictor)[aa_id])
          nextUUID <- cState$nextUUID
        }
        
        used_predictorState <- list()
        for(aa_id in seq_along(self$used_predictor)){
          aa <- self$used_predictor[[aa_id]]
          cState <- aa$getState(nextUUID)
          used_predictorState <- list.append(used_predictorState, cState)
          nextUUID <- cState$nextUUID
        }
        
        used_parameterState <- list()
        for(aa_id in seq_along(self$used_parameter)){
          aa <- self$used_parameter[[aa_id]]
          cState <- aa$getState(nextUUID)
          used_parameterState <- list.append(used_parameterState, cState)
          nextUUID <- cState$nextUUID
        }
        
        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          id = self$id,
          display_name = self$display_name,
          description = self$description,
          is.discrete = self$is.discrete,
          is.gaussian = self$is.gaussian,
          result_description = self$result_description,
          number_co_var = self$number_co_var,
          number_optional_co_var = self$number_optional_co_var,
          parameters = self$parameters,
          optional_values = self$optional_values,
          id_used = self$id_used,
          
          #R6
          myPerIterationDataModel = myPerIterationDataModelState,
          
          parameterLineElements = parameterLineElementsState, 
          modelProperties = modelPropertiesState,
          avail_parameter = avail_parameterState,
          avail_predictor = avail_predictorState,
          
          used_predictor = used_predictorState,
          used_parameter = used_parameterState
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'AbstractStanModel' = ret
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
      
      self$id = state$id
      self$display_name = state$display_name
      self$description = state$description
      self$is.discrete = state$is.discrete
      self$is.gaussian = state$is.gaussian
      self$result_description = state$result_description
      self$number_co_var = state$number_co_var
      self$number_optional_co_var = state$number_optional_co_var
      self$parameters = state$parameters
      self$optional_values = state$optional_values
      self$id_used = state$id_used
      
      #R6
      self$myPerIterationDataModel = state$myPerIterationDataModel
      
      self$parameterLineElements = state$parameterLineElements
      self$modelProperties = state$modelProperties
      self$avail_parameter = state$avail_parameter
      self$avail_predictor = state$avail_predictor
      
      self$used_predictor = state$used_predictor
      self$used_parameter = state$used_parameter
    },
    resetState = function(){
      if(!super$resetState()) return()
      if(!is.null(self$myPerIterationDataModel)) self$myPerIterationDataModel$resetState()
      
      for(aa in self$parameterLineElements){
        aa$resetState()
      }
      for(aa in self$modelProperties$must){
        aa$resetState()
      }
      for(aa in self$modelProperties$opt){
        aa$resetState()
      }
      for(aa in self$avail_parameter){
        aa$resetState()
      }
      for(aa in self$avail_predictor){
        aa$resetState()
      }
      for(aa in self$used_predictor){
        aa$resetState()
      }
      for(aa in self$used_parameter){
        aa$resetState()
      }
    }
    
  )

)






# Model properties (for concrete StanModels)
# If NA is given for any of the attributes, this attribute will be ignored for comparison.
# That means if e.g. lower=NA, there is no lower limit and a variable can have any kind of lower limit.
# Lower (Upper) Limit can have "-INF", "0", ">0", "INF"
ModelProp <- R6Class(
  classname="ModelProp",
  inherit = SerializationInterface,
  
  private = list(
    stateVersion = "0.1"
  ),
  
  public = list(
    name = "",
    posNumber = NA,
    char = NA,
    lower = NA,
    upper = NA,
    groupId = NA,
    
    initialize = function(name, char, lower, upper, posNumber, groupId,
                          emptyState = F){
      super$initialize()
      if(emptyState) return()
      
      self$name <- name
      self$posNumber <- posNumber
      self$char <- char
      self$lower <- lower
      self$upper <- upper
      self$groupId <- groupId
    },
    
    getInstance =  function(){
      newInstance <- ModelProp$new(name = self$name, char = self$char, lower = self$lower, 
                                   upper = self$upper, posNumber = self$posNumber, groupId = self$groupId)
      return(newInstance)
    },
    
    setInstance = function(instance){
      self$name <- instance$name
      self$posNumber <- instance$posNumber
      self$char <- instance$char
      self$lower <- instance$lower
      self$upper <- instance$upper
      self$groupId <- instance$groupId
    },
    
    getState = function(uuid){
      nextUUID <- uuid+1
      ret <- list()
      
      if(self$getUUID() == -1){
        self$setUUID(uuid)

        ret <- list(
          uuid = uuid, 
          stateVersion = private$stateVersion,
          
          name = self$name,
          posNumber = self$posNumber,
          char = self$char,
          lower = self$lower,
          upper = self$upper,
          groupId = self$groupId
        )
      }
      
      ret <- list(
        uuid = self$getUUID(),
        nextUUID = nextUUID,
        'ModelProp' = ret
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
      
      self$name = state$name
      self$posNumber = state$posNumber
      self$char = state$char
      self$lower = state$lower
      self$upper = state$upper
      self$groupId = state$groupId
    },
    resetState = function(){
      if(!super$resetState()) return()
    }
  )
)


