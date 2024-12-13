# Extend the list of characterstic with 'VAR' for variable/changeable
cEnum <- characteristicEnum()
cEnum$VAR <- 'VAR'

# GLM with binomial distribution
GLMBinomialStanModel <- R6Class(classname = "GLMBinomialStanModel",
                                inherit = BAYSISAbstractModel,
                                
                                
                                public = list(
                                  id = 4,
                                  display_name = 'GLM - Binomial distribution',
                                  description = 'The binomial distribution is used for modelling the number of successes in a sequence of N independet experiments.
                                  \nYour response variable should be discrete and have a lower limit of 0 and an upper limit of INF. Please make sure that your (theoretical) limits makes sense.',
                                  is.discrete = T,
                                  
                                  
                                  # response is a must have
                                  modelProperties = list(
                                    must = list(response = ModelProp$new("response", cEnum$Discrete, "0", "INF", 1, NA)),
                                    opt = list(predictor = ModelProp$new("predictor", NA, NA, NA, NA, NA))
                                  ),
                                  
                                  
                                  
                                  init_para_pred = function(){
                                    self$avail_parameter = list(
                                      Intercept = ModelParameter$new("Intercept", 0, NULL, "b","The intercept of the GLM.", F, T, "GLMPredictor",
                                                                     cNum$Continuous, "VAR", "VAR",
                                                                     1,
                                                                     FactoryDistribution(name="RegCoefDist",dist=dEnum$Normal, adjustable=T, is.vector=F),
                                                                     c(dEnum$Normal,dEnum$StudentT,dEnum$Cauchy)),
                                      N = ModelParameter$new("N", -1, -1, "N","Number of trials.", F, F, "AuxParameter",
                                                                 cNum$Discrete, "0", "VAR",
                                                                 1,
                                                                 FactoryDistribution(name="BinomialN",dist=dEnum$FixedBinomialN, adjustable=F, is.vector=F, paraProp=list(lower_limit="0", upper_limit="INF",discrete=T)),
                                                                 c(dEnum$FixedBinomialN)),
                                      #Reg coef, not selectable for user
                                      RegCoef = ModelParameter$new("RegCoef", 1, NULL, "b","Regression coefficient / slope of the GLM.", F, F, "GLMPredictor",
                                                                   cNum$Continuous, "VAR", "VAR",
                                                                   NULL, 
                                                                   FactoryDistribution(name="RegCoefDist",dist=dEnum$Normal, adjustable=T, is.vector=F),
                                                                   c(dEnum$Normal,dEnum$StudentT,dEnum$Cauchy))
                                    )
                                    self$avail_predictor = list(
                                      Intercept = ModelPredictor$new("Intercept", -1, NULL, "It is recommended to use one. But also legit without one.", T,
                                                                     cNum$Continuous, "VAR", "VAR",
                                                                     0,1,F,
                                                                     self$avail_parameter$Intercept$getInstance()),
                                      Predictor = ModelPredictor$new("Predictor", -1, "Predictor","Term of the GLM", T,
                                                                     'VAR', 'VAR', 'VAR',
                                                                     c(1,2,3,4,5,6,7,8,9),Inf,T, 
                                                                     self$avail_parameter$RegCoef$getInstance())
                                    )
                                  },
                                  

                                  default_para_pred = function(){
                                    self$add_predictor(self$avail_predictor$Intercept)
                                    self$add_parameter(self$avail_parameter$N)
                                  },
                                  
                                  # dontShowVar = c("sigma"),
                                  
                                  
                                  # Returns the interactive Formula for selecting predictors etc.
                                  new_formula = function(response){
                                    res <- list()
                                    
                                    ## FormulaElements
                                    # type, addPriorButton, predictorLine, 
                                    # leftSide, rightSide,
                                    # removable = F, myRemovableParamter = NULL
                                    res[[1]] <- FormulaElements$new("NoiseTerm",F,F,
                                                                    response, "~", tags$span(tags$span("Binomial", class="formulaDistribution"),
                                                                                             "(",
                                                                                             tags$span(HTML("N"), class="formulaParameter"),
                                                                                             ",",
                                                                                             tags$span(HTML("p"), class="formulaParameter"),
                                                                                             ")"),
                                                                    latex=toLatex(response,"~","\\text{Binomial}(N , p)"),
                                                                    description="The response variable, that will be inferred with a binoimal noise term.")
                                    
                                    element <- self$get_para_order("N")[[1]]
                                    right_side <- HTML(element$distribution$getFormula(index=element$id))
                                    left_side <- tags$span(element$getFullDisplayName(), class="formulaParameter")
                                    equalTilde <- element$distribution$getEqualOrTilde()
                                    res[[2]] <- FormulaElements$new("GLMPredictor",T,F, 
                                                                    left_side,equalTilde,right_side,
                                                                    latex=toLatex(element$getFullDisplayNameLatex(),equalTilde,element$distribution$getFormulaLatex(index=element$id)),
                                                                    description=element$description,
                                                                    isRemovable=F, element)
                                    
                                    res[[3]] <- FormulaElements$new("PredictorLine",F,T,
                                                                    tags$span("logit( ",tags$span(HTML("p"), class="formulaParameter"), " )"), " = ", "",
                                                                    latex=toLatex("\\text{logit}(p)","=",""),
                                                                    description=paste0(HTML("p")," includes all independent variables, which should explain the response variable. The link function logit is used to transform the values of p to a range from -Inf to Inf."))
                                    parameterLineElements <- self$get_para_order(c("Intercept", "RegCoef"))
                                    index  <- 4
                                    #element: ModelParameter
                                    for(element in parameterLineElements){
                                      right_side <- HTML(element$distribution$getFormula(index=element$id))
                                      left_side <- tags$span(element$getFullDisplayName(), class="formulaParameter")
                                      equalTilde <- element$distribution$getEqualOrTilde()
                                      parameter_Formula <- FormulaElements$new("GLMPredictor",T,F, 
                                                                               left_side,equalTilde,right_side,
                                                                               latex=toLatex(element$getFullDisplayNameLatex(),equalTilde,element$distribution$getFormulaLatex(index=element$id)),
                                                                               description=element$description,
                                                                               isRemovable=T, element)
                                      res[[index]] <- parameter_Formula
                                      index <- index+1
                                    }
                                    
                                    return(res)
                                  },
                                  
                                  #Returns additional variables (e.g. N of binomial dist)
                                  #with theoretical limits
                                  get_used_vars_additional = function(){
                                    binom_N <- self$getPrior(type="N")
                                    if(!is.numeric(binom_N)){
                                      df <- data.frame(var=binom_N, lower=1, upper=Inf)
                                      return(df)
                                    } 
                                    return(NULL)
                                  },
                                  
                                  #Return false if the stan model should not be run (no predictor (also no intercept) selected)
                                  #Return true otherwise
                                  isStanModelRunnable = function(){
                                    if(length(self$used_predictor) > 0){
                                      return(T)
                                    }else{
                                      return(F)
                                    }
                                  },
                                  
                                  #Return a list (see getTermCombinationsOfPredictorListOfList())
                                  #of each predictor and also the other parameters of the model will be returned, too.
                                  getTermCombinationsOfAllModelElements = function(withoutAux=F){
                                    ret <- list()
                                    for(pred in self$used_predictor){
                                      if(pred$name == "Intercept"){
                                        ret <- list.append(ret, NA, "(Intercept)")
                                      }else if(!pred$get.is.vector()){
                                        name <- paste0(pred$userVariable,collapse = ":")
                                        ret <- list.append(ret, NA, name)
                                      }else{
                                        name <- paste0(pred$userVariable,collapse = ":")
                                        ret <- list.append(ret, self$getTermCombinationsOfPredictorListOfList(pred), name)
                                      }
                                    }
                                    # ret <- list.append(ret,list(NA),"N")
                                    return(ret)
                                  },
                                  
                                  getExplanation = function(response){
                                    ret <- GLMBinomialHelpDescription(response)
                                    return(ret)
                                  },
                                  
                                  
                                  #Get priors
                                  getPrior = function(type = c("intercept","N","regCoef"), element_name = NULL){
                                    if(type == "N"){
                                      p <- self$get_parameter_of_type("N")[[1]]
                                      if(p$distribution$auxParameter[[1]]$value=="?"){
                                        return(NULL)
                                      }else{
                                        return(p$distribution$auxParameter[[1]]$value)
                                      }
                                    }else if(type == "intercept"){
                                      p <- self$get_predictor_of_type("Intercept")[[1]]
                                      if(is.null(p)){
                                        return(NULL)
                                      }else{
                                        d <- p$modelParameter$distribution
                                        return(d$getPrior())
                                      }
                                    }else if(type == "regCoef"){
                                      prior_list <- list()
                                      pred <- self$get_predictor_of_type("Predictor")
                                      for(p in pred){
                                        para <- p$modelParameter
                                        if(para$is.vector){
                                          for(d in para$distributions){
                                            if(is.null(d$element_name)) stop("Still null...") 
                                            if(d$element_name %in% element_name){
                                              index <- match(d$element_name, element_name)
                                              prior_list[[index]] <- d$getPrior()
                                            }
                                          }
                                        }else{
                                          d <- para$distribution
                                          if(is.null(d$element_name)) stop("Still null...") 
                                          # e_n <- c("A","B")
                                          # each_permut <- permutations(2,2,e_n)
                                          # s <- sapply(1:length(each_permut[,1]),function(i){paste0(each_permut[i,],collapse = ":")})
                                          # If there is an interaction between two or more non vector variables, d$element_name contains
                                          # each variable. Iterate through all permutations of (var=c(A,B,C)) A:B:C, A:C:B, ..., to get the
                                          # right one.
                                          each_permut <- permutations(length(d$element_name), length(d$element_name),d$element_name)
                                          s <- sapply(1:length(each_permut[,1]),function(i){paste0(each_permut[i,],collapse = ":")})
                                          if(any(s %in% element_name)){
                                            tmp <- s[s %in% element_name]
                                            index <- match(tmp, element_name)
                                            prior_list[[index]] <- d$getPrior()
                                          }
                                        }
                                      }
                                      #cast list to single distribution with vector of distribution parameters
                                      if(length(prior_list)==0){
                                        return(NULL)
                                      }else if(length(prior_list)==1){
                                        return(prior_list[[1]])
                                      }else {
                                        first <- prior_list[[1]]
                                        ret <- list(dist = first$dist, df = first$df, location = first$location, scale = first$scale, 
                                                    autoscale = first$autoscale)
                                        for(e_i in 2:length(prior_list)){
                                          e <- prior_list[[e_i]]
                                          if(!is.na(e$df)){
                                            ret$df <- c(ret$df,e$df)
                                          }
                                          if(!is.na(e$location)){
                                            ret$location <- c(ret$location,e$location)
                                          }
                                          if(!is.na(e$scale)){
                                            ret$scale <- c(ret$scale,e$scale)
                                          }
                                        }
                                        return(ret)
                                      }
                                    }else{
                                      stop("Wrong type selected")
                                    }
                                  },
                                  

                                  get_content_for_stan_code = function(response, data, stanParameter){
                                    #Is model applicable?
                                    d <- data[[response]]
                                    if(all(round(d)!=d)){
                                      showNotification("Your response has to be discrete!", type="error", duration=15)
                                      return(NULL)
                                    }else if(min(d) < 0 ){
                                      showNotification("Your response has to be 0 or greater!", type="error", duration=15)
                                      return(NULL)
                                    }
                                    
                                    binom_N <- self$getPrior(type="N")
                                    
                                    #If binom_N is not setted and still contains "?"
                                    if(is.null(binom_N)){
                                      showNotification("Please set the number of trials 'N' for the binomial distriubtion. It should be a single number or one of your variables that contains the number of trials per data point.", type="error", duration=15)
                                      return(NULL)
                                    }
                                    
                                    
                                    t <- self$r_formula()
                                    t <- paste0("cbind(",response,",",binom_N,"-",response,")",substr(t,nchar(response)+1,nchar(t)))
                                    
                                    formula <- terms(formula(t), allowDotAsName=T)
                                    
                                    
                                    #get priors
                                    usedVars <- self$get_used_vars(extras=T, response=T)
                                    if(!is.numeric(binom_N)) usedVars <- c(usedVars, binom_N)
                                    order_of_elements <- colnames(as.data.frame(model.matrix(formula, self$myDataModel$getDataModelInputData()$getLongFormatVariable(usedVars, completeCases=T))) %>% 
                                                                    select_if(~ !is.numeric(.) || sum(.) != 0))
                                    if(order_of_elements[1] == "(Intercept)") order_of_elements <- order_of_elements[-1]
                                    
                                    
                                    prior <- self$getPrior(type="regCoef",order_of_elements)
                                    prior_intercept <- self$getPrior(type="intercept")
                                    
                                    
                                    private$weights <- rep(NA, length(data[[response]]))
                                    if(is.numeric(binom_N)){
                                      private$weights <- rep(binom_N, length(data[,1]))
                                    }else{
                                      private$weights <- data[[binom_N]]
                                    }
                                    
                                    if(max(data[[response]]/private$weights) > 1 || min(data[[response]]/private$weights) < 0){
                                      showNotification("Please revise the number of trials 'N' for the binomial distriubtion. The number of trials has to be greater than the outcomes.", type="error", duration=15)
                                      return(NULL)
                                    }
                                    
                                    env <- new.env(parent = .GlobalEnv)
                                    family <- binomial(link="logit")
                                    
                                    content <- list(formula=deparse1(formula), 
                                                    prior=prior,
                                                    prior_intercept=prior_intercept,
                                                    prior_aux=normal(0,10), #dummy, not directly used. But nec. for prior predictives...
                                                    response=response,
                                                    binom_N=binom_N,
                                                    data=data,
                                                    family = family,
                                                    stanParameter=stanParameter)
                                    return(content)
                                  },
                                  
                                  postprocessing = function(stan_result, content){
                                    result_list <- stan_result
                                    used_vars <- c()
                                    for(i in self$used_predictor){
                                      used_vars <- c(used_vars, i$userVariable)
                                    }
                                    used_vars <- c(content$response, unique(used_vars))
                                    if(!is.numeric(content$binom_N) && !content$binom_N %in% used_vars){
                                      used_vars <- c(content$binom_N, unique(used_vars))
                                    }
                                    
                                    return(list(stanfit=result_list, used_vars=used_vars))
                                  },
                                  
                                  
                                  effects_description = function(type=c("slope","group")){
                                    if(type=="group"){
                                      return(paste0(
                                        "Understanding effects: For the binomial model with a logit link function, ",
                                        "the effects are given as ", tags$a("log odds ratios.", href="https://en.wikipedia.org/wiki/Odds_ratio",target="_blank", rel="noopener noreferrer"),
                                        " Use the 'Predictions ...' to get an idea of what this effect does according to the response.",
                                        " A <b>group effect</b> is defined as the difference of two (inner) group predictors."))
                                    }else if(type=="slope"){
                                      return(paste0(
                                        "Understanding effects: For the binomial model with a logit link function, ",
                                        "the effects are given as ", tags$a("log odds ratios.", href="https://en.wikipedia.org/wiki/Odds_ratio",target="_blank", rel="noopener noreferrer"),
                                        " Use the 'Predictions ...' to get an idea of what this effect does according to the response.",
                                        " A <b>slope effect</b> is either the size of the slope or the difference of two slopes."))
                                    }
                                  },
                                  effects_description_latex = function(type=c("slope","group")){
                                    if(type=="group"){
                                      return(paste0(
                                        "Understanding effects: For the binomial model with a logit link function, ",
                                        "the effects are given as log odds ratios.",
                                        " A group effect is defined as the difference of two (inner) group predictors."))
                                    }else if(type=="slope"){
                                      return(paste0(
                                        "Understanding effects: For the binomial model with a logit link function, ",
                                        "the effects are given as log odds ratios.",
                                        " A slope effect is either the size of the slope or the difference of two slopes."))
                                    }
                                  },
                                  
                                  
                                  get_density = function(stanObject, data, mean=F){
                                    epred <- self$make_predictions(stanObject, data, mean=T)
                                    binom_N <- self$getPrior(type="N")
                                    
                                    #If N is not given as a single numeric, but as a user variable
                                    if(!is.numeric(binom_N)){
                                      binom_N <- data[[binom_N]]
                                    }

                                    if(mean){
                                      #epred is in the first place the mean of the probability (p)
                                      epred <- epred*binom_N
                                      
                                      dens <- density(epred)
                                      dist <- CDF(dens)
                                      pp <- data.frame(x=dens$x, density=dens$y, cum_dist=dist(dens$x))
                                      return(pp)
                                    }else{
                                      pp <- prediction_dens_binom(N_binom=binom_N, p=epred)
                                      return(pp)
                                    }
                                  },
                                  
                                  get_overlap = function(stanObject, data1, data2, mean=F){
                                    
                                    binom_N <- self$getPrior(type="N")
                                    
                                    #If N (number of trials) is not given as a single numeric, but as a user variable
                                    if(!is.numeric(binom_N)){
                                      N1 <- data1[[binom_N]]
                                      N2 <- data2[[binom_N]]
                                    }else{
                                      N1 <- N2 <- binom_N
                                    }
                                    
                                    p1 <- rstanarm::posterior_epred(stanObject, newdata=data1)[,1]
                                    p2 <- rstanarm::posterior_epred(stanObject, newdata=data2)[,1]
                                    if(mean){
                                      epred <- N2*p2-N1*p1
                                      return(epred)
                                    }else{
                                      return(full_dens_diff_binom(p1, p2, N1, N2))
                                    }
                                  }

                                ),
                                
                              private = list(
                                weights = NULL
                              )
)
