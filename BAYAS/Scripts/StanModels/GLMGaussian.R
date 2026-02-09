# Extend the list of characterstic with 'VAR' for variable/changeable
cEnum <- characteristicEnum()
dEnum <- distributionEnum()
cEnum$VAR <- 'VAR'

## Declare concrete stan model classes
# GLM with gaussian distribution
GLMGaussianStanModel <- R6Class(
  classname = "GLMGaussianStanModel",
  inherit = BAYSISAbstractModel,
  
  public = list(
    
    id = 1,
    brmsClass = F,
    display_name = 'Normal distribution (GLM)',
    description = paste0('The normal distribution is widely used for various kinds of data, especially for unbounded continuously data.',
                         '\nYour response variable should be continuous and have a lower limit of -INF and an upper limit of INF. Please make sure that your (theoretical) limits makes sense.'),
    is.discrete = F,
    
    is.gaussian = T,

    # response is a must have
    modelProperties = list(
      must = list(response = ModelProp$new("response", cEnum$Continuous, "-INF", "INF", 1, NA)),
      opt = list(predictor = ModelProp$new("predictor", NA, NA, NA, NA, NA))
    ),
                                      
    

    init_para_pred = function(){
      self$avail_parameter = list(
        Intercept = ModelParameter$new("Intercept", 0, NULL, "b","The intercept of the GLM.", F, T, "GLMPredictor",
                                       cNum$Continuous, "VAR", "VAR",
                                       1,
                                       FactoryDistribution(name="RegCoefDist",dist=dEnum$Normal, adjustable=T, is.vector=F),
                                       c(dEnum$Normal,dEnum$StudentT,dEnum$Cauchy)),
        Sigma = ModelParameter$new("Sigma", -1, -1, "&sigma;","The variance of the normal noise distribution.", F, F, "AuxParameter",
                                   cNum$Continuous, ">0", "VAR",
                                   1,
                                   FactoryDistribution(name="SigmaNormalGLM",dist=dEnum$Exponential, adjustable=T, is.vector=F),
                                   c(dEnum$Exponential,dEnum$HalfNormal,dEnum$HalfCauchy,dEnum$HalfStudentT)),
        #Reg coef, not selectable for user
        RegCoef = ModelParameter$new("RegCoef", 1, NULL, "b","Regression coefficient / slope of the GLM.", F, F, "GLMPredictor",
                                     cNum$Continuous, "VAR", "VAR",
                                     NULL, 
                                     FactoryDistribution(name="RegCoefDist",dist=dEnum$Normal, adjustable=T, is.vector=F),
                                     c(dEnum$Normal,dEnum$StudentT,dEnum$Cauchy,dEnum$Horseshoe))
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
      self$add_parameter(self$avail_parameter$Sigma)
    },


    
    # Returns the interactive Formula for selecting predictors etc.
    new_formula = function(response){
      res <- list()
      
      ## FormulaElements
      # type, addPriorButton, predictorLine, 
      # leftSide, rightSide,
      # removable = F, myRemovableParamter = NULL
      res[[1]] <- FormulaElements$new("NoiseTerm",F,F,
                                       response, "~", tags$span(tags$span("Normal", class="formulaDistribution"),
                                                                                    "(",
                                                                                    tags$span(HTML("&mu;"), class="formulaParameter"),
                                                                                    ",",
                                                                                    tags$span(HTML("&sigma;"), class="formulaParameter"),
                                                                                    ")"),
                                      latex=toLatex(response,"~","\\text{Normal}(\\mu, \\sigma)"),
                                      description="The response variable, that will be inferred with a normal (Gaussian) noise term.")
      
      res[[2]] <- FormulaElements$new("PredictorLine",F,T,
                                       tags$span(HTML("&mu;"), class="formulaParameter"), " = ", "",
                                      latex=toLatex("\\mu","=",""),
                                      description=paste0(HTML("&mu;")," includes all independent variables, which should explain the response variable."))
      parameterLineElements <- self$get_para_order(c("Intercept", "RegCoef","Sigma"))
      index  <- 3
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
      if(!withoutAux) ret <- list.append(ret,list(NA),"sigma")
      return(ret)
    },
    

    #Get priors
    getPrior = function(type = c("intercept","aux","regCoef"), element_name = NULL){
      
      if(type == "aux"){
        p <- self$get_parameter_of_type("Sigma")[[1]]
        if(is.null(p)){
          return(NULL)
        }else{
          return(p$distribution$getPrior())
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
              if(localUse && is.null(d$element_name)) stop("Still null...") 
              if(d$element_name %in% element_name){
                index <- match(d$element_name, element_name)
                prior_list[[index]] <- d$getPrior()
              }
            }
          }else{
            d <- para$distribution
            if(localUse && is.null(d$element_name)) stop("Still null...")
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
        return(self$getPriorAsSingleVector(prior_list))
      }else{
        stop("Wrong type selected")
      }
    },
    
    getExplanation = function(response){
      ret <- GLMGaussianHelpDescription(response)
      return(ret)
    },
    

    get_content_for_stan_code = function(response, data, stanParameter){
      formula <- terms(formula(self$r_formula()), allowDotAsName=T)
      
      #get priors
      usedVars <- self$get_used_vars(extras=T, response=T)
      order_of_elements <- colnames(as.data.frame(model.matrix(formula, self$myPerIterationDataModel$getDataModelInputData()$getLongFormatVariable(usedVars, completeCases=T))) %>% 
                                      select_if(~ !is.numeric(.) || (length(unique(.)) != 1 || sum(.)!= 0)))
      if(order_of_elements[1] == "(Intercept)") order_of_elements <- order_of_elements[-1]
      
      prior <- self$getPrior(type="regCoef",order_of_elements)
      prior_intercept <- self$getPrior(type="intercept")
      prior_aux <- self$getPrior(type="aux")
      
      #https://discourse.mc-stan.org/t/stanfit-object-fit-inside-function-explodes-in-size-when-saved-to-rds/13656
      env <- new.env(parent = .GlobalEnv)
      family <- with(env, gaussian())
      
      content <- list(formula=deparse1(formula), 
                      prior=prior,
                      prior_intercept=prior_intercept,
                      prior_aux=prior_aux,
                      response=response,
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
      
      return(list(stanfit=result_list, used_vars=used_vars))
    },
    
    is_transformable = function(){
      return(F)
    },
    
    
    effects_description = function(type=c("slope","group")){
      if(type=="group"){
        return(paste0(
        "Understanding effects: For the normal model (identity link function) ",
        "a unit change of the predictor is identical to the change on the response. ",
        "A <b>group effect</b> is defined as the difference of the (inner) group predictors."))
      }else if(type=="slope"){
        return(paste0(
          "Understanding effects: For the normal model (identity link function) ",
          "a unit change of the predictor is identical to the change on the response. ",
          "A <b>slope effect</b> is either the size of the slope or the difference of two slopes."))
      }
    },
    effects_description_latex = function(type=c("slope","group")){
      if(type=="group"){
        return(paste0(
          "Understanding effects: For the normal model (identity link function) ",
          "a unit change of the predictor is identical to the change on the response. ",
          "A group effect is defined as the difference of the (inner) group predictors."))
      }else if(type=="slope"){
        return(paste0(
          "Understanding effects: For the normal model (identity link function) ",
          "a unit change of the predictor is identical to the change on the response. ",
          "A slope effect is either the size of the slope or the difference of two slopes."))
      }
    },
  
    get_density = function(stanObject, data, mean=F){
      epred <- self$make_predictions(stanObject, data, mean=T)

      if(mean){
        dens <- density(epred)
        dist <- CDF(dens)
        pp <- data.frame(x=dens$x, density=dens$y, cum_dist=dist(dens$x))
        return(pp)
      }else{
        post <- as.array(stanObject)
        sigma <- c(post[,,"sigma"])
        pp <- prediction_dens_norm(epred, sigma)
        return(pp)
      }
    },
    
    get_overlap = function(stanObject, data1, data2, mean=F){
      mu1 <- rstanarm::posterior_epred(stanObject, newdata=data1)[,1]
      mu2 <- rstanarm::posterior_epred(stanObject, newdata=data2)[,1]
      if(mean){
        epred <- mu2-mu1
        return(epred)
      }else{
        post <- as.array(stanObject)
        sigma <- c(post[,,"sigma"])
        return(full_dens_diff_norm(mu1,mu2,sigma))
      }
    }
    
  )
                                
)
