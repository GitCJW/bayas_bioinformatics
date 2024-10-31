
b0 <- tags$span(HTML(paste0("b",tags$sub("0"))), class="formulaParameter")
bn <- tags$span(HTML(paste0("b",tags$sub("1..n"))), class="formulaParameter")
bx <- tags$span(HTML(paste0("b",tags$sub("x"))), class="formulaParameter")
b0_n <- tags$span(HTML(paste0("b",tags$sub("0..n"))), class="formulaParameter")
sigma <- tags$span(HTML("&sigma;"), class="formulaParameter")
mu <- tags$span(HTML("&mu;"), class="formulaParameter")
N <- tags$span(HTML("N"), class="formulaParameter")
p <- tags$span(HTML("p"), class="formulaParameter")
lambda <- tags$span(HTML("&lambda;"), class="formulaParameter")
theta <- tags$span(HTML("&theta;"), class="formulaParameter")
phi <- tags$span(HTML("&phi;"), class="formulaParameter")
Phi <- tags$span(HTML("&Phi;"), class="formulaParameter")


normal <- tags$span("Normal", class="formulaDistribution")
binomial <- tags$span("Binomial", class="formulaDistribution")
poisson <- tags$span("Poisson", class="formulaDistribution")
negBinom <- tags$span("Neg-Binom", class="formulaDistribution")
exponential <- tags$span("Exponential", class="formulaDistribution")
distribution <- tags$span("Distribution", class="formulaDistribution")




################################################################################
################################## Checklist ###################################
################################################################################

hD_addPredictors <- function(){
  video <- tags$div(
    style="text-align:center;",
    tags$video(
      class="infoBox",
      src="Video/AddPredictor.mov", width="400", height="225", type='video/mov"', 
      autoplay="true", loop="true", muted="true"))
  
  ret <- list(short="", long="")
  ret$short <- paste0("Add predictors")
  ret$long <- paste0(video, "The response (datapoints) will be predicted by the sum of the independet variables (predictors). Each predictor has a parameter ", bn,
                     " that will be infered and has a prior distribution. Choose any combination of predictors you might think they will effect the response. 
                                                              You can also compare models with different combinations to get the 'best' result" )
  return(ret)
}

hD_keepRemoveIntercetp <- function(){
  video <- tags$div(style="text-align:center;",
                    tags$video(class="infoBox",
                               src="Video/RemoveAddIntercept.mov", width="400", height="225", type='video/mov"',
                               autoplay="true", loop="true", muted="true"))
  ret <- list(short="", long="")
  ret$short <- paste0("Keep or remove the intercept ", b0)
  ret$long <- paste0(video, "The intercept shifts the data to a certain level, when all other predictors are zero. 
                     If you are not sure whether to use one, try both models and compare them later.")
  return(ret)
}

hD_changePrior <- function(){
  video <- tags$div(style="text-align:center;",
                    tags$video(class="infoBox",
                               src="Video/ChangePrior.mov", width="400", height="225", type='video/mov"', 
                               autoplay="true", loop="true", muted="true"))
  ret <- list(short="", long="")
  ret$short <- paste0("(Optionally) change priors for ", bn,", ",sigma)
  ret$long <- paste0(video,"The prior is the preknowledge of a particular parameter. You can change the distribution and also the distribution parameters. Be aware of using very informative priors, unless you are sure about them.
                     If you are wondering of arbritary distribution parameters, they are coming from a parameter adjustment, to obtain weakly informative priors for your specific data.")
  return(ret)
}

hD_setBinomialN <- function(){
  video <- tags$div(style="text-align:center;",
                    tags$video(class="infoBox",
                               src="Video/ChangeBinomN.mov", width="400", height="225", type='video/mov"', 
                               autoplay="true", loop="true", muted="true"))
  ret <- list(short="", long="")
  ret$short <- paste0("Set the number of trials ",N, "")
  ret$long <- paste0(video, N, " is the overall number of trials in a count experiment. If you counted the number of \"1\" in 10 dice rolls, ", N, " shoulb be equal 10.
                     You can assign either a number that will be true for each data point or one of your variables that holds the number of trials per data point. That variable must be discrete and has a lower limit of \">0\"")
  return(ret)
}


################################################################################
#################################### Legend ####################################
################################################################################

{
  legend_response <- function(response){
    ret <- list(name="", short="",long="")
    ret$name = paste0(response)
    ret$short = paste0("The response variable")
    ret$long = paste0("Also known as the outcome or dependent variable.")
    return(ret)
  }
  
  legend_response_log <- function(expression){
    ret <- list(name="", short="",long="")
    ret$name = paste0(expression)
    ret$short = paste0("Log scale of your response")
    ret$long = paste0("Your response will be transformed to the logarithmized values, so that the values of the log scales will follow the choosen distribution.")
    return(ret)
  }
  
  legend_mu <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(mu))
    ret$short = paste0("The expected value")
    ret$long = HTML(paste0("For this GLM the parameter ", mu, " is the expected value which is linked (with the link function) to the linear predictor that contains all independet predictors ", b0_n ,"."))
    return(ret)
  }
  
  legend_theta <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(theta))
    ret$short = paste0("The overdispersion")
    ret$long = paste0("For this GLM the parameter &theta; controls the reciprocal of the dispersion.")
    return(ret)
  }
  
  legend_b0 <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(b0))
    ret$short = paste0("The intercept of this model")
    ret$long = paste0("The intercept shifts the data to a certain level, when all of your other predictors are zero. An intercept is mostly useful.")
    return(ret)
  }
  
  legend_bn <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(bn))
    ret$short = paste0("Parameters that are factors of the predictors")
    ret$long = paste0("Each predictor has a parameter, or each element of a predictor vector has a parameter. 
                      These parameters are unknown and will inferred.")
    return(ret)
  }
  
  legend_bold <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(tags$b(bx)))
    ret$short = paste0("Vector of parameters")
    ret$long = paste0("Bolded parameters are vectors. Sometimes they could also be just one element vectors. Mostly vectors coming from categorical variables, where each element of the parameter belongs to one element of the categorical variable.")
    return(ret)
  }
  
  legend_sigma <- function(){
    ret <- list(name="", short="",long="")
    ret$name = name = HTML(paste0(sigma))
    ret$short = paste0("Standard deviation of the normal noise term.")
    ret$long = paste0("The parameter &sigma; is also unknwon and will inferred like the other parameters.
                      If your noise coming only from measurment uncertainty, then &sigma; will be the magnitude of this uncertainty.")
    return(ret)
  }
  
  legend_tilde <- function(){
    ret <- list(name="", short="",long="")
    ret$name = paste0("~")
    ret$short = paste0("Sampling statement")
    ret$long = paste0("The left side of the tilde is unknown and will infered by the distribution on the right side.")
    return(ret)
  }
  
  legend_equal <- function(){
    ret <- list(name="", short="",long="")
    ret$name = paste0("=")
    ret$short = paste0("Assignment")
    ret$long = paste0("If a = b+c, then a will take the sum of b+c.")
    return(ret)
  }
  
  legend_samplingStatementNormal <- function(response){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(response, " ~ ", normal, "()"))
    ret$short = paste0("Sampling statement of the response")
    ret$long = paste0("The response will be inferred by a normal distribution.")
    return(ret)
  }
  
  legend_samplingStatementBinomial <- function(response){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(response, " ~ ", binomial, "()"))
    ret$short = paste0("Sampling statement of the response")
    ret$long = paste0("The response will be inferred by a binomial distribution.")
    return(ret)
  }
  
  legend_samplingStatementPoisson <- function(response){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(response, " ~ ", negBinom, "()"))
    ret$short = paste0("Sampling statement of the response")
    ret$long = paste0("The response will be inferred by a begative binomial (poisson-gamma) distribution.")
    return(ret)
  }
  
  legend_samplingStatementNegBinom <- function(response){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(response, " ~ ", poisson, "()"))
    ret$short = paste0("Sampling statement of the response")
    ret$long = paste0("The response will be inferred by a poisson distribution.")
    return(ret)
  }
  
  legend_prior <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(b0_n,"/",sigma, " ~ ", distribution, "()"))
    ret$short = paste0("Prior definition")
    ret$long = paste0("In contrast to the sampling statement, the prior distribution and also the distribution parameters can be changed for each single parameter.
    The priors are used to bring some preknowledge into the model. Be aware of using very informative priors, unless you are sure about them.
                      If you are wondering of arbritary distribution parameters, they are coming from a parameter adjustment, to obtain weakly informative prior for your specific data.")
    return(ret)
  }
  
  legend_N <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(N))
    ret$short = paste0("Number of trials")
    ret$long = paste0(N, " is the overall number of trials in a count experiment. If you counted the number of \"1\" in 10 dice rolls, the ", N, " is equal 10.")
    return(ret)
  }
  
  legend_p <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(p))
    ret$short = paste0("Success probability for each trial")
    ret$long = paste0(p, " is the probability for a success. E.g. a probability of 0.5 and 10 trials would lead in the mean to 5 successes.",
                      "\nFor this GLM the parameter ", p, " contains all independent predictors and therefore the response depends on it.")
    return(ret)
  }
  
  legend_theta_benoulli <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(theta))
    ret$short = paste0("Success probability")
    ret$long = paste0(theta, " is the probability for a success (=1). E.g a fair coin has a probabilty of 0.5.")
    return(ret)
  }

  legend_lambda <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(lambda))
    ret$short = paste0("Poisson rate parameter")
    ret$long = paste0(lambda, " is the rate parameter of the poisson distribution. The rate parameter is equal the mean and variance.")
    return(ret)
  }
  
  legend_phi <- function(){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0(Phi))
    ret$short = paste0("Scale parameter")
    ret$long = paste0(Phi, " is a scale parameter of the beta distribution. It controls the variance with mu(1-mu)/(1+Phi).")
    return(ret)
  }
  
  legend_link_general <- function(){
    ret <- paste0("In general a link function is used to transform the expected value to the linear predictor for several reasons: \n",
           tags$ul(tags$li("To match the support of the used distribution"),
                   tags$li("To use the canonical form (canonical link function)"),
                   tags$li("To have other benefits")),
           "")
    return(ret)
  }
  
  #type "support","canonical","other"
  legend_identity <- function(var="", support=T, canonical=T){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0("id(",var,")"))
    ret$short = paste0("Link function (not shown in the above formula)")
    if(support){
      ret$long = paste0("There is no need for data transformation, since the support of the used distriubtion is (-Inf, Inf).\n")
      if(canonical) ret$long = paste0(ret$long, "Furthermore it is also in the canonical form without any transformation.")
    }else if(canonical){
      ret$long = paste0("The identity function does nothing, but is the canonical link function of this glm.")
    }else{
      ret$long = paste0("The identity function does nothing.")
    }
    ret$long <- paste0(legend_link_general(), ret$long)
    return(ret)
  }
  
  legend_logit <- function(var="", support=T, canonical=T){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0("logit(",var,")"))
    ret$short = paste0("Link function")
    ret$long = paste0("The link function transforms the linear predictor (-Inf,Inf) to the desired scale. For this model ", var," is a probability, therefore the scale is between 0 and 1.",
                      "\n The logit function transforms data from (-Inf,Inf) to [0,1].")
    
    if(support){
      ret$long = paste0("The logit function transforms the expected value (0, 1) to the linear predictor (-Inf,Inf).\n")
      if(canonical) ret$long = paste0(ret$long, "Furthermore it is also the canonical link function.")
    }else if(canonical){
      ret$long = paste0("The logit function is the canonical link that transforms the expected value (0,1) to (-Inf,Inf).")
    }else{
      ret$long = paste0("The logit function transforms the expected value (0,1) to (-Inf,Inf).")
    }
    ret$long <- paste0(legend_link_general(), ret$long)
    return(ret)
  }
  
  #type "support","canonical","other"
  legend_log <- function(var="", support=T, canonical=T){
    ret <- list(name="", short="",long="")
    ret$name = HTML(paste0("log(",var,")"))
    ret$short = paste0("Link function")
    if(support){
      ret$long = paste0("The log function transforms the expected value (0, Inf) to the linear predictor (-Inf,Inf).\n")
      if(canonical) ret$long = paste0(ret$long, "Furthermore it is also the canonical link function.")
    }else if(canonical){
      ret$long = paste0("The log function is the canonical link that transforms the expected value (0,Inf) to (-Inf,Inf).")
    }else{
      ret$long = paste0("The log function transforms the expected value (0,Inf) to (-Inf,Inf).")
    }
    ret$long <- paste0(legend_link_general(), ret$long)
    return(ret)
  }
  
  legend_invGaussLink <- function(var=""){
    ret <- list(name="", short="",long="")
    ret$name = tags$span(HTML(paste0(var,tags$sup("-2"))))
    ret$short = paste0("Link function")
    ret$long = paste0("This is the canonical link funcktion for the inverse gaussian. Unlike other link functions, this one does not map the linear predictor to the range of the domain (0,Inf).")
    return(ret)
  }

}




################################################################################
#################################### Models ####################################
################################################################################

GLMGaussianHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  

  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_identity(mu)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_sigma()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementNormal(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMLogGaussianHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_response_log(paste0("log(",response,")"))
  legend[[3]] <- legend_mu()
  legend[[4]] <- legend_identity(mu)
  legend[[5]] <- legend_b0()
  legend[[6]] <- legend_bn()
  legend[[7]] <- legend_bold()
  legend[[8]] <- legend_sigma()
  legend[[9]] <- legend_tilde()
  legend[[10]] <- legend_equal()
  legend[[11]] <- legend_samplingStatementNormal(response)
  legend[[12]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMBinomialHelpDescription <- function(response){
  
  checklistMust <- list()
  checklistMust[[1]] <- hD_setBinomialN()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_N()
  legend[[3]] <- legend_p()
  legend[[4]] <- legend_logit(p)
  legend[[5]] <- legend_b0()
  legend[[6]] <- legend_bn()
  legend[[7]] <- legend_bold()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementBinomial(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMPoissonHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_lambda()
  legend[[3]] <- legend_log(lambda)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_tilde()
  legend[[8]] <- legend_equal()
  legend[[9]] <- legend_samplingStatementPoisson(response)
  legend[[10]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMNegBinomHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_theta()
  legend[[4]] <- legend_log(mu, support=T, canonical=T)
  legend[[5]] <- legend_b0()
  legend[[6]] <- legend_bn()
  legend[[7]] <- legend_bold()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementPoisson(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMInvserseGaussianHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_log(mu, support=T, canonical=F)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_sigma()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementNormal(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMGammaHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_log(mu, support=T, canonical=F)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_sigma()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementNormal(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMExponentialHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_log(mu, support=T, canonical=F)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_tilde()
  legend[[8]] <- legend_equal()
  legend[[9]] <- legend_samplingStatementNormal(response)
  legend[[10]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMBernoulliHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_theta_benoulli()
  legend[[3]] <- legend_logit(theta)
  legend[[4]] <- legend_b0()
  legend[[5]] <- legend_bn()
  legend[[6]] <- legend_bold()
  legend[[7]] <- legend_tilde()
  legend[[8]] <- legend_equal()
  legend[[9]] <- legend_samplingStatementBinomial(response)
  legend[[10]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}

GLMBetaHelpDescription <- function(response){
  
  checklistMust <- list()
  
  checklistCan <- list()
  checklistCan[[1]] <- hD_addPredictors()
  checklistCan[[2]] <- hD_keepRemoveIntercetp()
  checklistCan[[3]] <- hD_changePrior()
  
  legend <- list()
  legend[[1]] <- legend_response(response)
  legend[[2]] <- legend_mu()
  legend[[3]] <- legend_logit(mu)
  legend[[4]] <- legend_phi()
  legend[[5]] <- legend_b0()
  legend[[6]] <- legend_bn()
  legend[[7]] <- legend_bold()
  legend[[8]] <- legend_tilde()
  legend[[9]] <- legend_equal()
  legend[[10]] <- legend_samplingStatementBinomial(response)
  legend[[11]] <- legend_prior()
  
  return(list(checklistMust=checklistMust,
              checklistCan=checklistCan,
              legend=legend))
}