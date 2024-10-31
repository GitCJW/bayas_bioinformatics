greaterZero <- .Machine$double.xmin
planningDistributionEnum <- function(type=c("Continuous","Discrete","all")){
  
  if(type=="Continuous"){
    list(Uniform = "Uniform", Normal = "Normal", Lognormal = "Lognormal", Exponential = "Exponential", 
         Gamma = "Gamma", StudentT = "StudentT", Beta = "Beta", Cauchy = "Cauchy",
         HalfStudentT = "HalfStudentT", HalfNormal = "HalfNormal", HalfCauchy = "HalfCauchy")
  }else if(type=="Discrete"){
    list(Binomial = "Binomial", Poisson = "Poisson", NegBinom = "NegBinom")
  }else if(type=="all"){
    list(Uniform = "Uniform", Normal = "Normal", Lognormal = "Lognormal", Exponential = "Exponential", Binomial = "Binomial", 
         Gamma = "Gamma", StudentT = "StudentT", Beta = "Beta", Cauchy = "Cauchy",
         HalfStudentT = "HalfStudentT", HalfNormal = "HalfNormal", HalfCauchy = "HalfCauchy",
         Poisson = "Poisson", NegBinom = "NegBinom")
  }else{
    stop(paste0("Unknown type: ", type))
  }
}



getAuxParameterOfDistribution <- function(distEnum){
  dEnum <- planningDistributionEnum("all")
  
  if(distEnum == dEnum$Normal){
    mu <- DistributionParameter$new(name="mu", display_name="mu", description="", value=0, default_val=0, min_val=-Inf, max_val=Inf, discrete=F)
    sigma <- DistributionParameter$new(name="sigma", display_name="sigma", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    mu$tmp_value <- c(-2,5,0)
    sigma$tmp_value <- c(0.5,1,5)
    return(list(mu,sigma))
  }else if(distEnum == dEnum$Uniform){
    min <- DistributionParameter$new(name="min", display_name="min", description="", value=0, default_val=0, min_val=-Inf, max_val=Inf, discrete=F)
    max <- DistributionParameter$new(name="max", display_name="max", description="", value=1, default_val=1, min_val=-Inf, max_val=Inf, discrete=F)
    min$tmp_value <- c(-2.5,0,6)
    max$tmp_value <- c(-1.2,10,12.5)
    return(list(min,max))
  }else if(distEnum == dEnum$Lognormal){
    mu <- DistributionParameter$new(name="mu", display_name="mu", description="", value=0, default_val=0, min_val=-Inf, max_val=Inf, discrete=F)
    sigma <- DistributionParameter$new(name="sigma", display_name="sigma", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    mu$tmp_value <- c(-2,5,0)
    sigma$tmp_value <- c(1,5,0.5)
    return(list(mu,sigma))
  }else if(distEnum == dEnum$Exponential){
    lambda <- DistributionParameter$new(name="lambda", display_name="lambda", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    lambda$tmp_value <- c(1,5.5)
    return(list(lambda))
  }else if(distEnum == dEnum$Gamma){
    shape <- DistributionParameter$new(name="shape", display_name="shape", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    rate <- DistributionParameter$new(name="rate", display_name="rate", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    shape$tmp_value <- c(0.1,1,5.5)
    rate$tmp_value <- c(0.1,1,5.5)
    return(list(shape,rate))
  }else if(distEnum == dEnum$StudentT){
    df <- DistributionParameter$new(name="df", display_name="Degrees of freedom", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    ncp <- DistributionParameter$new(name="ncp", display_name="Non-centrality", description="", value=0, default_val=0, min_val=0, max_val=Inf, discrete=F)
    df$tmp_value <- c(0.1,1,5)
    ncp$tmp_value <- c(0,1,5.5)
    return(list(df,ncp))
  }else if(distEnum == dEnum$Beta){
    alpha <- DistributionParameter$new(name="alpha", display_name="alpha", description="", value=2, default_val=2, min_val=greaterZero, max_val=Inf, discrete=F)
    beta <- DistributionParameter$new(name="beta", display_name="beta", description="", value=2, default_val=2, min_val=greaterZero, max_val=Inf, discrete=F)
    alpha$tmp_value <- c(0.1,2,5.5)
    beta$tmp_value <- c(0.1,2,5.5)
    return(list(alpha,beta))
  }else if(distEnum == dEnum$Cauchy){
    location <- DistributionParameter$new(name="location", display_name="location", description="", value=0, default_val=0, min_val=-Inf, max_val=Inf, discrete=F)
    scale <- DistributionParameter$new(name="scale", display_name="scale", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    location$tmp_value <- c(-2,5,0)
    scale$tmp_value <- c(1,0.5,5.5)
    return(list(location,scale))
  }else if(distEnum == dEnum$HalfStudentT){
    df <- DistributionParameter$new(name="df", display_name="Degrees of freedom", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    ncp <- DistributionParameter$new(name="ncp", display_name="Non-centrality", description="", value=0, default_val=0, min_val=0, max_val=Inf, discrete=F)
    df$tmp_value <- c(0.1,1,5)
    ncp$tmp_value <- c(0,1,5.5)
    return(list(df,ncp))
  }else if(distEnum == dEnum$HalfNormal){
    sigma <- DistributionParameter$new(name="sigma", display_name="sigma", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    sigma$tmp_value <- c(0.5,1,5)
    return(list(sigma))
  }else if(distEnum == dEnum$HalfCauchy){
    scale <- DistributionParameter$new(name="scale", display_name="scale", description="", value=1, default_val=1, min_val=greaterZero, max_val=Inf, discrete=F)
    scale$tmp_value <- c(0.5,1,5)
    return(list(scale))
  }else if(distEnum == dEnum$Binomial){
    size <- DistributionParameter$new(name="size", display_name="Size", description="", value=0, default_val=0, min_val=0, max_val=Inf, discrete=T)
    prob <- DistributionParameter$new(name="prob", display_name="Probability", description="", value=0.5, default_val=0.5, min_val=0, max_val=1, discrete=F)
    size$tmp_value <- c(1,10,50)
    prob$tmp_value <- c(0,0.5,1)
    return(list(size,prob))
  }else if(distEnum == dEnum$Poisson){
    lambda <- DistributionParameter$new(name="lambda", display_name="lambda", description="", value=0, default_val=0, min_val=greaterZero, max_val=Inf, discrete=F)
    lambda$tmp_value <- c(1,10,50)
    return(list(lambda))
  }else if(distEnum == dEnum$NegBinom){
    size <- DistributionParameter$new(name="size", display_name="Size", description="", value=10, default_val=10, min_val=0, max_val=Inf, discrete=T)
    prob <- DistributionParameter$new(name="prob", display_name="Probability", description="", value=0.5, default_val=0.5, min_val=0, max_val=1, discrete=F)
    size$tmp_value <- c(1,10,50)
    prob$tmp_value <- c(0,0.5,1)
    return(list(size,prob))
  }else{
    stop(paste0("unknown dist: ", distEnum))
  }
  
}

generateData <- function(distEnum, auxPara, n){
  if(is.null(distEnum) || distEnum == "") return()
  dEnum <- planningDistributionEnum("all")
  
  #Verfiy auxPara values of equal length or of length 1 
  #Verify if given values are out of their limits. E.g. no negative values for sigma of Normal distribution
  ret <- ""
  notOk <- c()
  varLengths <- c()
  for(i in auxPara){
    if(any(i$value < i$min_val || i$value > i$max_val) || (i$discrete && any(round(i$value)!=i$value))){
      notOk <- c(notOk,i$name)
    }
    
    varLengths <- c(varLengths,length(i$value))
  }
  if(length(notOk) > 0){
    ret <- paste0("<p>Some values (<b>", paste(notOk,collapse=", ") ,"</b>) are not acceptable.</p>")
  }
  varLengths <- varLengths[varLengths > 1]
  if(length(unique(varLengths))> 1){
    ret <- paste0(ret, "<p>Parameters should be single valued or of equal length.</p>")
  }
  if(ret != "")return(ret)
  
  #Generate data of given distribution
  if(distEnum == dEnum$Normal){
    return(rnorm(n,getAuxParamterOutOfList(auxPara, "mu")$value, getAuxParamterOutOfList(auxPara, "sigma")$value))
  }else if(distEnum == dEnum$Uniform){
    return(runif(n,getAuxParamterOutOfList(auxPara, "min")$value, getAuxParamterOutOfList(auxPara, "max")$value))
  }else if(distEnum == dEnum$Lognormal){
    return(rlnorm(n,getAuxParamterOutOfList(auxPara, "mu")$value, getAuxParamterOutOfList(auxPara, "sigma")$value))
  }else if(distEnum == dEnum$Exponential){
    return(rexp(n,getAuxParamterOutOfList(auxPara, "lambda")$value))
  }else if(distEnum == dEnum$Gamma){
    return(rgamma(n,getAuxParamterOutOfList(auxPara, "shape")$value, getAuxParamterOutOfList(auxPara, "rate")$value))
  }else if(distEnum == dEnum$StudentT){
    return(rt(n,getAuxParamterOutOfList(auxPara, "df")$value, getAuxParamterOutOfList(auxPara, "ncp")$value))
  }else if(distEnum == dEnum$Beta){
    return(rbeta(n,getAuxParamterOutOfList(auxPara, "alpha")$value, getAuxParamterOutOfList(auxPara, "beta")$value))
  }else if(distEnum == dEnum$Cauchy){
    return(rcauchy(n,getAuxParamterOutOfList(auxPara, "location")$value, getAuxParamterOutOfList(auxPara, "scale")$value))
  }else if(distEnum == dEnum$HalfStudentT){
    return(rhalft(n,getAuxParamterOutOfList(auxPara, "ncp")$value, getAuxParamterOutOfList(auxPara, "df")$value))
  }else if(distEnum == dEnum$HalfNormal){
    return(rhalfnorm(n,getAuxParamterOutOfList(auxPara, "sigma")$value))
  }else if(distEnum == dEnum$HalfCauchy){
    return(rhalfcauchy(n,getAuxParamterOutOfList(auxPara, "scale")$value))
  }else if(distEnum == dEnum$Binomial){
    return(rbinom(n,getAuxParamterOutOfList(auxPara, "size")$value, getAuxParamterOutOfList(auxPara, "prob")$value))
  }else if(distEnum == dEnum$Poisson){
    return(rpois(n,getAuxParamterOutOfList(auxPara, "lambda")$value))
  }else if(distEnum == dEnum$NegBinom){
    return(rnbinom(n,getAuxParamterOutOfList(auxPara, "size")$value, getAuxParamterOutOfList(auxPara, "prob")$value))
  }else{
    stop(paste0("unknown dist: ", distEnum))
  }
}


plotDistriubtion <- function(data, typePlot, axisName){
  dat <- data.frame(axisName=data, index=1:length(data))
  index <- ifelse(axisName=="index","index_index","index")
  colnames(dat) <- c(axisName,index)
  
  if(typePlot=="points"){
    gg <- ggplot(dat) + geom_point(aes_string(x=index,y=axisName))
  }else if(typePlot=="dens"){
    suppressWarnings(gg <- ggplot(dat,aes_string(x=axisName)) + geom_histogram(aes(y=..density..)))
  }else{
    stop(paste0("Unknown plot type: ", typePlot))
  }
  return(gg)
}





getAuxParamterOutOfList <- function(l, name){
  for(i in l){
    if(i$name == name) return(i)
  }
}