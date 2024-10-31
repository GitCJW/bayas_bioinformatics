planningLinkEnum <- function(){
  l <- list(cauchit="cauchit", cloglog="cloglog", identity="identity",
            inv_square="inv_square", inverse="inverse",log="log",
            logit="logit",probit="probit", sqrt="sqrt", tmp="tmp")
  return(l)
}

distEnum <- planningDistribtionsEnum("response")
planningLinkFunctionMapping <- function(dist=c(distEnum$Normal, distEnum$Exponential,
                                               distEnum$Log_Normal, distEnum$Gamma, distEnum$Inverse_Gaussian,
                                               distEnum$Beta,
                                               distEnum$Poisson, distEnum$Negative_Binomial, 
                                               distEnum$Binomial, distEnum$Bernoulli, distEnum$Beta_Binomial)){
  match.arg(dist)
  
  linkEnum <- planningLinkEnum()
  
  if(is.null(dist)) return(NULL)
  
  if(dist %in% c(distEnum$Normal)){
    return(c(linkEnum$identity, linkEnum$log, linkEnum$inverse))
  }else if(dist %in% c(distEnum$Log_Normal)){
    return(c(linkEnum$identity, linkEnum$inverse))
  }else if(dist %in% c(distEnum$Binomial, distEnum$Bernoulli)){
    return(c(linkEnum$logit, linkEnum$probit, linkEnum$cauchit, linkEnum$log, linkEnum$cloglog))
  }else if(dist %in% c(distEnum$Beta_Binomial)){
    return(c(linkEnum$logit, linkEnum$probit, linkEnum$cauchit, linkEnum$cloglog))
  }else if(dist %in% c(distEnum$Gamma,distEnum$Exponential)){
    return(c(linkEnum$log, linkEnum$identity, linkEnum$inverse))
  }else if(dist %in% c(distEnum$Poisson)){
    return(c(linkEnum$log, linkEnum$identity, linkEnum$sqrt))
  }else if(dist %in% c(distEnum$Inverse_Gaussian)){
    return(c(linkEnum$log, linkEnum$inv_square, linkEnum$identity, linkEnum$inverse))
  }else if(dist %in% c(distEnum$Negative_Binomial)){
    return(c(linkEnum$log, linkEnum$identity, linkEnum$sqrt))
  }else if(dist %in% c(distEnum$Beta)){
    return(c(linkEnum$logit, linkEnum$probit, linkEnum$cloglog, linkEnum$cauchit))
  }else{
    return(NULL)
  }
}

# for(l in linkEnum){
#   if(!l %in% c("inv_square", "tmp"))
#     make.link(l)
# }

linkEnum <- planningLinkEnum()
applyLinkFunction = function(link=linkEnum, inverse=T, value){
  match.arg(link)
  if(link==linkEnum$cauchit){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$cloglog){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$identity){
    return(value)
  }else if(link==linkEnum$inv_square){
    link <- "1/mu^2"
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$inverse){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$log){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$logit){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$probit){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$sqrt){
    linkfunc <- make.link(link)
    if(inverse){
      return(linkfunc$linkinv(value))
    }else{
      return(linkfunc$linkfun(value))
    }
  }else if(link==linkEnum$tmp){
    stop("nah")
  }
}
