#From ModelCreatingData
pDistEnum <- planningDistribtionsEnum("all")

#dist from pDistEnum
#distColor=string
#para: list of list(name=string/value, vector=bool, color=string)
getDistributionNameForFormula <- function(dist=pDistEnum, distColor, para){
  match.arg(dist)
  
  if(dist==pDistEnum$FixedValue){
    ret <- paste0("<span>", para[[1]]$name ,"</span>")
    return(ret)
  }
  
  ret <- "<span> <span style=\""
  if(!is.null(distColor))
    ret <- paste0(ret, "color:", distColor, ";")
  ret <- paste0(ret, "\"> ",dist," </span>(")
  
  for(par in para){
    ret <- paste0(ret, "<span style=\"")
    if(!is.null(par$color))
      ret <- paste0(ret, "color:", par$color, ";")
    if(!is.null(par$vector) && par$vector==T)
      ret <- paste0(ret, "font-weight:bold;")
    ret <- paste0(ret, "\">",par$name,"</span>")
    ret <- paste0(ret, " , ")
  }
  ret <- sub_str(ret, -3)
  
  ret <- paste0(ret, ")</span>")  
  return(ret)
}

getDistributionNameAsLatex <- function(dist=pDistEnum, para){

  match.arg(dist)
  
  if(dist==pDistEnum$FixedValue){
    ret <- paste0(para[[1]]$name)
    return(ret)
  }
  dist <- wordToLatexConform(dist)
  ret <- paste0("\\text{",dist,"}(")

  for(par in para){

    if(!is.null(par$vector) && par$vector==T){
      ret <- paste0(ret, "\\underline{", par$latexName, "}")
    }else{
      ret <- paste0(ret, par$latexName)
    }
    ret <- paste0(ret, " , ")
  }
  ret <- sub_str(ret, -3)
  
  ret <- paste0(ret, ")")  
  return(ret)
}

getDistributionParameterForFormula <- function(dist=pDistEnum, glm=T){
  if(is.null(dist)) return(list())
  match.arg(dist)
  ret <- list()
  if(dist==pDistEnum$Log_Normal){
    ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    ret <- list.append(ret, list(name="&sigma;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="sigma", latexName="\\sigma"), "aux",extendBySameName=T)
  }else if(dist==pDistEnum$Gamma){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&kappa;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="shape", latexName="\\kappa"), "aux",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="&theta;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="Theta", latexName="\\theta"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&kappa;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="kappa", latexName="\\kappa"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Inverse_Gaussian){
    ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    ret <- list.append(ret, list(name="&lambda;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="shape", latexName="\\lambda"), "aux",extendBySameName=T)
  }else if(dist==pDistEnum$F){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="d<sub>1</sub>", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="d1", latexName="d_1"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="d<sub>2</sub>", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="d2", latexName="d_2"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Chi_squared_1){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="&kappa;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="df", latexName="\\kappa"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Exponential){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="&lambda;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="lambda", latexName="\\lambda"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Chi_squared_non_1){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="&kappa;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="df", latexName="\\kappa"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Weibull){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="&lambda;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="lambda", latexName="\\lambda"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&kappa;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="kappa", latexName="\\kappa"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Beta){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", ul="<1", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&Phi;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="phi", latexName="\\Phi"), "aux",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="&alpha;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="alpha", latexName="\\alpha"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&beta;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="beta", latexName="\\beta"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Normal){
    ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    ret <- list.append(ret, list(name="&sigma;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="sigma", latexName="\\sigma"), "aux",extendBySameName=T)
  }else if(dist==pDistEnum$Cauchy){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="x<sub>0</sub>", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="location", latexName="x_0"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&gamma;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="scale", latexName="\\gamma"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Logistic){
    ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    ret <- list.append(ret, list(name="s", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="scale", latexName="s"), "aux",extendBySameName=T)
  }else if(dist==pDistEnum$Uniform){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="a", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="min", latexName="a"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="b", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName="max", latexName="b"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Student_t){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, distParaName=NULL, latexName="\\mu"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&sigma;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName=NULL, latexName="\\sigma"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&nu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName=NULL, latexName="\\nu"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Poisson){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="&lambda;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="lambda", latexName="\\lambda"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Negative_Binomial){
    if(glm){
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&phi;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="shape", latexName="\\phi"), "aux",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="&alpha;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="successes", latexName="\\alpha"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&beta;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="prob", latexName="\\beta"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Geometric){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="p", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="prob", latexName="p"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Hypergeometric){
    if(glm){
      stop("This distribution: ", dist, " is not used in a glm context!")
    }else{
      ret <- list.append(ret, list(name="N", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="m", latexName="N"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&alpha;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="n", latexName="\\alpha"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&beta;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="k", latexName="\\beta"), "aux",extendBySameName=T)
    }
  }else if(dist==pDistEnum$Binomial){
    ret <- list.append(ret, list(name="N", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="N", latexName="N"), "fixPara",extendBySameName=T)
    ret <- list.append(ret, list(name="p", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", ul="<1", distParaName="prob", latexName="p"), "mean",extendBySameName=T)
  }else if(dist==pDistEnum$Beta_Binomial){
    if(glm){
      ret <- list.append(ret, list(name="N", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="N", latexName="N"), "fixPara",extendBySameName=T)
      ret <- list.append(ret, list(name="&mu;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", ul="<1", distParaName="mu", latexName="\\mu"), "mean",extendBySameName=T)
      ret <- list.append(ret, list(name="&phi;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="phi", latexName="\\phi"), "aux",extendBySameName=T)
    }else{
      ret <- list.append(ret, list(name="N", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="N", latexName="N"), "fixPara",extendBySameName=T)
      ret <- list.append(ret, list(name="&alpha;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="alpha", latexName="\\alpha"), "aux",extendBySameName=T)
      ret <- list.append(ret, list(name="&beta;", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", distParaName="beta", latexName="\\beta"), "aux",extendBySameName=T)
    } 
  }else if(dist==pDistEnum$Bernoulli){
    ret <- list.append(ret, list(name="p", vector=F, color=BAYAS_COLORS$`--formula-color-3`, ll=">0", ul="<1", distParaName="prob", latexName="p"), "mean",extendBySameName=T)
  }
  return(ret)
}

# para: list(name, vector, color)
getParaSpanWithColor <- function(para){
  paste0(tags$span(
    style=paste0("color:",para$color,";"), 
    HTML(ifelse(para$vector==T,paste0(tags$b(para$name)),para$name))))
}
