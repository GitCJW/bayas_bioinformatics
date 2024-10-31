
planningExperimentToLatex <- function(model){
  tex <- ""
  
  if(localUse) browser()
  
  
  #Formula
  #Generative
  formula <- model$getMcdFormula()
  fO <- model$getFormulaObject()
  formulaLatex <- transposePlanningFormulaToLatex(formula)
  
  #Prior
  
  
  #Response
  
  #Other variables
  
  #Parameters
  
  #Est 'N'
  #Goals
  
  #Result
  
  
  
  return(tex)
}

transformParaNameToLatex <- function(paraName){
  ret <- switch(
    paraName,
    "&sigma;" = "\\sigma",
    "&mu;" = "\\mu",
    "&kappa;" = "\\kappa",
    "&theta;" = "\\theta",
    "&lambda;" = "\\lambda",
    "&Phi;" = "\\Phi",
    "&gamma;" = "\\gamma",
    "&alpha;" = "\\alpha",
    "&beta;" = "\\beta",
    "&nu;" = "\\nu",
    "&phi;" = "\\phi")
  return(ret)
}

pLinkEnum <- planningLinkEnum()
getLinkFunctionTermToLatex <- function(link=pLinkEnum, content){
  if(is.null(link)) return(content)
  match.arg(link)

  ret <- switch(
    link,
    cauchit = paste0("\\text{cauchit}(",content,")"),
    cloglog = paste0("\\text{cloglog}(",content,")"),
    identity = content,
    inv_square = paste0("(",content,")\\textsuperscript{-2}"),
    inverse = paste0("(",content,")\\textsuperscript{-1}"),
    log = paste0("\\text{log}(",content,")"),
    logit = paste0("\\text{logit}(",content,")"),
    probit = paste0("\\text{probit}(",content,")"),
    sqrt = paste0("\\sqrt(",content,")"),
    tmp = content)

  if(is.null(ret) && localUse) browser()
  
  return(ret)
}
