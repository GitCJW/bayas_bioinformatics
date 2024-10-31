pLinkEnum <- planningLinkEnum()

getLinkFunctionTerm <- function(link=pLinkEnum, content){
  if(is.null(link)) return(content)
  match.arg(link)
  ret <- ""
  
  if(link==pLinkEnum$cauchit){
    ret <- paste0("cauchit(",content,")")
  }else if(link==pLinkEnum$cloglog){
    ret <- paste0("cloglog(",content,")")
  }else if(link==pLinkEnum$identity){
    ret <- content
  }else if(link==pLinkEnum$inv_square){
    ret <- paste0("(",content,")<sup>-2</sup>")
  }else if(link==pLinkEnum$inverse){
    ret <- paste0("(",content,")<sup>-1</sup>")
  }else if(link==pLinkEnum$log){
    ret <- paste0("log(",content,")")
  }else if(link==pLinkEnum$logit){
    ret <- paste0("logit(",content,")")
  }else if(link==pLinkEnum$probit){
    ret <- paste0("probit(",content,")")
  }else if(link==pLinkEnum$sqrt){
    ret <- paste0("&radic;(",content,")")
  }else if(link==pLinkEnum$tmp){
    ret <- content
  }
  return(ret)
}
