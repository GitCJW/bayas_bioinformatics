# sex <- rep(c("male","female"),each=1500)
# sex_num <- rep(c(0.1,0.6),each=1500)
# 
# y <- rnorm(3000,sex_num,1)
# data <- data.frame(y=y,
#                    sex=sex)
# rstanarmfit1 <- rstanarm::stan_glm(y~1,data=data)
# 
# data$y <- rnbinom(3000, mu=100, size=10)
# rstan <- rstanarm::stan_glm(formula = "y~1", data=data, family = neg_binomial_2("log"))
#
# pe <- posterior_epred(rstan)
# pp <- posterior_predict(rstan)

# rstanarmfit2 <- rstanarm::stan_glm(y~1+sex,data=data)
# 
# p1 <- rstanarm::posterior_epred(rstanarmfit1, newdata=NULL)[,1, drop=F]
# p2 <- rstanarm::posterior_epred(rstanarmfit2, newdata=data.frame(sex="male"))

# p <- pp_check(rstanarmfit1)
# ggsave("R1.jpg",p,device="jpeg",path="E:/Dateien/Arbeit/Bioinf/BAYAS/BAYSIS/BAYSIS/Images/Report",
#        units="px", width=100, height=100, dpi=10)

# 
# s1 <- rstanarmfit1$stanfit
# ex1 <- extract(s1)
# s2 <- rstanarmfit2$stanfit
# ex2 <- extract(s2)
# diff <- ex1$beta
# diff2 <- ex2$beta[,2] - ex2$beta[,1]
# diff2a <- ex2$beta[,2] - sample(ex2$beta[,1])
# 
# range(diff)
# range(diff2)
# range(diff2a)
# plot(density(diff))
# plot(density(diff2))


# sex <- rep(c("male","female"),each=150)
# sex_num <- rep(c(0.1,0.5),each=150)
# id <- rep(c("1","2","3"),c(50,50,50))
# id_num <- rep(c(1,2,3),c(50,50,50))
# id_num_inter <- rep(c(2,0,1),c(50,50,50))
# size <- rlnorm(300,5,0.1)
# weight <- rlnorm(300,4.3,0.15)
# total <- rpois(300,50)
# # y <- rnorm(300,sex_num+id_num_inter+size*0.05+weight*0.1+0.2*size*weight,1)
# y <- rnorm(300,sex_num+0.1*weight,1)
# # y <- rbinom(300,total,plogis(sex_num+0.2*(weight-mean(weight))))
# # y <- log(y)
# data <- data.frame(y=y,
#                    sex=sex,
#                    id=id,
#                    weight=weight-mean(weight),
#                    size=size,
#                    total=total)
# # # rstanarmfit <- rstanarm::stan_glm(y~sex*id+weight*size + sex:weight,data=data)
# # # rstanarmfit <- rstanarm::stan_glm(y~sex*id,data=data)
# # # rstanarmfit <- rstanarm::stan_glm(y~sex,data=data)
# # # rstanarmfit <- rstanarm::stan_glm(y~sex+id:weight,data=data)
# # # rstanarmfit <- rstanarm::stan_glm(y~sex,data=data)
# rstanarmfit <- rstanarm::stan_glm(y~-1+weight+sex,data=data, iter=2000, warmup=200)
# pairs(rstanarmfit, condition="accept_stat__")
# # # rstanarmfit <- rstanarm::stan_glm(cbind(y,total-y)~-1+sex+sex:weight,data=data, family=binomial())
# # # pp_check(rstanarmfit)
# #
# #Make predictions
# data_m <- data.frame(sex="male",
#                    weight=mean(weight))
# data_f <- data.frame(sex="female",
#                    weight=mean(weight))
# 
# #Not deterministic
# pp_male1 <- rstanarm::posterior_predict(rstanarmfit, newdata=data_m)
# pp_male2 <- rstanarm::posterior_predict(rstanarmfit, newdata=data_m)
# plot(density(pp_male1)) + lines(density(pp_male2))
# 
# pp_female1 <- rstanarm::posterior_predict(rstanarmfit, newdata=data_f)
# pp_female2 <- rstanarm::posterior_predict(rstanarmfit, newdata=data_f)
# plot(density(pp_female1)) + lines(density(pp_female2))
# 
# 
# 
# 
# pp_lin_m <- c(rstanarm::posterior_epred(rstanarmfit, newdata=data_m))
# pp_lin_f <- c(rstanarm::posterior_epred(rstanarmfit, newdata=data_f))
# post <- as.array(rstanarmfit$stanfit)
# sigma <- c(post[,,4])
# 
# #Full posterior, mean of all distributions (deterministic)
# ret_dens_m <- prediction_dens_norm(pp_lin_m,sigma)
# ret_dens_f <- prediction_dens_norm(pp_lin_f,sigma)
# plot(ret_dens_m$density_norm~ret_dens_m$x, type="l", ylab="Density", xlab="Predicted value", xlim=c(11,23), main="male vs female") +
# lines(ret_dens_f$density_norm~ret_dens_f$x, type="l", ylab="Density", xlab="Predicted value", col="red") #+
#   # lines(density(rstanarm::posterior_predict(rstanarmfit, newdata=data_m)))
#   # lines(density(rnorm(1e6, mean(pp_lin_m), mean(sigma))))
# 
# 
# #Overlap of the mean distributions
# mu <- c(pp_lin_m,pp_lin_f)
# vec1 <- prediction_dens_norm(pp_lin_m,sigma, seq=seq(min(mu)-3*max(sigma),max(mu)+3*max(sigma),length=512))
# vec2 <- prediction_dens_norm(pp_lin_f,sigma, seq=seq(min(mu)-3*max(sigma),max(mu)+3*max(sigma),length=512))
# overlap(vec1$density,vec2$density)
# 
# 
# #Full posterior 'diff' (deterministic)
# #calculate the overlap for each draw (4000)
# full_diff <- full_dens_diff_norm(pp_lin_m,pp_lin_f,sigma,1e3)
# plot(density(full_diff), main="overlap")
# plot(density(1-full_diff), main="1-overlap") #Probability of getting a higher value for females than males.
# 
# #Mean diff: difference of the two expected predictors (deterministic)
# sex_diff <- average_diff(pp_lin_f,pp_lin_m)
# plot(density(sex_diff))
# 
# ret_dens_m_log <- ret_dens_m
# ret_dens_f_log <- ret_dens_f
# ret_dens_m_log$x <- exp(ret_dens_m_log$x)
# ret_dens_f_log$x <- exp(ret_dens_f_log$x)
# data <- list(a=ret_dens_m_log, b=ret_dens_f_log)
# data <- list(a=ret_dens_m, b=ret_dens_f)
# colors <- c("blue","red")
# x_axis <- "test"
# method <- "eti"
# plotSeveralAreas(data, x_axis=x_axis, method=method, colors=colors)








# N <- 1e4
# mu1 <- rnorm(N,3,0.1)
# mu2 <- rnorm(N,5,0.1)
# sigma <- rnorm(N,2,0.1)
# overlap_vector <- full_dens_diff_norm(mu1,mu2,sigma)
# hist(overlap_vector, breaks=50)
# mean(overlap_vector)
# 
# seq <- seq(min(mu1,mu2)-3*max(sigma), max(mu1,mu2)+3*max(sigma),length=2^9)
# pd1 <- prediction_dens_norm(mu1, sigma, seq=seq)
# pd2 <- prediction_dens_norm(mu2, sigma, seq=seq)
# 
# overlap(pd1, pd2)
# 
# 
# ab <- seq(1,10,length=1000)
# mu1 <- rnorm(1e3, 1, 10)
# mu2 <- rnorm(1e3, 1.1, 10)
# 
# # mu1 <- mu1[order(mu1)] 
# # mu2 <- mu2[order(mu2)] 
# 
# mu1 <- rnorm(1e3, 1, 10)
# mu2 <- mu1 + rnorm(1e3,0,0.1)
# 
# t.test(mu1,mu2)
# t.test(mu1,mu2, paired=T)




average_diff <- function(a,b){
  return(a-b)
}


group_mp_effects <- function(eff, catVar, numVar){
  comb <- eff$combinations
  mps <- eff$mp

  if(is.null(catVar)){
    return(eff)
  }else{
    targetComb <- unique(expand.grid(comb[,catVar,drop=F], stringsAsFactors = F))
    integrateComb <- unique(expand.grid(comb[,!colnames(comb) %in% catVar,drop=F], stringsAsFactors = F))
    if(dim(integrateComb)[1]==0) return(eff)

    mp <- list()
    for(i in 1:dim(targetComb)[1]){

      values <- c()
      for(j in 1:dim(integrateComb)[1]){

        both <- targetComb[i,,drop=F]
        both <- cbind(both, integrateComb[j,,drop=F])


        for(k in mps){
          cComb <- k$comb
          flag <- T
          for(l in colnames(both)){
            if(both[l] != cComb[l]) flag <- F
          }
          if(flag) values <- c(values, k$values)
        }
      }
      tmp_ret <- list(values=values,
                      comb=targetComb[i,,drop=F],
                      type=eff$type,
                      num=eff$num)
      mp[[i]] <- tmp_ret
    }

    ret <- list(
      type = eff$type,
      num = eff$num,
      mp = mp,
      combinations = targetComb
    )
    return(ret)
  }
}

## Returns a named list containing:
## mp: mps vectors for the given categorical variable or the numerical
##     variables. 
## combinations: A dataframe with all combinations of the mps 
## The mps could be really marginal posteriors or transformed mps (with inverse link)
## predictType = c("linpred","epred") epred is linpred with applied inverse link
mp_effects <- function(rstanarmBrmsFit, catVar=NULL, numVar=NULL, numVal=NULL, 
                       response, predictType="linpred", transformFunction=NULL){
  
  # Necessary?
  # class(rstanarmfit) <- class(rstanarmfit)[!class(rstanarmfit) %in% "betareg"]

  
  if("brmsfit" %in% class(rstanarmBrmsFit)){
    post <- as.array(rstanarmBrmsFit)
    dP <- dim(post)
    post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
    names <- names(post[1,1,])
    
    f <- rstanarmBrmsFit$formula$formula
    t <- terms(f)
    
    fit_factors <- attr(t,"term.labels")
    
    fit_pred <- c()
    fit_pred_names <- c()
    for(pred in fit_factors){
      splitted_pred <- str_split(pred, ":")[[1]]
      s_pred_classes <- c()
      for(s_pred in splitted_pred){
        fit_pred <- c(fit_pred, class(rstanarmBrmsFit$data[[s_pred]]))
        fit_pred_names <- c(fit_pred_names, s_pred)
      }
    }
    names(fit_pred) <- fit_pred_names
    fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
    fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
  }else{
    post <- as.array(rstanarmBrmsFit)
    names <- names(post[1,1,])
    names <- names[!names %in% c("mean_PPD","log-posterior")]
    fit_pred <- attr(rstanarmBrmsFit$terms,"dataClasses")
    fit_pred <- fit_pred[!names(fit_pred) %in% response]
    fit_factors <- colnames(attr(rstanarmBrmsFit$terms,"factors"))
    fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
    fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
  }
  
  # class(rstanarmfit) <- class(rstanarmfit)[!class(rstanarmfit) %in% "betareg"]
  # post <- as.array(rstanarmfit)
  # names <- names(post[1,1,])
  # names <- names[!names %in% c("mean_PPD","log-posterior")]
  # fit_pred <- attr(rstanarmfit$terms,"dataClasses")
  # fit_pred <- fit_pred[!names(fit_pred) %in% response]
  # fit_pred <- fit_pred[!fit_pred %in% c("nmatrix.2")]
  # fit_factors <- colnames(attr(rstanarmfit$terms,"factors"))
  # fit_pred_non_numeric <- fit_pred[fit_pred %in% c("character","factor")]
  # fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
  
  data <- rstanarmBrmsFit$data
  
  if(!is.null(catVar)){
    nec_terms <- catVar
    for(terms in fit_factors){
      el <- strsplit(terms, ":")[[1]]
      if(length(el) > 1 && any(catVar %in% el)){
        nec_terms <- c(nec_terms, el)
      }
    }
    nec_terms <- unique(nec_terms)

    comb_list <- list()
    for(i in length(nec_terms):1){
      t <- nec_terms[i]
      if(t %in% names(fit_pred_non_numeric)) comb_list[[t]] <- unique(data[[t]])
    }
    all_comb <- expand.grid(comb_list, stringsAsFactors = F)
    
    
    new_data <- data[1,]
    # new_data <- data.frame(0)
    # for(i in 1:length(fit_pred)){
    #   new_data[,i] <- unique(data[[names(fit_pred)[i]]])[1]
    # }
    # colnames(new_data) <- names(fit_pred)
    for(n in names(fit_pred_numeric)){
      if(n %in% nec_terms){
        new_data[n] <- numVal[[n]]
      }else{
        new_data[n] <- 0
      }
    }
    
    res <- list()
    for(r in 1:dim(all_comb)[1]){
      tmp <- new_data
      tmp[names(all_comb)] <- all_comb[r,]
      
      single_ret <- list()
      single_ret$type <- "intercept"
      single_ret$num <- NULL
      if(any(nec_terms %in% names(fit_pred_numeric))) single_ret$num <- nec_terms[nec_terms %in% names(fit_pred_numeric)]
      
      single_ret$comb <- tmp[,colnames(tmp) %in% names(fit_pred_non_numeric), drop=F]
      if(predictType=="linpred"){
        single_ret$values <- posterior_linpred(rstanarmBrmsFit, newdata=tmp)
      }else if(predictType=="epred"){
        single_ret$values <- posterior_epred(rstanarmBrmsFit, newdata=tmp)
      }
      
      res[[r]] <- single_ret
    }
    
    ret <- list()
    ret$type <- "intercept"
    ret$num <- NULL
    if(any(nec_terms %in% names(fit_pred_numeric))) ret$num <- nec_terms[nec_terms %in% names(fit_pred_numeric)]
    ret$mp <- res
    ret$combinations <- all_comb
    return(ret)
    
  }else if(!is.null(numVar)){
    ret <- list()
    
    notRelevantNumerics <- names(fit_pred_numeric)[!names(fit_pred_numeric) %in% numVar]
    
    nec_terms <- c()
    for(terms in fit_factors){
      el <- strsplit(terms, ":")[[1]]
      if(all(numVar %in% el) && !any(notRelevantNumerics %in% el)){
        nec_terms <- c(nec_terms, el)
      }
    }
    nec_terms <- unique(nec_terms)
    
    #if nec_terms contains no element, a numerical variable is chosen that is not
    #present alone, but in an interaction. 
    if(length(nec_terms)==0){
      return()
    }
    
    comb_list <- list()
    for(i in length(nec_terms):1){
      t <- nec_terms[i]
      if(t %in% names(fit_pred_non_numeric)) comb_list[[t]] <- unique(data[[t]])
    }
    all_comb <- expand.grid(comb_list, stringsAsFactors = F)
    
    new_data <- data[1,]
    # new_data <- data.frame(0)
    # for(i in 1:length(fit_pred)){
    #   new_data[,i] <- unique(data[[names(fit_pred)[i]]])[1]
    # }
    # colnames(new_data) <- names(fit_pred)
    for(n in names(fit_pred_numeric)){
      if(n %in% numVar){
        new_data[n] <- 1
      }else{
        new_data[n] <- 0
      }
    }
    
    ret$combinations <- all_comb
    if(dim(all_comb)[1] == 0){
      all_comb <- new_data
      ret$combinations <- all_comb[nec_terms]
      ret$combinations[1,] <- colnames(ret$combinations)
      ret$combinations <- NULL
    }
    
    res <- list()

    for(r in 1:dim(all_comb)[1]){
      tmp <- new_data
      tmp[names(all_comb)] <- all_comb[r,]
      single_ret <- list()
      single_ret$comb <- tmp[,colnames(tmp) %in% names(ret$combinations),drop=F]

      if(predictType=="linpred"){
        a <- posterior_linpred(rstanarmBrmsFit, newdata=tmp)
        tmp[,numVar] <- 0
        b <- posterior_linpred(rstanarmBrmsFit, newdata=tmp)
        single_ret$values <- a-b
      }else if(predictType=="epred"){
        a <- posterior_epred(rstanarmBrmsFit, newdata=tmp)
        tmp[,numVar] <- 0
        b <- posterior_epred(rstanarmBrmsFit, newdata=tmp)
        single_ret$values <- a-b
      }
      
      single_ret$type <- "slope"
      single_ret$num <- NULL
      if(any(nec_terms %in% names(fit_pred_numeric))) single_ret$num <- nec_terms[nec_terms %in% names(fit_pred_numeric)]
      
      res[[r]] <- single_ret
    }
    
    ret$type <- "slope"
    ret$num <- NULL
    if(any(nec_terms %in% names(fit_pred_numeric))) ret$num <- nec_terms[nec_terms %in% names(fit_pred_numeric)]
    ret$mp <- res
    return(ret)
  }
}


#singleEffect comes frome function 'single_effect' 
summary_effect <- function(singleEffect, hdiType="hdi", ci=1){
  
  # type <- c("Group difference", "Slope", "Slope difference")
  # min <- 0 #eti-low
  # median <- 0
  # max <- 0 # eti-high
  # pi <- 0
 
  data_formatted <- data.frame(type = singleEffect$type,
                     CI_low = formatC(eff_diff(singleEffect$linpred$A,singleEffect$linpred$B,"min", hdiType=hdiType, ci=ci), digits=4),
                     median = formatC(eff_diff(singleEffect$linpred$A,singleEffect$linpred$B,"median"), digits=4),
                     mean = formatC(eff_diff(singleEffect$linpred$A,singleEffect$linpred$B,"mean"), digits=4),
                     CI_high = formatC(eff_diff(singleEffect$linpred$A,singleEffect$linpred$B,"max", hdiType=hdiType, ci=ci), digits=4),
                     pi = formatC(eff_diff(singleEffect$linpred$A,singleEffect$linpred$B,"pi"), digits=4))

  dt <- datatable(data_formatted, rownames=F,
                  options=list(paging=F,searching=F, info=F, ordering=F, scrollX=T,
                               columnDefs = list(list(className='dt-center', targets="_all")))
  )
  return(dt)
}



#returns a named list containing:
#epred: a named list with 1 or 3 elements. (For posterior_epred draws)
# First element: diff (intercept, slope) or effect (slope)
# (optional) second (A), third (B) element: numerics of the posterior draws, if two distinct are selected
#linpred: a named list with the 1 or 3 elements.  (For posterior_linpred draws)
# First element: diff (intercept, slope) or effect (slope)
# (optional) second (A), third (B) element: numerics of the posterior draws, if two distinct are selected
#type: c("Group difference", "Slope", "Slope difference")
single_effect <- function(ncol, nrow,
                          rstanarmBrmsFit, catVar=NULL, numVar=NULL, numVal=NULL, 
                          response){
  ret <- list()
  
  eff <- mp_effects(rstanarmBrmsFit, catVar, numVar, numVal, 
                    response, predictType="linpred")
  eff_g_linpred <- group_mp_effects(eff, catVar, numVar)
  
  eff <- mp_effects(rstanarmBrmsFit, catVar, numVar, numVal, 
                    response, predictType="epred")
  eff_g_epred <- group_mp_effects(eff, catVar, numVar)
  
  slope <- eff$type=="slope"
  
  comb <- eff_g_linpred$combinations
  if(is.null(comb)){
    comb <- data.frame(paste0(eff$num, collapse=":"))
    colnames(comb) <- paste0(eff$num, collapse=":")
  }
  if(slope){
    if(nrow < (ncol-dim(comb)[2]+1)){
      return(NULL)
    }
  }else{
    if(nrow <= (ncol-dim(comb)[2]+1)){
      return(NULL)
    }
  }
  if(ncol < dim(comb)[2]) {
    return(NULL)
  }
  

  combB <- comb[nrow,,drop=F]
  combA <- comb[ncol-dim(comb)[2]+1,,drop=F]
  
  if(any(is.na(combB))) return(NULL)
  
  ret_linpred <- list()
  ret_linpred$diff <- NULL
  
  for(n in eff_g_linpred$mp){
    nC <- n$comb
    flagA <- T
    flagB <- T
    for(col in colnames(nC)){
      if(!col %in% colnames(comb)) next
      if(nC[,col,drop=F] != combA[,col,drop=F]) flagA <- F
      if(nC[,col,drop=F] != combB[,col,drop=F]) flagB <- F
    }
    if(flagA) ret_linpred$A <- c(n$values)
    if(flagB) ret_linpred$B <- c(n$values)
  }
  if(nrow == (ncol-dim(comb)[2]+1)) ret_linpred$B <- 0
  
  
  # comb <- eff_g_epred$combinations
  combB <- comb[nrow,,drop=F]
  combA <- comb[ncol-dim(comb)[2]+1,,drop=F]
  
  ret_epred <- list()
  ret_epred$diff <- NULL
  for(n in eff_g_epred$mp){
    nC <- n$comb
    flagA <- T
    flagB <- T
    for(col in colnames(nC)){
      if(!col %in% colnames(comb)) next
      if(nC[,col,drop=F] != combA[,col,drop=F]) flagA <- F
      if(nC[,col,drop=F] != combB[,col,drop=F]) flagB <- F
    }
    if(flagA) ret_epred$A <- c(n$values)
    if(flagB) ret_epred$B <- c(n$values)
  }
  if(nrow == (ncol-dim(comb)[2]+1)) ret_epred$B <- 0
  
  nA <- c()
  nB <- c()
  for(i in dim(combA)[2]:1){
    nA <- c(nA, as.character(combA[1,i]))
  }
  for(i in dim(combB)[2]:1){
    nB <- c(nB, as.character(combB[1,i]))
  }
  
  ret$linpred <- ret_linpred
  ret$epred <- ret_epred
  ret$type <- "Group difference"

  ret$vars <- c(paste0(nA,collapse=":"), paste0(nB,collapse=":"))
  if(slope){
    if(nrow == (ncol-dim(comb)[2]+1)){
      ret$type <- "Slope"
    }else{
      ret$type <- "Slope difference"
    }
  }
  return(ret)
}



#Returns information about the selected variables (e.g. if their are grouped)
#General information about what is shown in the effect matrix etc.
summarizeEffectsVerbal <- function(rstanarmBrmsFit, catVar=NULL, numVar=NULL, 
                                   numVal=NULL, response, baysisModel){
  
  
  if("brmsfit" %in% class(rstanarmBrmsFit)){
    post <- as.array(rstanarmBrmsFit)
    dP <- dim(post)
    post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
    names <- names(post[1,1,])
    
    f <- rstanarmBrmsFit$formula$formula
    t <- terms(f)
    
    fit_factors <- attr(t,"term.labels")
    
    fit_pred <- c()
    for(pred in fit_factors){
      post <- as.array(rstanarmBrmsFit)
      dP <- dim(post)
      post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
      names <- names(post[1,1,])
      
      f <- rstanarmBrmsFit$formula$formula
      t <- terms(f)
      
      fit_factors <- attr(t,"term.labels")
      
      fit_pred <- c()
      fit_pred_names <- c()
      for(pred in fit_factors){
        splitted_pred <- str_split(pred, ":")[[1]]
        s_pred_classes <- c()
        for(s_pred in splitted_pred){
          fit_pred <- c(fit_pred, class(rstanarmBrmsFit$data[[s_pred]]))
          fit_pred_names <- c(fit_pred_names, s_pred)
        }
      }
      names(fit_pred) <- fit_pred_names
      fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
      fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
    }
  }else{
    post <- as.array(rstanarmBrmsFit)
    names <- names(post[1,1,])
    names <- names[!names %in% c("mean_PPD","log-posterior")]
    fit_pred <- attr(rstanarmBrmsFit$terms,"dataClasses")
    fit_pred <- fit_pred[!names(fit_pred) %in% response]
    fit_factors <- colnames(attr(rstanarmBrmsFit$terms,"factors"))
    fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
    fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
  }
  
  ret <- list()
  # post <- as.array(rstanarmfit)
  # names <- names(post[1,1,])
  # names <- names[!names %in% c("mean_PPD","log-posterior")]
  # fit_pred <- attr(rstanarmfit$terms,"dataClasses")
  # fit_pred <- fit_pred[!names(fit_pred) %in% response]
  # fit_factors <- colnames(attr(rstanarmfit$terms,"factors"))
  # fit_pred_non_numeric <- fit_pred[fit_pred %in% c("character","factor")]
  # fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
  
  data <- rstanarmBrmsFit$data
  
  # Group vars: are there interaction?
  nec_terms <- c()
  if(!is.null(catVar)){
    for(terms in fit_factors){
      el <- strsplit(terms, ":")[[1]]
      if(length(el) > 1 && any(catVar %in% el)){
        if(!all(el %in% catVar) && all(el[!el %in% catVar] %in% names(fit_pred_non_numeric))) 
          nec_terms <- c(nec_terms, el[!el %in% catVar])
      }
    }
  }
  nec_terms <- unique(nec_terms)

  if(length(nec_terms) > 0){
    txt <- tags$p(HTML(paste0("The effects are integrated over <b>",
                       paste0(nec_terms, collapse=", "),
                       "</b>. ",
                       "If you want to examine the effects per subgroup of <b>",
                       paste0(nec_terms, collapse=", "),
                       "</b>, you should also select <b>", paste0(nec_terms, collapse=", "),"</b> (by holding CTRL)."
                       )))
    ret <- list.append(ret, txt)
  }
  
  # Is the numeric predictor group dependent?
  if(!is.null(numVar)){
    nec_terms <- c()
    for(terms in fit_factors){
      el <- strsplit(terms, ":")[[1]]
      if(all(numVar %in% el)){
        if(any(el %in% names(fit_pred_non_numeric))) 
          nec_terms <- c(nec_terms, el[el %in% names(fit_pred_non_numeric)])
      }
    }
    nec_terms <- unique(nec_terms)
    if(length(nec_terms) > 0){
      txt <- tags$p(HTML(paste0("There is no single slope <b>",
                                paste0(numVar, collapse=":"),
                                "</b>,",
                                " but slopes for the subgroups of <b>",
                                paste0(nec_terms, collapse=", "),
                                "</b>."
      )))
      ret <- list.append(ret, txt)
    }
  }
  
  
  dON <- dependsOnNumeric(rstanarmBrmsFit, catVar, response)$used
  
  # Group vars: numerical values
  if(!is.null(dON)){
    txt <- tags$p(HTML(
      paste0(ifelse(length(dON)==1,"The effects also depend on the given value of <b>","The effects also depend on the given values of: <b>"),
      paste0(dON, collapse = ","),
      "</b>.")))
    ret <- list.append(ret, txt)
  }
  
  
  # Are there certain effects?
  # Both: What are the effects? (log odds etc.)
  eM <- eff_matrix(catVar, numVar, numVal, "pi", rstanarmBrmsFit, response)
  
  effects <- c()
  for(i in eM){
    for(j in i){
      # print(j)
      asNum <- suppressWarnings(as.numeric(j))
      if(!is.na(asNum)){
        effects <- c(effects,asNum)
      } 
    }
  }
  txt <- tags$p(HTML(paste0(
    "The distribution for <b>",length(effects[effects>=0.95]),"/",length(effects), 
    "</b> effects does not contain zero, lending some credibility to the effects. ",
    "The effect directionality (pi), i.e. the probability of the effect having a certain direction. ",
    "The greatest pi in your data is <b>", max(effects), "</b>."
  )))
  ret <- list.append(ret, txt)
  
  
  # General information
  txt <- tags$p(HTML(paste0("Examine each effect in the table below. ",
                            "Effects are given by distributions that shows their uncertainties. ",
                            "The pi value gives probability of the effect having a certain direction. ",
                            "A pi of 0 means a fifty-fifty probability of a positive or negative effect. ",
                            "A pi of 1 means that the effect clearly lies in one direction. ",
                            "Select mean, median, CI_low, CI_high to show the corresponding value of the effect.",
                            "Effects are shown as: (column element - row element). ")))
  ret <- list.append(ret, txt)
  
  
  # Model dependent description (effect type) 
  # Differ between group and slope
  type <- ifelse(!is.null(catVar),"group","slope")
  txt <- tags$p(HTML(baysisModel$effects_description(type=type)))
  ret <- list.append(ret, txt)
  
  return(tagList(ret))
}


eff_matrix <- function(selGroup, selSlope, numericVal, matrixType, fit, response,
                       hdiType="hdi", ci=1){
  data <- data.frame()
  cnames <- list()
  
  eff <- list()
  
  if(!is.null(selGroup) && (length(selGroup) > 1 || selGroup != "")){
    eff <- mp_effects(fit, catVar=selGroup, numVar=NULL, numVal=numericVal, response=response, "linpred") 
    eff <- group_mp_effects(eff, catVar=selGroup, numVar=selSlope)
    mps <- eff$mp
    lMatrix <- length(mps)
    m <- matrix(NA, lMatrix, lMatrix)
    
    for(i in 1:lMatrix){
      cnames <- list.append(cnames, paste0(mps[[i]]$comb, collapse="."))
      for(j in 1:lMatrix){
        if(i==j){
          m[j,i] <- "-"
        } else if(j < i){
          m[j,i] <- ""
        } else{
          m[j,i] <- str_trim(formatC(eff_diff(mps[[j]]$values,mps[[i]]$values,matrixType, hdiType=hdiType, ci=ci), digits=4))
        }
      }
    }
    data <- data.frame(m)
  }else if(!is.null(selSlope) && selSlope != ""){
    eff <- mp_effects(fit, catVar=NULL, numVar=selSlope, numVal=NULL, response=response,"linpred") 
    eff <- group_mp_effects(eff, catVar=selGroup, numVar=selSlope)
    mps <- eff$mp
    lMatrix <- length(mps)
    m <- matrix(NA, lMatrix, lMatrix)
    
    for(i in 1:lMatrix){
      cnames <- list.append(cnames, paste0(mps[[i]]$comb, collapse="."))
      for(j in 1:lMatrix){
        if(j < i){
          m[j,i] <- ""
        } else  if(i==j){
          m[j,i] <- str_trim(formatC(eff_diff(mps[[i]]$values,rep(0,length(mps[[i]]$values)),matrixType, hdiType=hdiType, ci=ci), digits=4))
        }else{
          m[j,i] <- str_trim(formatC(eff_diff(mps[[i]]$values,mps[[j]]$values,matrixType, hdiType=hdiType, ci=ci), digits=4))
        }
      }
    }
    data <- data.frame(m)
  }else{
    return(datatable(data))
  }
  
  return(data)
}



#returns different measurements depending on type
#type = c("pi","mean","median","min","max")
eff_diff <- function(a,b,type, hdiType="hdi", ci=1){
  diff <- a-b
  ci_interval <- NULL
  if(type=="min" || type=="max"){
    if(hdiType=="hdi"){
      ci_interval <- hdi(diff, ci)
    }else{
      ci_interval <- eti(diff, ci)
    }
  }
  if(type=="pi"){
    return(PI.value(diff))
  }else if(type=="mean"){
    return(mean(diff))
  }else if(type=="median"){
    return(median(diff))
  }else if(type=="min"){
    return(ci_interval$CI_low)
  }else if(type=="max"){
    return(ci_interval$CI_high)
  }
}

mp_list_to_data.frame <- function(dat){
  ret <- empty.data.frame(colnames=c(".val",colnames(dat[[1]]$comb)))

  for(i in 1:length(dat)){
    rep <- length(dat[[i]]$values)
    
    a <- dat[[i]]$comb[rep(1,length(dat[[i]]$values)),]
    
    a[[".val"]] <- c(dat[[i]]$values)

    ret <- rbind(ret,a)
  }
  
  return(ret)
}

#Returns the dependent and not dependent numeric vars by a given (set) of group vars
dependsOnNumeric <- function(rstanarmBrmsFit, groupVars, response){
  ret <- list()
  
  if(is.null(groupVars) || length(groupVars)==0){
    return(ret)
  }
  
  
  if("brmsfit" %in% class(rstanarmBrmsFit)){
    post <- as.array(rstanarmBrmsFit)
    dP <- dim(post)
    post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
    names <- names(post[1,1,])
    
    f <- rstanarmBrmsFit$formula$formula
    t <- terms(f)
    
    fit_factors <- attr(t,"term.labels")
    
    fit_pred <- c()
    for(pred in fit_factors){
      post <- as.array(rstanarmBrmsFit)
      dP <- dim(post)
      post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
      names <- names(post[1,1,])
      
      f <- rstanarmBrmsFit$formula$formula
      t <- terms(f)
      
      fit_factors <- attr(t,"term.labels")
      
      fit_pred <- c()
      fit_pred_names <- c()
      for(pred in fit_factors){
        splitted_pred <- str_split(pred, ":")[[1]]
        s_pred_classes <- c()
        for(s_pred in splitted_pred){
          fit_pred <- c(fit_pred, class(rstanarmBrmsFit$data[[s_pred]]))
          fit_pred_names <- c(fit_pred_names, s_pred)
        }
      }
      names(fit_pred) <- fit_pred_names
      fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
      fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
    }

  }else{
    post <- as.array(rstanarmBrmsFit)
    names <- names(post[1,1,])
    names <- names[!names %in% c("mean_PPD","log-posterior")]
    fit_pred <- attr(rstanarmBrmsFit$terms,"dataClasses")
    fit_pred <- fit_pred[!names(fit_pred) %in% response]
    fit_factors <- colnames(attr(rstanarmBrmsFit$terms,"factors"))
    fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
  }
  
  # post <- as.array(rstanarmfit)
  # names <- names(post[1,1,])
  # names <- names[!names %in% c("mean_PPD","log-posterior")]
  # fit_pred <- attr(rstanarmfit$terms,"dataClasses")
  # fit_pred <- fit_pred[!names(fit_pred) %in% response]
  # fit_factors <- colnames(attr(rstanarmfit$terms,"factors"))
  # fit_pred_non_numeric <- fit_pred[fit_pred %in% c("character","factor")]
  # fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
  
  nums <- c()
  for(terms in fit_factors){
    el <- strsplit(terms, ":")[[1]]
    if(length(el) > 1 && any(groupVars %in% el) && any(el %in% names(fit_pred_numeric))){
      nums <- c(nums, el[el %in% names(fit_pred_numeric)])
    }
  }
  ret$used <- unique(nums)
  ret$unused <- names(fit_pred_numeric)[!names(fit_pred_numeric) %in% ret$used] 
  
  return(ret)
}


#fit is either a stanreg (rstanarm) or a brmsFit
isNumericPossible <- function(rstanarmBrmsFit, catVar=NULL, numVar=NULL, 
                              numVal=NULL, response){


  # rstanarmBrmsFit <- brmsFit
  # rstanarmBrmsFit <- rstanarmFit
  # numVar <- "weight"
  # numVar <- NULL
  # response <- "y"
  
  if("brmsfit" %in% class(rstanarmBrmsFit)){
    post <- as.array(rstanarmBrmsFit)
    dP <- dim(post)
    post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
    names <- names(post[1,1,])
    
    f <- rstanarmBrmsFit$formula$formula
    t <- terms(f)
    
    fit_factors <- attr(t,"term.labels")
    
    fit_pred <- c()
    for(pred in fit_factors){
      post <- as.array(rstanarmBrmsFit)
      dP <- dim(post)
      post <- post[1:dP[1], 1:dP[2], 1:(dP[3]-2)]
      names <- names(post[1,1,])
      
      f <- rstanarmBrmsFit$formula$formula
      t <- terms(f)
      
      fit_factors <- attr(t,"term.labels")
      
      fit_pred <- c()
      fit_pred_names <- c()
      for(pred in fit_factors){
        splitted_pred <- str_split(pred, ":")[[1]]
        s_pred_classes <- c()
        for(s_pred in splitted_pred){
          fit_pred <- c(fit_pred, class(rstanarmBrmsFit$data[[s_pred]]))
          fit_pred_names <- c(fit_pred_names, s_pred)
        }
      }
      names(fit_pred) <- fit_pred_names
      fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
      fit_pred_non_numeric <- fit_pred[fit_pred!="numeric"]
    }
  }else{
    post <- as.array(rstanarmBrmsFit)
    names <- names(post[1,1,])
    names <- names[!names %in% c("mean_PPD","log-posterior")]
    fit_pred <- attr(rstanarmBrmsFit$terms,"dataClasses")
    fit_pred <- fit_pred[!names(fit_pred) %in% response]
    fit_factors <- colnames(attr(rstanarmBrmsFit$terms,"factors"))
    fit_pred_numeric <- fit_pred[fit_pred=="numeric"]
  }
  
  
  
  
  ret <- list()
  ret$bool <- T
  ret$alt <- c()
  for(terms in fit_factors){
    el <- strsplit(terms, ":")[[1]]
    if(all(numVar %in% el) && all(el[el %in% names(fit_pred_numeric)] %in% numVar)){
      return(ret)
    }
  }
  ret$bool <- F
  for(terms in fit_factors){
    el <- strsplit(terms, ":")[[1]]
    if(all(numVar %in% el)){
      ret$alt <- c(ret$alt, paste0(el[el %in% names(fit_pred_numeric)],collapse=":"))
    }
  }
  if(length(ret$alt)>0) return(ret)
  
  for(num in numVar){
    tmpNum <- isNumericPossible(rstanarmBrmsFit, catVar, num, numVal, response)
    if(tmpNum$bool){
      ret$alt <- c(ret$alt, num)
    }else{
      ret$alt <- c(ret$alt, tmpNum$alt)
    }
  }
  return(ret)
}


PI.value <- function(data, lim=0){
  pi <- length(data[data>lim])/length(data)
  return(2*max(pi,1-pi)-1)
}


