
##################################################
##################### Normal #####################
##################################################

# Good for plotting distributions
# Calculates the density for N windows. Returns a df of three columns:
# x: mean/center of the certain window
# density: the density of the certain window
# cum_dist: cumulative density distribution function
prediction_dens_norm <- function(mu, sigma, N=512, acc=1e-4, seq=NULL){
  if(length(mu)!=length(sigma)) stop("mu and sigma must be of equal length!")

  # N=512
  # mu <- rnorm(4000,100,2)
  # sigma <- rnorm(4000,1,0.1)
  # seq=NULL
  # time <- as.numeric(Sys.time())*1000
  
  if(is.null(seq)){
    min <- qnorm(acc, min(mu), max(sigma))
    max <- qnorm(1-acc, max(mu), max(sigma))
    seq <- seq(min,max,length=N)
  }

  ret <- matrix(0,length(mu),N)
  cum_ret <- matrix(0,length(mu),N)
  
  for(i in 1:length(mu)){
    ret[i,] <- dnorm(seq,mu[i],sigma[i])
    cum_ret[i,] <- pnorm(seq,mu[i],sigma[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  # as.numeric(Sys.time())*1000 - time
  
  return(df)
}




##################################################
###################### Gamma #####################
##################################################

# Good for plotting distributions
# Calculates the density for N windows. Returns a df of three columns:
# x: mean/center of the certain window
# density: the density of the certain window
# cum_dist: cumulative density distribution function
prediction_dens_gamma <- function(mu, shape, N=512, acc=1e-4, seq=NULL){
  if(length(mu)!=length(shape)) stop("mu and shape must be of equal length!")
  
  # N=512
  # mu <- rnorm(4000,100,2)
  # shape <- rnorm(4000,1,0.1)
  # seq=NULL
  # time <- as.numeric(Sys.time())*1000
  # 
  
  rate <- shape/mu
  
  if(is.null(seq)){
    min <- qgamma(acc, shape=min(shape), rate=min(rate))
    max <- qgamma(1-acc, shape=max(shape), rate=min(rate))
    seq <- seq(min,max,length=N)
  }
  
  ret <- matrix(0,length(mu),N)
  cum_ret <- matrix(0,length(mu),N)
  
  for(i in 1:length(mu)){
    ret[i,] <- dgamma(seq,shape=shape[i], rate=rate[i])
    cum_ret[i,] <- pgamma(seq,shape=shape[i], rate=rate[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  # as.numeric(Sys.time())*1000 - time
  
  return(df)
}


##################################################
################### Exponential ##################
##################################################

# pp <- prediction_dens_exponential(5)
# plot(pp$density ~ pp$x)

# Good for plotting distributions
# Calculates the density for N windows. Returns a df of three columns:
# x: mean/center of the certain window
# density: the density of the certain window
# cum_dist: cumulative density distribution function
prediction_dens_exponential <- function(mu, N=512, acc=1e-4, seq=NULL){

  # N=512
  # mu <- rnorm(4000,100,2)
  # shape <- rnorm(4000,1,0.1)
  # seq=NULL
  # time <- as.numeric(Sys.time())*1000
  # 
  
  rate <- 1/mu
  
  if(is.null(seq)){
    min <- 0#.Machine$double.xmin
    max <- qexp(1-acc, rate=median(rate))
    seq <- seq(min,max,length=N)
  }
  
  ret <- matrix(0,length(mu),N)
  cum_ret <- matrix(0,length(mu),N)
  
  for(i in 1:length(mu)){
    ret[i,] <- dexp(seq,rate=rate[i])
    cum_ret[i,] <- pexp(seq,rate=rate[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  # as.numeric(Sys.time())*1000 - time
  
  return(df)
}


##################################################
################ Inverse Gaussian  ###############
##################################################

# Good for plotting distributions
# Calculates the density for N windows. Returns a df of three columns:
# x: mean/center of the certain window
# density: the density of the certain window
# cum_dist: cumulative density distribution function
prediction_dens_invgaussian <- function(mu, lambda, N=512, acc=1e-4, seq=NULL){
  if(length(mu)!=length(lambda)) stop("mu and lambda must be of equal length!")

  # N=512
  # mu <- rnorm(4000,100,2)
  # lambda <- rnorm(4000,1,0.1)
  # seq=NULL
  # time <- as.numeric(Sys.time())*1000
  
  if(is.null(seq)){
    min <- qinvgauss(acc, mean=min(mu), shape=min(lambda))
    max <- qinvgauss(1-acc, mean=max(mu), shape=min(lambda))
    seq <- seq(min,max,length=N)
  }
  
  ret <- matrix(0,length(mu),N)
  cum_ret <- matrix(0,length(mu),N)
  
  for(i in 1:length(mu)){
    ret[i,] <- dinvgauss(seq,mean=mu[i],shape=lambda[i])
    cum_ret[i,] <- pinvgauss(seq,mean=mu[i],shape=lambda[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  # as.numeric(Sys.time())*1000 - time
  
  return(df)
}


##################################################
###################### Beta  #####################
##################################################

# Good for plotting distributions
# Calculates the density for N windows. Returns a df of three columns:
# x: mean/center of the certain window
# density: the density of the certain window
# cum_dist: cumulative density distribution function
prediction_dens_beta <- function(mu, phi, N=512, acc=1e-4, seq=NULL){
  if(length(mu)!=length(phi)) stop("mu and phi must be of equal length!")
  
  # N=512
  # mu <- rbeta(4000,2,3)
  # phi <- rnorm(4000,1,0.1)
  # seq=NULL
  # time <- as.numeric(Sys.time())*1000
  
  alpha <- mu*phi
  beta <- (1-mu)*phi
  
  if(is.null(seq)){
    min <- qbeta(acc, min(alpha), max(beta))
    max <- qbeta(1-acc, max(alpha), min(beta))
    seq <- seq(min,max,length=N+2)
    seq <- seq[2:(N+1)]
  }
  
  ret <- matrix(0,length(mu),N)
  cum_ret <- matrix(0,length(mu),N)
  
  for(i in 1:length(mu)){
    ret[i,] <- dbeta(seq,alpha[i],beta[i])
    cum_ret[i,] <- pbeta(seq,alpha[i],beta[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  # as.numeric(Sys.time())*1000 - time
  
  return(df)
}

##################################################
##################### Poisson ####################
##################################################

# Good for plotting distributions
# Calculates the mass. Returns a df of three columns:
# x: 
# density: the mass of the value x
# cum_dist: cumulative mass distribution function
prediction_dens_poisson <- function(lambda, seq=NULL, N=512, acc=1e-4){
 
  # seq=NULL
  # N=512
  # time <- as.numeric(Sys.time())*1000
  
  if(is.null(seq)){
    min <- qpois(acc, min(lambda))
    max <- qpois(1-acc, max(lambda))
    
    diff <- max-min
    N <- N-1
    if(diff < N){
      N <- diff
    }else{
      steps <- ceiling(diff /N)
      new_range <- steps*N
      diff_d <- new_range-diff
      min_rest <- min-floor(diff_d/2)
      if(min_rest <0){
        min <- 0
        max <- new_range
      }else{
        min <- min-floor(diff_d/2)
        max <- max+ceiling(diff_d/2)
      }
    }
    N<-N+1
    seq <- seq(min,max, length=N)
  }
  seq_diff <- seq[2] - seq[1]
  ret <- matrix(0,length(lambda),N)
  cum_ret <- matrix(0,length(lambda),N)
  
  for(i in 1:length(lambda)){
    ret_a <- ppois(pmax(0,c(seq-(seq_diff/2),last(seq)+(seq_diff/2))),lambda[i])
    ret[i,] <- ret_a[2:(N+1)]-ret_a[1:N]
    cum_ret[i,] <- ppois(seq,lambda[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  
  # as.numeric(Sys.time())*1000 - time
}



##################################################
#################### Binomial ####################
##################################################

# Good for plotting distributions
# Calculates the mass. Returns a df of three columns:
# x: 
# density: the mass of the value x
# cum_dist: cumulative mass distribution function
prediction_dens_binom <- function(N_binom, p, seq=NULL, N=512, acc=1e-4){
  
  # N_binom <- 100000
  # p <- rbeta(4000,100,200)
  # seq=NULL
  # N=512
  # time <- as.numeric(Sys.time())*1000
  
  if(is.null(seq)){
    min <- qbinom(acc, N_binom, min(p))
    max <- qbinom(1-acc, N_binom, max(p))
    
    diff <- max-min
    N <- N-1
    if(diff < N){
      N <- diff
    }else{
      steps <- ceiling(diff /N)
      if(steps*N > N_binom){
        # diff <- N_binom
        N <- floor(diff/steps)
        min <- 0
        max <- N*steps
        diff <- max-min
      }
      new_range <- steps*N
      diff_d <- new_range-diff
      
      if(floor(diff_d/2) > min){
        min <- 0
        max <- new_range
      }else if(max+ceiling(diff_d/2) > N_binom){
        min <- N_binom-new_range
        max <- N_binom
      }else{
        min <- min-floor(diff_d/2)
        max <- max+ceiling(diff_d/2)
      }
    }
    N<-N+1
    seq <- seq(min,max, length=N)
  }
  seq_diff <- seq[2] - seq[1]
  ret <- matrix(0,length(p),N)
  cum_ret <- matrix(0,length(p),N)
  
  for(i in 1:length(p)){
    ret_a <- pbinom(pmax(0,c(seq-(seq_diff/2),last(seq)+(seq_diff/2))),N_binom, p[i])
    ret[i,] <- ret_a[2:(N+1)]-ret_a[1:N]
    cum_ret[i,] <- pbinom(seq,N_binom, p[i])
  }
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  
  # as.numeric(Sys.time())*1000 - time
}






##################################################
################## Neg-Binomial ##################
##################################################

# Good for plotting distributions
# Calculates the mass. Returns a df of three columns:
# x: 
# density: the mass of the value x
# cum_dist: cumulative mass distribution function
prediction_dens_negbinom <- function(mu, phi, seq=NULL, N=512, acc=1e-4){
  if(length(mu)!=length(phi)) stop("mu and phi must be of equal length!")
  
  # mu <- rnorm(4000,10000,100)
  # phi <- rnorm(4000,100,5)
  # seq=NULL
  # N=c(257,512)
  # acc=1e-4
  # time <- as.numeric(Sys.time())*1000

  if(is.null(seq)){
    min <- qnbinom(acc, mu=min(mu), size=min(phi))
    max <- qnbinom(1-acc, mu=max(mu), size=min(phi))

    diff <- max-min
    N <- N-1
    if(diff < N){
      N <- diff
    }else{
      steps <- ceiling(diff /N)
      new_range <- steps*N
      diff_d <- new_range-diff
      min_rest <- min-floor(diff_d/2)
      if(min_rest <0){
        min <- 0
        max <- new_range
      }else{
        min <- min-floor(diff_d/2)
        max <- max+ceiling(diff_d/2)
      }
    }
    N<-N+1
    seq <- seq(min,max, length=N)
  }
  seq_diff <- seq[2] - seq[1]
  ret <- matrix(0,length(phi),N)
  cum_ret <- matrix(0,length(phi),N)
  
  for(i in 1:length(phi)){
    ret_a <- pnbinom(pmax(0,c(seq-(seq_diff/2),last(seq)+(seq_diff/2))),mu=mu[i],size=phi[i])
    ret[i,] <- ret_a[2:(N+1)]-ret_a[1:N]
    cum_ret[i,] <- pnbinom(seq,mu=mu[i],size=phi[i])
  }

  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
  
  # as.numeric(Sys.time())*1000 - time
}




##################################################
#################### Bernoulli ###################
##################################################

# Good for plotting distributions
# Calculates the mass. Returns a df of three columns:
# x: 
# density: the mass of the value x
# cum_dist: cumulative mass distribution function
prediction_dens_bernoulli <- function(p){
  if(length(mu)!=length(phi)) stop("mu and phi must be of equal length!")
  
  # p <- rbeta(4000,100,200)
  seq=c(0,1)
  N=2

  ret <- matrix(0,length(p),N)
  cum_ret <- matrix(0,length(p),N)
  
  for(i in 1:length(p)){
    ret[i,] <- dbinom(seq,1,p[i])
    cum_ret[i,] <- pbinom(seq,1,p[i])
  }
  
  
  val <- sapply(1:N,function(j){
    mean(ret[,j])
  })
  cum_dist <- sapply(1:N,function(j){
    mean(cum_ret[,j])
  })
  
  df <- data.frame(x=seq, density=val, cum_dist = cum_dist)
}



#data: from functions prediction_dens_***
#ci value of interval (0.9)
#method: string
ciOfDens <- function(data, ci, method = c("hdi","eti", "hdi2")){
  
  # write.csv(data, "C:/Users/CJW/Desktop/pos.csv")
  # data <- read.csv("C:/Users/CJW/Desktop/pos.csv")
  # data <- data[,-1]
  # plotSeveralAreas(data=list(a=data), countData=F, prop=0.9, x_axis="x", method="hdi", colors=c("red")) +
  #   scale_x_continuous(trans="log10")
  
  approx <- approxfun(x=data$x,y=data$density, yleft=0, yright=0)
  approx_cum <- approxfun(x=data$x,y=data$cum_dist, yleft=0, yright=1)
  approx_cum_rev <- approxfun(x=data$cum_dist,y=data$x, yleft=0, yright=1)
  
  centerPoint <- 0
  centerPointDens <- 0
  lowerDens <- 0
  upperDens <- 0
  interval <- c(low=0,high=0)

  
  if(method == "eti"){
    interval <- c(low=approx_cum_rev((1-ci)/2),high=approx_cum_rev(ci+(1-ci)/2))
    #median
    centerPoint <- approx_cum_rev(0.5)
    centerPointDens <- approx(centerPoint)
    lowerDens <- approx(interval[1])
    upperDens <- approx(interval[2])
  # }else if(method=="hdi"){
    # current_acc <- 1
    # acc <- 1e-6
    # step_size_start <- abs((data$x[length(data$x)]-data$x[1])/length(data$x))
    # step_size <- step_size_start
    # start_val <- data$x[1]
    # signPos <- T
    # while(current_acc > acc){
    #   ret <- HDIFunction(data,ci,start_val)
    #   current_acc <- abs(ret)
    #   if(current_acc < acc) break;
    #   if(step_size_start/step_size > 1e100) return(NULL)
    #   if(ret < 0){
    #     if(signPos) step_size <- step_size/2
    #     signPos <- F
    #     start_val <- start_val - step_size
    #   }else{
    #     if(!signPos) step_size <- step_size/2
    #     signPos <- T
    #     start_val <- start_val + step_size
    #   }
    # }
    # cumLow <- approx_cum(start_val)
    # upper <- approx_cum_rev(cumLow+ci)
    # interval <- c(low=start_val,high=upper)
    # 
    # #median
    # centerPoint <- approx_cum_rev(0.5)
    # centerPointDens <- approx(centerPoint)
    # lowerDens <- approx(interval[1])
    # upperDens <- approx(interval[2])
  }else if(method=="hdi2" || method=="hdi"){
    interval <- HDIFunction2(data,ci)
    
    #median
    centerPoint <- approx_cum_rev(0.5)
    centerPointDens <- approx(centerPoint)
    lowerDens <- approx(interval[1])
    upperDens <- approx(interval[2])
    
  }else{
    stop(paste0("Unknwon method: ", method))
  }
  
  ret <- data.frame(lower=interval[1], center=centerPoint, upper=interval[2], 
                    lowerDens=lowerDens, centerPointDens=centerPointDens,
                    upperDens=upperDens)
  rownames(ret) <- method
  return(ret)
}


HDIFunction <- function(data, ci, lower){
  approx <- approxfun(x=data$x,y=data$density, yleft=0, yright=0)
  approx_cum <- approxfun(x=data$x,y=data$cum_dist, yleft=0, yright=1)
  approx_cum_rev <- approxfun(x=data$cum_dist,y=data$x, yleft=0, yright=1)
  
  densLow <- approx(lower)
  cumLow <- approx_cum(lower)
  if((cumLow+ci) > 1) return(-1)
  cumHigh <- approx_cum_rev(cumLow+ci)
  densHigh <- approx(cumHigh)
  
  return(densHigh-densLow)
}

#Returns the ci-interval with the highest density over all other ci-intervals
HDIFunction2 <- function(data, ci){
  approx <- approxfun(x=data$x,y=data$density, yleft=0, yright=0)
  approx_cum <- approxfun(x=data$x,y=data$cum_dist, yleft=0, yright=1)
  approx_cum_rev <- approxfun(x=data$cum_dist,y=data$x, yleft=min(data$x), yright=max(data$x))
  
  seq <- seq(1e-10,((1-ci)-1e-10), length=1e3)
  ret <- c()
  for(i in seq){
    ret <- c(ret, approx_cum_rev(i+ci) - approx_cum_rev(i))
  }
  
  index <- match(min(ret),ret)
  return(c(approx_cum_rev(seq[index]),approx_cum_rev(seq[index]+ci)))
}

# plot(density(c(rnorm(1e6),rnorm(1e6,10,1))))
# hdi(c(rnorm(1e6),rnorm(1e6,10,1)))
# hdi(c(rnorm(1e6),rnorm(1e6,10,1)), ci=0.5)
