
##################################################
##################### Normal #####################
##################################################

# Calculates the overlap of two normal distribution given two vectors of mu. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_norm <- function(mu1, mu2, sigma){
  if(length(mu1)!=length(mu2) && length(mu2)!=length(sigma)) stop("mu1,mu2 and sigma must be of equal length!")
  
  intersection <- (mu1+mu2)/2
  ret <- pmin(pnorm(intersection, mu1, sigma),pnorm(intersection, mu2, sigma)) + 
      pmin(1-pnorm(intersection, mu1, sigma),1-pnorm(intersection, mu2, sigma))
  ret[is.nan(ret)] <- 1 # if both are equal
  return(ret)
}


##################################################
###################### Gamma #####################
##################################################

# Calculates the overlap of two Gamma distribution given two vectors of mu. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_gamma <- function(mu1, mu2, shape){
  if(length(mu1)!=length(mu2) && length(mu2)!=length(shape)) stop("mu1,mu2 and shape must be of equal length!")
  
  rate1 <- shape/mu1
  rate2 <- shape/mu2
  intersection <- (log((rate2/rate1)^shape)) / (rate2-rate1)
  
  ret <- pmin(pgamma(intersection, shape=shape, rate=rate1),pgamma(intersection, shape=shape, rate=rate2)) + 
    pmin(1-pgamma(intersection, shape=shape, rate=rate1),1-pgamma(intersection, shape=shape, rate=rate2))
  ret[is.nan(ret)] <- 1 # if both are equal
  
  return(ret)
}

##################################################
################### Exponential ##################
##################################################


# Calculates the overlap of two exponential distribution given two vectors of mu. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_exponential <- function(mu1, mu2){
  if(length(mu1)!=length(mu2)) stop("mu1 and mu2 must be of equal length!")
  
  lambda1 <- 1/mu1
  lambda2 <- 1/mu2
  intersection <- (log(lambda1/lambda2))/(lambda1-lambda2)
  
  ret <- pmin(pexp(intersection, lambda1),pexp(intersection, lambda2)) + 
    pmin(1-pexp(intersection, lambda1),1-pexp(intersection, lambda2))
  ret[is.nan(ret)] <- 1 # if both are equal
  
  return(ret)
}

##################################################
################## Inv-Gaussian ##################
##################################################

# Calculates the overlap of two normal distribution given two vectors of mu. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_invgaussian <- function(mu1, mu2, lambda){
  if(length(mu1)!=length(mu2) && length(mu2)!=length(lambda)) stop("mu1,mu2 and lambda must be of equal length!")
  
  a <- mu2/mu1 *-1
  intersect <- (a*mu1 - mu2)/(a-1)
  
  ret <- pmin(pinvgauss(intersection, mean=mu1, shape=lambda),pinvgauss(intersection, mean=mu2, shape=lambda)) + 
    pmin(1-pinvgauss(intersection, mean=mu1, shape=lambda),1-pinvgauss(intersection, mean=mu2, shape=lambda))
  ret[is.nan(ret)] <- 1 # if both are equal
  
  return(ret)
}



##################################################
###################### Beta ######################
##################################################

# Calculates the overlap of two normal distribution given two vectors of mu. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_beta <- function(mu1, mu2, kappa){
  if(length(mu1)!=length(mu2) && length(mu2)!=length(kappa)) stop("mu1,mu2 and kappa must be of equal length!")
  
  alpha1 <- mu1*kappa
  alpha2 <- mu2*kappa
  beta1 <- (1-mu1)*kappa
  beta2 <- (1-mu2)*kappa
  
  a <- beta(alpha2,beta2)/beta(alpha1,beta1)
  b <- exp((log(a))/(kappa*(mu2-mu1)))
  intersection <- b/(1+b)
  
  ret <- pmin(pbeta(intersection, alpha1, beta1),pbeta(intersection, alpha2, beta2)) + 
    pmin(1-pbeta(intersection, alpha1, beta1),1-pbeta(intersection, alpha2, beta2))
  ret[is.nan(ret)] <- 1 # if both are equal
  
  return(ret)
}


##################################################
#################### Poisson #####################
##################################################

# Calculates the overlap of two poisson distributions given two vectors of lambda. 
# Returns a list of overlapping values of each lambda pair
full_dens_diff_poisson <- function(lambda1, lambda2, acc=1e-4, N=512){
  if(length(lambda1)!=length(lambda2)) stop("lambda1 and lambda2 must be of equal length!")
  ret <- c()
  
  min1 <- qpois(acc, lambda1)
  min2 <- qpois(acc, lambda1)
  max1 <- qpois(1-acc, lambda2)
  max2 <- qpois(1-acc, lambda2)
  
  min <- pmin(min1,min2)
  max <- pmax(max1,max2)
  
  for(i in 1:length(lambda1)){
    N_t <- N
    diff <- max[i] - min[i]
    if(diff < N_t) N_t <- diff
    seq <- seq(min[i], max[i], length=N_t+1)
    
    f1_tmp <- ppois(seq, lambda1[i])
    f1 <- c(f1_tmp[1],f1_tmp[2:N_t]-f1_tmp[1:(N_t-1)], 1-f1_tmp[N_t])
    f2_tmp <- ppois(seq, lambda2[i])
    f2 <- c(f2_tmp[1],f2_tmp[2:N_t]-f2_tmp[1:(N_t-1)], 1-f2_tmp[N_t])
    ret[i] <- sum(pmin(f1, f2))
  }
  # as.numeric(Sys.time())*1000 - time
  
  return(ret)
}


##################################################
#################### Binomial ####################
##################################################

# Calculates the overlap of two binomial distributions given two vectors of p and N.
# Returns a list of overlapping values of each p/N pair
full_dens_diff_binom <- function(p1, p2, N1, N2, N=512, acc=1e-4) {
  ret <- c()
  
  min1 <- qbinom(acc, N1, p1)
  min2 <- qbinom(acc, N2, p2)
  max1 <- qbinom(1-acc, N1, p1)
  max2 <- qbinom(1-acc, N2, p2)
  
  min <- pmin(min1,min2)
  max <- pmin(pmax(max1,max2), N1,N2)
  
  for(i in 1:length(p1)){
    N_t <- N
    diff <- max[i] - min[i]
    if(diff < N_t) N_t <- diff
    seq <- seq(min[i], max[i], length=N_t+1)

    f1_tmp <- pbinom(seq, N1, p1[i])
    f1 <- c(f1_tmp[1],f1_tmp[2:N_t]-f1_tmp[1:(N_t-1)], 1-f1_tmp[N_t])
    f2_tmp <- pbinom(seq, N2, p2[i])
    f2 <- c(f2_tmp[1],f2_tmp[2:N_t]-f2_tmp[1:(N_t-1)], 1-f2_tmp[N_t])
    ret[i] <- sum(pmin(f1, f2))
  }
  return(ret)
}



##################################################
################## Neg-Binomial ##################
##################################################

# Calculates the overlap of two Neg-Binomial distributions given two vectors of mu and phi. 
# Returns a list of overlapping values of each mu pair
full_dens_diff_negbinom <- function(mu1, mu2, phi, N=512, acc=1e-4){
  
  if(length(mu1)!=length(mu2) || length(mu1)!=length(phi)) stop("mu1, mu2  and phi must be of equal length!")
  
  # time <- as.numeric(Sys.time())*1000
  
  ret <- c()
  
  min1 <- qnbinom(acc, mu=mu1, size=phi)
  min2 <- qnbinom(acc, mu=mu2, size=phi)
  max1 <- qnbinom(1-acc, mu=mu1, size=phi)
  max2 <- qnbinom(1-acc, mu=mu2, size=phi)
  
  min <- pmin(min1,min2)
  max <- pmax(max1,max2)
  
  for(i in 1:length(mu1)){
    N_t <- N
    diff <- max[i] - min[i]
    if(diff < N_t) N_t <- diff
    seq <- seq(min[i], max[i], length=N_t+1)
    
    f1_tmp <- pnbinom(seq, mu=mu1[i], size=phi[i])
    f1 <- c(f1_tmp[1],f1_tmp[2:N_t]-f1_tmp[1:(N_t-1)], 1-f1_tmp[N_t])
    f2_tmp <- pnbinom(seq, mu=mu2[i], size=phi[i])
    f2 <- c(f2_tmp[1],f2_tmp[2:N_t]-f2_tmp[1:(N_t-1)], 1-f2_tmp[N_t])
    ret[i] <- sum(pmin(f1, f2))
  }
  # as.numeric(Sys.time())*1000 - time
  
  return(ret)
}



##################################################
#################### Bernoulli ###################
##################################################

# Calculates the overlap of two Bernoulli distributions given two vectors of p. 
# Returns a list of overlapping values of each p pair
full_dens_diff_bernoulli <- function(p1, p2){
  
  if(length(p1)!=length(p2)) stop("p1 and p2 must be of equal length!")
  
  ret <- c()
  seq <- c(0,1)
  for(i in 1:length(p1)){

    f1 <- dbinom(seq, 1, p1[i])
    f2 <- dbinom(seq, 1, p2[i])
    ret[i] <- sum(pmin(f1, f2))
  }

  return(ret)
}


#Function for overlap (integrate over)
int_f <- function(x, mu1, mu2, sd1, sd2) {
  f1 <- dnorm(x, mean=mu1, sd=sd1)
  f2 <- dnorm(x, mean=mu2, sd=sd2)
  pmin(f1, f2)
}
int_normal <- function(mu1, mu2, sigma1, sigma2){
  return(integrate(int_f, -Inf, Inf,mu1, mu2, sigma1, sigma2)$value)
}
