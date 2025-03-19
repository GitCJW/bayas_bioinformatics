
#density and quantile function for half-normal
dhalfnorm <- function (x, mean = 0,sd = 1, log = FALSE) {
  dens <- log(2) + dnorm(x, mean = mean, sd = sd, 
                         log = TRUE)
  if (log == FALSE) 
    dens <- exp(dens)
  return(dens)
}
qhalfnorm <- function (p, mean = 0,sd = 1, lower.tail = TRUE, log.p = FALSE) {
  p <- as.vector(p)
  sd <- as.vector(sd)
  if (any(p < 0) || any(p > 1)) 
    stop("p must be in [0,1].")
  if (any(sd <= 0)) 
    stop("The sd parameter must be positive.")
  NN <- max(length(p), length(sd))
  p <- rep(p, len = NN)
  sd <- rep(sd, len = NN)
  if (log.p == TRUE) 
    p <- exp(p)
  if (lower.tail == FALSE) 
    p <- 1 - p
  q <- qnorm((p + 1)/2, mean = mean, sd = sd)
  return(q)
}


# Approximated horseshoe density function. Used for plotting only.
dhs <- function(x, df = 1, global_df = 1, global_scale = 1, 
                       slab_df = Inf, slab_scale = 1, log = FALSE) {
  if (global_scale <= 0 || slab_scale <= 0) stop("Scale parameters must be positive.")

  # Compute expected values for tau and lambda (approximation)
  tau_sq <- global_scale^2 * global_df / (global_df - 2)  # Expected squared global shrinkage
  lambda_sq <- df / (df - 2)  # Expected squared local shrinkage
  
  if (df <= 2) lambda_sq <- df / (df + 1)  # Approximate for df ≤ 2
  if (global_df <= 2) tau_sq <- global_scale^2 * global_df / (global_df + 1)  # Approximate for global_df ≤ 2
  
  # Combined shrinkage parameter
  z_sq <- tau_sq * lambda_sq  # Expected squared shrinkage
  
  # Slab correction (expected variance of slab component)
  if (slab_df > 2) {
    c_sq <- (slab_scale^2) * slab_df / (slab_df - 2)  # Expected slab variance
  } else {
    c_sq <- slab_scale^2 * slab_df / (slab_df + 1)  # Approximate for slab_df ≤ 2
  }
  
  w <- sqrt(c_sq / (c_sq + z_sq))  # Mixing weight
  
  # Compute density
  hs_density <- (1 - w) * 2 / (pi * sqrt(z_sq) * (1 + (x^2 / z_sq))) + w * dnorm(x, sd = sqrt(c_sq))
  
  if (log) return(log(hs_density))
  return(hs_density)
}


# Approximated horseshoe quantile function. Used for plotting only.
qhs <- function(p, df = 1, global_df = 1, global_scale = 1, 
                       slab_df = Inf, slab_scale = 1, lower.tail = TRUE, log.p = FALSE) {
  if (global_scale <= 0 || slab_scale <= 0) stop("Scale parameters must be positive.")
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  
  # Compute expected values for tau and lambda (approximation)
  tau_sq <- global_scale^2 * global_df / (global_df - 2)  
  lambda_sq <- df / (df - 2)  
  
  if (df <= 2) lambda_sq <- df / (df + 1)  
  if (global_df <= 2) tau_sq <- global_scale^2 * global_df / (global_df + 1)  
  
  z_sq <- tau_sq * lambda_sq  
  
  # Slab variance
  if (slab_df > 2) {
    c_sq <- (slab_scale^2) * slab_df / (slab_df - 2)
  } else {
    c_sq <- slab_scale^2 * slab_df / (slab_df + 1)
  }
  
  w <- sqrt(c_sq / (c_sq + z_sq))  
  
  # Compute quantiles
  hs_quantile <- sqrt(z_sq) * tan(pi * (p - 0.5))  # Horseshoe quantile approximation
  slab_quantile <- sqrt(c_sq) * qnorm(p)  # Gaussian slab quantile
  
  return((1 - w) * hs_quantile + w * slab_quantile)
}


rhs <- function(n, df = 1, global_df = 1, global_scale = 1, 
                slab_df = 1, slab_scale = 1) {
  if (global_scale <= 0 || slab_scale <= 0) stop("Scale parameters must be positive.")
  if (df <= 0 || global_df <= 0 || slab_df <= 0) stop("Degrees of freedom must be positive.")
  
  # Sample global shrinkage tau ~ half-t(global_df, 0, global_scale)
  tau <- global_scale * sqrt(global_df / rchisq(n, df = global_df))
  
  # Sample local shrinkage lambda ~ half-t(df, 0, 1)
  lambda <- sqrt(df / rchisq(n, df = df))
  
  # Base horseshoe effect
  z <- tau * lambda
  
  # Sample slab scale squared c^2 ~ Inv-Gamma(0.5 * slab_df, 0.5 * slab_df)
  c2 <- 1 / rgamma(n, shape = 0.5 * slab_df, rate = 0.5 * slab_df) * slab_scale^2
  
  # Mixing weight for slab correction
  w <- sqrt(c2 / (c2 + z^2))
  
  # Horseshoe samples
  hs_samples <- z * rt(n, df = 1)  # Standard Cauchy-like
  
  # Slab samples
  slab_samples <- rnorm(n, mean = 0, sd = sqrt(c2))  # Gaussian slab with sampled variance
  
  # Final mixture
  return((1 - w) * hs_samples + w * slab_samples)
}
