
#density and quantile function for half-normal
#Replaced by truncnorm_at_0
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




dtruncnorm_at_0 <- function(x, mean = 0, sd = 1, log = FALSE) {
  if (sd <= 0) stop("sd must be positive.")
  if (any(x < 0)) {
    # Handle values outside the truncated range.
    # For values <= 0, the density is 0.
    dens <- rep(-Inf, length(x)) # -Inf for log density
    dens[x > 0] <- dnorm(x[x > 0], mean = mean, sd = sd, log = TRUE) - pnorm(0, mean = mean, sd = sd, lower.tail = FALSE, log.p = TRUE)
  } else {
    dens <- dnorm(x, mean = mean, sd = sd, log = TRUE) - pnorm(0, mean = mean, sd = sd, lower.tail = FALSE, log.p = TRUE)
  }
  
  if (log == FALSE) {
    dens <- exp(dens)
    # Ensure density is 0 for x <= 0 if exp was applied to -Inf
    dens[x <= 0] <- 0
  }
  return(dens)
}

# R-like implementation for qtruncnorm (truncated at 0)
qtruncnorm_at_0 <- function(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) {
  if (sd <= 0) stop("sd must be positive.")
  if (any(p < 0) || any(p > 1)) stop("p must be in [0,1].")
  
  if (log.p == TRUE) p <- exp(p)
  if (lower.tail == FALSE) p <- 1 - p
  
  # Calculate the CDF value of 0 in the original normal distribution
  cdf_0_orig <- pnorm(0, mean = mean, sd = sd, lower.tail = TRUE)
  
  # Map p from the truncated distribution's scale to the original distribution's scale
  # The probability for the original normal distribution corresponding to p
  # on the truncated distribution is:
  # p_orig = cdf_0_orig + p * (1 - cdf_0_orig)
  p_orig <- cdf_0_orig + p * (1 - cdf_0_orig)
  
  # Calculate the quantile using the original normal distribution's quantile function
  q <- qnorm(p_orig, mean = mean, sd = sd)
  
  # Ensure that the returned quantile is within the truncated range (x >= 0)
  # This should generally hold if p_orig calculation is correct, but good for robustness.
  # If q is somehow slightly negative due to floating point, floor it at 0.
  q[q < 0] <- 0
  
  return(q)
}


# R-like implementation for dtrunccauchy (truncated at 0)
dtrunccauchy_at_0 <- function(x, location = 0, scale = 1, log = FALSE) {
  if (scale <= 0) stop("scale must be positive.")
  
  # Calculate the denominator (probability of X > 0)
  prob_gt_0 <- pcauchy(0, location = location, scale = scale, lower.tail = FALSE)
  
  if (any(x < 0)) {
    dens <- rep(-Inf, length(x)) # -Inf for log density
    valid_x_idx <- which(x > 0)
    
    if (length(valid_x_idx) > 0) {
      dens[valid_x_idx] <- dcauchy(x[valid_x_idx], location = location, scale = scale, log = TRUE) - log(prob_gt_0)
    }
  } else {
    dens <- dcauchy(x, location = location, scale = scale, log = TRUE) - log(prob_gt_0)
  }
  
  if (log == FALSE) {
    dens <- exp(dens)
    dens[x <= 0] <- 0 # Ensure density is 0 for x <= 0
  }
  return(dens)
}

# R-like implementation for qtrunccauchy (truncated at 0)
qtrunccauchy_at_0 <- function(p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  if (scale <= 0) stop("scale must be positive.")
  if (any(p < 0) || any(p > 1)) stop("p must be in [0,1].")
  
  if (log.p == TRUE) p <- exp(p)
  if (lower.tail == FALSE) p <- 1 - p
  
  # Calculate the CDF value of 0 in the original Cauchy distribution
  cdf_0_orig <- pcauchy(0, location = location, scale = scale, lower.tail = TRUE)
  
  # Map p from the truncated distribution's scale to the original distribution's scale
  # The probability for the original Cauchy distribution corresponding to p
  # on the truncated distribution is:
  # p_orig = cdf_0_orig + p * (1 - cdf_0_orig)
  p_orig <- cdf_0_orig + p * (1 - cdf_0_orig)
  
  # Calculate the quantile using the original Cauchy distribution's quantile function
  q <- qcauchy(p_orig, location = location, scale = scale)
  
  # Ensure that the returned quantile is within the truncated range (x >= 0)
  # This should generally hold if p_orig calculation is correct.
  q[q < 0] <- 0
  
  return(q)
}

# R-like implementation for dtrunct (truncated at 0)
dtrunct_at_0 <- function(x, df, location = 0, scale = 1, log = FALSE) {
  if (df <= 0) stop("df (degrees of freedom) must be positive.")
  if (scale <= 0) stop("scale must be positive.")
  
  # Calculate the denominator (probability of X > 0)
  prob_gt_0 <- pt(0, df = df, ncp = location / scale, lower.tail = FALSE) # ncp handles non-central t for location
  
  # Handle cases where x is outside the truncated range
  if (any(x < 0)) {
    dens <- rep(-Inf, length(x)) # -Inf for log density
    valid_x_idx <- which(x > 0)
    
    if (length(valid_x_idx) > 0) {
      # Calculate log density for original t-distribution
      # Using a non-central t-distribution for location
      # For a general t-distribution (location, scale), the density is:
      # dt((x - location) / scale, df = df) / scale
      log_dens_orig <- dt((x[valid_x_idx] - location) / scale, df = df, log = TRUE) - log(scale)
      dens[valid_x_idx] <- log_dens_orig - log(prob_gt_0)
    }
  } else {
    log_dens_orig <- dt((x - location) / scale, df = df, log = TRUE) - log(scale)
    dens <- log_dens_orig - log(prob_gt_0)
  }
  
  if (log == FALSE) {
    dens <- exp(dens)
    dens[x <= 0] <- 0 # Ensure density is 0 for x <= 0
  }
  return(dens)
}

# R-like implementation for qtrunct (truncated at 0)
qtrunct_at_0 <- function(p, df, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) {
  if (df <= 0) stop("df (degrees of freedom) must be positive.")
  if (scale <= 0) stop("scale must be positive.")
  if (any(p < 0) || any(p > 1)) stop("p must be in [0,1].")
  
  if (log.p == TRUE) p <- exp(p)
  if (lower.tail == FALSE) p <- 1 - p
  
  # Calculate the CDF value of 0 in the original t-distribution
  cdf_0_orig <- pt(0, df = df, ncp = location / scale, lower.tail = TRUE)
  
  # Map p from the truncated distribution's scale to the original distribution's scale
  # p_orig = cdf_0_orig + p * (1 - cdf_0_orig)
  p_orig <- cdf_0_orig + p * (1 - cdf_0_orig)
  
  # Calculate the quantile using the original t-distribution's quantile function
  q <- qt(p_orig, df = df, ncp = location / scale)
  
  # Ensure that the returned quantile is within the truncated range (x >= 0)
  q[q < 0] <- 0
  
  return(q)
}