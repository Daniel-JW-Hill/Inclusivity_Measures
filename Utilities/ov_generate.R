
# This function generates a new variable Z
# Which has defined correlation structures with a variable X and Y
# THis creates hypothetical omitted variables in which we can explroe a potential range 
# of causal inference for a given variable X. 

#This function is adapted from 
# https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables

ov_generate <- function(y, rho, x, threshold=1e-12) {
  #
  # Process the arguments.
  #
  if(!is.matrix(y)) y <- matrix(y, ncol=1)
  d <- ncol(y)
  n <- nrow(y)
  y <- scale(y, center=FALSE) # Makes computations simpler
  if (missing(x)) x <- rnorm(n)
  #
  # Remove the effects of `y` on `x`.
  #
  e <- residuals(lm(x ~ y))
  #
  # Calculate the coefficient `sigma` of `e` so that the correlation of
  # `y` with the linear combination y.dual %*% rho + sigma*e is the desired
  # vector.
  #
  y.dual <- with(svd(y), (n-1)*u %*% diag(ifelse(d > threshold, 1/d, 0)) %*% t(v))
  sigma2 <- c((1 - rho %*% cov(y.dual) %*% rho) / var(e))
  #
  # Return this linear combination.
  #
  if (sigma2 >= 0) {
    sigma <- sqrt(sigma2) 
    z <- y.dual %*% rho + sigma*e
  } else {
    warning("Correlations are impossible.")
    z <- rep(0, n)
  }
  return(z)
}