# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function creates the empirical (cumulative) distribution function
#'
#' @param formular 
#' @param data Data in long format. To create this data, 
#'                    you can call wide2long.R and use wide2long()
#' @param group.var
#' @param alt.var
#' @return list consisting of coefficients = estimates, 
#'                            std.errors = se, 
#'                            z.values = z.values, 
#'                            p.values = p.values, 
#'                            logLik = -fit$minimum
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
clogit <- function(formula, data, group.var, alt.var) {
  # Extract response and predictors
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)
  
  # Alternative-Specific Constants (ASCs)
  alt.levels <- unique(data[[alt.var]])
  asc.matrix <- model.matrix(~ factor(data[[alt.var]]) - 1)  # Dummies for alternatives
  X <- cbind(asc.matrix[,-1], X[,-1])  # ASCs with predictors
  
  # Extract group information (each choice set)
  groups <- data[[group.var]]
  
  # Log-likelihood function
  log.likelihood <- function(beta) {
    eta <- X %*% beta  # Linear predictor
    exp.eta <- exp(eta)
    
    # Prob of each choice within its group
    probs <- exp.eta / ave(exp.eta, groups, FUN = sum)
    
    # Log-likelihood
    loglik <- sum(y * log(probs))
    return(-loglik)  # Negative because we minimize
  }
  
  # Optimization using Newton-Raphson (via nlm)
  start.vals <- rep(0, ncol(X))  # Initial values (zeros)
  fit <- nlm(log.likelihood, p = start.vals, hessian = TRUE)
  
  # Extract results
  estimates <- fit$estimate
  se <- sqrt(diag(solve(fit$hessian)))  # Standard errors from Hessian
  z.values <- estimates / se  # Z-values
  p.values <- 2 * (1 - pnorm(abs(z.values)))  # P-values
  
  # Return as a list
  return(list(coefficients = estimates, std.errors = se, z.values = z.values, p.values = p.values, logLik = -fit$minimum))
}
