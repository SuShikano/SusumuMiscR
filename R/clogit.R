clogit <- function(formula, data, group_var, alt_var) {
  # Extract response and predictors
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data)
  
  # Create Alternative-Specific Constants (ASCs)
  alt_levels <- unique(data[[alt_var]])
  asc_matrix <- model.matrix(~ factor(data[[alt_var]]) - 1)  # Create dummies for alternatives
  X <- cbind(asc_matrix[,-1], X[,-1])  # Combine ASCs with predictors
  
  # Extract group information (each choice set)
  groups <- data[[group_var]]
  
  # Log-likelihood function
  log_likelihood <- function(beta) {
    eta <- X %*% beta  # Compute linear predictor
    exp_eta <- exp(eta)
    
    # Compute probability of each choice within its group
    probs <- exp_eta / ave(exp_eta, groups, FUN = sum)
    
    # Log-likelihood
    loglik <- sum(y * log(probs))
    return(-loglik)  # Negative because we minimize
  }
  
  # Optimization using Newton-Raphson (via nlm)
  start_vals <- rep(0, ncol(X))  # Initial values (zeros)
  fit <- nlm(log_likelihood, p = start_vals, hessian = TRUE)
  
  # Extract results
  estimates <- fit$estimate
  se <- sqrt(diag(solve(fit$hessian)))  # Standard errors from Hessian
  z_values <- estimates / se  # Z-values
  p_values <- 2 * (1 - pnorm(abs(z_values)))  # P-values
  
  # Return as a list
  return(list(coefficients = estimates, std_errors = se, z_values = z_values, p_values = p_values, logLik = -fit$minimum))
}
