# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function estimate the population mean
#'
#' @param x Sample data
#' @param perc.ci The range of the confidence interval. Default is .95
#' @param pop.var The population variance. If it is unknown, NULL, which is default. 
#' @param print Logical. If TRUE, the obtained estimates are printed. 
#' @return Graphics
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################

pop.mean.estimation <- function(x,
                                perc.ci=0.95,
                                pop.var=NULL,
                                print=TRUE){
  
  x.original <- x
  x <- x[!is.na(x)]
  
  lower.perc <- (1-perc.ci)/2
  upper.perc <- 1-lower.perc

  
  sample.size <- length(x)
  
  sample.mean <- mean(x)
  
  sample.var <- mean((x - sample.mean)^2)
  
  standard.err <- sqrt(sample.var/(sample.size-1))
  
  if (is.null(pop.var)){
    lower.bound <- sample.mean + standard.err * qt(lower.perc, df = sample.size -1)
    upper.bound <- sample.mean + standard.err * qt(upper.perc, df = sample.size -1)
  }else{
    lower.bound <- sample.mean + sqrt(pop.var/sample.size) * qnorm(lower.perc)
    upper.bound <- sample.mean + sqrt(pop.var/sample.size) * qnorm(upper.perc)
  }
  
  if (print){
    print(c("Sample size",sample.size ))
    print(c("Sample mean",sample.mean ))
    print(c("Sample variance",sample.var))
    print(c("Standard error",standard.err))
    print(c("Lower bound of CI",lower.bound ))
    print(c("Upper bound of CI",upper.bound ))
  }
  
  output <- list(data = x.original,
                 perc.ci = perc.ci,
                 pop.var=pop.var,
                 size = sample.size,
                 mean = sample.mean,
                 var = sample.var,
                 se = standard.err,
                 ci = c(lower.bound,upper.bound)
  )
  
  output 
  
}

