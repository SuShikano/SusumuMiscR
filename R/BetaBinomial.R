# ##############################################################################
#' Beta binomial distribution
#'
#' @param x matrix. Data from which bootstrap samples will be drawn.
#' @param size
#' @param mu
#' @param prec
#' @return Density
# ---------------------------------------------------------------------------- #
dBetaBinomial <- function(x=NULL,
                    size=NULL,
                    mu = NULL,
                    prec = NULL
){
  
  
  alpha <- (prec -1)*mu
  beta <- (prec -1)* (1- mu )

  density <- choose(size,x)*beta(x+alpha,size-x+beta)/beta(alpha,beta)
  
  density
  
}




