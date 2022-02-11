# ##############################################################################
#' Drawing simple bootstrap samples
#'
#' @param x matrix. Data from which bootstrap samples will be drawn.
#' @param n.sim
#' @param sample.size
#' @param statistic
#' @return A list with the computed duration and start/end time measures
#'         in the POSIX class.
# ---------------------------------------------------------------------------- #
bootstr <- function(x=NULL,
                     n.sim = 1000,
                     sample.size = NULL,
                     statistic = "mean",
                     interval = 0.95
                     ){
 
  if (!is.matrix(x)) stop("x must be a matrix.")
  
  if (!is.null(sample.size)) {
    sample.size <- sample.size
  } else {
      sample.size <- nrow(x)
    }

  collected.statistic <- matrix(NA,nrow=n.sim,ncol=ncol(x))
  
  for (i.sim in 1:n.sim){
    
    selected.row <- sample(1:nrow(x),size=sample.size, replace=T)
    
    this.boot.sample <- x[selected.row,]
    
    collected.statistic[i.sim,] <- apply(this.boot.sample,2,statistic,na.rm=T)
  }
  
 mean.est <- apply(collected.statistic,2,mean,na.rm=T)
 var.est  <- apply(collected.statistic,2,var,na.rm=T)
 
 interval.prob <- c((1-interval)/2,1-(1-interval)/2)
 
 interval.est <- apply(collected.statistic,2,quantile,prob=interval.prob,na.rm=T)
 
 list(mean.est = mean.est,
      var.est = var.est,
      interval.est = interval.est,
      all.values = collected.statistic)   
}
    
  


