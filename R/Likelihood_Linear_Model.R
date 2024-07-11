# ##############################################################################
#' Plotting likelihood values for linear regression models
#'
#' @param x matrix. Data from which bootstrap samples will be drawn.
#' @param size
#' @param mu
#' @param prec
#' @return Density
# ---------------------------------------------------------------------------- #
plot.likelihood.regression <- function(x=NULL,
                          y=NULL,
                          intercept=NULL,
                          slope = NULL,
                          err.var = NULL
){
  
  if (is.null(x)|is.null(y)|is.null(intercept)|is.null(slope)|is.null(err.var)){
    stop("Input the complete set of parameters.")
  }
  
  plot(y ~ x,xlim=range(x),ylim=range(y))
  abline(coef=c(intercept,slope))
  par(new=T)
  plot(predicted ~ x,xlim=range(x),ylim=range(y),pch=19,col="red",ann=F,axes=F)
  
  y.values <- seq(min(y),max(y),length.out = 100)
  for (i in 1:length(y)){
    temp.density <- dnorm(y.values,mean=predicted[i],sd=sqrt(norm.var)) + x[i]
    abline(v=x[i],lty=2,col="pink")
    par(new=T)
    plot(y.values ~ temp.density,
         xlim=range(x),ylim=range(y),
         type="l",col="blue",ann=F,axes=F)
    lines(c(x[i],dnorm(y[i],mean=predicted[i],sd=sqrt(norm.var)) + x[i]),
          rep(y[i],2),col="green",lwd=2)
    
  }  
}




