# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function creates the empirical (cumulative) distribution function
#'
#' @param x
#' @return Graphics
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
emp.dist.func <- function(x,xlab="x",lwd=2,main=NULL){
  freq.x <- table(x) # frequency table
  cum.freq.x <- cumsum(freq.x)/max(cumsum(freq.x)) # cumulative frequency
  x.axis <- as.numeric(names(cum.freq.x))
  x.axis <- c(min(x.axis)-1 , x.axis , max(x.axis)+1)
  
  
  cum.freq.x <- c(0,cum.freq.x,1)
  

  x.lim <- range(x.axis)

  plot(x.axis[2:(length(x.axis)-1)],cum.freq.x[2:(length(x.axis)-1)],pch=19,
       xlim=x.lim,
       ylim=c(0,1),
       xlab=xlab,
       ylab=expression(P(X <=  x)),
       main=main)
  par(new=T)
  plot(x.axis[2:(length(x.axis)-1)],cum.freq.x[1:(length(x.axis)-2)],pch=1,
       xlim=x.lim,
       ylim=c(0,1),
       ann=F,axes=F)
  for (i in 1:(length(x.axis)-1)){
    lines(x.axis[i:(i+1)],rep(cum.freq.x[i],2),lwd=lwd)
  }
  

  
  
  
  }

