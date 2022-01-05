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
emp.dist.func <- function(x){
  freq.x <- table(x)
  cum.freq.x <- cumsum(freq.x)/max(cumsum(freq.x))
  x.axis <- c(0,cum.freq.x,1)
  names(x.axis)[1] <- 0
  names(x.axis)[length(x.axis)] <- length(x.axis)-1

  x.lim <- c(min(as.numeric(names(x.axis)))+.5,max(as.numeric(names(x.axis)))+.5)

  plot(names(cum.freq.x),cum.freq.x,type="s",ylim=c(0,1),
       xlim=,
       axes=F,xlab="Response",ylab="Relative Freq")
  axis(1,at=as.numeric(names(cum.freq.x)))
  axis(2,at=c(0,1/4,1/2,3/4,1))
  }

