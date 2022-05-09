# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function makes a scatter plot with confidence intervals on x and y axes.
#'
#' @param x.means A vector representing the means on the x-axis.
#' @param y.means A vector representing the means on the y-axis.
#' @param x.se A vector representing the standard errors on the x-axis.
#' @param y.se A vector representing the standard errors on the y-axis.
#' @param x.ci Logical. If FALSE, confidence intervals on x-axis do not appear. 
#'        Default is TRUE.
#' @param y.ci Logical. If FALSE, confidence intervals on y-axis do not appear. 
#'        Default is TRUE.
#' @param xlim Range of the x-axis.
#' @param ylim Range of the y-axis.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param axes Logical. If FALSE, no axes appear. Defalt is TRUE.
#' @param main Title over the figure.
#' @param pch Type of dotts. Default is 20.
#' @param ci.col Color of confidence intervals. Default is grey(.7).
#' @param dot.col Color of dots. Default is black.
#' @param ci.percent The percentage for the confindence intervals.
#'         Default is .95.
#' @param overlay If TRUE, a figure without axes and labels is generated
#'        so that it can be overlaid on the existing figure.
#'        Default is FALSE.
#' @return Figure.
#' @examples
#' scatter.ci(x,y,x.se,y.se)
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
scatter.ci <- function(x.means,y.means,
                       x.se,y.se,
                       x.ci=TRUE,y.ci=TRUE,
                       ci.percent=0.95,
                       pch=20,
                        xlab="",ylab="",
                        xlim=NULL,ylim=NULL,
                        overlay=FALSE,
                        ci.col=NULL,
                        dot.col="black",
                       axes=TRUE,
                       main=NULL,...){

  if(length(x.means)!=length(x.means)){
    stop("The length of mean vectors is not identical.")
  }
  if(length(x.means)!=length(x.se)|length(y.means)!=length(y.se)){
    stop("The length of a mean vector and a se vector is not identical.")
  }

  percentile <- (1-ci.percent)/2
  percentile <- c(percentile,1-percentile)
  multiplier <- qnorm(percentile)

  if (is.null(xlim)){
    xlim <- c(
      min(x.means+x.se*multiplier[1],na.rm=T),
      max(x.means+x.se*multiplier[2],na.rm=T)
    )
    }
  if (is.null(ylim)){
    ylim <- c(
      min(y.means+y.se*multiplier[1],na.rm=T),
      max(y.means+y.se*multiplier[2],na.rm=T)
    )
  }
  if (is.null(ci.col)){
    ci.col <- rep(grey(.7),length(x.means))
  }

  if (overlay){
      plot(0,0,
           ylim=ylim,xlim=xlim,type="n",axes=FALSE,ann=FALSE)
  }else{
    plot(0,0,ylab=ylab,xlab=xlab,main=main,axes=axes,
         ylim=ylim,xlim=xlim,type="n")
  }
  # confidence itnervals
    for (i in 1:length(x.means)){
      if (y.ci){
        lines(rep(x.means[i],2),y.means[i]+multiplier*y.se[i],col=ci.col[i])
      }
      if (x.ci){
        lines(x.means[i]+multiplier*x.se[i],rep(y.means[i],2),col=ci.col[i])
      }
    }
    par(new=T)
    plot(x.means,y.means,pch=pch,ylim=ylim,xlim=xlim,axes=F,ann=F,col=dot.col)
}
