# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function display confidence intervals plot based on 
#'    estimated coefficients and standard errors.
#'
#' @param estimates A vector containing the point estimates.
#' @param se A vector containing the standard errors.
#' @param var.labels A vector containing the variable labels.
#' @param main specifies the main title of the figure. Default is NULL.
#' @param xlab specifies the label on the x-axis.
#' @param var.lab.scale specifies the space for the variable labels. Default is 1. 
#'          Larger values make the space larger, and vice versa.
#' @param lwd.ci Line width of confidence intervals. Default is 1.
#' @param y.positions Location on the y-axis. Default is NULL.
#' @return Figure.
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
coef.ci <- function(
  estimates,
  se,
  var.labels,
  var.lab.scale=1,
  y.positions=NULL,
  lwd.ci=1,
  lty.ci=1,
  ci.col=NULL,
  main=NULL,
  xlab=NULL,
  overlay=FALSE,
  tick.x=NULL,
  xlim=NULL,
  ylim=NULL,
  xylim.output =FALSE,
  pch=20
){
  # some checks
  if(length(estimates)!=length(se)){
    stop("The length of the point estimates is not identical with that of the standard errors.")
  }
  
  # load the scatter.ci function
  source("https://raw.githubusercontent.com/SuShikano/SusumuMiscR/main/R/Scatter_CI.R")
  
  if (is.null(y.positions)) y.positions <- 1:length(estimates)
  y.se <- rep(1,length(y.positions))
  
  # setting the x-axis
  ## maximum value 
  max.x.axis <- max(estimates + se*2.5,na.rm=T)
  if (max.x.axis<0) max.x.axis<- 0
  
  ## minimum value (room for the labels!)
  min.x.axis0 <- min(estimates - se*2.5,na.rm=T)
  if (min.x.axis0 > 0) min.x.axis0 <- 0
  max.length.label <- max(nchar(var.labels),na.rm=T)
  x.range <- max.x.axis - min.x.axis0
  
  min.x.axis <- min.x.axis0 - max.length.label*x.range*0.02*var.lab.scale
  ## where tick-marks?
  if (is.null(tick.x)) tick.x <- seq(round(min.x.axis0),round(max.x.axis))
  
  if (is.null(xlim)) xlim <- c(min.x.axis,max.x.axis)
  if (is.null(ylim)) ylim <- c(max(y.positions),min(y.positions))
  
  if (xylim.output){
    list(xlim=xlim,ylim=ylim)
  }else {
    scatter.ci(x.means=estimates,
               y.means=y.positions,
               x.se = se,
               y.se = y.se,
               x.ci=TRUE,
               y.ci=FALSE,
               ylim=ylim,
               xlim=xlim,
               xlab=xlab,
               overlay=overlay,
               main=main,
               axes=FALSE,
               lwd.ci=lwd.ci,
               lty.ci=lty.ci,
               pch=pch,
               ci.col=ci.col
    )
    axis(1,at=tick.x)
    if (!overlay) text(min.x.axis0,y.positions,var.labels,pos=2)
  }
}
  