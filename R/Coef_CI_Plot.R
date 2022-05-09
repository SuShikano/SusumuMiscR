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
  main=NULL,
  xlab=NULL
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
  max.x.axis <- max(estimates + se*2.5)
  if (max.x.axis<0) max.x.axis<- 0
  
  ## minimum value (room for the labels!)
  min.x.axis0 <- min(estimates - se*2.5)
  if (min.x.axis0 > 0) min.x.axis0 <- 0
  max.length.label <- max(nchar(var.labels))
  x.range <- max.x.axis - min.x.axis0
  
  min.x.axis <- min.x.axis0 - max.length.label*x.range*0.02*var.lab.scale
  ## where tick-marks?
  tick.x <- seq(round(min.x.axis0),round(max.x.axis))
  
  scatter.ci(x.means=estimates,
             y.means=y.positions,
             x.se = se,
             y.se = y.se,
             x.ci=TRUE,
             y.ci=FALSE,
             ylim=c(max(y.positions),min(y.positions)),
             xlim=c(min.x.axis,max.x.axis),
             xlab=xlab,
             overlay=FALSE,
             main=main,
             axes=FALSE,
             lwd.ci=lwd.ci
             )
  axis(1,at=tick.x)
  text(min.x.axis0,y.positions,var.labels,pos=2)
  
}
  