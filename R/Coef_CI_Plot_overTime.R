# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function display confidence intervals plot based on 
#'    estimated coefficients and standard errors of multiple models over time.
#'    The plot will be horizontal.
#'
#' @param estimates A vector containing the point estimates.
#' @param se A vector containing the standard errors.
#' @param timepoints A vector containing various time points.
#' @param main specifies the main title of the figure. Default is NULL.
#' @param xlab specifies the label on the x-axis.
#' @param var.lab.scale specifies the space for the variable labels. Default is 1. 
#'          Larger values make the space larger, and vice versa.
#' @param lwd.ci Line width of confidence intervals. Default is 1.
#' @return Figure.
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
coef.ci.time <- function(
    estimates,
    se,
    timepoints,
    tp.labels=NULL,
    x.positions=NULL,
    var.lab.scale=1,
    pch=20,
    lwd.ci=1,
    lty.ci=1,
    ci.col=NULL,
    main=NULL,
    ylab=NULL,
    overlay=FALSE,
    xlim=NULL,
    ylim=NULL,
    xylim.output =FALSE
){
  # some checks
  if(length(estimates)!=length(se)){
    stop("The length of the point estimates is not identical with that of the standard errors.")
  }
  if (is.null(timepoints)) {
    stop("The parameter timepoints has to be specified.")
    }
  if(length(estimates)!=length(timepoints)){
    stop("The length of the point estimates is not identical with that of the time points.")
  }
  
  # load the scatter.ci function
  source("https://raw.githubusercontent.com/SuShikano/SusumuMiscR/main/R/Scatter_CI.R")
  
  
  if (is.null(x.positions)) x.positions <- timepoints
  if (is.null(tp.labels)) tp.labels <- timepoints
  
  x.se <- rep(1,length(timepoints))
  
  # setting the y-axis
  ## maximum value 
  max.y.axis <- max(estimates + se*2.5,na.rm=T)
  if (max.y.axis<0) max.y.axis<- 0
  
  ## minimum value 
  min.y.axis <- min(estimates - se*2.5,na.rm=T)
  if (min.y.axis > 0) min.y.axis <- 0
  y.range <- max.y.axis - min.y.axis
  
  ## where tick-marks?
  tick.y <- seq(round(min.y.axis),round(max.y.axis))
  
  if (is.null(ylim)) ylim <- c(min.y.axis,max.y.axis)
  if (is.null(xlim)) xlim <- range(timepoints,na.rm=T)
  
  if (xylim.output){
    list(xlim=xlim,ylim=ylim)
  }else {
    scatter.ci(y.means=estimates,
               x.means=x.positions,
               y.se = se,
               x.se = x.se,
               x.ci=FALSE,
               y.ci=TRUE,
               ylim=ylim,
               xlim=xlim,
               ylab=ylab,
               overlay=overlay,
               main=main,
               axes=FALSE,
               lwd.ci=lwd.ci,
               lty.ci=lty.ci,
               ci.col=ci.col,
               pch=pch
    )
    axis(2,at=tick.y)
    #if (!overlay) text(min.y.axis,timepoints,timepoints,pos=2)
    axis(1,at=timepoints,tp.labels)
  }
}