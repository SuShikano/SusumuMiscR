# ##############################################################################
#' Customized barplot
#'
#' @param tab.out An output object of table().
#' @param angle
#' @param main
#' @param ylab
#' @param ylim
#' @param cex
#' @return 
# ---------------------------------------------------------------------------- #

barplot_custom <- function(tab.out=NULL,angle=45,main=NULL,ylab=NULL,ylim=NULL,cex=0.6){
  plt <- barplot(tab.out, col='steelblue', 
                 xaxt="n",
                 main=main,ylab=ylab,
                 ylim=ylim)
  text(plt, par("usr")[3], 
       labels = names(tab.out), srt = angle, 
       adj = c(1.1,1.1), xpd = TRUE, cex=cex) 
}