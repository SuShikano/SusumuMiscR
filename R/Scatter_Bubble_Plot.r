# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function makes a bubble plot
#'
#' @param x.var A vector representing the variable for the horizontal axis.
#' @param y.var A vector representing the variable for the vertical axis.
#' @param xlim Range of the x-axis.
#' @param ylim Range of the y-axis.
#' @param axes If FALSE no axes will be shown. Deafult is TRUE.
#' @param bubble.size It specifies the size of bubble. The default is 0.4.
#' @return Figure.
#' @examples
#' bubble.plot(x.var=xs,y.var=ys)
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
bubble.plot <- function(x.var,y.var,
                        xlab=NULL,ylab=NULL,
                        xlim=NULL,ylim=NULL,
                        col="black",
                        filling.col=NA,
                        add=FALSE,
                        axes=TRUE,
                        bubble.size=.4,
                        log=FALSE,
                        log10=FALSE,
                        main=NULL,
                        ...){
  tab <- table(x.var,y.var) # Hier x- und y-Achse
  tab <- as.data.frame(tab)
  tab[tab[,3]==0,3] <- NA
  if (log) tab[,3] <- log(tab[,3])
  if (log10) tab[,3] <- log10(tab[,3])
  if (is.null(xlim)) xlim <- range(x.var,na.rm=T)
  if (is.null(ylim)) ylim <- range(y.var,na.rm=T)
  symbols(as.numeric(as.character(tab[,1])),as.numeric(as.character(tab[,2])),
         circles = tab[,3],inches=bubble.size,
         xlab=xlab,ylab=ylab,
         xlim=xlim,ylim=ylim,
         fg=col,bg=filling.col,
         add=add,
         axes=axes,
         main=main
         )
}
