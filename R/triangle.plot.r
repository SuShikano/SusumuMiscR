# Mapping Triangle

# ##############################################################################
#' Nagayama-style plot of percentages of three components (e.g. vote share)
#'
#' @param raw.values Data with three variables in column
#' @param label A vector with labels of the three variables
#' @param title A text for the title
#' @return Triangle plot
# ---------------------------------------------------------------------------- #

triangle.plot <- function(raw.values,
                          label=c("A","B","C"),
                          title="",
                          col="black",
                          pch=1,
                          label.space = 1,
                          cex=1,
                          ...){
   raw.sum <- apply(raw.values,1,sum)
   values <- raw.values / raw.sum
   horizontal <- (values[,2]-values[,1])*100
   vertical <- (sqrt(3)/2) * values[,3] * 100
   plot(horizontal,vertical,
                   xlim=c(-100,100)*label.space,
                   ylim=c(0-100*(label.space-1),
                          (sqrt(3)/2)*100*label.space),
                   xlab="",ylab="",
                   #xlab=paste(label[1],"<-    ->",label[2]),ylab=paste("->",label[3]),
                   main=title,axes=F,
                   col=col,
                  pch=pch,
        cex=cex)
    lines(c(-100,0,100,-100),c(0,(sqrt(3)/2)*100,0,0))
    lines(rep(0,2),c(0,(sqrt(3)/2)*100),lty=2)
    lines(c(-100,50),c(0,(sqrt(3)/2)*50),lty=2)
    lines(c(100,-50),c(0,(sqrt(3)/2)*50),lty=2)
    text(-100,0,label[1],pos=1)
    text(100,0,label[2],pos=1)
    text(0,(sqrt(3)/2)*100,label[3],pos=3)
}



