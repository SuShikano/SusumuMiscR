# Mapping Triangle

# ##############################################################################
#' Nagayama-style plot of percentages of three components (e.g. vote share)
#'
#' @param raw.values Data with three variables in column
#' @param label A vector with labels of the three variables
#' @param title A text for the title
#' @return Triangle plot
# ---------------------------------------------------------------------------- #

triangle.plot <- function(raw.values,label,title){
   raw.sum <- apply(raw.values,1,sum)
   values <- raw.values / raw.sum
   horizontal <- (values[,2]-values[,1])*100
   vertical <- (sqrt(3)/2) * values[,3] * 100
   plot(horizontal,vertical,
                   xlim=c(-100,100),ylim=c(0,(sqrt(3)/2)*100),
                   xlab=paste(label[1],"<-    ->",label[2]),ylab=paste("->",label[3]),
                   main=title,axes=F)
    lines(c(-100,0,100,-100),c(0,(sqrt(3)/2)*100,0,0))
}



