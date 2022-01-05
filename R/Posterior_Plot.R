# ##############################################################################
#' Plot posterior from a posterior matrix
#'
#' @param post.mat POsterior matrix. The rows correspond to iterations. Default is NULL.
#' @param main Title of the figure.
#' @param para.label Labels of the parameters.
#' @param density.height Relative height of the density plot. Default is 0.8.
#' @return A plot of posterior distributions
# ---------------------------------------------------------------------------- #
plot.posterior <- function(post.mat=NULL,
                     main=NULL,
                     para.label = NULL,
                     density.height = 0.8
){

  n.para <- ncol(post.mat)
  post.range <- range(unlist(post.mat),na.rm=T)

  if (is.null(para.label)){
    para.label <- colnames(post.mat)
  }

  plot(0,type="n",xlim=post.range,ylim=c(n.para,0),
       axes=F,ylab="",xlab="Posterior",
       main=main)
  axis(1)
  #axis(2,at=c(1:n.para),c(para.label))
  for (i.para in 1:n.para){
    par(new=T)
    this.density <- density(post.mat[,i.para])
    this.density$y <- this.density$y*density.height / max(this.density$y)
    plot(this.density$x,this.density$y*-1+i.para,type="l",
         xlim=post.range,ylim=c(n.para,0),
         ann=F,axes=F)
    # label for parameters
    text(this.density$x[which(this.density$y==max(this.density$y))],
         -1*density.height+i.para,
         para.label[i.para],
         pos=3)
    # posterior percentage
    this.perc <- round(table(post.mat[,i.para]>0)*100/nrow(post.mat))
    text(min(this.density$x),-0.1+i.para,
         paste0(this.perc[1],"%"),pos=4)
    text(max(this.density$x),-0.1+i.para,
         paste0(this.perc[2],"%"),pos=2)
  }
  abline(h=c(1:n.para))
  #abline(v=0,lty=2)
}



