# rasch.estimate
# ##############################################################################
#' Esatimateion of Rasch models
#'
#' @param Y Data matrix, with observations in row and items in column
#' @return An object with the data matrix, JAGS output object, identified item difficulty and latent variables
# ---------------------------------------------------------------------------- #

rasch.estimate <- function(Y){

# Rasch model
library(rjags)
rasch <- "model{
for (i in 1:N){ # loop for individual respondents
for (j in 1:J){ # loop for items
Y[i,j] ~ dbern(p[i,j])
p[i,j] <- exp.diff[i,j]/(1+exp.diff[i,j])
exp.diff[i,j]  <- exp(x[i] - beta[j])
}
}

# PRIORS:
for (i in 1:N){x[i] ~ dnorm(0,0.1)}
for (j in 1:J){beta[j] ~ dnorm(0,0.01)}

}"
write(rasch, "rasch.bug")

N <- nrow(Y)
J <- ncol(Y)

jags.data <- list(Y =Y, N=N,J=J)

## Running JAGS
jags.rasch <- jags.model(data=jags.data,
                         file="rasch.bug",
                         n.chains=3)

update(jags.rasch, 4000)
load.module('dic')
jags.rasch.out <- coda.samples(jags.rasch,
                               variable.names=c("deviance","beta","x"),
                               n.iter=2000, thin=5)

# identify the scale
posterior <- as.matrix(jags.rasch.out)
x <- posterior[,substr(colnames(posterior),1,2)=="x["]
beta <- posterior[,substr(colnames(posterior),1,5)=="beta["]

means <- apply(x,1,mean) # für jede iteration wird für jede dimension seperate der Mean ausgercehnet
sds <- apply(x,1,sd)  # warum wird hier multipliziert? # dimensionsweise varianz
x.st <- (x - means)/sds
beta.st <- (beta - means)/sds

list(Y=Y,beta.st=beta.st,x.st=x.st,jags.out = jags.rasch.out)
}



plot.irt.curve <- function(irt.object,only.items=TRUE){

  label.item <- colnames(irt.object$Y)

  x.st <- irt.object$x.st
  beta.st <- irt.object$beta.st

  n.item <- ncol(beta.st)

  mean.x <- apply(x.st,2,mean)
  ci.x <- apply(x.st,2,quantile,pr=c(0.025,0.975))
  mean.x <- mean.x[order(ci.x[1,])]
  ci.x <- ci.x[,order(ci.x[1,])]

  x.scal <- seq(range(ci.x)[1],range(ci.x)[2],by=0.01)
  all.upper <- all.lower <- matrix(NA,ncol=ncol(beta.st),nrow=length(x.scal))
  for (i.item in 1:6){
    for (i in 1:length(x.scal)){
      this.exp.diff  <- exp(x.scal[i] - beta.st[,i.item] )
      this.p <- this.exp.diff/(1+this.exp.diff)
      this.ci <- quantile(this.p,pr=c(0.025,0.975))
      all.lower[i,i.item] <- this.ci[1]
      all.upper[i,i.item] <- this.ci[2]
    }
  }

# plot of IR curve

  plot(0,type="n",
       xlim=range(x.scal),ylim=c(-.1,1),axes=F,
       xlab="Latent Scale",ylab="Response Probability")
  if (only.items==FALSE){
    for (i in 1:length(mean.x)){
      lines(ci.x[,i],rep(seq(-.1,-.01,length=length(mean.x))[i],2),
            col="grey")
    }
    par(new=T)
    plot(mean.x,seq(-.1,-.01,length=length(mean.x)),type="p",pch=19,cex=0.2,
         xlim=range(x.scal),ylim=c(-.1,1),axes=F,ann=F)
  }
  axis(1)
  axis(2,at=c(0,0.5,1))
  for (i.item in 1:n.item){
    this.x.scal <- seq(i.item,length(x.scal),by=n.item)
    for (i in this.x.scal){
      lines(rep(x.scal[i],2),c(all.lower[i,i.item],all.upper[i,i.item]),
            col=i.item)
    }
  }
  legend("topleft",bty="n",col=1:n.item,label.item,lty=1,lwd=2)
}
