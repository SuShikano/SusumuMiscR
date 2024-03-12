#
# bootstrapping
#
#

bootsstr <- function(x){


library(boot)
library(xtable)

daten0v <- cbind(c.vote,s.vote,p.vote)
### ideology
daten01 <- cbind(c.ideo.rm,s.ideo.rm,p.ideo.rm,c.ideo.pr,s.ideo.pr,p.ideo.pr)
### nuclear energy
daten02 <- cbind(c.atom.rm,s.atom.rm,p.atom.rm,c.atom.pr,s.atom.pr,p.atom.pr)
### immigration
daten03 <- cbind(c.immi.rm,s.immi.rm,p.immi.rm,c.immi.pr,s.immi.pr,p.immi.pr)
### europa
daten04 <- cbind(c.euro.rm,s.euro.rm,p.euro.rm,c.euro.pr,s.euro.pr,p.euro.pr)

daten01a <- daten01



daten0 <- cbind(daten0v,daten01, daten02, daten03, daten04 )

daten1 <- daten0[westost==2 & three.parties==1,] #for each voters
#daten1 <- daten0[westost==2 & three.parties==1 & gewerk==1,] # only for unionized




daten <- na.omit(daten1)


befragten <- dim(daten)[1]


nummer <- c(1:befragten)



anzahl <- 2000  ##### Wie oft wiederholt??

ergebnis.boot<- array(0, dim=c(anzahl,9))

#############################
# Wiederholung
#############################


for (j in 1:anzahl)  {

res.nr <- sample(nummer, befragten, replace=T)


res.data <- daten

for (i in 1:befragten) res.data[i,] <- daten[res.nr[i],]


etikett.mix2 <-c(rep("constant",times=2),rep("Mu",times=3),
            "Policy Utility",rep("Beta",times=3))

ll.mix2 <- function(p) {
            as     <- p[1] # Constants
            ap     <- p[2]

            mu1     <- p[3]
            mu2     <- p[4]
            mu3     <- p[5]

            apol   <- p[6]

            alpha1   <- p[7]
            alpha2   <- p[8]
            alpha3   <- p[9]

            beta1  <- 1/(1+exp(-alpha1))
            beta2   <-1/(1+exp(-alpha2))
            beta3   <-1/(1+exp(-alpha3))

#            beta1  <- p[7]
#            beta2   <-p[8]
#            beta3   <-p[9]


#            Beta1   <- P[7]; Beta1 <- Ifelse(Beta1>1,0,Beta1)
#            Beta2   <- P[8]; Beta2 <- Ifelse(Beta2>1,0,Beta2)
#            Beta3   <- P[9]; Beta3 <- Ifelse(Beta3>1,0,Beta3)

            mu4     <- 1-p[4]-p[5]-p[3]

            # Utility Function
            Ucdu <- exp(      apol*(2*(1-beta1)*(res.data[,4]*mu1 +  res.data[,10]*mu2 + res.data[,16]*mu3 +res.data[,22]* mu4) -
                                             beta1*(res.data[,7]*mu1 +  res.data[,13]*mu2 + res.data[,19]*mu3 +res.data[,25]* mu4)) )
            Uspd <- exp(as +  apol*(2*(1-beta2)*(res.data[,5]*mu1 +  res.data[,11]*mu2 + res.data[,17]*mu3 +res.data[,23]* mu4) -
                                             beta2*(res.data[,8]*mu1 +  res.data[,14]*mu2 + res.data[,20]*mu3 +res.data[,26]* mu4)) )
            Upds <- exp(ap +  apol*(2*(1-beta3)*(res.data[,6]*mu1 +  res.data[,12]*mu2 + res.data[,18]*mu3 +res.data[,24]* mu4) -
                                             beta3*(res.data[,9]*mu1 +  res.data[,15]*mu2 + res.data[,21]*mu3 +res.data[,27]* mu4)) )


            # Probability

            Pcdu <- Ucdu/(Ucdu+Uspd+Upds)
            Pspd <- Uspd/(Ucdu+Uspd+Upds)
            Ppds <- Upds/(Ucdu+Uspd+Upds)

            # Calculating Log-Likelihood
            Llcdu <- res.data[,1] * log(Pcdu)
            Llspd <- res.data[,2] * log(Pspd)
            Llpds <- res.data[,3] * log(Ppds)

            # Endproduct
            sum(Llcdu , Llspd , Llpds  )
            #Na.omit(Cbind(Llcdu , Llspd , Llpds , Llgru , Llpds ))
             #Sum(Llcdu + Llspd + Llpds  + Llpds )
            #Cbind(Llcdu , Llspd , Llpds , Llgru , Llpds )
            #Cbind(Ucdu , Uspd , Llpds , Ugru , Upds )
            }
begin <-   c(0,0, .25,.25,.25,0.5,0.5,0.5,0.5)
                  # Checking Processing
                   test.cs <- ll.mix2(begin)
                   test.cs


# Estimation Through Nlm

ll.mix2.m <- function(p) {
             -1 * ll.mix2(p)
             }




Outnlm <- nlm(ll.mix2.m,p=begin,print.level=1,hessian=FALSE)
#Begin <- Outnlm$Estimate


ergebnis.boot[j,] <- Outnlm$estimate

}



dim(ergebnis.boot4)


ergebnis.boot5 <- rbind(ergebnis.boot4[1:1999,],ergebnis.boot)


summary(ergebnis.boot5)



beta.bt <- 1/(1+exp(-ergebnis.boot5[,7:9]))

hist(betac[,2])



Loglike.mix2 <- round(c(0,Outnlm$minimum *-1,0),2)

Se <- sqrt(diag(solve(Outnlm$hessian)))
Twert <- Outnlm$estimate / Se

Ergebnis.mix2 <- cbind(etikett.mix2, round(Outnlm$estimate,2), round(Twert,2))
Ergebnis.mix2








#############################
# Jacknife
#############################

ergebnis.jn<- array(0, dim=c(dim(daten)[1],9))

for (j in 1:dim(daten)[1])  {

jn.data <- daten[-c(j),]



etikett.mix2 <-c(rep("constant",times=2),rep("Mu",times=3),
            "Policy Utility",rep("Beta",times=3))

ll.mix2 <- function(p) {
            as     <- p[1] # Constants
            ap     <- p[2]

            mu1     <- p[3]
            mu2     <- p[4]
            mu3     <- p[5]

            apol   <- p[6]

            alpha1   <- p[7]
            alpha2   <- p[8]
            alpha3   <- p[9]

            beta1  <- 1/(1+exp(-alpha1))
            beta2   <-1/(1+exp(-alpha2))
            beta3   <-1/(1+exp(-alpha3))

#            beta1  <- p[7]
#            beta2   <-p[8]
#            beta3   <-p[9]


#            Beta1   <- P[7]; Beta1 <- Ifelse(Beta1>1,0,Beta1)
#            Beta2   <- P[8]; Beta2 <- Ifelse(Beta2>1,0,Beta2)
#            Beta3   <- P[9]; Beta3 <- Ifelse(Beta3>1,0,Beta3)

            mu4     <- 1-p[4]-p[5]-p[3]

            # Utility Function
            Ucdu <- exp(      apol*(2*(1-beta1)*(jn.data[,4]*mu1 +  jn.data[,10]*mu2 + jn.data[,16]*mu3 +jn.data[,22]* mu4) -
                                             beta1*(jn.data[,7]*mu1 +  jn.data[,13]*mu2 + jn.data[,19]*mu3 +jn.data[,25]* mu4)) )
            Uspd <- exp(as +  apol*(2*(1-beta2)*(jn.data[,5]*mu1 +  jn.data[,11]*mu2 + jn.data[,17]*mu3 +jn.data[,23]* mu4) -
                                             beta2*(jn.data[,8]*mu1 +  jn.data[,14]*mu2 + jn.data[,20]*mu3 +jn.data[,26]* mu4)) )
            Upds <- exp(ap +  apol*(2*(1-beta3)*(jn.data[,6]*mu1 +  jn.data[,12]*mu2 + jn.data[,18]*mu3 +jn.data[,24]* mu4) -
                                             beta3*(jn.data[,9]*mu1 +  jn.data[,15]*mu2 + jn.data[,21]*mu3 +jn.data[,27]* mu4)) )


            # Probability

            Pcdu <- Ucdu/(Ucdu+Uspd+Upds)
            Pspd <- Uspd/(Ucdu+Uspd+Upds)
            Ppds <- Upds/(Ucdu+Uspd+Upds)

            # Calculating Log-Likelihood
            Llcdu <- jn.data[,1] * log(Pcdu)
            Llspd <- jn.data[,2] * log(Pspd)
            Llpds <- jn.data[,3] * log(Ppds)

            # Endproduct
            sum(Llcdu , Llspd , Llpds  )
            #Na.omit(Cbind(Llcdu , Llspd , Llpds , Llgru , Llpds ))
             #Sum(Llcdu + Llspd + Llpds  + Llpds )
            #Cbind(Llcdu , Llspd , Llpds , Llgru , Llpds )
            #Cbind(Ucdu , Uspd , Llpds , Ugru , Upds )
            }
begin <-   c(0,0, .25,.25,.25,0.5,0.5,0.5,0.5)
                  # Checking Processing
                   test.cs <- ll.mix2(begin)
                   test.cs


# Estimation Through Nlm

ll.mix2.m <- function(p) {
             -1 * ll.mix2(p)
             }




Outnlm <- nlm(ll.mix2.m,p=begin,print.level=1,hessian=FALSE)
#Begin <- Outnlm$Estimate


ergebnis.jn[j,] <- Outnlm$estimate

}



beta.jn <- 1/(1+exp(-ergebnis.jn[,7:9]))


mittel.theta.cdu <- mean(beta.jn[,1])
mittel.theta.spd <- mean(beta.jn[,2])
mittel.theta.pds <- mean(beta.jn[,3])

a.hat.cdu <- (sum((mittel.theta.cdu-beta.jn[,1])^3))/(6*sum((mittel.theta.cdu-beta.jn[,1])^2)^(3/2))
a.hat.spd <- (sum((mittel.theta.spd-beta.jn[,2])^3))/(6*sum((mittel.theta.spd-beta.jn[,2])^2)^(3/2))
a.hat.pds <- (sum((mittel.theta.pds-beta.jn[,3])^3))/(6*sum((mittel.theta.pds-beta.jn[,3])^2)^(3/2))


z.hat.cdu <- qnorm(sum(ifelse(beta.bt[,1] < 1.00,1,0)/2000))
z.hat.spd <- qnorm(sum(ifelse(beta.bt[,2] < 0.79,1,0)/2000))
z.hat.pds <- qnorm(sum(ifelse(beta.bt[,3] < 0.36,1,0)/2000))


alpha.05.cdu <- pnorm(z.hat.cdu+(z.hat.cdu-1.644854)/(1-a.hat.cdu*(z.hat.cdu-1.644854)))
alpha.95.cdu <- pnorm(z.hat.cdu+(z.hat.cdu+1.644854)/(1-a.hat.cdu*(z.hat.cdu+1.644854)))

alpha.05.spd <- pnorm(z.hat.spd+(z.hat.spd-1.644854)/(1-a.hat.spd*(z.hat.spd-1.644854)))
alpha.95.spd <- pnorm(z.hat.spd+(z.hat.spd+1.644854)/(1-a.hat.spd*(z.hat.spd+1.644854)))

alpha.05.pds <- pnorm(z.hat.pds+(z.hat.pds-1.644854)/(1-a.hat.pds*(z.hat.pds-1.644854)))
alpha.95.pds <- pnorm(z.hat.pds+(z.hat.pds+1.644854)/(1-a.hat.pds*(z.hat.pds+1.644854)))




alpha.10.cdu <- pnorm(z.hat.cdu+(z.hat.cdu-1.281552)/(1-a.hat.cdu*(z.hat.cdu-1.281552)))
alpha.90.cdu <- pnorm(z.hat.cdu+(z.hat.cdu+1.281552)/(1-a.hat.cdu*(z.hat.cdu+1.281552)))

alpha.10.spd <- pnorm(z.hat.spd+(z.hat.spd-1.281552)/(1-a.hat.spd*(z.hat.spd-1.281552)))
alpha.90.spd <- pnorm(z.hat.spd+(z.hat.spd+1.281552)/(1-a.hat.spd*(z.hat.spd+1.281552)))

alpha.10.pds <- pnorm(z.hat.pds+(z.hat.pds-1.281552)/(1-a.hat.pds*(z.hat.pds-1.281552)))
alpha.90.pds <- pnorm(z.hat.pds+(z.hat.pds+1.281552)/(1-a.hat.pds*(z.hat.pds+1.281552)))




Loglike.mix2 <- round(c(0,Outnlm$minimum *-1,0),2)

Se <- sqrt(diag(solve(Outnlm$hessian)))
Twert <- Outnlm$estimate / Se

Ergebnis.mix2 <- cbind(etikett.mix2, round(Outnlm$estimate,2), round(Twert,2))
Ergebnis.mix2


par(mfrow = c(2,3))

hist(beta.bt[,1],xlim=c(0,1))
hist(beta.bt[,2],xlim=c(0,1))
hist(beta.bt[,3],xlim=c(0,1))

hist(beta.jn[,1],xlim=c(0,1))
hist(beta.jn[,2],xlim=c(0,1))
hist(beta.jn[,3],xlim=c(0,1))

par(mfrow = c(1,1))


sort(beta.bt[,1])[c(100,1900)]
sort(beta.bt[,2])[c(100,1900)]
sort(beta.bt[,3])[c(100,1900)]


sd(ergebnis.boot5)
sd(beta.bt)
}
