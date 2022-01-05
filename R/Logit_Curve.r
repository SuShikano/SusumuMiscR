#
#  Programm for die Logit-Kurve
#


alpha <- 0.5
beta <- 0.2

rasch.func <- function(theta){
   p <- 1/(1+exp(alpha - beta * theta))
}


curve(rasch.func,-5,5,ylim=c(0,1))

