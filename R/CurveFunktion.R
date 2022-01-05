
#
# Funktion abbilden
#


# curve(expr, from, to, n = 101 )

curve(-.152 *(x ^  1.942),0,1)


curve(1 *(x ^  3),0,1)

curve(1/(1 + exp(.152 -1.942 * log(x/(1-x)) )) ,0,1)
abline(h=0,lty=2)

exp(1)
