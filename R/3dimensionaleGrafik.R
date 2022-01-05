
#
#  3 Dimensionale Grafik
#

x <- seq(-3,3,length=50)
y <- x
rho <- 0.9
gauss3d <- function(x,y) {
 1/(2*pi*sqrt(1-rho^2))*exp(-(x^2-2*rho*x*y+y^2) / (2*(1-rho^2)))
 }


z <- outer(x,y,gauss3d)
z[is.na(z)] <- 1
#persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
