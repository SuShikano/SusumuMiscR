# ##############################################################################
#' Variance estimate via integration of multiple analysis
#'
#' @param point.estimates A vector of point estimates
#' @param variance.estimates A vector of variance estimates
#' @return Variance estimate based on multiple analysis
# ---------------------------------------------------------------------------- #
integrate.multiple.analysis <- function(point.estimates=NULL,
                     variance.estimates=NULL){

  if (sum(is.na(point.estimates))>0) stop("Point estimates include missings.")
  if (sum(is.na(variance.estimates))>0) stop("Variance estimates include missings.")

  num.iter <- length(point.estimates)

  expected.var <- mean(variance.estimates)

  sum.sqr.point.est <- sum((point.estimates-mean(point.estimates))^2)
  bias.correction <- (1+1/num.iter)*(1/(num.iter-1))*sum.sqr.point.est

  var.est <- expected.var + bias.correction

  var.est
}
