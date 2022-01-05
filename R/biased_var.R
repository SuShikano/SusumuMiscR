# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' function to calculate the biased variance
#'
#' @param x A numeric vector. If missing values will be ignored.
#' @return The biased variance of x.
#' @examples
#' biased.var(c(1,2,3,4,5))
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################

biased.var <- function(x){
  sum((x[!is.na(x)] - mean(x[!is.na(x)]))^2)/(length(x[!is.na(x)]))
}


# function to calculate the biased variance
biased.cov <- function(x,y){
  sum((x - mean(x))*(y - mean(y)))/(length(x))
}

