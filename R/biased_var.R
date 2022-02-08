# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' function to calculate the biased variance and covariance
#'
#' @param x A numeric vector whose biased variance is to be computed. 
#'          Missing values will be ignored.
#' @param y A numeric vector. If this is not empty, the biased covariance 
#'          of x and y is computed. Deafult is NULL.
#' @return The biased variance of x or the biased covariance of x and y.
#' @examples
#' biased.var(c(1,2,3,4,5))
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################

biased.var <- function(x=NULL,y=NULL){
  # if y is empty
  if (is.null(y)) {
    sum((x[!is.na(x)] - mean(x[!is.na(x)]))^2)/(length(x[!is.na(x)]))
  }else{
    sum((x - mean(x))*(y - mean(y)))/(length(x))
  }
}


# function to calculate the biased variance
biased.cov <- function(x,y){
  sum((x - mean(x))*(y - mean(y)))/(length(x))
}

