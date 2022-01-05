# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' function to calculate the standard error
#'
#' @param x A numeric vector. Missing values will be ignored.
#' @return The sandard error of sample mean based on x.
#' @examples
#' se.mean(c(1,2,3,4,5))
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################

se.mean <- function(x){

  st.dev <- sd(x,na.rm=T)
  ns <- sum(!is.na(x))

  st.dev/(sqrt(ns))
}

