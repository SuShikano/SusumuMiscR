# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function divide a number into individual digits
#'
#' @param x The number to be divided
#' @param max.digits The number of digits
#' @return A vector of individual digit numbers.
#' @examples
#' bubble.plot(x=123456,max.digits=6)
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
Ziffer.auseinander <- function(x,max.digits){
  outcome <- rep(NA,max.digits)
  for (i.digit in 1:max.digits){
    outcome[i.digit] <- floor(x/(10^(max.digits-i.digit)))
  }
  outcome <- outcome- c(0,outcome[1:(max.digits-1)]*10)
  outcome
}





