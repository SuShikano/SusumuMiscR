# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function divide a number into individual digits
#'
#' @param x The number to be divided. This can be numeric or character.
#' @param num.digits The number of digits
#' @return A vector of individual digit numbers.
#' @examples
#' bubble.plot(x=123456,max.digits=6)
#'
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
Ziffer.auseinander <- function(x,num.digits){
  
  outcome <- rep(NA,num.digits)
  
  if (is.numeric(x)){
    for (i.digit in 1:num.digits){
      outcome[i.digit] <- floor(x/(10^(num.digits-i.digit)))
    }
    outcome <- outcome- c(0,outcome[1:(num.digits-1)]*10)
  }else{
    for (i.digit in 1:num.digits){
      outcome[i.digit] <- as.numeric(substr(x,i.digit,i.digit))
    }
  }
  outcome
}





