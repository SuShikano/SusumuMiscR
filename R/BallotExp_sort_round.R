# ##############################################################################
#' Sorting the variables in the order on the screen
#'
#' @param to.be.sorted The variable names to be sorted in the data frame (if specified by data) 
#'          or a matrix with three columns.
#' @param order.var The variable names for the order in the data frame (if specified by data) 
#'          or a matrix with three columns.
#' @param data The name of the data.
#' @return A list with the computed duration and start/end time measures
#'         in the POSIX class.
# ---------------------------------------------------------------------------- #
be.sort.round <- function(to.be.sorted=NULL,
                            order.var=c("round_one",                                        
                                        "round_two",                                        
                                        "round_three"),
                            data=NULL){
  if (is.null(to.be.sorted)) stop("Input the variable name to be sorted.")
  if (length(order.var) !=3) stop("order.var has the length of 3.")
  if (is.null(data)) {
    original.mat <- as.matrix(to.be.sorted)
    round.order <- as.matrix(order.var)
  }else{
    original.mat <- as.matrix(data[,to.be.sorted])
    round.order <- as.matrix(data[,order.var])
  }
  
  new.mat <- matrix(NA,nrow=nrow(original.mat),ncol=ncol(original.mat))
  for (i in 1:nrow(original.mat)){
    new.mat[i,] <- original.mat[i,order(round.order[i,])]
  }  

  list(new.mat=new.mat,
       original.mat=original.mat,
       round.order=round.order)
}