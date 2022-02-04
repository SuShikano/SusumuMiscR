# ##############################################################################
#' Computing survey duration time for the ballot experiment data
#'
#' @param start.var 
#' @param end.var
#' @param data
#' @return A list with the computed duration and start/end time measures
#'         in the POSIX class.
# ---------------------------------------------------------------------------- #
be.convert.time <- function(start.var=NULL,
                            end.var=NULL,
                            data=NULL){
  if (is.null(start.var)) stop("Input the variable name for the start time")
  if (is.null(end.var)) stop("Input the variable name for the end time")
  if (is.null(data)) stop("Input the data name")
  
  for (i in 1:2){
    if (i == 1) {
      this.time.stamp <- data[,start.var]
    }else{
      this.time.stamp <- data[,end.var]
    }
    
    this.time.stamp <- gsub("Z","",this.time.stamp)
    this.time.stamp <- gsub("T"," ",this.time.stamp)
    this.time.stamp <- strptime(this.time.stamp, format = "%Y-%m-%d %H:%M:%OS")
    
    if (i == 1) {
      start.time <- this.time.stamp 
    }else{
      end.time <- this.time.stamp 
    }
  }
  
  dur <- end.time - start.time
  
  list(duration=dur,start.time=start.time,end.time=end.time)
}
