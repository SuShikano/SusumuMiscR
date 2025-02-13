# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function computes the moving average over XX hours
#'
#' @param x
#' @return 
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
moving.average <- function(
    time, 
    values, 
    window.hours = 6
    ) {
  # Convert hours to seconds
  window.seconds <- window.hours * 3600  
  # Storage for moving averages
  ma.values <- numeric(length(time))  
  
  for (i in seq_along(time)) {
    # Find indices within the time window
    in.window <- (time >= (time[i] - window.seconds)) & (time <= time[i])
    # Compute mean for the window
    ma.values[i] <- mean(values[in.window], na.rm = TRUE)  
  }
  
  return(ma.values)
}
