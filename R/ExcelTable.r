# ##############################################################################
#' Copy a table to excel
#'
#' @param tab 
#' @return Variance estimate based on multiple analysis
# ---------------------------------------------------------------------------- #

write.excel <- function(
  tab, ...) {
    write.table( tab, "clipboard-128", sep="\t", row.names=F)
  }

