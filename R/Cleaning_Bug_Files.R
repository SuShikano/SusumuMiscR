# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function cleans up XXX.bugs files from the specified folder.
#'
#' @param folder The folder to be cleaned up.
#' @return None.
#' @examples
#' cleanup.bug.file()
#' cleanup.bug.file("c:/R-Programme/Daten")
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
cleanup.bug.file <- function(folder="c:/R-Programme/Daten"){
  setwd(folder)
  all.files <- dir()
  bug.files <- all.files[grep(".bug", all.files)]
  file.remove(bug.files)
}





