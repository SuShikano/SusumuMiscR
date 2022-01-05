# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function creates frequency table
#'
#' @param x
#' @return Graphics
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
freq.table <- function(x){

  library(xtable)

  if (sum(is.na(x))>0){
    na.value <- max(x,na.rm=T)+1
    x[is.na(x)] <- na.value
    freq.x <- table(x)

    rel.x <- round(freq.x/sum(freq.x),3)

    rel.x.na <- freq.x[as.numeric(names(freq.x))!=na.value]
    rel.x.na <- round(rel.x.na/sum(rel.x.na),3)
    rel.x.na <- c(rel.x.na,NA)

    out.tab <- cbind(freq.x, rel.x, rel.x.na,cumsum(rel.x.na))

    colnames(out.tab) <- c("Häufigkeit","Prozente","Gültige Pr.","Kumulierte Pr.")

    xtable(out.tab,digits=c(0,0,3,3,3))

  }else{
    freq.x <- table(x)

    rel.x <- round(freq.x/sum(freq.x),3)
    out.tab <- cbind(freq.x, rel.x, cumsum(rel.x))

    colnames(out.tab) <- c("Häufigkeit","Prozente","Kumulierte Pr.")

    xtable(out.tab,digits=c(0,0,3,3))
  }


}

