# ##############################################################################
#' Make a statistics table
#'
#' @param dist Distribution
#' @param prob
#' @param df
#' @return
# ---------------------------------------------------------------------------- #
stat.table <- function(dist="t",
                     prob=c( 0.1 , 0.05 , 0.025 , 0.01 , 0.005 ),
                     df = c(1:20,90:120)){
  output.mat <- matrix(NA,ncol=length(prob),nrow=length(df))
  if (dist=="t"){
    for (i in 1:length(df)){
      output.mat[i,] <- qt(prob,df=df[i])
    }
  rownames(output.mat) <- df
  colnames(output.mat) <- prob
  }
  output.mat
}
