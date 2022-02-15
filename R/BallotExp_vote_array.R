# ##############################################################################
#' Creating the vote decision array (resp,round,cand/party).
#'
#' @param data The name of the data.
#' @return An array
# ---------------------------------------------------------------------------- #
be.vote.array <- function(data=NULL){
  
  party.vote <- data[,grep("party_r",names(data))]
  party.vote <- party.vote[,grep("_pos",names(party.vote),invert=T)]
  
  cand.vote <- data[,grep("candidate_r",names(data))]
  cand.vote <- cand.vote[,grep("_pos",names(cand.vote),invert=T)]

  vote.array <- array(c(unlist(cand.vote),unlist(party.vote)),
                    dim=c(nrow(party.vote),3,2))
  vote.array[vote.array==""] <- NA
  vote.array
}