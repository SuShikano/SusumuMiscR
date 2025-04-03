# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
#' This function reformat a wide-format data in the long-format.
#'
#' @param data Data in the wide format.
#' @param choice Choice variable.
#' @param id Respondent ID.
#' @param alt.specific.vars The list of alternative specific variables
#' @param alt.labels
#' @return Data in the long format.
#' @examples
# ---------------------------------------------------------------------------- #
# ##############################################################################
# ##############################################################################
wide2long <- function(data, choice, id, alt.specific.vars = list(), alt.labels = NULL) {
  # Does the choice variable exist in the dataset?
  if (!(choice %in% names(data))) {
    stop("The choice variable must be a column in the dataset.")
  }
  
  # Identify unique alternatives from the choice variable
  unique.alts <- unique(data[[choice]])
  
  # If alternative labels are not provided, use unique alternatives
  if (is.null(alt.labels)) {
    alt.labels <- unique.alts
  }
  
  # Ensure the number of alternatives matches expectations
  num.alts <- length(alt.labels)
  
  # Debugging: Print the number of alternatives
  message("Number of alternatives: ", num.alts)
  
  # Ensure alternative-specific variables have correct lengths
  varying.list <- lapply(alt.specific.vars, function(vars) {
    if (length(vars) != num.alts) {
      stop("Each alternative-specific variable must have ", num.alts, " columns in 'data'.")
    }
    vars
  })
  
  # Convert alternative-specific variables into separate lists for reshaping
  varying.names <- names(varying.list)
  varying <- do.call(c, varying.list)
  
  # Debugging: Print the reshaping columns
  message("Columns used for reshaping: ", paste(varying, collapse = ", "))
  
  # Reshape data to long format for alternative-specific variables
  long.data <- reshape(data,
                       varying = varying,
                       v.names = varying.names,
                       timevar = "alternative",
                       times = seq_len(num.alts),  # Ensures every row is repeated num.alts times
                       idvar = id,
                       direction = "long")
  
  # Convert alternative index to factor with correct labels
  long.data$alternative <- factor(long.data$alternative, labels = alt.labels)

    # Assign correct choice values based on the original data
  choice.match <- data[[choice]][match(long.data[[id]], data[[id]])]  # Retrieve correct choice values
  long.data$choice <- as.numeric(long.data$alternative == choice.match)
  
  
  # Sort for clarity
  long.data <- long.data[order(long.data[[id]], long.data$alternative), ]
  
  return(long.data)
}
