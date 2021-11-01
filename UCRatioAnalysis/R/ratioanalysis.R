#' Creates Ratio Analysis for Utah County
#' @export
#' @param pred numeric predicted values from model
#' @param real numeric actual values of homes/apts
#' @import stats
ratioanalysis <- function(pred, real) {
  Ratio <- pred / real
  Ratio <- na.omit(Ratio)
  weightedMean <- sum(pred) / sum(real)
  COD <- (sum(abs(Ratio - median(Ratio)))/length(Ratio))/median(Ratio)
  COV <- sd(Ratio) / mean(Ratio)
  PRD <- mean(Ratio) / weightedMean
  return_values <- c("Weighted Mean", weightedMean, "COD", COD, "COV", COV, "PRD", PRD)
  return(return_values)
}

