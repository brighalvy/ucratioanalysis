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
  return_value_name <- c("Weighted Mean", "COD", "COV", "PRD")
  return_values <- c(weightedMean, COD, COV, PRD)
  names(return_values) <- return_value_name
  return(return_values)
}

