classPerf <- function(preds,trues) {
  require(performanceEstimation)
  
  if (is.factor(preds)) {
    preds <- as.integer(as.character(preds))
  }
  
  if (is.factor(trues)) {
    trues <- as.integer(as.character(trues))
  }
  
  preds <- factor(preds, levels = c(1L,0L))
  trues <- factor(trues, levels = c(1L,0L))
  
  cm <- round(classificationMetrics(trues = trues, preds = preds),2)
  
  c(cm[c("sens", "spec","prec","F","acc")])
}
