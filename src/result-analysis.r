wrapEval <- function(piperesults, nperiods) {
  metrics_l <-
    lapply(piperesults,
           function(ps) {
             M <-
               lapply(ps$predsl,
                      function(preds) {
                        evalFramework(preds, ps$trues, nperiods)$metrics
                      })
             
             do.call(rbind, M)
           })
  
  faph <-
    lapply(piperesults,
           function(ps) {
             M <-
               lapply(ps$predsl,
                      function(preds) {
                        unlist(evalFramework(preds, ps$trues, nperiods)$falsea)
                      })
             
             M
           })
  
  faph <- lapply(faph, function(x) do.call(data.frame, x))
  faph <- Reduce(rbind.data.frame, faph)
  
  
  antt <-
    lapply(piperesults,
           function(ps) {
             M <-
               lapply(ps$predsl,
                      function(preds) {
                        unlist(evalFramework(preds, ps$trues, nperiods)$antt)
                      })
             
             M
           })
  antt <- lapply(antt, function(x) do.call(data.frame, x))
  antt <- Reduce(rbind.data.frame, antt)
  
  metrics_l <- lapply(metrics_l, function(x) replace(x, is.na(x), 0))
  
  metrics <- Reduce("+",metrics_l)  / length(metrics_l)
  
  metrics <- round(metrics, 2)
  metrics <- metrics[order(metrics[,"ER"]),]
  
  InnerEval_l <- lapply(piperesults, function(x) x$inner_evals)
  InnerEval_l <-
    lapply(InnerEval_l,
           function(x)
             replace(x, is.na(x), 0))
  
  InnerEval <- Reduce("+",InnerEval_l) / length(InnerEval_l)
  
  list(
    metrics = metrics,
    metrics_l = metrics_l,
    InnerEval = InnerEval,
    InnerEval_l = InnerEval_l,
    faph = faph,
    antt=antt)
}

evalFramework <- function(preds, trues, obsr_size) {
  predsl <- pre_process_predictions(preds,trues, obsr_size)
  truesl <- pre_process_trues(trues, obsr_size)
  
  amon_m <- amon_eval(predsl, truesl, obsr_size)
  class_m <- classPerf(preds,trues)
  
  metrics <- c(amon_m, class_m)
  falsea <- false_alarms(predsl, truesl, obsr_size)
  
  antt <- ant_time_l(predsl, truesl)
  
  list(metrics=metrics, falsea=falsea, antt=antt)
}

false_alarms <- function(preds, trues, nperiods) {
  seq_episodes <- seq_along(trues)
  
  FApH <-
    vapply(seq_episodes,
           function(ep_id) {
             false_alarm_per_hour(preds[[ep_id]],
                                  trues[[ep_id]], nperiods)
           }, FUN.VALUE = double(1L), USE.NAMES = FALSE)
  
  FApH
}

false_alarm_per_hour <-
  function(preds, trues, nperiods) {
    negs <- trues < 1
    
    if (!any(negs)) {
      return(0)
    }
    
    preds_on_negs <- preds[negs]
    
    sum(preds_on_negs) / (length(preds_on_negs) / nperiods)
  }

ant_time_l <- function(preds, trues) {
  seq_episodes <- seq_along(trues)
  
  FApH <-
    vapply(seq_episodes,
           function(ep_id) {
             anticipation_time(preds[[ep_id]],
                               trues[[ep_id]])
           }, FUN.VALUE = double(1L), USE.NAMES = FALSE)
  
  FApH
}


anticipation_time <- function(preds,trues) {
  if (is.factor(preds)) {
    preds <- as.numeric(as.character(preds))
  }
  
  if (is.factor(trues)) {
    trues <- as.numeric(as.character(trues))
  }
  
  l <- length(trues)
  
  any_positive <- any(trues == 1)
  
  if (!any_positive) {
    return(NA_real_)
  }
  # minimum point for acceptable predictions
  min.p <- min(which(trues == 1))
  # alarms launched during period
  acceptable.zone <- min.p:l
  acceptable.alarms <- preds[acceptable.zone]
  acc.al.lgcl <- acceptable.alarms > 0
  
  # if there was an alarm
  if (sum(acceptable.alarms) > 0) {
    # retrieve first point where it happens
    accepted.alarm <- min(acceptable.zone[which(acc.al.lgcl)])
    no_anticipation_periods <- l - accepted.alarm + 1
  } else {
    # or return 0 -- no detection
    no_anticipation_periods <- 0
  }
  
  lp <- length(trues[trues > 0])
  
  no_anticipation_periods / lp
}

pre_process_predictions <- 
  function(preds,trues,obsr_size) {
  
  nms <- names(trues)
  
  spl_nms <-
    vapply(nms, function(x) {
      split_by(x, split = ".")[1]
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE)
  
  truesl <- split(trues, spl_nms)
  preds <- as.numeric(as.character(preds))
  predsl <- split(preds, spl_nms)
  
  for (i in seq_along(truesl)) {
    if (length(truesl[[i]]) <= obsr_size) {
      next
    }
    
    if (!any(truesl[[i]] == 1)) {
      next
    }
    
    which1 <- which(truesl[[i]] == 1)
    which0 <- which(!truesl[[i]] == 1)
    
    which1useful <- utils::head(which1, obsr_size)
    
    truesl[[i]] <- truesl[[i]][c(which0, which1useful)]
    predsl[[i]] <- predsl[[i]][c(which0, which1useful)]
  }
  
  predsl
  }


pre_process_trues <- function(trues, obsr_size) {
  
  nms <- names(trues)
  
  spl_nms <-
    vapply(nms, function(x) {
      split_by(x, split = ".")[1]
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE)
  
  truesl <- split(trues, spl_nms)
  
  for (i in seq_along(truesl)) {
    if (length(truesl[[i]]) <= obsr_size) {
      next
    }
    
    if (!any(truesl[[i]] == 1)) {
      next
    }
    
    which1 <- which(truesl[[i]] == 1)
    which0 <- which(!truesl[[i]] == 1)
    
    which1useful <- utils::head(which1, obsr_size)
    
    truesl[[i]] <- truesl[[i]][c(which0, which1useful)]
  }
  truesl
}


amon_eval <- function(preds, trues, nperiods) {
  
  seq_episodes <- seq_along(trues)
  
  assertion_ <-
    lapply(seq_episodes,
           function(j) {
             stopifnot(length(preds[[j]]) == length(trues[[j]]))
           })
  
  AT <-
    vapply(seq_episodes,
           function(ep_id) {
             anticipation_time(preds[[ep_id]],
                               trues[[ep_id]])
           }, double(1L))
  
  DFP <-
    vapply(seq_episodes,
           function(ep_id) {
             discounted_false_positives(preds[[ep_id]],
                                        trues[[ep_id]], nperiods)
           }, double(1L))
  
  FApH <-
    vapply(seq_episodes,
           function(ep_id) {
             false_alarm_per_hour(preds[[ep_id]],
                                  trues[[ep_id]], nperiods)
           }, double(1L))
  
  
  RP <- reduced_precision(DFP, AT)
  
  avgAT <- mean(AT, na.rm = TRUE)
  
  AT_noNaN <- AT[!is.na(AT)]
  ER <- sum(AT_noNaN > 0) / length(AT_noNaN)
  
  TP <- sum(AT_noNaN > 0)
  dFP <- sum(DFP)
  FN <- sum(AT_noNaN == 0)
  
  avgFApH <- stats::median(FApH) / nperiods
  
  
  metrics <-
    c(
      RP = RP,
      ER = ER,
      avgFApH = avgFApH,
      avgAT = avgAT)
  
  metrics <- round(metrics, 2)
  
  metrics
}

reduced_precision <- function(DFP, AT) {
  sumDFP <- sum(DFP)
  EP <- sum(AT[!is.na(AT)] > 0)
  
  EP / (EP + sumDFP)
}

discounted_false_positives <- function(preds,trues, shut_period) {
  preds <- preds[trues < 1]
  trues <- trues[trues < 1]
  
  l <- length(trues)
  
  if (l < 1) {
    return(0.)
  }
  
  fa_count <- 0
  #fa_vector <- numeric(l)
  pass_this_one <- logical(l)
  for (i in 1:l) {
    if (pass_this_one[i]) {
      next
    }
    
    mh <- min(i+shut_period-1, l)
    falarm <- trues[i] < 1 & preds[i] > 0
    if (falarm) {
      #fa_vector[i:mh] <- 1L
      fa_count <- fa_count + 1
      pass_this_one[(i+1):mh] <- TRUE
    }
  }
  
  #split_vec <- split(fa_vector, rleid(fa_vector))
  
  #no_fp <- sum(sapply(split_vec, function(x) x[1] == 1))
  
  fa_count
}