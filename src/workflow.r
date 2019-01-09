LL_workflow <-
  function(form, tr_eps, vl_eps, ts_eps, RU, classifier) {
    has_any_ahe <-
      sapply(tr_eps,
             function(x) {
               y_x <- get_y(x, form)

               any(y_x == 1L, na.rm = TRUE)
             })

    tr <- lapply(tr_eps[has_any_ahe],
                 prune_events,
                 form = form,
                 event_max_size = 60)
    
    tgt_window_size<-30
    tr <- lapply(tr,
                 function(x) {
                   seq_1 <- 1:nrow(x)
                   id_1 <- which(seq_1 %% tgt_window_size == 1)
                   
                   x[id_1,]
                 })

    test <- do.call(rbind.data.frame, ts_eps)
    trues <- get_y(test, form)
    names(trues) <- rownames(test)

    TGT <-
      targets_predtasks(
        form_surr = target_surrogate ~ .,
        form = target ~ .,
        x = test)

    validation <- do.call(rbind.data.frame, vl_eps)
    validation$target <- as.factor(validation$target)

    cat("Regr\n")
    regrpreds <-
      regression_ahe_pred(
        form = target ~.,
        form_num = target_tgtavg ~.,
        train_eps = tr,
        validation = validation,
        test = test)

    tr <- 
      lapply(tr,
             function(x) {
               x$target_tgtavg <- NULL
               
               x
             })
    
    validation$target_tgtavg <- test$target_tgtavg <- NULL
        
    cat("IF\n")
    ifpreds <-
      IsolationForest(
        form = target ~.,
        train_eps = tr,
        validation = validation,
        test = test)
    
    cat("CLASS\n")
    t0 <- Sys.time()
    classpreds <-
      StdClassification(
        form = target ~.,
        train_eps = tr,
        validation = validation,
        test = test,
        classifier = classifier)
    CLASS_runtime <- Sys.time() - t0

    cat("LL\n")
    t0 <- Sys.time()
    llpreds <-
      LayeredLearning(
        form = form,
        form_surr = target_surrogate ~.,
        tr_eps = tr,
        validation = validation,
        test = test,
        RU = RU, 
        classifier=classifier)
    LL_runtime <- Sys.time() - t0

    InEval <-
      InnerEvaluation(yhat_l = llpreds,
                      task_targets = TGT)
    
    predsf <- c(CLASS=list(classpreds$CLASS$predsf),
                IF = list(ifpreds$IF$predsf),
                REGR = list(regrpreds$REGR$predsf),
                llpreds$preds_i)

    predsp <- c(CLASS=list(classpreds$CLASS$predsp),
                IF=list(ifpreds$IF$predsp),
                llpreds$preds_p)
    
    runtimes <- 
      list(CLASS_runtime=CLASS_runtime,
           LL_runtime = LL_runtime)

    list(predsl = predsf,
         predsfsp = predsp,
         trues = trues,
         inner_evals = InEval,
         runtimes = runtimes)
  }

InnerEvaluation <-
  function(yhat_l, task_targets) {

    task_targets$T2_tgt <- as.integer(as.character(task_targets$T2_tgt))
    task_targets$T1_tgt <- as.integer(as.character(task_targets$T1_tgt))

    #RQ1 - fA predicting event A
    P1 <- yhat_l$preds_i$f1_t1_yhat
    T1 <- task_targets$T1_tgt
    RQ1 <- round(classPerf(P1, T1), 2)

    #RQ2 - fB predicting event B
    T2_A1 <- task_targets$T2_tgt[task_targets$T1_tgt > 0]
    P2_A1 <- yhat_l$preds_i$f2_t2_yhat[task_targets$T1_tgt > 0]
    RQ2 <- round(classPerf(P2_A1, T2_A1), 2)

    #RQ3 - fB predicting non-event A
    T2_A0 <- task_targets$T2_tgt[task_targets$T1_tgt < 1]
    P2_A0 <- yhat_l$preds_i$f2_t2_yhat[task_targets$T1_tgt < 1]
    RQ3 <- round(classPerf(P2_A0, T2_A0), 2)

    rbind(RQ1, RQ2, RQ3)
  }
