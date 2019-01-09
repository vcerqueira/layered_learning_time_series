LayeredLearning <-
  function(form,
           form_surr,
           tr_eps,
           validation,
           test,
           RU,
           classifier) {
    
    require(UBL)
    
    epdata <- vector("list", length(tr_eps))
    for (i in seq_along(epdata)) {
      epdata[[i]] <-
        layered_predtasks( 
          form_surrogate = form_surr,
          form = form,
          x = tr_eps[[i]]
        )
    }
    names(epdata) <- names(tr_eps)
    
    T1 <- lapply(epdata, function(x) x$T1)
    T2 <- lapply(epdata, function(x) x$T2)
    
    T1 <- do.call(rbind.data.frame, T1)
    T2 <- do.call(rbind.data.frame, T2)
    
    if (RU) {
      T1 <- tryCatch(RandUnderClassif(T1_lbl ~ ., T1),
                     error=function(er) T1)
      T2 <- tryCatch(RandUnderClassif(T2_lbl ~ ., T2),
                     error=function(er) T2)
    }
    
    print(table(T1$T1_lbl))
    print(table(T2$T2_lbl))
    
    thrs <-
      OptimizeLLthresholds(
        form = target ~ .,
        T1 = T1,
        T2 = T2,
        validation = validation,
        classifier = classifier
      )
    
    predsll <-
      fit_cc_layers(
        T1 = T1,
        T2 = T2,
        form_T1 = T1_lbl ~ .,
        form_T2 = T2_lbl ~ .,
        test = test,
        classifier = classifier
      )
    
    yhat <- making_predictions(yhat_l = predsll, thrs = thrs)
    
    yhat
  }

making_predictions <-
  function(yhat_l, thrs) {
    f_1 <- yhat_l$f1_hat
    f_2 <- yhat_l$f2_hat
    
    f1_solo <- f_1
    f2_solo <- f_2
    g <- f_1 * f_2

    f1_solo_yhat <- as.integer(f1_solo > thrs[["thr_f1_solo"]])
    f1_t1_yhat <- as.integer(f1_solo > thrs[["thr_f1_t1"]])
    f2_solo_yhat <- as.integer(f2_solo > thrs[["thr_f2_solo"]])
    f2_t2_yhat <- as.integer(f2_solo > thrs[["thr_f2_t2"]])
    g_yhat <- as.integer(g > thrs[["thr_g"]])
    gii1_yhat <- f1_t1_yhat * f2_t2_yhat
    gii2_yhat <- f1_solo_yhat * f2_solo_yhat
    
    pred_all_1 <- rep(1L, times = length(f1_solo_yhat))
    pred_all_0 <- rep(0L, times = length(f1_solo_yhat))
    
    preds_i <-
      list(
        f1_solo_yhat = f1_solo_yhat,
        f1_t1_yhat = f1_t1_yhat,
        f2_solo_yhat = f2_solo_yhat,
        f2_t2_yhat = f2_t2_yhat,
        g_yhat = g_yhat,
        gii1_yhat = gii1_yhat,
        gii2_yhat = gii2_yhat,
        pred_all_1 = pred_all_1,
        pred_all_0 = pred_all_0
      )
    
    preds_p <-
      list(
        f1_solo = f1_solo,
        f2_solo = f2_solo,
        g = g)
    
    list(preds_i = preds_i, preds_p = preds_p)
  }
