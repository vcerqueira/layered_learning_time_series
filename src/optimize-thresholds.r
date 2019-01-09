OptimizeLLthresholds <-
  function(form, T1, T2, validation, classifier) {
    form1<<-form#<-form1
    T11<<-T1#<-T11
    T21<<-T2#<-T21
    vl<<-validation#<-vl
    
    vl_targets <-
      targets_predtasks(
        form_surr = target_surrogate ~ .,
        form = target ~ .,
        x = validation)

    validation$target_surrogate <- NULL
    
    predsll <-
      fit_cc_layers(
        T1 = T1,
        T2 = T2,
        form_T1 = T1_lbl ~ .,
        form_T2 = T2_lbl ~ .,
        test = validation,
        classifier = classifier)

    validation$trues_t1 <- vl_targets$T1_tgt
    validation$trues_t2 <- validation$target#vl_targets$T2_tgt

    f1_solo <- predsll$f1_hat
    f2_solo <- predsll$f2_hat
    
    g <- f1_solo * f2_solo

    thr_f1_solo <- optimize_threshold(form, validation, f1_solo)
    thr_f2_solo <- optimize_threshold(form, validation, f2_solo)
    thr_g <- optimize_threshold(form, validation, g)
    thr_f1_t1 <- optimize_threshold(trues_t1 ~., validation, f1_solo)
    thr_f2_t2 <- optimize_threshold(trues_t2 ~., validation, f2_solo)

    c(thr_f1_solo = thr_f1_solo,
      thr_f2_solo = thr_f2_solo,
      thr_g = thr_g,
      thr_f1_t1= thr_f1_t1,
      thr_f2_t2 = thr_f2_t2)
  }


adjust_threshold <-
  function(predsl, test, form, decision_thresh_seq) {
    trues <- get_y(test, form)

    cm <- sapply(predsl,
                 function(x) {
                   classPerf(x, trues)
                 })

    senspec <-
      apply(cm, 2, function(x) {
        (x["sens"] + x["spec"]) / 2
      })

    best_thresh <- decision_thresh_seq[which.max(senspec)]

    best_thresh
  }

optimize_threshold <-
  function(form, x, y_hat_prob) {
    dseq <- seq(from=.025, to=.9, by=.025)
    Y_hat <- Map(function(u) as.integer(y_hat_prob > u), dseq)

    best_thresh <-
      adjust_threshold(predsl = Y_hat,
                       test = x,
                       form = form,
                       decision_thresh_seq = dseq)

    best_thresh
  }

