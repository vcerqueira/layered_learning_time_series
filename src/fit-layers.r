fit_cc_layers <-
  function(T1, T2,
           form_T1, form_T2,
           test, classifier) {

    T1$T1_lbl <- as.factor(T1$T1_lbl)
    T2$T2_lbl <- as.factor(T2$T2_lbl)

    probs_t1 <-
      Classification(
        form = form_T1,
        train = T1,
        test = test,
        predictive_algorithm = classifier
      )
    
    probs_t2 <-
      Classification(
        form = form_T2,
        train = T2,
        test = test,
        predictive_algorithm = classifier
      )
    
    list(f1_hat = probs_t1, 
         f2_hat = probs_t2)
  }
