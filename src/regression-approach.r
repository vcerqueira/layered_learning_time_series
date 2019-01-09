regression_ahe_pred <- 
  function(form, form_num, train_eps,validation,test) {
    
    train <- do.call(rbind.data.frame, train_eps)
    
    train$target_surrogate <- NULL
    train$target <- NULL
    
    yhat <-
      multistep_forecasting(
        form = form_num,
        train = train,
        test = validation,
        ntrees = 25,
        prediction_horizon = 30,
        multi_step_fun = msf_direct
      )
    
    ratio_below_60_val <-
      apply(yhat, 1, function(z) {
        sum(z < 60) / ncol(yhat)
      })
    
    
    thr <- optimize_threshold(form = form,
                              x = validation,
                              y_hat_prob = ratio_below_60_val)
    
    yhat_test <-
      multistep_forecasting(
        form = form_num,
        train = train,
        test = test,
        ntrees = 25,
        prediction_horizon = 30,
        multi_step_fun = msf_direct
      )
    
    ratio_below_60_test <-
      apply(yhat_test, 1, function(z) {
        sum(z < 60) / ncol(yhat_test)
      })
    
    predsf <- as.integer(ratio_below_60_test > thr)
    
    list(REGR=list(predsf=predsf, predsp = ratio_below_60_test))
  }



multistep_forecasting <-
  function(form,
           train,
           test,
           ntrees,
           prediction_horizon,
           multi_step_fun = msf_direct) {
    
    require(ranger)
    
    f_x <-
      ranger(form,
             train,
             num.trees = ntrees,
             classification = FALSE)
    
    predictions <-
      multi_step_fun(train = train,
                     test = test,
                     h = prediction_horizon,
                     model_h1 = f_x,
                     form = form,
                     ntrees = ntrees)
    
    predictions
  }


msf_direct <-
  function(train, test, h, model_h1, form, ntrees) {
    require(ranger)
    
    predictions <-
      matrix(0, nrow = nrow(test), ncol = h)
    
    colnames(predictions) <-
      paste("t", 1:h, sep = "+")
    
    tgtID <- grep(get_target(form), colnames(train))
    
    for (i in 1:h) {
      cat("Step ", i, "\n")
      if (i == 1) {
        predictions[,i] <- predict(model_h1, test)$predictions
      } else {
        train[,tgtID] <- c(train[-1,tgtID], NA)
        train <- train[-nrow(train), ]
        
        m_i <-
          ranger(form,
                 train,
                 num.trees = ntrees,
                 classification = FALSE)
        
        predictions[,i] <- predict(m_i, test)$predictions
      }
    }
    
    predictions
  }