xgb_predict <-
  function(model, newdata) {
    require(xgboost)
    Y <- get_y(newdata, model$xgb_optimd$form)
    
    newdata  <- model.matrix(model$xgb_optimd$form, newdata)
    
    dtest <- xgb.DMatrix(newdata, label = Y)
    
    predict(model$xgb_optimd$xgb_optimd$xgb_optimd, dtest)
  }

XGBprobs <-
  function(form, data, test) {
    require(xgboost)
    
    cn <- colnames(data)
    data <- data[sample(1:nrow(data), nrow(data)),]
    Y <- get_y(data, form)
    Y <- as.integer(as.character(Y))
    
    # grid_search <-
    #   expand.grid(
    #     eta = c(.1, .2,.3, .7, .9),
    #     max_depth = c(2, 4, 6, 10),
    #     nrounds = c(50, 25, 15,10, 6)
    #   )
    
    data <- stats::model.matrix(form, data)
    dtrain <- xgb.DMatrix(data, label = Y)
    
    #params <- xgb_optimizer(data, Y, grid_search)
    
    fparams <-
      list(
        max_depth = 4,#params$max_depth,
        eta = .2,#params$eta,
        silent = 1,
        objective = "binary:logistic",
        eval_metric = "auc"
      )
    
    utils::capture.output(model <-
                            xgb.train(fparams,
                                      dtrain,
                                      nrounds = 20))#params$nrounds))#
    
    test$target <- NULL
    test$target_surrogate <- NULL
    
    tgt <- get_target(form)
    if (get_target(form) %in% c("T1_lbl", "T2_lbl")) {
      test$target <-NULL
    }
    
    test$tmp <- rep(0L, times=nrow(test))
    colnames(test)[ncol(test)] <- as.character(form[[2]])
    
    Y <- tryCatch(get_y(test, form), 
                  error = function(e) {
                    rep(0L, times=nrow(test))
                  })
    Y <- as.integer(as.character(Y))
    
    test  <- model.matrix(form, as.data.frame(test))

    dtest <- xgb.DMatrix(test, label = Y)
    preds <- predict(model, dtest)
    
    preds
  }
