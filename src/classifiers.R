Classification <-
  function(form, train, test, predictive_algorithm) {
    pred_algo <- switch(predictive_algorithm,
                        "rf" = RFprobs,
                        "svm" = SVMprobs,
                        "xgb" = XGBprobs,
                        "mlp" = MLP_classifier,
                        RFprobs)
    
    cat("Fitting a model using a", predictive_algorithm, "\n")
    modelprobs <- pred_algo(form, train, test)
    
    modelprobs
  }


RFprobs <-
  function(form, train, test) {
    require(ranger)
    ntrees <- 25
    
    f_x <-
      ranger(form,
             train,
             num.trees = ntrees,
             probability = FALSE,
             classification = TRUE)
    
    preds <- predict(f_x, test, predict.all = TRUE)$predictions
    preds <- preds - 1
    
    probs <- rowSums(preds) / ntrees
    
    probs
  }


SVMprobs <-
  function(form, train, test) {
    require(kernlab)
    C=10;
    epsilon=.01;
    kernel="rbfdot"
    
    model <-
      ksvm(
        form,
        train,
        scaled = TRUE,
        kernel = kernel,
        C = C,
        epsilon=epsilon,
        prob.model =TRUE)
    
    probs_test <- predict(model, test, type = "prob")[,"1"]
    
    probs_test
  }
