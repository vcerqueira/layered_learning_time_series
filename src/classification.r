StdClassification <-
  function(form, train_eps, validation, test, classifier) {
    require(UBL)
    
    train <- do.call(rbind.data.frame, train_eps)
    
    train$target_surrogate <- NULL
    train$target_surrogate60 <- NULL
    
    tgt <- get_target(form)
    tgtID <- which(colnames(train) %in% tgt)
    train[,tgtID] <- as.factor(train[,tgtID])
    
    train <- RandUnderClassif(form, train)
    train <- train[sample(1:nrow(train), nrow(train)),]
    
    probs_validation <-
      Classification(
        form = form,
        train = train,
        test = validation,
        predictive_algorithm = classifier
      )
    
    thr <- optimize_threshold(form = form,
                              x = validation,
                              y_hat_prob = probs_validation)
    
    
    probsf <-
      Classification(
        form = form,
        train = train,
        test = test,
        predictive_algorithm = classifier
      )
    
    predsf <- as.integer(probsf > thr)
    
    list(CLASS=list(predsf=predsf, predsp = probsf))
  }


