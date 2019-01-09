IsolationForest <-
  function(form,
           train_eps,
           validation,
           test) {
    
    train <- do.call(rbind.data.frame, train_eps)
    
    train$target_surrogate <- NULL
    train$target_surrogate60 <- NULL
    
    tgt <- get_target(form)
    tgtID <- which(colnames(train) %in% tgt)
    train[, tgtID] <- as.factor(train[, tgtID])
    
    train <- train[sample(1:nrow(train), nrow(train)), ]
    
    probs_validation <-
      isoforest_probs(
        form = form,
        train = train,
        test = validation,
        sample.size = .1
      )
    
    thr <- optimize_threshold(form = form,
                              x = validation,
                              y_hat_prob = probs_validation)
    
    probsf <-
      isoforest_probs(
        form = form,
        train = train,
        test = test,
        sample.size = .1
      )
    
    predsf <- as.integer(probsf > thr)
    
    list(IF = list(predsf = predsf, predsp = probsf))
  }


isoforest_probs <-
  function(form,
           train,
           test, sample.size) {
    
    require(isofor)#Zelazny7/isofor
    
    tg <- get_target(form)
    colid <- which(colnames(train) %in% tg)
    
    train <- subset(train, select = - colid)
    test <- subset(test, select = - colid)
    
    iF <-
      iForest(train,
              nt = 25,
              phi = ceiling(sample.size * nrow(train)))
    
    y_hat_probs <- predict(iF, test)
    
    y_hat_probs
  }