MLP_classifier <- 
  function(form, train, test) {
    form1<<-form#<-form1
    train1<<-train#<-train1
    test1<<-test#<-test1
    epochs <- 100
    require(tsensembler)
    require(keras)
    
    colID <- which(colnames(train) %in% get_target(form))
    
    y_tr <- get_y(train, form)
    y_tr <- as.integer(as.character(y_tr))
    #y_ts <- get_y(test, form)
    test$target_tgtavg <- NULL
    test$target_tgt30 <- NULL
    test$target_tgtmin <- NULL
    test$target_tgtmax <- NULL
    
    
    X_tr <- subset(train, select = -colID)
    nfeats <- ncol(X_tr)
    X_ts <- subset(test, select = -colID)
    X_tr <- scale(X_tr)
    X_ts <- scale(X_ts)
    
    model <- keras_model_sequential() %>%
      layer_dense(units = 32, 
                  activation = "relu",
                  input_shape = nfeats) %>%
      layer_dropout(rate = 0.33) %>% 
      layer_dense(units = 32, activation = "relu") %>%
      layer_dropout(rate = 0.33) %>% 
      layer_dense(units = 2, activation = "softmax")
    
    model %>% compile(
      loss = "sparse_categorical_crossentropy",
      optimizer = 'adam',
      metrics = 'crossentropy'
    )
    
    print_dot_callback <- callback_lambda(
      on_epoch_end = function(epoch, logs) {
        if (epoch %% 100 == 0) cat(".\n")
        #cat("")
      }
    )    
    
    mlp.fit <- model %>% fit(
      X_tr,
      y_tr,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 0,
      callbacks = list(print_dot_callback)
    )
    
    yhat <- model %>% predict_on_batch(X_ts)
    #yhat <- model %>% predict_classes(X_ts)
    
    #as.integer(yhat[,1] > .5)
    yhat[,2]
  } 
