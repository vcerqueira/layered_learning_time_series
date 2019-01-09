layered_predtasks <-
  function(form_surrogate, form, x) {
    
    tgt_surr <- get_target(form_surrogate)
    tgt_lbl <- get_target(form)
    
    tgtnID <- which(colnames(x) %in% tgt_surr)
    tgtID <- which(colnames(x) %in% tgt_lbl)
    
    y_surr <- get_y(x, form_surrogate)
    x$T1_lbl <- y_surr
    
    T1 <- subset(x, select = -tgtnID)
    
    T2 <- T1[T1$T1_lbl > 0,]
    T2$T2_lbl <- get_y(T2, form)
    
    T2 <- subset(T2, select = -tgtID)
    T2$T1_lbl <- NULL
    
    T1 <- subset(T1, select = -tgtID)
    
    T1$T1_lbl <- as.factor(T1$T1_lbl)
    T2$T2_lbl <- as.factor(T2$T2_lbl)
    
    list(T1=T1, T2=T2)
  }



targets_predtasks <-
  function(form_surr, form, x) {
    x1<<-x
    ltasks <-
      layered_predtasks(form_surrogate = form_surr,
                        form = form,
                        x = x)
    
    T1_tgt <- ltasks$T1$T1_lbl
    T2_tgt <- get_y(x, form)
    
    T1_tgt <- factor(T1_tgt, levels = c(1L,0L))
    T2_tgt <- factor(T2_tgt, levels = c(1L,0L))

    list(T1_tgt=T1_tgt, T2_tgt=T2_tgt)
  }

