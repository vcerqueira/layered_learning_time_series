library(xts)
library(tsensembler)

embedfinal <- 
  function(x) {
    xe <- large_episode_embed(x)
    
    obser <-
      lapply(xe,
             function(u) {
               subset(u, 
                      select = grep("obsW.", 
                                    colnames(u)))
             })

    FEATS <- feng(obser)
    target_hyp <- mark_hypotension(xe$MAP, 60, .9)
    target_surr <- mark_hypotension(xe$MAP, 70, .9)
    
    FEATS$target <- target_hyp
    FEATS$target_surrogate <- target_surr
    
    FEATS
  }


large_episode_embed <- 
  function(x,tgtW=30,gapW=60,obsW=60) {
    x <- xts(x = x[,-1], order.by = x[,1])
    dttm <- as.POSIXct(rownames(as.data.frame(x)))
    seq. <- 1:ncol(x)
    cn <- colnames(x)
    
    extended_embedded_episode <-
      lapply(seq.,
             function(j) {
               x <- xts(x = x[, j], order.by = dttm)
               
               X_kd <- embed_timeseries(x, tgtW + gapW + obsW)
               
               colnames(X_kd)[seq_len(tgtW)] <-
                 paste0("tgtW.", seq_len(tgtW))
               colnames(X_kd)[(tgtW + 1):(tgtW + gapW)] <-
                 paste0("gapW.", seq_len(gapW))
               colnames(X_kd)[(tgtW + gapW + 1):ncol(X_kd)] <-
                 paste0("obsW.", seq_len(obsW))
               colnames(X_kd) <-
                 paste(cn[j], colnames(X_kd), sep = ".")
               
               X_kd
             })
    
    names(extended_embedded_episode) <- cn
    
    extended_embedded_episode
  }


mark_hypotension <-
  function(x, blood_pressure_threshold=60, ratio_threshold=.9) {
    tgtWids <- grep("tgtW.", colnames(x))
    
    X_tgt <- subset(x, select = tgtWids)
    
    is_hypotensive <-
      apply(X_tgt, 1,
            function(w) {
              below_thr <- w < blood_pressure_threshold
              
              ratio_below_thr <- sum(below_thr) / length(w)
              
              ifelse(ratio_below_thr >= ratio_threshold, 1L, 0L)
            })
    
    is_hypotensive
  }


feng <- 
  function(dfl) {
    require(wmtsa)
    cn <- names(dfl)
    
    cat("Computing stats\n")
    statistical_feats <- 
      lapply(dfl, 
             function(u) {
               capture.output(suppressWarnings(sts_u <- feature_extraction_i(u)))
               sts_u
             })
    
    X1 <- do.call(cbind.data.frame, statistical_feats)
    cat("Computing CCFs\n")
    cc_feats <- pairwise_ccf(dfl, cn)
    cat("Computing wave trans\n")
    wavelet_feats <-
      lapply(dfl,
             function(x) {
               df <-
                 t(apply(x, 1,
                         function(u) {
                           tryCatch(wavelet_dau(u, lvl = 4),
                                    error = function(e) rep(0, times = 5))
                         }))
               
               as.data.frame(df)
             })
    
    for (i in seq_along(wavelet_feats)) {
      colnames(wavelet_feats[[i]]) <-
        paste0(names(wavelet_feats[i]), "_WV",1:5)

      wavelet_feats[[i]][is.na(wavelet_feats[[i]])] <- 0.
    }
    
    X2 <- do.call(cbind.data.frame, wavelet_feats)
    
    df <- cbind.data.frame(X1,X2,cc_feats)
    rownames(df) <- rownames(dfl[[1]]) 
    
    df
  }

# non_overlap_windows <-
#   function(ds, k, varname, strat) {
#     n <- nrow(ds)
#     xrep <- ceiling(n / (k-1))
#     
#     ids <- rep(seq_len(k-1), times = xrep)
#     
#     embed_ids <- utils::head(ids, n)
#     
#     condition_1 <- !(embed_ids %in% 1)
#     
#     cond.aux <- colnames(ds) %in% varname
#     tgtvec <- as.numeric(as.character(subset(ds, select=cond.aux)[[1]]))
#     
#     if (strat=="classification") {
#       condition_2 <- tgtvec == 0#in neg activity
#     } else if (strat=="survival") {
#       condition_2 <- tgtvec > 59#in neg activity
#     } else if (strat=="regression") {
#       condition_2 <- tgtvec >= 65#in neg activity
#     }
#     
#     dsf <- subset(ds, subset = !(condition_1 & condition_2))
#     #rownames(dsf) <- NULL
#     
#     dsf
#   }


