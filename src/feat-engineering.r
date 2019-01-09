feature_extraction_i <-
  function(x) {
    cat("Computing trend ...\n")
    ts_trend <- apply(x, 1, trend)
    
    cat("Computing slope ...\n")
    ts_slope <- apply(x, 1, 
                      function(u) {
                        tryCatch(slope_vector(u),
                                 error = function(e) NA)
                      })
    
    cat("Computing mean ...\n")
    ts_mean <- rowMeans(x, na.rm = TRUE)
    
    cat("Computing skewness ...\n")
    ts_skew <- apply(x, 1, moments::skewness, na.rm=TRUE)
    
    cat("Computing kurtosis ...\n")
    ts_kts <- apply(x, 1, moments::kurtosis, na.rm=TRUE)
    
    cat("Computing median ...\n")
    ts_median <- apply(x, 1, median, na.rm=TRUE)
    
    cat("Computing min ...\n")
    ts_min <- apply(x, 1, min, na.rm=TRUE)
    
    cat("Computing max ...\n")
    ts_max <- apply(x, 1, max, na.rm=TRUE)
    
    cat("Computing var ...\n")
    ts_var <- apply(x, 1, var, na.rm=TRUE)
    
    cat("Computing standard deviation ...\n")
    ts_stddev <- apply(x, 1, sd, na.rm=TRUE)
    
    cat("Computing iqr ...\n")
    ts_iqr <- apply(x, 1, IQR, na.rm=TRUE)
    
    ts_dyns <-
      data.frame(
        ts_skew = ts_skew,
        ts_trend = ts_trend,
        ts_slope = ts_slope,
        ts_kts = ts_kts,
        ts_median = ts_median,
        ts_min = ts_min,
        ts_max = ts_max,
        ts_var = ts_var,
        ts_mean = ts_mean,
        ts_stddev = ts_stddev,
        ts_iqr = ts_iqr
      )
    
    ts_dyns <- replace_inf(ts_dyns)
    
    rownames(ts_dyns) <- NULL
    
    dplyr::as_tibble(ts_dyns)
  }


slope_vector <- 
  function(x) {
    #ver se a direcao do x ta certa
    time_x <- seq_along(x)
    lmfit <- lm(x ~ time_x)
    
    lmfit$coefficients[[2]]
  }

trend <-
  function(x) {
    sd(x) / sd(diff(x)[-1])
  }

wavelet_dau <- 
  function(x, lvl=4) {
    
    if (all(is.na(x))) {
      return(rep(0, times = lvl+1))
    }
    
    result <- tryCatch(wavDWT(x, wavelet="s8", n.levels=lvl),
                       error = function(e) NA)
    
    if (is.na(result[1])) {
      return(rep(0, times = lvl+1))
    }
    
    dwt_result <- result$data
    
    E_a5 <- reconstruct(result)
    E_a5 <- norm(t(E_a5))
    
    E_dk <- sapply(dwt_result[1:4], 
                   function(x) norm(t(x)))
    names(E_dk) <- paste0("Ed", 1:length(E_dk))
    
    E_T <- E_a5 + sum(E_dk)
    
    E_ra5 <- E_a5 / E_T
    
    E_rdk <- E_dk / E_T
    
    c(E_ra5=E_ra5,E_rdk)
  }

pairwise_ccf <- 
  function(dfl, feats) {
    seq. <- 1:nrow(dfl[[1]])
    
    combs <- combn(x = feats, m = 2)
    
    m <- matrix(0, nrow=length(seq.), ncol=ncol(combs))
    
    for (u in 1:ncol(combs)) {
      select_ds <- dfl[combs[, u]]
      
      m[,u] <-
        vapply(seq.,
               function(i) {
                 x_1 <- unlist(select_ds[[1]][i,])
                 x_2 <- unlist(select_ds[[2]][i,])
                 
                 tryCatch(ccf(
                   x = x_1,
                   y = x_2,
                   lag.max = 0,
                   plot = FALSE)$acf[[1]], 
                   error=function(e) NA)
                 
                 
               }, FUN.VALUE = double(1L))
    }
    
    colnames(m) <-
      paste0("ccf_",
             apply(combs, 2, function(u) {
               paste(u,
                     collapse = "")
             }))
    
    as.data.frame(m)
  }

trend <-
  function(x) {
    sd(x) / sd(diff(x)[-1])
  }

max_lyapunov_exp <-
  function(x) {
    require(nonlinearTseries)
    
    len <- length(x)
    Reduce(max,
           nonlinearTseries::divergence(
             nonlinearTseries::maxLyapunov(
               time.series = x,
               min.embedding.dim = ceiling(len / 4),
               max.embedding.dim = ceiling(len / 2),
               radius = ceiling(len / 6),
               do.plot = FALSE
             )
           ))
  }

HURST <-
  function(x) {
    #require(Rwave)

    cwtwnoise <- DOG(x, 10, 3, 1, plot = FALSE)
    mcwtwnoise <- Mod(cwtwnoise)
    mcwtwnoise <- mcwtwnoise * mcwtwnoise
    wspwnoise <- tfmean(mcwtwnoise, plot = FALSE)

    hurst.est(wspwnoise, 1:7, 3, plot = FALSE)[[2]]
  }


replace_inf <- 
  function (df)  {
    do.call(data.frame, lapply(df, function(j) {
        replace(j, is.infinite(j), NA)
    }))
}
