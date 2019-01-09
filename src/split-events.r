split_episodes_in_database <-
  function(ep_list, form) {
    ep_list <-
      lapply(ep_list,
             split_episodes_by_event,
             form = form)
    
    ep_list <- unlist(ep_list, recursive = FALSE)
    
    names(ep_list) <-
      gsub("\\.","_", names(ep_list))
    
    ep_list
  }

split_episodes_by_event <-
  function(x, form) {
    require(data.table)
    y <- get_y(x, form)
    
    xrleid <- rleid(y)
    if (y[1] == 0) {
      splt_x <- split(x, xrleid)
      l <- length(splt_x)
      
      fvec <- list()
      iters <- which(1:l %% 2 > 0)#grab odds
      for (i in iters) {
        if (i < l) {
          fvec[[i]] <-
            rbind.data.frame(splt_x[[i]],
                             splt_x[[i+1]])
        } else {
          fvec[[i]] <- splt_x[[i]]
        }
      }
      fvec <- rm.null(fvec)
      names(fvec) <- paste0("PT", seq_along(fvec))
    } else {
      #fvec <- list(x)
      fvec<-NULL
    }
    fvec
  }


prune_events <-
  function(x, form, event_max_size) {
    y <- get_y(x,form)
    if (!any(y==1)) {
      return(x)
    }
    
    #ids2prune <- which(y==1)[-seq_len(event_max_size)]
    which1 <- which(y == 1)
    which0 <- which(!(y == 1))
    
    which1useful <- utils::head(which1, event_max_size)
    
    ids <- c(which0, which1useful)
    
    x <- x[ids,]
    
    x
  }