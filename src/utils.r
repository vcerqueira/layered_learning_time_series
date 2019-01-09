get_y <-
  function(data, form) {
    stats::model.response(stats::model.frame(form, data, na.action = NULL))
  }


get_target <-
  function(form) {
    split_by(deparse(form), " ")[1]
  }

rm.null <- 
  function(l) {
    l[!sapply(l, is.null)]
  }


split_by <- 
  function(expr, split, unlist. = TRUE, ...) {
    expr <- strsplit(expr, split = split, fixed = TRUE, ...)
    if (unlist.) expr <- unlistn(expr)
    
    expr
  }

unlistn <- function(x) unlist(x, use.names = FALSE)
