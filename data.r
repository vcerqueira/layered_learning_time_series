source("src/data-munging.r")
source("src/feat-engineering.r")

load("data_sample/mimicsampleraw.rdata")

library(parallel)
mimic <-
  lapply(1:length(mimic),
           function(i) {
             cat("EPISODE ", i, "\n")
             X <- mimic[[i]] 
             X <- embedfinal(X)
             print(head(X,1))
             X
           })

mimic <- rm.null(mimic)

mimic <-
  lapply(mimic,
         function(x) {
           x[complete.cases(x),]
         })

mimic <- mimic[sapply(mimic, nrow) > 100]
names(mimic) <- paste("EP", 1:length(mimic), sep = "_")

#save(mimic, file = "data_sample/mimicsample.rdata")
#