load("data_sample/mimicsample.rdata")

library(parallel)
library(caret)

source("src/classification.r")
source("src/classifiers.R")
source("src/fit-layers.r")
source("src/layered-learning.r")
source("src/optimize-thresholds.r")
source("src/task-decomposition.r")
source("src/workflow.r")
source("src/classifier-xgboost.r")
source("src/classifier-mlp.r")
source("src/utils.r")
source("src/split-events.r")
source("src/performance-metrics.r")
source("src/result-analysis.r")
source("src/regression-approach.r")
source("src/isolation-forest.r")


has_episode <-
  sapply(mimic,
         function(x) {
           any(x$target == 1L, na.rm = TRUE)
         })

as.numeric <- as.numeric(has_episode)


#set.seed(1234)
fresults <-
  mclapply(1:5,
           function(i) {

             nfolds <- 10
             folds <- createFolds(y = has_episode, 
                                  k = nfolds, 
                                  list = FALSE)

             res <-
               lapply(1:nfolds,
                        function(i) {
                          cat(i, "\n")

                          tr_folds <- which(!(1:nfolds %in% i))
                          smpI <- sample(tr_folds, 1)
                          vl_folds <- tr_folds[tr_folds %in% smpI]
                          tr_folds <- tr_folds[!tr_folds %in% smpI]

                          tr <- mimic[folds %in% tr_folds]
                          vl <- mimic[folds %in% vl_folds]
                          ts <- mimic[folds %in% i]
                          
                          tr <- split_episodes_in_database(tr, target ~.)
                          vl <- split_episodes_in_database(vl, target ~.)
                          ts <- split_episodes_in_database(ts, target ~.)

                          LL_workflow(
                            form = target ~.,
                            tr_eps = tr,
                            vl_eps = vl,
                            ts_eps = ts,
                            RU = TRUE,
                            classifier = "xgb"
                          )
                        })
             res
           }, mc.cores = 5)


fresults <- unlist(fresults, recursive = FALSE)
fresults_analysis <- wrapEval(fresults, 60)

#