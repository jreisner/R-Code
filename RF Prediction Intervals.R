# ------------------------------------------------------------------------------
# ----------------- PREDICTION INTERVALS FOR RANDOM FORESTS --------------------
# ------------------------------------------------------------------------------
# This function uses mclapply from the parallel package, so it works for Mac
# and Linux
#
# Variable Definitions
#   rf: a random forest of class ranger
#   train.dat: training data; all variables in training data except for dependent
#   train.y: dependent variable in training data
#   test.dat: test data, including all independent and dependent variables
#   cores: number of cores to parallel process on
#   alpha: sample quantile from vector of weighted oob errors
#
# Last updated: February 6, 2018
# ------------------------------------------------------------------------------

library(ranger)
ISU_RFpi <- function(rf, train.dat, train.y, test.dat = train.dat, 
                     cores = 4, alpha = 0.95) {
  
  if (is.null(rf$inbag.counts)) {
    stop("Random forest must be trained with keep.inbag = TRUE")
  } 
  if("parallel" %in% rownames(installed.packages()) == FALSE) {
    stop("Parallel package not installed. Install it and run again")
  }
  
  pred_train <- predict(rf, train.dat, type = 'terminalNodes', predict.all = TRUE)
  chnodes <- pred_train[["predictions"]]
  
  pred_test <- predict(rf, test.dat, type = 'terminalNodes', predict.all = TRUE)
  pred.nodes <- pred_test[["predictions"]]
  
  inbag.matrix <- do.call(cbind, rf$inbag.counts)
  
  d <- NULL
  d.func <- function(i) {
    leaf.i <- chnodes[i,]
    
    weight.i <- inbag.matrix
    weight.i[chnodes != matrix(leaf.i, nrow(chnodes), ncol(chnodes), byrow = TRUE)] <- 0
    weight.i <- weight.i / matrix(colSums(weight.i), nrow(chnodes), ncol(chnodes), byrow = TRUE)
    
    avg.weight.i <- rowMeans(weight.i[, inbag.matrix[i, ] == 0]) 
    
    d[i] <- abs((train.y[i] - rf[["predictions"]][i]) / 
                  sqrt(1 + crossprod(avg.weight.i, avg.weight.i)))
  }
  d <- unlist(parallel::mclapply(1:rf$num.samples, d.func, mc.cores = cores), use.names = FALSE)
  d.a <- quantile(d, alpha) 
  
  pred.weight <- matrix(nrow = nrow(test.dat), ncol = nrow(train.dat))
  pred.func <- function(i) {
    leaf.i <- pred.nodes[i,]
    
    weight.i <- inbag.matrix
    weight.i[chnodes != matrix(leaf.i, nrow(chnodes), ncol(chnodes), byrow = TRUE)] <- 0
    weight.i <- weight.i / matrix(colSums(weight.i), nrow(chnodes), ncol(chnodes), byrow = TRUE)
    
    pred.weight[i,] <- rowMeans(weight.i) 
  }
  pred.weight <- parallel::mclapply(1:nrow(test.dat), pred.func, mc.cores = cores)
  pred.weight <- t(sapply(pred.weight, unlist))
  
  predicted <- predict(rf, test.dat)
  ci.lwr <- predicted[["predictions"]] - d.a * sqrt(diag(tcrossprod(pred.weight)) + 1)
  ci.upr <- predicted[["predictions"]] + d.a * sqrt(diag(tcrossprod(pred.weight)) + 1)
  
  out <- data.frame(pred = predicted[["predictions"]], 
                    ci.lwr = ci.lwr, 
                    ci.upr = ci.upr)
  
  return(out)
}