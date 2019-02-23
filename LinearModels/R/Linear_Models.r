LMSquareLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec ) {
  # what is opt.thresh and how do you minimize the loss function implemented below? I thought the penalty vec was
  # taking steps to minimize loss through the different penalty values
  optimal.weight.vec <- initial.weight.vec
  y.pred.vec <- X.scaled.mat %*% initial.weight.vec
  y.residuals.vec <- y.vec - y.pred.vec
  y.redisuals.squared.vec <- y.residuals.vec * y.residuals.vec
  y.residuals.squared <- sum(y.redisuals.squared.vec)
  loss <- y.residuals.squared + penalty * (initial.weight.vec * initial.weight.vec)
  return(optimal.weight.vec)
}

LMLogisticLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec ) {
  # what is opt.thresh and how do you minimize the loss function implemented below? I thought the penalty vec was
  # taking steps to minimize loss through the different penalty values
  optimal.weight.vec <- initial.weight.vec
  y.pred.vec <- X.scaled.mat %*% initial.weight.vec
  y.residuals.vec <- y.vec - y.pred.vec
  y.redisuals.squared.vec <- y.residuals.vec * y.residuals.vec
  y.residuals.squared <- sum(y.redisuals.squared.vec)
  loss <- y.residuals.squared + penalty * (initial.weight.vec * initial.weight.vec)
  return(optimal.weight.vec)
}

LMSquareLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # set y vec seq(1, 3, by=0.2)
  # set X.mat replicate(4, rnorm(15))
  # set y vec y.vec = runif(n = 15, min = 1, max = 5)
  # start by scaling X.mat so each column mean is 0 and std is 1
  X.scaled.mat <- scale(X.mat)
  print(X.scaled.mat)
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  opt.thresh <- 1
  # random non-zero weights?
  initial.weight.vec <- runif(n = ncol(X.mat), min = 1, max = 5)
  W.mat <- sapply(penalty.vec, LMSquareLossL2, y.vec = y.vec, opt.thresh = opt.thresh, initial.weight.vec = initial.weight.vec)
  # output should be W.mat (n_features x n_penalties) on original scale
  return(W.mat)
}

LMLogisticLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # set y vec seq(1, 3, by=0.2)
  # set X.mat replicate(4, rnorm(15))
  # set y vec y.vec = runif(n = 15, min = 1, max = 5)
  # start by scaling X.mat so each column mean is 0 and std is 1
  X.scaled.mat <- scale(X.mat)
  print(X.scaled.mat)
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  opt.thresh <- 1
  # random non-zero weights?
  initial.weight.vec <- runif(n = ncol(X.mat), min = 1, max = 5)
  W.mat <- sapply(penalty.vec, LMSquareLossL2, y.vec = y.vec, opt.thresh = opt.thresh, initial.weight.vec = initial.weight.vec)
  # output should be W.mat (n_features x n_penalties) on original scale
  return(W.mat)
}
