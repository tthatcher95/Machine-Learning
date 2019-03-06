LMSquareLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
  # what is opt.thresh and how do you minimize the loss function implemented below? I thought the penalty vec was
  # taking steps to minimize loss through the different penalty values
  w.t.minus.1 <- initial.weight.vec
  step.iteration = step.size
  g <- 1000
  while(g > opt.thresh ) {
    # print('X.scaled.mat')
    # print(X.scaled.mat) 
    # y.pred.vec <- X.scaled.mat %*% w.t.minus.1
    # print('y.pred.vec')
    # print(y.pred.vec)    
    # print('y.vec')
    # print(y.vec)
    # y.residuals.vec <- y.pred.vec - y.vec 
    # print('y.residuals.vec')
    # print(y.residuals.vec)
    # y.redisuals.squared.vec <- y.residuals.vec * y.residuals.vec
    # print('y.redisuals.squared.vec')
    # print(y.redisuals.squared.vec)
    # y.residuals.squared <- sum(y.redisuals.squared.vec)
    # print('y.residuals.squared')
    # print(y.residuals.squared)
    # loss <- y.residuals.squared + penalty * sum(w.t.minus.1 * w.t.minus.1)
    # print('Loss')
    # print(loss)
    # if (loss < opt.thresh) {
    #   optimal.weight.vector <- w.t.minus.1
    #   return(w.t.minus.1)
    # }
    
    # w.t <- w.t.minus.1 - step.iteration * loss
    y.res.vec <- X.scaled.mat %*% w.t.minus.1 - y.vec
    g.vec <- t(X.scaled.mat) %*% y.res.vec + (penalty * w.t.minus.1) 

    g <- sum(abs(g.vec))
    # print('g')
    # print(g)
    if (g < opt.thresh) {
      optimal.weight.vector <- w.t.minus.1

      return(optimal.weight.vector)
    }
    
    w.t <- w.t.minus.1 - step.size * g.vec
    
    w.t.minus.1 <- w.t
    step.iteration <- step.iteration + step.size
  }

}


LMLogisticLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
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
  # set penalty.vec <- seq(10, 1, by=-1)
  # set X.mat <- ozone[, c("temperature","radiation")]
  # set y.vec <- ozone[, "ozone"]
  # start by scaling X.mat so each column mean is 0 and std is 1
  X.scaled.mat <- scale(X.mat)
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  opt.thresh <- 0.05
  step.size = 0.01
  # random non-zero weights?
  # initial.weight.vec <- runif(n = ncol(X.mat), min = 1, max = 5)
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  W.mat <- sapply(penalty.vec, LMSquareLossL2, X.scaled.mat = X.scaled.mat, y.vec = y.vec, opt.thresh = opt.thresh, initial.weight.vec = initial.weight.vec, step.size = step.size)
  # Warm restart
  print(ncol(X.mat))
  print(length(penalty.vec))
  W.scaled.mat <- matrix(0, nrow = ncol(X.mat), ncol = length(penalty.vec))
  
  warm.weight.vec <- initial.weight.vec
  for (i in 1:length(penalty.vec)) {
    warm.weight.vec <- LMSquareLossL2(X.scaled.mat = X.scaled.mat, y.vec = y.vec, penalty = penalty.vec[i], opt.thresh = opt.thresh, initial.weight.vec = warm.weight.vec, step.size = step.size)
    W.scaled.mat[, i] <- warm.weight.vec
  }
  print(all.equal(W.scaled.mat, W.mat))
  # output should be W.mat (n_features x n_penalties) on original scale
  print(W.mat)
  print(W.scaled.mat)

  return(W.scaled.mat)
}


LMLogisticLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # set y vec seq(1, 3, by=0.2)
  # set X.mat replicate(4, rnorm(15))
  # set y vec y.vec = runif(n = 15, min = 1, max = 5)
  # start by scaling X.mat so each column mean is 0 and std is 1
  X.scaled.mat <- scale(X.mat)
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  opt.thresh <- 1
  # random non-zero weights?
  initial.weight.vec <- runif(n = ncol(X.mat), min = 1, max = 5)
  W.mat <- sapply(penalty.vec, LMSquareLossL2, y.vec = y.vec, opt.thresh = opt.thresh, initial.weight.vec = initial.weight.vec)
  # output should be W.mat (n_features x n_penalties) on original scale

  return(W.mat)
}
