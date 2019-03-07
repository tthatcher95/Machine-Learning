# data(ozone, package="ElemStatLearn")
# penalty.vec <- seq(10, 1, by=-0.5)
# X.mat <- ozone[, c("temperature","radiation")]
# y.vec <- ozone[, "ozone"]

LMSquareLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
  w.t.minus.1 <- initial.weight.vec
  
  g <- 100000
  
  while(g > opt.thresh ) {
    y.res.vec <- (X.scaled.mat %*% w.t.minus.1) - y.vec
    g.vec <- (t(X.scaled.mat) %*% (y.res.vec)) + (penalty * w.t.minus.1)
    # g <- sum(g.vec^2)
    g <- sum(abs(g.vec))

    if (g <= opt.thresh) {
      optimal.weight.vector <- w.t.minus.1

      return(optimal.weight.vector)
    }
    w.t <- w.t.minus.1 - (step.size * g.vec)
    w.t.minus.1 <- w.t
  }

}


LMLogisticLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
  w.t.minus.1 <- initial.weight.vec
  
  g <- 100000
  
  while(g > opt.thresh ) {
    y.res.vec <- (X.scaled.mat %*% w.t.minus.1) - y.vec
    g.vec <- (t(X.scaled.mat) %*% (y.res.vec)) + (penalty * w.t.minus.1)
    # g <- sum(g.vec^2)
    g <- sum(abs(g.vec))
    
    if (g <= opt.thresh) {
      optimal.weight.vector <- w.t.minus.1
      
      return(optimal.weight.vector)
    }
    w.t <- w.t.minus.1 - (step.size * g.vec)
    w.t.minus.1 <- w.t
  }
  
}


LMSquareLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # start by scaling X.mat so each column mean is 0 and std is 1
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  
  # y.vec <- scale(y.vec)
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)

  opt.thresh <- 0.005
  step.size = 0.0001
  # random non-zero weights?
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  # Warm restart
  W.scaled.mat <- matrix(0, nrow = ncol(X.mat), ncol = length(penalty.vec))
  warm.weight.vec <- initial.weight.vec
  
  for (i in 1:length(penalty.vec)) {
    warm.weight.vec <- LMSquareLossL2(X.scaled.mat = X.scaled.mat, y.vec = y.vec, penalty = penalty.vec[i], opt.thresh = opt.thresh, initial.weight.vec = warm.weight.vec, step.size = step.size)
    # W.scaled.mat[, i] <- (t(warm.weight.vec) * sd ^-1)
    # print('warm.weight.vec')
    # print(warm.weight.vec)    
    # print('sd')
    # print(sd)
    # print('warm.weight.vec * sd')
    # print(t(warm.weight.vec) * sd ^-1)    
    }
  
  W.mat <- (t(W.scaled.mat) * sd^-1)
  B <- t(W.scaled.mat) * sd^-1 * mean
  W.unscaled.mat <- (X.mat %*% W.mat) + B
  return(W.unscaled.mat)
}


LMLogisticLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # start by scaling X.mat so each column mean is 0 and std is 1
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  
  # y.vec <- scale(y.vec)
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  
  opt.thresh <- 0.005
  step.size = 0.0001
  # random non-zero weights?
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  # Warm restart
  W.scaled.mat <- matrix(0, nrow = ncol(X.mat), ncol = length(penalty.vec))
  warm.weight.vec <- initial.weight.vec
  
  for (i in 1:length(penalty.vec)) {
    warm.weight.vec <- LMLogisticLossL2(X.scaled.mat = X.scaled.mat, y.vec = y.vec, penalty = penalty.vec[i], opt.thresh = opt.thresh, initial.weight.vec = warm.weight.vec, step.size = step.size)
    W.scaled.mat[, i] <- (t(warm.weight.vec) * sd ^-1)
    # print('warm.weight.vec')
    # print(warm.weight.vec)
    # print('sd')
    # print(sd)
    # print('warm.weight.vec * sd')
    # print(t(warm.weight.vec) * sd ^-1)
  }
  
  W.mat <- (t(W.scaled.mat) * sd^-1)
  W.mat <- ((W.scaled.mat) * sd^-1)
  B <- t(W.scaled.mat) * sd^-1 * mean
  return(W.scaled.mat)
}


LMSquareLossL2CV <- function( X.mat, y.vec, fold.vec, penalty.vec ) {
  
}
