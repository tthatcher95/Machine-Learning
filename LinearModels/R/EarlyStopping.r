LMSquareLossIterations <- function(x.unsc.mat, y.vec, max.iterations, step.size) {
  x.mat <- scale(x.unsc.mat)
  W.v <- numeric(ncol(x.mat))
  mean.mat <- attr(x.mat, "scaled:center")
  sd.mat <- attr(x.mat, "scaled:scaled")
  
  # R Scaling/Filtering
  ## Filters out variablity == 0
  # is.zero <- attr(X.sc, "scaled:scale") == 0
  # x.mat <- X.sc[, !is.zero]
  # keep <- attr(X.sc, "scaled:scale") != 0
  
  ## Make X-Matrix 
  # x.mat <- X.sc[, keep]
  
  ## Unscale Matrix
  # W.original.scale[, keep] <- W.mat / attr(X.sc, "scaled:scale)
  
  W.mat = matrix(NA, ncol(x.mat + 1), max.iterations)
  W.mat[, 1] <- W.v
  
  for(iteration in 2:max.iterations) {
    gradient <- 2 * t(x.mat) %*% ((x.mat %*% W.v)  - y.vec) 
    W.v <- W.v - step.size * gradient
    W.mat[, iteration] <- W.v
  }
  return(W.mat)
}


LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {

}

data = ElemStatLearn::ozone
data[, -1]
sum = LMSquareLossIterations(data[, -1], data[, 1], 3, 0.35)
print(sum)