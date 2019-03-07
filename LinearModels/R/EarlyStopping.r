UNscale <- function(scaled.matrix) {
  scaled.matrix * attr(scaled.matrix,'scaled:scale') + attr(scaled.matrix, 'scaled:center')
  return(scaled.matrix)
}
orig.mat = data[, -1]
x.mat <- scale(orig.mat)

x.unsc.mat <- data[,-1]
y.vec <-data[,1]
max.iterations=10
step.size=0.35
LMSquareLossIterations <- function(x.unsc.mat, y.vec, max.iterations, step.size) {
  x.mat <- scale(x.unsc.mat)
  #orig.scale = t(apply(x.mat, 1, function(r) r*attr(x.mat,'scaled:scale') + attr(x.mat, 'scaled:center')))
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
  #W.original.scale[, keep] <- W.mat / attr(X.sc, "scaled:scale)
  
  W.mat = matrix(NA, ncol(x.mat), max.iterations)
  W.mat[, 1] <- W.v
  
  for(iteration in 2:max.iterations) {
    gradient <-  t(x.mat) %*% (x.mat %*% W.v  - y.vec)/nrow(x.mat)
    print(gradient)
    W.v <- W.v - step.size * gradient
    W.mat[, iteration] <- W.v
  }
  W.orig.mat = W.mat/attr(x.mat, "scaled:scale")
  intercept = -t(W.mat/ attr(x.mat, "scaled:scale")) %*% attr(x.mat, "scaled:center")
  all.equal(x.mat %*% W.mat, cbind(1, as.matrix(x.unsc.mat)) %*% W.out)
  W.out = rbind(t(intercept), W.orig.mat)
  W.out
}

LMSquareLossEarlyStoppingCV<- function(X.mat, y.vec, fold.vec, max.iterations=100, step.size=0.01) {
  # Make CV select this
  selected.steps=7
  w.vec = LMSquareLossIterations(X.mat, y.vec, selected.steps, step.size)[, selected.steps]
  list(
    predict=function(testX.mat){
      cbind(1, as.matrix(testX.mat)) %*% w.vec
    }
  )
}

fit <- LMSquareLossEarlyStoppingCV(x.unsc.mat, y.vec)

LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {

}

data = ElemStatLearn::ozone
LMSquareLossIterations(data[, -1], data[, 1], 3, 0.35)
