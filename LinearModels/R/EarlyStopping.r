data = ElemStatLearn::ozone

orig.mat = data[, -1]
x.mat <- scale(orig.mat)

x.unsc.mat <- data[,-1]
y.vec <-data[,1]
max.iterations=10
step.size=0.35

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
  #W.original.scale[, keep] <- W.mat / attr(X.sc, "scaled:scale)

  W.mat = matrix(NA, ncol(x.mat), max.iterations)
  W.mat[, 1] <- W.v

  for(iteration in 2:max.iterations) {
    gradient <-  t(x.mat) %*% (x.mat %*% W.v  - y.vec)/nrow(x.mat)
    W.v <- W.v - step.size * gradient
    W.mat[, iteration] <- W.v
  }
  W.orig.mat = W.mat/attr(x.mat, "scaled:scale")
  intercept = -t(W.mat/ attr(x.mat, "scaled:scale")) %*% attr(x.mat, "scaled:center")
  # print(all.equal(x.mat %*% W.mat, cbind(1, as.matrix(x.unsc.mat)) %*% W.out))
  W.out = rbind(t(intercept), W.orig.mat)
  return(W.out)
}

LMSquareLossEarlyStoppingCV<- function(X.mat, y.vec, fold.vec=NULL, max.iterations=100, step.size=0.05) {

  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  }

  W.valid <- matrix(NA, length(unique(fold.vec)), max.iterations)

  for(fold in 1:length(unique(fold.vec))) {
    fold_data <- which(fold.vec == fold)

    X.train <- X.mat[-fold_data ,]
    X.valid <- X.mat[fold_data ,]

    Y.train <- y.vec[-fold_data]
    Y.valid <- y.vec[fold_data]

    W.train <- LMSquareLossIterations(X.train, Y.train, max.iterations, step.size)
    W.valid[fold , ] = colMeans((cbind(1, as.matrix(X.valid)) %*% W.train - Y.valid)^2)
  }

  mean.valid.vec = colMeans(W.valid)

  min.valid.mean = which(mean.valid.vec == min(mean.valid.vec), arr.ind = TRUE)

  selected.steps = min.valid.mean

  w.vec = LMSquareLossIterations(X.mat, y.vec, selected.steps, step.size)[, selected.steps]


  ret <- list(
    'mean.validation.loss' = mean.valid.vec[selected.steps],
    'mean.train.loss.vec' = mean.valid.vec,
    'selected.steps' = selected.steps,
    'weight.vec' = w.vec,
    'predict' = predict
  )
  return(ret)
}

fit <- LMSquareLossEarlyStoppingCV(x.unsc.mat, y.vec)

LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {

}
