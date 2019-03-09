# data(ozone, package="ElemStatLearn")
# penalty.vec <- seq(10, 1, by=-0.5)
# X.mat <- ozone[, c("temperature","radiation")]
# y.vec <- ozone[, "ozone"]
# data(spam, package="ElemStatLearn")
# X.mat.binary <- spam[,1:ncol(spam) -1]
# y.vec.label <- spam[,'spam']
# y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
# LMLogisticLossL2penalties(X.mat.binary, y.vec.binary, penalty.vec)

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
  y.vec[y.vec == 1] <- -1
  y.vec[y.vec == 0] <- 1
  Y <- diag(y.vec)
  # print(-t(X.scaled.mat) %*% Y %*% (1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1))))  # print(Y)
  # print(g.vec <- -t(X.scaled.mat) %*% Y %*% (1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1))))
  # g.vec <- -t(X.scaled.mat) %*% Y %*% (1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1)))
  # print((1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1))))
  # print(-Y %*% X.scaled.mat %*% w.t.minus.1)
  # print(g <- sum(abs(g.vec)))
  while(g > opt.thresh ) {
    # y.res.vec <- (X.scaled.mat %*% w.t.minus.1) - y.vec
    # g.vec <- (t(X.scaled.mat) %*% (y.res.vec)) + (penalty * w.t.minus.1)
    g.vec <- -t(X.scaled.mat) %*% Y %*% (1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1))) + (penalty * w.t.minus.1)
    # print(dim(g.vec))
    # g <- sum(g.vec^2)
    g <- sum(abs(g.vec))
    print(g)
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
    W.scaled.mat[, i] <- warm.weight.vec

    }

  W.unscaled.mat <- W.scaled.mat/attr(X.scaled.mat, "scaled:scale")
  B <- -t(W.scaled.mat/ attr(X.scaled.mat, "scaled:scale")) %*% attr(X.scaled.mat, "scaled:center")
  W.mat = rbind(t(B), W.unscaled.mat)

  return(W.mat)
}


LMLogisticLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  # start by scaling X.mat so each column mean is 0 and std is 1
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  
  # y.vec <- scale(y.vec)
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  
  opt.thresh <- 0.005
  step.size = 0.01
  # random non-zero weights?
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  # 'loop' over penalty values and call LMSquareLossL2 to get optimal weight vector for each
  # Warm restart
  W.scaled.mat <- matrix(0, nrow = ncol(X.mat), ncol = length(penalty.vec))
  warm.weight.vec <- initial.weight.vec
  
  for (i in 1:length(penalty.vec)) {
    warm.weight.vec <- LMLogisticLossL2(X.scaled.mat = X.scaled.mat, y.vec = y.vec, penalty = penalty.vec[i], opt.thresh = opt.thresh, initial.weight.vec = warm.weight.vec, step.size = step.size)
    W.scaled.mat[, i] <- warm.weight.vec
    
  }
  
  W.unscaled.mat <- W.scaled.mat/attr(X.scaled.mat, "scaled:scale")
  B <- -t(W.scaled.mat/ attr(X.scaled.mat, "scaled:scale")) %*% attr(X.scaled.mat, "scaled:center")
  W.mat = rbind(t(B), W.unscaled.mat)
  
  return(W.mat)
}


LMSquareLossL2CV <- function( X.mat, y.vec, fold.vec = NULL, penalty.vec ) {

  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  }

  W.valid <- matrix(NA, length(unique(fold.vec)), length(penalty.vec))
  for(fold in 1:length(unique(fold.vec))) {
    fold_data <- which(fold.vec == fold)

    X.train <- X.mat[-fold_data ,]
    X.valid <- X.mat[fold_data ,]

    Y.train <- y.vec[-fold_data]
    Y.valid <- y.vec[fold_data]

    W.train <- LMSquareLossL2penalties(X.train, Y.train, penalty.vec)
    W.valid[fold , ] = colMeans((cbind(1, as.matrix(X.valid)) %*% W.train - Y.valid)^2)
  }

  mean.validation.loss.vec = colMeans(abs(W.valid))

  min.valid.mean.index = which(mean.validation.loss.vec == min(mean.validation.loss.vec), arr.ind = TRUE)
  selected.penalty = penalty.vec[min.valid.mean.index]

  w.vec = LMSquareLossL2penalties(X.mat, y.vec, selected.penalty)

  predict <- function(testX.mat){
    cbind(1, as.matrix(testX.mat)) %*% w.vec
  }

  ret <- list(
    'mean.validation.loss' = mean.validation.loss.vec[min.valid.mean.index],
    'mean.train.loss.vec' = mean.validation.loss.vec,
    'penalty.vec' = penalty.vec,
    'selected.penalty' = selected.penalty,
    'weight.vec' = w.vec,
    'predict' = predict
  )
  
  return(ret)
}

LMLogisticLossL2CV <- function( X.mat, y.vec, fold.vec = NULL, penalty.vec ) {
  
  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  }
  
  W.valid <- matrix(NA, length(unique(fold.vec)), length(penalty.vec))
  for(fold in 1:length(unique(fold.vec))) {
    fold_data <- which(fold.vec == fold)
    
    X.train <- X.mat[-fold_data ,]
    X.valid <- X.mat[fold_data ,]
    
    Y.train <- y.vec[-fold_data]
    Y.valid <- y.vec[fold_data]
    
    W.train <- LMLogisticLossL2penalties(X.train, Y.train, penalty.vec)
    W.valid[fold , ] = colMeans((cbind(1, as.matrix(X.valid)) %*% W.train - Y.valid)^2)
  }
  
  mean.validation.loss.vec = colMeans(W.valid)
  
  min.valid.mean.index = which(mean.validation.loss.vec == min(mean.validation.loss.vec), arr.ind = TRUE)
  selected.penalty = penalty.vec[min.valid.mean.index]
  
  w.vec = LMLogisticLossL2penalties(X.mat, y.vec, selected.penalty)
  
  predict <- function(testX.mat){
    cbind(1, as.matrix(testX.mat)) %*% w.vec
  }
  
  ret <- list(
    'mean.validation.loss' = mean.validation.loss.vec[min.valid.mean.index],
    'mean.train.loss.vec' = mean.validation.loss.vec,
    'penalty.vec' = penalty.vec,
    'selected.penalty' = selected.penalty,
    'weight.vec' = w.vec,
    'predict' = predict
  )
  
  return(ret)
}
