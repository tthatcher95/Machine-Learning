#' L2 Square loss
#'
#' This function attempts to minimize the Loss function [f(x) - y]^2. Where f(x) is the resulting prediction values of x and y are truth values.
#' 
#' @param X.scaled.mat numeric scaled input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param penalty numeric value for the penalty on weights
#' @param opt.thresh numeric value compare for convergence
#' @param initial.weight.vec numeric 0 vector [p]
#' @param step.size numeric learning rate
#'
#' @return numeric scaled optimal weight vector for the given penalty [p]
#' @export
#'
#' @examples
#  data(ozone, package="ElemStatLearn")
#  penalty <- 5
#  X.mat <- ozone[, 2:ncol(ozone)]
#  y.vec <- ozone[, "ozone"]
#  mean <- apply(X.mat, 2, mean)
#  sd <- apply(X.mat, 2, sd)
#  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
#  opt.thresh <- 0.005
#  step.size = 0.0001
#  initial.weight.vec <- integer(ncol(X.scaled.mat))
#  LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size)
LMSquareLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( length(initial.weight.vec) != ncol(X.scaled.mat) ){
    stop("initial.weight.vec should have a row for every feature in X.scaled.mat")
  }
  if( penalty <= 0 ){
    stop("penalty should be greater than 0")
  }
  if( opt.thresh <= 0 ){
    stop("opt.thresh should be greater than 0")
  }
  if( step.size <= 0 ){
    stop("step.size should be greater than 0")
  }
  w.t.minus.1 <- initial.weight.vec
  
  g <- 100000
  
  while(g > opt.thresh ) {
    y.res.vec <- (X.scaled.mat %*% w.t.minus.1) - y.vec
    g.vec <- (t(X.scaled.mat) %*% (y.res.vec)) + 2 * (penalty * w.t.minus.1)
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


#' L2 Logistic loss
#'
#' This function attempts to minimize the Loss function log[1 + exp(-YXw)]
#' 
#' @param X.scaled.mat numeric scaled input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param penalty numeric value for the penalty on weights
#' @param opt.thresh numeric value compare for convergence
#' @param initial.weight.vec numeric 0 vector [p]
#' @param step.size numeric learning rate
#'
#' @return numeric scaled optimal weight vector for the given penalty [p]
#' @export
#'
#' @examples
# data(spam, package="ElemStatLearn")
# X.mat.binary <- spam[,1:ncol(spam) -1]
# y.vec.label <- spam[,'spam']
# y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
# penalty <- 5
# mean <- apply(X.mat.binary, 2, mean)
# sd <- apply(X.mat.binary, 2, sd)
# X.scaled.mat <- scale(X.mat.binary, center = mean, scale = sd)
#' opt.thresh <- 0.005
#' step.size = 0.1
#' initial.weight.vec <- integer(ncol(X.scaled.mat))
#' LMLogisticLossL2( X.scaled.mat, y.vec.binary, penalty, opt.thresh, initial.weight.vec, step.size )
LMLogisticLossL2 <- function( X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( length(initial.weight.vec) != ncol(X.scaled.mat) ){
    stop("initial.weight.vec should have a row for every feature in X.scaled.mat")
  }
  if( penalty <= 0 ){
    stop("penalty should be greater than 0")
  }
  if( opt.thresh <= 0 ){
    stop("opt.thresh should be greater than 0")
  }
  if( step.size <= 0 ){
    stop("step.size should be greater than 0")
  }
  w.t.minus.1 <- initial.weight.vec
  
  g <- 100000
  y.vec[y.vec == 1] <- -1
  y.vec[y.vec == 0] <- 1
  Y <- diag(y.vec)
  
  while(g > opt.thresh ) {
    
    g.vec <- -t(X.scaled.mat) %*% Y %*% (1/(1+exp(-Y %*% X.scaled.mat %*% w.t.minus.1))) + 2 * (penalty * w.t.minus.1)
    g <- sum(abs(g.vec))
    
    if (g <= opt.thresh) {
      optimal.weight.vector <- w.t.minus.1
      return(optimal.weight.vector)
    }
    w.t <- w.t.minus.1 - (step.size * g.vec)
    w.t.minus.1 <- w.t
  }
  
}


#' L2 Square Loss 
#' 
#' returns a weight matrix where every vector is the optimal weight for a given penalty
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param penalty.vec numeric vector of decending penalties for weights [m]
#'
#' @return numeric weight matrix with the optimal weight for every penalty value and the intercept term augmented to the top row of each vector [ p + 1 x m]
#' @export
#'
#' @examples
#' data(ozone, package="ElemStatLearn")
#' X.mat <- ozone[, 2:ncol(ozone)]
#' y.vec <- ozone[, "ozone"]
#' penalty.vec <- seq(10, 1, by=-0.5)
#' LMSquareLossL2penalties(X.mat, y.vec, penalty.vec)
LMSquareLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( all(diff(penalty.vec) >= 0) ){
    stop("penalty.vec should be a vector of decreasing values")
  }
  
  # start by scaling X.mat so each column mean is 0 and std is 1
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  
  opt.thresh <- 0.005
  step.size = 0.0001
  
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


#' L2 Square Loss
#' 
#' returns a weight matrix where every vector is the optimal weight for a given penalty
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param penalty.vec numeric vector of decending penalties for weights [m]
#'
#' @return numeric weight matrix with the optimal weight for every penalty value and the intercept term augmented to the top row of each vector [ p + 1 x m]
#' @export
#'
#' @examples
#' data(spam, package="ElemStatLearn")
#' X.mat.binary <- spam[,1:ncol(spam) -1]
#' y.vec.label <- spam[,'spam']
#' y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
#' penalty.vec <- seq(10, 1, by=-0.5)
#' LMLogisticLossL2penalties(X.mat.binary, y.vec.binary, penalty.vec)
LMLogisticLossL2penalties <- function( X.mat, y.vec, penalty.vec ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( all(diff(penalty.vec) >= 0) ){
    stop("penalty.vec should be a vector of decreasing values")
  }
  # start by scaling X.mat so each column mean is 0 and std is 1
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  
  opt.thresh <- 0.005
  step.size = 0.1
  
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


#' L2 Square Loss CV
#' 
#' Cross validation using L2 Square Loss regularization
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param fold.vec numeric fold ids for each observation in X.mat [n]
#' @param penalty.vec numeric vector of decending penalties for weights [m]
#'
#' @return list containing the mean validation loss, mean training loss vector, pentalty vector, optimal penalty, optimal weight vector using optimal penalty, predict function
#' @export
#'
#' @examples
#' data(ozone, package="ElemStatLearn")
#' X.mat <- ozone[, 2:ncol(ozone)]
#' y.vec <- ozone[, "ozone"]
#' fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
#' penalty.vec <- seq(10, 1, by=-0.5)
#' LMSquareLossL2CV(X.mat, y.vec, fold.vec, penalty.vec)
LMSquareLossL2CV <- function( X.mat, y.vec, fold.vec = NULL, penalty.vec ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( all(diff(penalty.vec) >= 0) ){
    stop("penalty.vec should be a vector of decreasing values")
  }
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


#' L2 Logistic CV
#' 
#' Cross validation using L2 Logistic regularization
#'
#' @param X.mat numeric input feature matrix [n x p]
#' @param y.vec numeric input label vector [n]
#' @param fold.vec numeric fold ids for each observation in X.mat [n]
#' @param penalty.vec numeric vector of decending penalties for weights [m]
#'
#' @return list containing the mean validation loss, mean training loss vector, pentalty vector, optimal penalty, optimal weight vector using optimal penalty, predict function
#' @export
#'
#' @examples
#' data(spam, package="ElemStatLearn")
#' X.mat.binary <- spam[,1:ncol(spam) -1]
#' y.vec.label <- spam[,'spam']
#' y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
#' penalty.vec <- seq(10, 1, by=-0.5)
#' LMLogisticLossL2CV(X.mat.binary, y.vec.binary, fold.vec, penalty.vec)
LMLogisticLossL2CV <- function( X.mat, y.vec, fold.vec = NULL, penalty.vec ) {
  if( length(y.vec) != nrow(X.scaled.mat) ){
    stop("y.vec and X.scaled.mat should have the same number of rows")
  }
  if( all(diff(penalty.vec) >= 0) ){
    stop("penalty.vec should be a vector of decreasing values")
  }
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
