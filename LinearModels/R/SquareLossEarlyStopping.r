#' LMSquareLossIterations
#'
#' @param x.unsc.mat An unscaled feature matrix [n x p]
#' @param y.vec A vector of predictions [p x 1]
#' @param max.iterations The number of times to step through your gradient descent 
#' @param step.size The size of the step to take during gradient descent
#'
#' @return A unscaled weight matrix [max.iterations x p]
#' @export
#'
#' @examples
#' x.unsc.mat <- data[,-1]
#' y.vec <-data[,1]
#' max.iterations=10
#' step.size=0.35
#' W.train <- LMLogisticLossIterations(X.train, Y.train, max.iterations, step.size)
#' cbind(1, X.mat) %*% W.mat

LMSquareLossIterations <- function(x.unsc.mat, y.vec, max.iterations, step.size) {
  x.mat <- scale(x.unsc.mat)
  W.v <- numeric(ncol(x.mat))
  mean.mat <- attr(x.mat, "scaled:center")
  sd.mat <- attr(x.mat, "scaled:scaled")
  
  if( length(y.vec) != nrow(x.unsc.mat) ){
    stop("y.vec and X.unsc.mat should have the same number of rows")
  }
  if( length(W.v) != ncol(x.unsc.mat) ){
    stop("W.v should have a row for every feature in X.unsc.mat")
  }
  if( step.size <= 0 ){
    stop("step.size should be greater than 0")
  }
  
  
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

#' LMSquareLossEarlyStoppingCV
#'
#' @param x.unsc.mat An unscaled feature matrix [n x p]
#' @param y.vec A vector of predictions [p x 1]
#' @param fold.vec A vector of FoldID to pass for the cross-validation data split [p x 1]
#' @param max.iterations The number of times to step through your gradient descent 
#' @param step.size The size of the step to take during gradient descent
#'
#' @return A list comprised of: mean.valid.vec [max.iterations x 1], selected.steps, w.vec [max.iterations x p], and a function predict()
#' @export
#'
#' @examples
#' x.unsc.mat <- data[,-1]
#' y.vec <-data[,1]
#' max.iterations=10
#' step.size=0.35
#' fitLog <- LMLogisticLossEarlyStoppingCV(X.mat.binary, y.vec.binary, NULL, max.iterations, step.size)
#' fitLog$predict(x.unsc.mat)
#' fitLog$mean.valid.vec
#' fitLog$selected.steps
#' fitLog$w.vec

LMSquareLossEarlyStoppingCV<- function(X.mat, y.vec, fold.vec=NULL, max.iterations=100, step.size=0.35) {
  
  if( length(y.vec) != nrow(X.mat) ){
    stop("y.vec and X.mat should have the same number of rows")
  }
  
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
  
  list(
    mean.valid.vec=mean.valid.vec,
    selected.steps=selected.steps,
    w.vec=w.vec,
    predict=function(testX.mat){
      cbind(1, as.matrix(testX.mat)) %*% w.vec
    }
  )
}

data = ElemStatLearn::ozone

# orig.mat = data[, -1]
# x.mat <- scale(orig.mat)

x.unsc.mat <- data[,-1]
y.vec <-data[,1]
max.iterations=10
step.size=0.35

fit <- LMSquareLossEarlyStoppingCV(x.unsc.mat, y.vec)


