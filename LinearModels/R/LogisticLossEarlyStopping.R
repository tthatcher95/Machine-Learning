#' Title
#'
#' @param x.unsc.mat
#' An unscaled feature matrix
#' @param y.vec 
#' A vector of predictions
#' @param max.iterations
#' The number of times to step through your gradient descent 
#' @param step.size 
#' The size of the step to take during gradient descent
#'
#' @return
#' A unscaled weight matrix (W.out) which you can use to get a matrix of predictions (probabilities)
#' With the Beta/Intercept term as the first row
#' @export
#'
#' @examples
#' W.train <- LMLogisticLossIterations(X.train, Y.train, max.iterations, step.size)
#' cbind(1, X.mat) %*% W.mat -- Returns the matrix of predictions



LMLogisticLossIterations <- function(x.unsc.mat, y.vec, max.iterations=100, step.size=0.05) {
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
    #top = (-y.vec %*% x.mat)
    #bottom = 1 + exp(-y.vec * t(W.v) %*% x.mat)
    # -t(x.mat * y.vec)
    y.vec[y.vec == 1] <- -1
    y.vec[y.vec == 0] <- 1
    gradient <- -t(x.mat) %*% diag(y.vec) %*% (1/(1 + exp(-(diag(y.vec) %*% x.mat %*% W.v))))
    W.v <- W.v - step.size * gradient
    W.mat[, iteration] <- W.v
    
  }
  
  W.orig.mat = W.mat/attr(x.mat, "scaled:scale")
  intercept = -t(W.mat/ attr(x.mat, "scaled:scale")) %*% attr(x.mat, "scaled:center")
  W.out = rbind(t(intercept), W.orig.mat)
  # Test to make sure unscaling works
  #print(all.equal(x.mat %*% W.mat, cbind(1, as.matrix(x.unsc.mat)) %*% W.out))
  return(W.out)
}

#' Title
#'
#' @param x.unsc.mat
#' An unscaled feature matrix
#' @param y.vec 
#' A vector of predictions
#' @param fold.vec
#' A vector of FoldID to pass for the cross-validation data split
#' @param max.iterations
#' The number of times to step through your gradient descent 
#' @param step.size 
#' The size of the step to take during gradient descent
#'
#' @return
#' A list comprised of: mean.valid.vec, selected.steps, w.vec, and a function predict()
#' @export
#'
#' @examples
#' fitLog <- LMLogisticLossEarlyStoppingCV(X.mat.binary, y.vec.binary, NULL, max.iterations, step.size)
#' fitLog$predict(any_x_matrix) -- Returns matrix of predictions
#' fitLog$mean.valid.vec -- Returns the Mean Vector ran from the Cross Validation on the Validation set
#' fitLog$selected.steps -- Returns the optimal number of steps from CV
#' fitLog$w.vec -- Returns the weight vector learned from the training set
LMLogisticLossEarlyStoppingCV<- function(X.mat, y.vec, fold.vec=NULL, max.iterations=100, step.size=0.35) {
  
  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  }
  
  W.valid <- matrix(NA, 4, max.iterations)
  
  for(fold in 1:4) {
    fold_data <- which(fold.vec == fold)
    
    X.train <- X.mat[-fold_data ,]
    Y.train <- y.vec[-fold_data]
    
    X.valid <- X.mat[fold_data ,]
    Y.valid <- y.vec[fold_data]
    
    W.train <- LMLogisticLossIterations(X.train, Y.train, max.iterations, step.size)
    W.valid[fold , ] = colMeans((cbind(1, as.matrix(X.valid)) %*% W.train - Y.valid)^2)
    
  }
  
  
  mean.valid.vec = colMeans(W.valid)
  
  print(mean.valid.vec)
  min.valid.mean = which(mean.valid.vec == min(mean.valid.vec), arr.ind = TRUE)
  
  selected.steps = min.valid.mean

  w.vec <- LMLogisticLossIterations(X.mat, y.vec, selected.steps, step.size)[, selected.steps]
  
  list(
    mean.valid.vec,
    selected.steps,
    w.vec,
    predict=function(testX.mat){
      cbind(1, as.matrix(testX.mat)) %*% w.vec
    }
  )
}
data(spam, package="ElemStatLearn")
X.mat.binary <- spam[,1:ncol(spam) -1]
y.vec.label <- spam[,'spam']
y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0)
max.iterations=50
step.size=0.001

fitLog <- LMLogisticLossEarlyStoppingCV(X.mat.binary, y.vec.binary, NULL, max.iterations, step.size)

