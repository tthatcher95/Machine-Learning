data(zip.train, package="ElemStatLearn")
x <- as.matrix(zip.train[, -1])
y <- as.vector(zip.train[, 1])
testx <- zip.train[6, -1]
max_neighbors = 3
ret <- vector(mode="double", length=max_neighbors)
overall.loss.mat <- matrix()
testy <- zip.train[6, 1]
fold <- vector()

# if the below line is uncommented then it fails to build dont know why
NN1toMaxPredict_func <- function(x, y, max_neighbors, testx) {
    res <- .C("NN1toKmaxPredict", as.double(x), as.double(y), as.integer(nrow(x)), as.integer(ncol(x)), as.integer(max_neighbors), as.double(testx), test.predictions=double(max_neighbors), PACKAGE="NearestNeighbors")
    res$test.predictions
  }

NN1toMaxPredictMatrix_func <- function(trainx, trainy, max.neighbors, testx) {
  if(!all( is.matrix(testx))){
    stop("testx must be a matrix")
  }
  if(!all( is.matrix(trainx))){
    stop("trainx must be a matrix")
  }
  if(!all( is.vector(trainy))){
    stop("trainy must be a vector")
  }
  if( max.neighbors < 1 || max.neighbors > nrow(trainx)){
    stop("max.neighbors must be greater than 0 and less than the number of rows in trainx")
  }
  if( length(trainy) != nrow(trainx) ){
    stop("trainy and trainx must be equal length")
  }
  if( ncol(trainx) != ncol(testx) ){
    stop("trainx and testx must have an equal amount of features")
  }
  res <- .C("NN1toKmaxMatrixPredict", as.double(trainx), as.double(trainy), as.integer(nrow(trainx)), as.integer(ncol(trainx)), as.integer(max.neighbors), as.integer(nrow(testx)), as.double(testx), test.predictions=double(max.neighbors*nrow(testx)), PACKAGE="NearestNeighbors")
  matrix(res$test.predictions, nrow(testx), max.neighbors, byrow=TRUE)
}

NNLearnCV <- function(X.mat, y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5) {
  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
  }
  
  if(!all(length(y.vec)==length(fold.vec))) {
    stop("Vectors 'y.vec'  and 'fold.vec' must be same length.")
  }
  
  train.loss.mat <- matrix(, nrow = length(x), ncol = max.neighbors)
  validation.loss.mat <- matrix(, nrow = length(x), ncol = max.neighbors)
  
  validation_num <- sample(1:n.folds, 1)
  
  for(fold.i in seq_along(which(fold.vec != validation_num))) {
    fold <- which(fold.vec == fold.i)
    data.train <- x[fold,]
    data.test <- y[fold]
    pred.mat <- NN1toMaxPredictMatrix_func(data.train, data.test, max_neighbors, testx)
    set.list <- list(train=data.train, validation=!data.train)
    for(set.name in names(set.list)){
      is.set <- set.list[[set.name]]
      print(is.set)
      set.pred.mat <- pred.mat[is.set,]
      set.label.vec <- data.test[is.set]
      loss.mat <- if(all(y == 1 || y == 0)) {
        ifelse(pred.mat > 0.5, 1, 0) != set.label.vec #zero-one loss for binary classification.
      }
      else {
        (pred.mat - set.label.vec)^2 #square loss for regression.
      }
      
      train.loss.mat[fold.i] <- colMeans(as.matrix(loss.mat))
    }
  }
}

#NNLearnCV(x, y)







