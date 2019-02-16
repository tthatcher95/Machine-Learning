data(zip.train, package="ElemStatLearn")
x <- zip.train[, -1]
y <- zip.train[, 1]
is.matrix(x)
is.vector(y)

# if the below line is uncommented then it fails to build dont know why
NN1toMaxPredict_func <- function(x, y, max_neighbors, testx) {
    res <- .C("NN1toKmaxPredict", as.double(x), as.double(y), as.integer(nrow(x)), as.integer(ncol(x)), as.integer(max_neighbors), as.double(testx), test.predictions=double(max_neighbors), PACKAGE="NearestNeighbors")
    res$test.predictions
  }

NN1toMaxPredictMatrix_func <- function(trainx, trainy, max.neighbors, testx) {
  if(!is.matrix(testx)){
    stop("testx must be a matrix")
  }
  if(!is.matrix(trainx)){
    stop("trainx must be a matrix")
  }
  if(!is.vector(trainy)){
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
  # testing remove after
  X.mat = x
  y.vec = y
  typeof(X.mat)
  max.neighbors=30
  fold.vec=NULL
  n.folds=5
  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
  }

  if(!all(length(y.vec)==length(fold.vec))) {
    stop("Vectors 'y.vec'  and 'fold.vec' must be same length.")
  }
  
  train.loss.mat <- matrix(0, nrow = length(x), ncol = max.neighbors)
  validation.loss.mat <- matrix(0, nrow = length(x), ncol = max.neighbors)

  for(fold.i in seq_along((fold.vec))){
      test.i <- which(fold.vec == fold.i, arr.ind=TRUE)  
      
      train.features <- matrix(X.mat[-test.i, ], nrow = nrow(X.mat) - length(test.i), ncol(X.mat))
      nrow(train.features)
      ncol(train.features)
      is.matrix(train.features)
      train.labels <- y.vec[-test.i]
      
      validation.features <- matrix(X.mat[test.i], nrow = length(test.i), ncol = ncol(X.mat))
      nrow(validation.features)
      ncol(validation.features)
      
      validation.labels <- y.vec[test.i]
      
      nrow(validation.features)
      pred.mat <- NN1toMaxPredictMatrix_func(
      train.features, train.labels,
      max.neighbors, validation.features)
      is.matrix(validation.features)
      pred.mat
      # 
      # loss.mat <- if(all(y == 1 || y == 0)) {
      #   ifelse(pred.mat > 0.5, 1, 0) != set.label.vec #zero-one loss for binary classification.
      # }
      # 
      # else {
      #   (pred.mat - set.label.vec)^2 #square loss for regression.
      # }
      # 
      # train.loss.mat[fold.i] <- colMeans(as.matrix(loss.mat))
  }
      # ifelse(pred.mat>0.5, 1, 0) != y.vec #zero-one loss for binary classification.
      # }else{
      #   (pred.mat-y.vec)^2 #square loss for regression.
      # }
      # train.or.validation.loss.mat[, fold.i] <- colMeans(loss.mat)
  }
  # for(fold.i in seq_along(which(fold.vec != validation_num))) {
  #   fold <- which(fold.vec == fold.i)
  #   data.train <- x[fold,]
  #   data.train
  #   data.test <- y[fold]
  #   pred.mat <- NN1toMaxPredictMatrix_func(data.train, data.test, max_neighbors, testx)
  #   set.list <- list(train=data.train, validation=!data.train)
  #   
  #   for(set.name in names(set.list)){
  #     is.set <- set.list[[set.name]]
  #     print(is.set)
  #     set.pred.mat <- pred.mat[is.set]
  #     set.label.vec <- data.test[is.set]
  #     
  #     loss.mat <- if(all(y == 1 || y == 0)) {
  #       ifelse(pred.mat > 0.5, 1, 0) != set.label.vec #zero-one loss for binary classification.
  #     }
  #     
  #     else {
  #       (pred.mat - set.label.vec)^2 #square loss for regression.
  #     }
  #     
  #     train.loss.mat[fold.i] <- colMeans(as.matrix(loss.mat))
  #   }
  # }


#NNLearnCV(x, y)







