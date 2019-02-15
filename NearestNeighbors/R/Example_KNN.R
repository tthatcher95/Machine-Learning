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
NN1toMaxPredict_func <- function(x, y, max_neighbors, testx, ret) {
  .C("NN1toKmaxPredict", as.double(x), as.double(y), as.integer(nrow(x)), as.integer(ncol(x)), as.integer(max_neighbors), as.double(testx), as.double(ret), PACKAGE="NearestNeighbors")
  }

NNLearnCV <- function(X.mat, y.vec, max.neighbors=30, fold.vec=NULL, n.folds=5) {
  if(is.null(fold.vec)) {
    fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
  }
  
  if(!all(length(y.vec)==length(fold.vec))) {
    stop("Vectors 'y.vec'  and 'fold.vec' must be same length.")
  }
    validation_num <- sample(1:n.folds, 1)
    for(fold.i in seq_along(unique(unlist(fold.vec, use.names = FALSE)))) {
      fold <- which(fold.vec == fold.i)
      data.train <- x[-fold,]
      data.test <- x[fold,]
    }
    ret <- vector(mode="double", length=max_neighbors)
    for(row in c(data.train, data.test)){
      NN1toMaxPredict_func(x, y, max_neighbors, testx, ret)
      #pred.mat <- ret
      #print(pred.mat)
      loss.mat <- if(all(y <= 1)) {
        ifelse(pred.mat > 0.5, 1, 0) != y.vec #zero-one loss for binary classification.
      }
      else {
        #(pred.mat - y.vec)^2 #square loss for regression.
      }
      #overall.loss.mat[, fold.i] <- colMeans(as.matrix(loss.mat))
      
    }
}

#NNLearnCV(x, y)






