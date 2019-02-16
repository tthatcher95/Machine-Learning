library(NearestNeighbors)
library(testthat)
context("knn")

test_that("C++ computes same answer as R", {
  
  data(zip.train, package="ElemStatLearn")
  x <- zip.train[1:5, -1]
  y <- zip.train[1:5, 1]
  testx <- zip.train[6, -1]
  max.neighbors = 3
  testy <- zip.train[6, 1]

  pred.vec <- NN1toMaxPredict_func(x, y, max.neighbors, testx)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  dist.vec
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max.neighbors]
  expected_prediction <- cumsum(y[closest.indices])/(1:max.neighbors)
  expect_equal(pred.vec, expected_prediction)
  })

test_that("valid input gets valid output",{ 
  # (1) for a valid input (one of the data sets mentioned below), 
  # your function returns a valid numeric prediction matrix with the expected dimensions
  data(zip.train, package="ElemStatLearn")
  x <- zip.train[, -1]
  y <- zip.train[, 1]
  max.neighbors = 3 
  
  trainx <- zip.train[0:100, -1]
  trainx <- trainx
  trainy <- zip.train[0:100, 1]
  testx <- zip.train[101:111, -1]
  
  pred.matrix <- NN1toMaxPredictMatrix_func(trainx, trainy, max.neighbors, testx)
  pred.matrix
  # is matrix
  expect_equal(is.matrix(pred.matrix), TRUE)
  # has numbers
  expect_equal(all(is.numeric(pred.matrix)), TRUE)
  # expected cols
  expect_equal(ncol(pred.matrix), max.neighbors)
  # expected rows
  expect_equal(nrow(pred.matrix), nrow(testx))
  
  data(ozone, package="ElemStatLearn")
  
  # Ozone
  x <- ozone[, -1]
  y <- ozone[, 1]
  max.neighbors = 3 
  
  trainx <- as.matrix(ozone[0:100, -1])
  trainy <- ozone[0:100, 1]
  testx <- as.matrix(ozone[101:111, -1])
  
  pred.matrix <- NN1toMaxPredictMatrix_func(trainx, trainy, max.neighbors, testx)
  pred.matrix
  # is matrix
  expect_equal(is.matrix(pred.matrix), TRUE)
  # has numbers
  expect_equal(all(is.numeric(pred.matrix)), TRUE)
  # expected cols
  expect_equal(ncol(pred.matrix), max.neighbors)
  # expected rows
  expect_equal(nrow(pred.matrix), nrow(testx))
  })

test_that("invalid input, should stop", {
  # (2) for an invalid input, your function stops with an informative error message.
  data(zip.train, package="ElemStatLearn")
  x <- zip.train[, -1]
  y <- zip.train[, 1]

  trainx <- zip.train[0:100, -1]
  trainx <- trainx
  trainy <- zip.train[0:100, 1]
  testx <- zip.train[101:111, -1]

  # the 4000000 neighbors is invalid
  invalid_neighbors = 4000000
  expect_error(NN1toMaxPredictMatrix_func(trainx, trainy, invalid_neighbors, testx))
})
