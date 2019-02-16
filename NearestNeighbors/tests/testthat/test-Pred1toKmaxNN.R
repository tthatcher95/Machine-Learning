library(NearestNeighbors)
library(testthat)
context("knn")

test_that("C++ computes same answer as R", {
  data(zip.train, package="ElemStatLearn")
  x <- zip.train[1:5, -1]
  y <- zip.train[1:5, 1]
  
  testx <- zip.train[6, -1]
  max_neighbors = 3
  testy <- zip.train[6, 1]

  pred.vec <- NN1toMaxPredict_func(x, y, max_neighbors, testx)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  dist.vec
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max_neighbors]
  expected_prediction <- cumsum(y[closest.indices])/(1:max_neighbors)
  expect_equal(pred.vec, expected_prediction)
  })


test_that("valid input gets valid output", {
  # (1) for a valid input (one of the data sets mentioned below), 
  # your function returns a valid numeric prediction matrix with the expected dimensions
  data(zip.train, package="ElemStatLearn")
  x <- zip.train[1:5, -1]
  y <- zip.train[1:5, 1]
  
  testx <- zip.train[6, -1]
  max_neighbors = 3
  testy <- zip.train[6, 1]
  
  pred.vec <- NN1toMaxPredict_func(x, y, max_neighbors, testx)
  dist.mat <- t(x) - testx
  dist.vec <- sqrt(colSums(dist.mat * dist.mat))
  dist.vec
  sorted.index.vec <- order(dist.vec)
  closest.indices <- sorted.index.vec[1:max_neighbors]
  expected_prediction <- cumsum(y[closest.indices])/(1:max_neighbors)
  expect_equal(pred.vec, expected_prediction)
})

test_that("should not fail", {
  expect_equal(1, 1)
})
