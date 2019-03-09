library(LinearModels)
library(testthat)
context("LinearModels")

test_that("LMSquareLossIterations correct return dimensions", {
  
  data(ozone, package="ElemStatLearn")
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  step.size = 0.35
  max.iterations = 10
  ret <- LMSquareLossIterations(X.scaled.mat, y.vec, max.iterations, step.size)
  expect_equal(length(ret), ncol(X.mat))
})


test_that("LMSquareLossIterations stops on incorrect input", {
  
  data(ozone, package="ElemStatLearn")
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  step.size = 0
  max.iterations = 10
  expect_error(LMSquareLossIterations(X.scaled.mat, y.vec, max.iterations, step.size))
})


test_that("LMLogisticLossIterations correct return dimensions", {
  
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  step.size = 0.01
  max.iterations = 10
  ret <- LMLogisticLossIterations(X.mat.binary, y.vec.binary, max.iterations, step.size)
  
  expect_equal(length(ret), ncol(X.mat.binary))
})


test_that("LMLogisticLossIterations stops on incorrect input", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  step.size = 0
  max.iterations = 10
  expect_error(LMLogisticLossIterations(X.mat.binary, y.vec.binary, max.iterations, step.size))
})


test_that("LMSquareLossEarlyStoppingCV correct return dimensions", {
  data(ozone, package="ElemStatLearn")
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  ret <- LMSquareLossL2CV(X.mat, y.vec, fold.vec)
  expect_equal(ncol(ret$predict(X.mat)), length(penalty.vec.mat))
})


