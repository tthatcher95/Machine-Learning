library(LinearModels)
library(testthat)
context("LinearModels")

test_that("LMSquareLossL2 correct return dimensions", {
  
  data(ozone, package="ElemStatLearn")
  penalty <- 5
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  opt.thresh <- 0.005
  step.size = 0.0001
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  ret <- LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size)
  
  expect_equal(length(ret), ncol(X.scaled.mat))
})


test_that("LMSquareLossL2 stops on incorrect input", {
  
  data(ozone, package="ElemStatLearn")
  
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  mean <- apply(X.mat, 2, mean)
  sd <- apply(X.mat, 2, sd)
  X.scaled.mat <- scale(X.mat, center = mean, scale = sd)
  opt.thresh <- 0.005
  step.size = 0.0001
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  
  penalty <- -5
  expect_error(LMSquareLossL2(X.scaled.mat, y.vec, penalty, opt.thresh, initial.weight.vec, step.size))
})


test_that("LMLogisticLossL2 correct return dimensions", {
  
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  penalty <- 5
  mean <- apply(X.mat.binary, 2, mean)
  sd <- apply(X.mat.binary, 2, sd)
  X.scaled.mat <- scale(X.mat.binary, center = mean, scale = sd)
  opt.thresh <- 0.005
  step.size = 0.25
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  ret <- LMLogisticLossL2(X.scaled.mat, y.vec.binary, penalty, opt.thresh, initial.weight.vec, step.size)
  
  expect_equal(length(ret), ncol(X.scaled.mat))
})


test_that("LMLogisticLossL2 stops on incorrect input", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  mean <- apply(X.mat.binary, 2, mean)
  sd <- apply(X.mat.binary, 2, sd)
  X.scaled.mat <- scale(X.mat.binary, center = mean, scale = sd)
  
  opt.thresh <- 0.005
  step.size = 0.25
  initial.weight.vec <- integer(ncol(X.scaled.mat))
  # invalid penalty
  penalty <- -5
  expect_error(LMLogisticLossL2(X.scaled.mat, y.vec.binary, penalty, opt.thresh, initial.weight.vec, step.size))
})


test_that("LMLogisticLossL2penalties correct return dimensions", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  penalty.vec <- seq(10, 1, by=-0.5)
  ret <- LMLogisticLossL2penalties(X.mat.binary, y.vec.binary, penalty.vec)
  expect_equal(ncol(ret), length(penalty.vec.mat))
  expect_equal(nrow(ret), ncol(X.mat.binary) + 1)
})


test_that("LMLogisticLossL2penalties stops on incorrect input", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  penalty.vec <- seq(1, 10, by=0.5)
  expect_error(LMLogisticLossL2penalties(X.mat.binary, y.vec.binary, penalty.vec))
})


test_that("LMSquareLossL2CV correct return dimensions", {
  data(ozone, package="ElemStatLearn")
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  penalty.vec <- seq(10, 1, by=-0.5)
  ret <- LMSquareLossL2CV(X.mat, y.vec, fold.vec, penalty.vec)
  expect_equal(ncol(X.mat), length(ret$weight.vec))
})


test_that("LMSquareLossL2CV stops on incorrect input", {
  data(ozone, package="ElemStatLearn")
  X.mat <- ozone[, 2:ncol(ozone)]
  y.vec <- ozone[, "ozone"]
  fold.vec <- sample(rep(1:4, l=nrow(X.mat)))
  penalty.vec <- seq(1, 10, by=0.5)
  expect_error(LMSquareLossL2CV(X.mat, y.vec, fold.vec, penalty.vec))
})


test_that("LMLogisticLossL2CV correct return dimensions", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  penalty.vec <- seq(10, 1, by=-0.5)
  ret <- LMLogisticLossL2CV(X.mat.binary, y.vec.binary, fold.vec, penalty.vec)
  expect_equal(ncol(X.mat.binary), length(ret$weight.vec))
})


test_that("LMLogisticLossL2CV stops on incorrect input", {
  data(spam, package="ElemStatLearn")
  X.mat.binary <- spam[,1:ncol(spam) -1]
  y.vec.label <- spam[,'spam']
  y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0 )
  penalty.vec <- seq(1, 10, by=0.5)
  expect_error(LMLogisticLossL2CV(X.mat.binary, y.vec.binary, fold.vec, penalty.vec))
})
