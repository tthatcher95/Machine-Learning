% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogisticLossEarlyStopping.R
\name{LMLogisticLossIterations}
\alias{LMLogisticLossIterations}
\title{LMLogisticLossIterations}
\usage{
LMLogisticLossIterations(x.unsc.mat, y.vec, max.iterations = 100,
  step.size = 0.05)
}
\arguments{
\item{x.unsc.mat}{An unscaled feature matrix [n x p]}

\item{y.vec}{A vector of predictions [p x 1]}

\item{max.iterations}{The number of times to step through your gradient descent [scalar]}

\item{step.size}{The size of the step to take during gradient descent [scalar]}
}
\value{
A unscaled weight matrix [max.iterations x p]
}
\description{
LMLogisticLossIterations
}
\examples{
data(spam, package="ElemStatLearn")
X.mat.binary <- spam[,1:ncol(spam) -1]
y.vec.label <- spam[,'spam']
y.vec.binary <- ifelse(y.vec.label == 'spam', 1, 0)
max.iterations=50
step.size=1
W.train <- LMLogisticLossIterations(X.train, Y.train, max.iterations, step.size)
cbind(1, X.mat) \%*\% W.mat
}
