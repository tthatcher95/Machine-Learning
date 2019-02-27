LMSquareLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {
  W.v = list()
  append(W.v, 0)
  W.t.0 = 0
  for(x in seq(0, step.size, max.iterations)) {
    print(x)
    W.t.1 = W.t.0 - step.size * 2 * (W.t.0)
    append(W.v, W.t.1)
    W.t.0 = W.t.1
  }
  return(W.v)
}

Wt = LMSquareLossIterations(c(1, 2 ,3 ,4, 5), c(5, 5, 3, 2, 1), 5, 0.5)
print(Wt)

LMLogisticLossIterations <- function(x.mat, y.vec, max.iterations, step.size) {

}
