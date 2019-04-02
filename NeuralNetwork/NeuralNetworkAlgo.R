data(ozone, package = "ElemStatLearn")
head(ozone)

# Neural Network Algorithm
#1 A = XV
#2 Z = S[A]
#3 b = ZW
#4 delta(w) = b - y
#5 delta(v) = Diag(delta(w)) S'[A] Diag(w)
#6 GradDesc(L(w, V)) = t(Z) delta(w) / n
#7 GradDesc(L(w, v)) = t(X) delta(V) / n

X.unscaled.mat <- as.matrix(ozone[, -1])
head(X.unscaled.mat)


X.scaled.mat <- scale(X.unscaled.mat)
head(X.scaled.mat)
y.vec <- ozone[, 1]
n.hidden.units <- 2 # U

# Only Init Once
(V <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units)) # P x U
(w <- rnorm(n.hidden.units))

# Init V vector as random numbers
head(A <- X.scaled.mat %*% V) #1

# Sigmoid Function
sigmoid <- function(a){
  1/(1+exp(-a))
}

head(Z <- sigmoid(A)) #2 / S[A]

# Init weight vec as random numbers 
head(b <- as.numeric(Z %*% w)) #3
head(delta.w <- b - y.vec) #4
head(A.deriv <- Z * (1 - Z)) #S'[A]
head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(w)) #5
head(grad.w <- t(Z) %*% delta.w / nrow(X.scaled.mat)) #6
head(grad.v <- t(X.scaled.mat) %*% delta.v / nrow(X.scaled.mat)) #7

# Take a step
step.size = 0.01
for(step in 0:5) {
  (w <- w - step.size * grad.w)
  (V <- V - step.size * grad.v)
  
  # See how it is working
  print(sum(abs(c(grad.w, as.numeric(grad.v)))))
}

