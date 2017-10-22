#' This function computes the QR decompostion using Gram-Schmidt algorithm
#' Input matrix object
#' Output: two matrix Q, and R

ma <- matrix(1:4, 2, 2)

GramSchm <- function(A){
  # initialization
  Q = matrix(NA, nrow(A), ncol(A))
  R = matrix(0, ncol(A),ncol(A))
  v = A[,1]
  R[1,1] <- sqrt(sum(v^2))
  Q[,1] <- v/R[1,1]

  for(j in 2:ncol(A)){
    v <- A[,j]
    for(i in 1:(j-1)){
      R[i,j] <- t(Q[,i]) %*% A[,j]
      v <- v - R[i,j] * Q[,i]
    }
    R[j,j] <- sqrt(sum(v^2))
    Q[,j] <- v/R[j,j]
  }
  # stopifnot((Q%*%R - A))
  return(list(Q=Q,R=R))
}
rslt <- GramSchm(ma)
GramSchm(ma)

Householder <- function(A){
  n <- nrow(A)
  p <- ncol(A)
  U <- matrix(0, n, p )
  for(k in 1:(p-1) ){
    w <- A[k:n,k]
    w[1] <- w[1] - sqrt(sum(w^2))
    u <- w/sqrt(sum(w^2))
    U[k:n,k] <- u
    A[k:n,k:p] <- A[k:n,k:p] - 2*u %*% (t(u) %*% A[k:n,k:p] )
    # print(A)
  }
  return(list(Q=U, R=A[1:p,1:p] ) )
}
Householder(ma)


solve <- function(X,y){
  #'Input X, response y
  #'Output LSE
  n <- ncol(X)
  beta <- rep( NA, n)
  rslt <- GramSchm(X)
  Q <- rslt[[1]]
  R <- rslt[[2]]
  rhs <- t(Q) %*% y
  beta[n] <- rhs[n] / R[n,n]
  for(i in (n-1):1){
    v <- rhs[i]
    for(j in n:(i+1)){
      v <- v- R[i,j] * beta[j]
    }
    beta[i] <- v / R[i,i]
    #print(beta)
  }
  return(beta)
}

beta <- solve(dt[,1:5], dt[,6] )


fit <- lm(y ~ 0+ X1 + X2 + X3 + X4 + X5, data=dt)
summary(fit)















