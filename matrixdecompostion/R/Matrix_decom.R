#' GramSchm decompostion
#'
#' This function computes the QR decompostion using Gram-Schmidt algorithm
#'
#' @param A the matrix waiting for QR decom
#' @return  a list of matrx
#' @export


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
  return(list(Q=Q,R=R))
}

#' Compute orthognal matrix Q from U
#'
#'
#' @param U Compute orthognal matrix Q from U
#' @return  matrix Q using householder algorithm
#' @export
U2Q <- function(U){
  p <- ncol(U)
  n <- nrow(U)
  u <- U[,1]
  H <- diag(1, n)
  # H <- I- 2 * u %*% t(u)
  for(i in 1:p){
    u <- U[,i]
    I <- diag(1, n)
    # temp <- diag(1,n)
    temp <- I-2*u %*% t(u)
    H <- temp %*% H
  }
  return(H)
}

#' Householder decompostion
#'
#' This function computes the QR decompostion using Householder algorithm
#'
#' @param A the matrix waiting for QR decom
#' @return  a list of matrx
#' @export
Householder <- function(A){
  A <- as.matrix(A) # transform datatype: A to double
  n <- nrow(A)
  p <- ncol(A)
  U <- matrix(0, n, p)
  for(k in 1:p ){
    w <- A[k:n,k]
    w[1] <- w[1] - sqrt(sum(w^2))
    if(length(w) > 1){
      u <- w/sqrt(sum(w^2))
    }else{
      u <- 0
    }
    U[k:n,k] <- u
    A[k:n,k:p] <- A[k:n,k:p] - 2*u %*% ( t(u) %*% A[k:n,k:p] )
  }
  Q = t(U2Q(U))[,1:p]
  return(list(Q=Q, R=A[1:p,1:p] ) )
}

















