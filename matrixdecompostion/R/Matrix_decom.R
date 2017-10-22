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
    #print(v)
    R[j,j] <- sqrt(sum(v^2))
    Q[,j] <- v/R[j,j]
  }
  print(Q%*%R)
  print(R)
  return(Q)  
}
GramSchm(ma)


Householder <- function(A){
  
}
