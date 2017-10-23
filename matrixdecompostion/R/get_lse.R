#' Get LSE 
#'
#' This function compute LSE of beta using the result of QR decompostion 
#'
#' @param X,y,algorithm,rep the matrix waiting for QR decom
#' @return  LSE: beta 
#' @export
get_lse <- function(X,y,algorithm,rep=20){
  #'Input X, response y
  #'Output LSE
  X <- as.matrix(X)
  y <- as.matrix(y)
  n <- ncol(X)
  beta <- rep( NA, n)
  if(algorithm == 'Householder'){
    rslt <- Householder(X)
  }else if(algorithm == 'GramSchm'){
    rslt <- GramSchm(X)
  }else if(algorithm == 'Jacobi'){
    beta <- rep(0,n)
    for(i in 1:rep){
      A <- t(X)%*% X
      b <- t(X) %*% y
      P <- diag(diag(A))
      beta <- (diag(1,n) - solve(P)%*%A ) %*% beta + solve(P) %*% b
    }
    return(beta)
  }else{
    print("Error:  Algorithm can only be one of:Householder, GramSchm and Jacobi")
  }
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
  }
  return(beta)
}

# beta <- get_lse(dt[,1:5], dt[,6],algorithm = "Jacobi" ,rep=10)
# beta1 <- get_lse(dt[,1:5], dt[,6],algorithm = "GramSchm" )
#
#
# fit <- lm(y ~ 0+ X1 + X2 + X3 + X4 + X5, data=dt)
# summary(fit)



