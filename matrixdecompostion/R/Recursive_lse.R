#' Recursive Least Square Estimate 
#'
#' This function compute LSE recursively 
#'
#' @param X1,X2,y1,y2,beta1 the matrix waiting for QR decom
#' @return  LSE: beta 
#' @export
updated_lse <- function(X1,X2,y1,y2,beta){
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  y1 <- as.matrix(y1)
  y2 <- as.matrix(y2)
  Rn <- t(X1) %*% X1
  inv_Rn <- solve(Rn)
  I <- diag(1, nrow(X2))
  inv_Rnp1 <- inv_Rn - inv_Rn%*%t(X2)%*% solve(I+X2%*%inv_Rn%*%t(X2)) %*%X2%*%inv_Rn
  beta_new <- beta + inv_Rnp1%*%t(X2)%*%(y2 - X2%*%beta)
  return(beta_new)
}