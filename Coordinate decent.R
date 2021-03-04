library("MASS")
sig <- 0.9
n <- 100
data <- mvrnorm(n, mu=c(0,0), Sigma=matrix(data=c(1,sig, sig, 1), nrow=2))

error <- rnorm(n)

y <- data[,1]-data[,2]+ error

fit <- lm(y ~ data[,1]+data[,2]-1)


summary(fit)

OLS <- function(y, x1, x2) {
  b1 <- 0 
  b2 <- 0 
  b11 <- 1
  b22 <- 1
  k <- 0 
  while (abs(b11 - b1) > 0.1 | abs(b22 - b2) > 0.1){
    k <- k +1 
    b11 <- b1
    b22 <- b2
    r1 <- y - x2 * as.numeric(b2)
    b1 <- solve(t(x1) %*% x1) %*% t(x1) %*% r1
    r2 <- y - x1 * as.numeric(b1)
    b2 <- solve(t(x2) %*% x2) %*% t(x2) %*% r2
  }
  return(c(k,b1,b2))  
}


OLS(y, data[,1], data[,2])
