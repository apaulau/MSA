library(MASS)

p <- 2; q <- 3; n1 <- 25; n2 <- 35; n3 <- 40; epsilon <- 0.05;
mu1 <- c(1, 2); mu2 <- c(3, 4); mu3 <- c(2, 3); mu <- c(0, 0);
I <- matrix(c(1, 0, 0, 1), 2, 2)
Sigma1 <- I; Sigma2 <- I; Sigma3 <- I; Sigma <- I;

a1 <- 1; a2 <- 1; a3 <- -2; a11 <- 2; a22 <- 1; a33 <- 3;

# Generating samples

X1 <- mvrnorm(n=n1, mu1, Sigma1)
X2 <- mvrnorm(n=n2, mu2, Sigma2)
X3 <- mvrnorm(n=n3, mu3, Sigma3)

X <- list(X1, X2, X3)
n <- sum(n1, n2, n3)

xmean <- function(x){
  c(mean(x[,1]), mean(x[,2]))
}

xmn <- c(xmean(X1), xmean(X2), xmean(X3))

calcA <- function(X, mn, k, l, n) {
  A <- matrix(0, nrow=k, ncol=l);
  count <- 1
  while(count <= n) {
    x <- X[count,]
    A <- A + (x - mn) %*% t(x - mn)
    count  <- count + 1
  }
}

calcS <- function(X, q, n){
  S <- matrix(nrow=q, ncol=n, 0)
  for(x in X) {
    tmp <- c()
    count <- 1
    ni <- length(x)
    while(count < ni)
      tmp[count] <-
  }
}
}