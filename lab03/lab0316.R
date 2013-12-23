N=4
n=25
m=2

x1 <- c(191,195,181,183,176,208,189,197,188,192,179,183,174,190,188,163,195,186,181,175,192,174,176,197,190)
x2 <- c(155,149,148,153,144,157,150,159,152,150,158,147,150,159,151,137,155,153,145,140,154,143,139,167,163)
x3 <- c(179,201,185,188,171,192,190,189,197,187,186,174,185,195,187,161,183,173,182,165,185,178,176,200,187)
x4 <- c(145,152,149,149,142,152,149,152,159,151,148,147,152,157,158,130,158,148,146,137,152,147,143,158,150)


X <- cbind(x1,x2,x3,x4)
mn  <- c(mean(x1), mean(x2), mean(x3), mean(x4))

A <- matrix(0, nrow=4, ncol=4);
count <- 1
while(count <= 25) {
  x <- X[count,]
  A <- A + (x - mn) %*% t(x - mn)
  count  <- count + 1
}
Aijkl <- function(A, i, j, k, l) {
  matrix(c(A[k, i], A[k,j], A[l,i], A[l,j]), 2, 2)
}
Aij.kl <- function(A, i, j, k, l) {
  A1 <- Aijkl(A, i, j, i, j)
  A2 <- Aijkl(A, k, l, i, j)
  A3 <- Aijkl(A, k, l, k, l)
  A1 - A2 %*% solve(A3) %*% t(A2)
}
rij.kl <- function(A, i, j, k, l) {
  A <- Aij.kl(A, i, j, k, l)
  A[1,2] / sqrt(A[1,1] * A[2,2])
}
r34.12 <- rij.kl(A, 3, 4, 1, 2)

alpha <- 1 - 0.95
delta <- qnorm(1 - alpha/2, 0, 1)
Z <- function(r) {
  (1/2) * log((1 + r)/(1 - r))
}
th <- function(v) {
  (exp(v) - exp(-v))/(exp(v) + exp(-v))
}
rho_left <- th(Z(r34.12) - delta/sqrt(24))
rho_right <- th(Z(r34.12) + delta/sqrt(24))

Rj.kl <- function(A, i, j, k, l){
  A22 <- Aijkl(A, k, l, k, l)
  A12 <- Aijkl(A, k, l, i, j)
  if(j > 2) {
    j <- j - 2
  }
  sqrt((t(A12[,j]) %*% solve(A22) %*% A12[,j])/A[j,j])
}

R3.12 <- Rj.kl(A, 4, 3, 1, 2)
R4.12 <- Rj.kl(A, 3, 4, 1, 2)

delta <- qf(1-alpha, df1=N-m, df2=n-(N-m)-1)

d <- function(x) {
  (n - (N-m) - 1)/(N-m)*x^2/(1-x^2)
}

d(R3.12) <= delta
d(R4.12) <= delta
