#' Including libraries
library(car)
library(MASS)
library(knitr)
library(psych)

#' Initializing variables
mu <- c(1, 2)
rho <- c(-.5, 0, .5)
sigma1 <- sqrt(1);
sigma2 <- sqrt(4);
probability <- .95

print(mu)
print(probability)

count <- 0
for (correlation in rho) {
  count <- count + 1
  print(paste("correlation: ", correlation))
  
  # Calculating the covariance matrix
  covariance <- correlation*sigma1*sigma2 # elements (1,2) and (2,1) in covariance matrix Sigma
  Sigma <- matrix(c(1, covariance, covariance, 4), 2, 2)
  
  # Generating samples ~N(mu, Sigma)
  sample <- mvrnorm(n=100, mu, Sigma)
    
  #Getting mean and covariance matrix for new sample
  smean <- apply(sample, 2, mean)
  scov <- cov(sample)
  
  print("Sample mean:")
  print(smean)
  print("Sample covariance matrix:")
  print(scov)
  
  variance <- apply(sample, 2, var)
  print("Variance: ");
  print(variance);
  
  # Descriptive statistics
  dstat <- describe(sample)
  print(dstat)
  
  # Getting the limits of the plot
  xmax <- max(sample[, 1])
  ymax <- max(sample[, 2])
  xmin <- min(sample[, 1])
  ymin <- min(sample[, 2])
  
  # Visualizing sample on plot with two ellipses: expected and observed
  plot(sample, sub=paste("Sample ", count, ", correlation=", correlation),
       xlab="x", ylab="y",
       xlim=c(xmin - 1, xmax + 1), ylim=c(ymin - 1, ymax + 1),
       pch=21, col='blue', bg='lightblue')
  ellipse(smean, scov, sqrt(qchisq(probability,2)))
  ellipse(mu, Sigma, 
          sqrt(qchisq(probability, 2)), col=3, lty=2)  
}