#' Including libraries
library(car)
library(MASS)
library(knitr)

#' Initializing variables
mu <- c(1, 2)
rho <- c(-.5, 0, .5)
sigma1 <- sqrt(1);
sigma2 <- sqrt(4);
probability <- .95

print(mu)
print(probability)

for (correlation in rho) {
  #Generating samples ~N(mu, Sigma)
  covariance <- correlation*sigma1*sigma2
  Sigma <- matrix(c(1, covariance, covariance, 4), 2, 2)
  sample <- mvrnorm(n=100, mu, Sigma)
  print(paste("correlation: ", correlation))
  
  
  #Getting mean and covariance matrix for new sample
  smean <- apply(sample, 2, mean)
  scov <- cov(sample)
  
  print("Sample mean: ")
  print(mu)
  print("Sigma:")
  print(Sigma)
  
  variance <- apply(sample, 2, var)
  print("Variance: ");
  print(variance);
  
  # Getting the limits of the plot
  xmax <- max(sample[, 1])
  ymax <- max(sample[, 2])
  xmin <- min(sample[, 1])
  ymin <- min(sample[, 2])
  
  # Visualizing sample on plot with two elipses: expected and observed
  plot(sample, sub="Sample", xlab="x", ylab="y", xlim=c(xmin - 1, xmax + 1), ylim=c(ymin - 1, ymax + 1), pch=21, col='blue', bg='lightblue')
  ellipse(smean, scov, sqrt(qchisq(probability,2)))
  ellipse(mu, Sigma, 
          sqrt(qchisq(probability, 2)), col=3, lty=2)  
}
