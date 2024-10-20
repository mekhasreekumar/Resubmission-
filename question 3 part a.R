# Load necessary library
library(ggplot2)

# Parameters for the mixture distribution
alpha1 <- 0.2
mu1 <- 1
sigma1 <- sqrt(0.5)
alpha2 <- 0.8
mu2 <- 2
sigma2 <- sqrt(0.1)

# Fine-tuned proposal distribution parameters (Example)
mu_q <- 1.6
sigma_q <- sqrt(0.6)

# Number of samples
n_samples <- 10000

# Precompute the maximum ratio for global c
x_vals <- seq(-2, 6, length.out = 10000)  # Extend range to cover the distributions better
p_x_vals <- alpha1 * dnorm(x_vals, mean = mu1, sd = sigma1) + alpha2 * dnorm(x_vals, mean = mu2, sd = sigma2)
q_x_vals <- dnorm(x_vals, mean = mu_q, sd = sigma_q)
c <- max(p_x_vals / q_x_vals)

# Rejection sampling
samples <- numeric(n_samples)
accepted <- 0

for (i in 1:n_samples) {
  # Sample from the proposal distribution
  x <- rnorm(1, mean = mu_q, sd = sigma_q)
  
  # Mixture pdf
  p_x <- alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  
  # Proposal pdf
  q_x <- dnorm(x, mean = mu_q, sd = sigma_q)
  
  # Acceptance criterion
  u <- runif(1)
  if (u < (p_x / (c * q_x))) {
    samples[accepted + 1] <- x
    accepted <- accepted + 1
  }
}

# Trim the samples array to the accepted length
samples <- samples[1:accepted]

# Calculate acceptance rate
acceptance_rate <- accepted / n_samples

# Output results
cat("Acceptance Rate:", acceptance_rate, "\n")

# Plotting the results
ggplot(data.frame(samples), aes(x = samples)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 'blue', alpha = 0.5) +
  stat_function(fun = function(x) {
    alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  }, color = 'red') +
  labs(title = paste("Acceptance Rate:", round(acceptance_rate, 4))) +
  xlab("Value") + ylab("Density")

# Composition method
n_comp_samples1 <- round(n_samples * alpha1)
samples1 <- rnorm(n_comp_samples1, mean = mu1, sd = sigma1)
n_comp_samples2 <- round(n_samples * alpha2)
samples2 <- rnorm(n_comp_samples2, mean = mu2, sd = sigma2)

# Combined samples
comp_samples <- c(samples1, samples2)

# Plotting the composition method results
ggplot(data.frame(comp_samples), aes(x = comp_samples)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = 'green', alpha = 0.5) +
  stat_function(fun = function(x) {
    alpha1 * dnorm(x, mean = mu1, sd = sigma1) + alpha2 * dnorm(x, mean = mu2, sd = sigma2)
  }, color = 'red') +
  labs(title = "Composition Method") +
  xlab("Value") + ylab("Density")
