islands
hist(islands, breaks = "Sturges", main = "Sturges's Rule")
hist(islands, breaks = "Scott", main = "Scott's Rule")
# Even though Scott's rule has more bins overall, Sturges's rule creates two bars per bin 
# So especially in the 0–5000 region which makes most of the dataset, Sturge's rule presents a clearer overview 
# With Scott's rule, we see how the dataset is even more skewed to the right than we would have with Sturge's rule

par(mfrow = c(1, 2)) # Set up a 1x2 layout for side-by-side plots
qqnorm(islands, main = "Q-Q Plot")
qqnorm(log(islands), main = "Logarithmic Q-Q Plot")
# logarithmic plot resembles more the bell-shaped normal distribution curve

set.seed(09062024)
U <- runif(1000)

sample_mean <- mean(U)
sample_variance <- var(U)
sample_sd <- sd(U)
sample_mean
sample_variance
sample_sd
# For Uniform(0,1), the theoretical mean is 0.5 
# Theoretical variance is (1-0)^2 / (12) = 1 / 12 = 0.083333...
# It follows that theoretical standard deviation would be sqrt(12)
# Sample mean is greater, sample variance is smaller, and sample sd is smaller

proportion_less_than <- mean(U < 0.6)
proportion_less_than
# probability that a Uniform(0, 1) random variable < 0.6 is 0.6
# so the theoretical probability is greater 

expected_value <- mean(1 / (U + 1))
expected_value

hist(U, main = "Histogram of U", xlab = "U")
hist(1 / (U + 1), main = "Histogram of 1/(U + 1)", xlab = "1/(U + 1)")
par(mfrow = c(1, 1))

plot(density(U), main = "Density Estimate")
curve(dunif(x, min = 0, max = 1), add = TRUE)

p_less <- pbinom(5, size = 20, prob = 0.3)
p_less
p_equals <- dbinom(5, size = 20, prob = 0.3)
p_equals
p_range <- pbinom(7, size = 20, prob = 0.3) - pbinom(4, size = 20, prob = 0.3)
p_range
quantile_90 <- qbinom(0.9, size = 20, prob = 0.3)
quantile_90

set.seed(123) 
data <- rbinom(30, size = 20, prob = 0.3)
data_quantile_90 <- quantile(data, 0.9)
data_quantile_90
quantile_90
# so sample quantile from data is slightly greater

prop_less_than_equal_5 <- mean(data <= 5)
prop_less_than_equal_5
p_less
# theoretical proportion is greater

plot(ecdf(data), main = "Empirical CDF of data")
rug(data)