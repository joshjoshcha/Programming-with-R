# Q1
170166719 %% 31079

# Q2
areas <- pi * (3:100)^2
areas

# Q3
net_worth <- 2000 * (1.03^(1:30))
interest <- net_worth - 2000
interest

# Q4
x <- rep(0:4, each = 5)
x

y <- rep(1:5, times = 5)
y

z <- rep(1:5, times = 5) + rep(0:4, each = 5)
z

# Q5 
r <- 1.08
exponents <- 1:100
sum_values <- (r * (1 - r^exponents)) / (1 - r)
sum_values

# Q6
seq1 <- 2^(1:15)  
seq2 <- (1:15)^3  

result <- seq1 > seq2
result
# so the 1st element and 10~15th element of the first seq. exceeds the corresponding elements of the second seq

# Q7
solar.radiation <- c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2)

mean_solar <- mean(solar.radiation)
median_solar <- median(solar.radiation)
range_solar <- range(solar.radiation)
variance_solar <- var(solar.radiation)
mean_solar
median_solar
range_solar
variance_solar

sr10 <- solar.radiation + 10

mean_sr10 <- mean(sr10)
median_sr10 <- median(sr10)
range_sr10 <- range(sr10)
variance_sr10 <- var
mean_sr10 <- mean(sr10)
median_sr10 <- median(sr10)
range_sr10 <- range(sr10)
variance_sr10 <- var(sr10)
mean_sr10 
median_sr10 
range_sr10 
variance_sr10 
# Adding 10 to each observation increases the mean, median, and range (both lower and upper limit) by that amount
# Variance is unchanged

srm2 <- solar.radiation * -2

mean_srm2 <- mean(srm2)
median_srm2 <- median(srm2)
range_srm2 <- range(srm2)
variance_srm2 <- var(srm2)
mean_srm2
median_srm2
range_srm2
variance_srm2
# Multiplying by -2 multiplies the mean, median by that amount
# Similar thing happens for the range, but because the sign is changed, the lower and upper limits swap places
# Variance multiplies by the square of the (-2), that is 4
