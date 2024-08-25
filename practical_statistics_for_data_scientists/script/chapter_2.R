# this file contains the worked examples and solutions to the problems from Chapter 2 of
# Bruce, P., Bruce, A. 2017. "Practical Statistics for Data Scientists", O'Reilly.

# ---------------
# pages [63;88)
# sampling from a population, bootstrap

income <- read.csv("data/loans_income.csv", stringsAsFactors = FALSE)
N = nrow(income) # 5*1e4

# take 1000 random samples of size 20 to 20 * 1000
# and check how the sample mean differs from that of population as the sample size increases
sample_size <- 20
num_samples <- 1e3
sample_mean <- c()
total_mean <- c()

sample <- sample(income$x, sample_size * num_samples, replace = FALSE)
sample_mean <- rep_len(0, num_samples)
sample_sd <- rep_len(0, num_samples)
sample_se <- rep_len(0, num_samples)

for (i in 1:num_samples) {
  sample_mean[[i]] <- mean(sample[1:(i*sample_size)]) 
  sample_sd[[i]] <- sd(sample[1:(i*sample_size)])
}
sample_se <- sample_sd / sqrt(sample_size * 1:num_samples)

plot(1:num_samples, sample_mean, col = "green", xlim = c(1, num_samples), ylim = c(1, 7e4))

points(1:num_samples, sample_sd, col = "blue")
plot(500:num_samples, sample_se[500:num_samples], col = "red")
abline(h = sd(income$x), col = "red", lty = 2)

# bootstrapping, i.e. sampling with replacement en masse
library(boot)
stat_fun <- function(x, idx) mean(x[idx])
boot_obj <- boot(income$x, R = 1000, statistic=stat_fun)

# ---------------
# pages [88; 106)
# normal distribution and other common distributions

# a QQ-plot plots z-scores of the observed distribution and a reference diagonal line that imitates the z-scores of a normal distribution
# the closer are the points to the diagonal, the better the observed distribution approximate a normal one
norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a = 0, b = 1, col = "grey")

norm_samp <- sort(rnorm(100))
# z-score is the result of data standardisation, i.e. the transformation of a distribution into one with the mean value of zero and the sd of one
# to standardise a dataset, subtract the mean from each data point and divide the result by the sd
z_score <- (norm_samp - mean(norm_samp)) / sd(norm_samp)
# as expected, z score of a normally distributed sample is bell-shaped
hist(z_score)

