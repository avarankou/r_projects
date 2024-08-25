# this file contains the worked examples and solutions to the problems from Chapter 3 of
# Bruce, P., Bruce, A. 2017. "Practical Statistics for Data Scientists", O'Reilly.

# ---------------
# pages [106;138)
# permutation test vs Student's T-test

# when (i) empirical data is available and (ii) empirical distribution is poorly approximated with common theoretical distributions, permutation testing can substitute classic statistical significance testing to answer the question if the observed difference between (two or more) groups is statistically significant or not
# saying that the difference is not significant amounts to saying that it could have been brought up by chance; while statistical significance tests calculate the likelihood of the observed difference when some assumptions about the sampling population are granted, a permutation test uses brute computational power to see if one gets notably different results from what has been observed when randomly permuting the observed data points

library(ggplot2)

sample_size <- c(A = 25, B = 30)
sample_a <- runif(sample_size["A"], min = 0, max = 1)
sample_b <- runif(sample_size["B"], min = 0, max = 1)
sample <- data.frame(response = c(sample_a, sample_b), x = c(rep("A", sample_size["A"]), rep("B", sample_size["B"])))

diff <- mean(sample_a) - mean(sample_b)

ggplot(sample, aes(x=x, y=response)) +
  geom_boxplot()

# to perform a permutation test for two groups (a.k.a. A/B test):
# (i)     permute the observed data points to get samples A' and B' of the original size and record the statistics of interest
# (ii)    repeat step (i) R times, where R is rather big
# (iii)   compare the distribution of the statistics of interest produced at step (ii) with the one originally observed
# (iii-a) if the original difference is out of the produced distribution or is close to its border, it is statistically significant, i.e. could not have been (easily) brought up by chance
# (iii-b) otherwise, it is not statistically significant
# NB: the distribution obtained at step (ii) can be used to calculate the p-value for the observed values of the statistics of interest

perm_func <- function(sample, sample_size) {
  idx_a <- sample(1:(sample_size["A"] + sample_size["B"]), sample_size["A"], rep = FALSE)
  idx_b <- setdiff(1:(sample_size["A"] + sample_size["B"]), idx_a)
  
  diff <- mean(sample[idx_a]) - mean(sample[idx_b])
  return (diff)
}

num_runs <- 1000
diffs <- rep_len(0, num_runs)
for (i in 1:num_runs) {
  diffs[[i]] <- perm_func(sample$response, sample_size)
}

hist(diffs, xlab = "Response in permutation test")
abline(v = diff, col = "red")

p_value <- mean(abs(diffs - diff) < 0.05)

# Cf. Student's t-Test results
t.test(response ~ x, data = sample, alternative = "two.sided")

# ---------------
# pages [138;144)
# permutation test vs (M)ANOVA

# a permutation test can also substitute an ANOVA test, i.e. the approach described above can be used to test if there is a significant difference in the results observed in multiple (>2) groups
sample <- data.frame(time = c(164, 172, 177, 195, 156, 178, 191, 182, 185, 177, 175, 193, 171, 163, 176, 155, 166, 164, 170, 168), page = rep(c("Page A", "Page B", "Page C", "Page D"), times = rep(5, 4)))

diff <- var(tapply(sample$time, INDEX = sample$page, FUN = mean))
ggplot(sample, aes(x=page, y=time)) +
  geom_boxplot()

# to perform a permutation test for variance between multiple groups
# (the assumption that the groups have equal size can be made to simplify the procedure and make it compatible with a classic ANOVA test)
# (i)     permute the observed data points to get samples A', B', C' etc. of the original size and record the mean value of each sample as well as the variance of samples' means
# (ii)    repeat step (i) R times where R is rather big
# (iii)   compare the distribution of variance produced at step (ii) with the one originally observed
# (iii-a) if the original variance is out of the produced distribution or is close to its border, it is statistically significant, i.e. could not have been (easily) brought up by chance
# (iii-b) otherwise, it is not statistically significant

perm_func <- function(sample, num_groups, group_size) {
  # indexes for a random permutation of the observed data points
  idx <- sample(rep(1:num_groups, group_size))
  
  sample_mean <- rep(0, num_groups)
  for (i in 1:num_groups) {
    sample_mean[[i]] <- mean(sample[idx == i])
  }
  
  return (var(sample_mean))
}

num_runs <- 1000
vars <- rep_len(0, num_runs)
for (i in 1:num_runs) {
  vars[[i]] <- perm_func(sample$time, 4, 5)
}

hist(vars, xlab = "Time variance in permutation test, sec^2")
abline(v = diff, col = "red")
p_value <- mean(vars > diff)

# there is a routine for anova-style permutation tests in lmPerm package
library(lmPerm)
summary(aovp(time ~ page, data = sample))

# Cf. ANOVA results
summary(aov(time ~ page, data = sample))

# ---------------
# pages [144;153)
# permutation test vs Pearson's Chi-Square Test

# a permutation test can also substitute Pearson's Chi-Square Test, i.e. the approach described above can be use to test if there is a significant difference between the individual columns of an r*c contingency matrix describing the observed results

sample <- data.frame(count = c(14, 8, 12, 986, 992, 988), headline = rep(c("A", "B", "C"), times = 2), click = rep(c(T, F), times = c(3, 3)))
sample_t <- as.table(rbind(sample$count[sample$click == TRUE], sample$count[sample$click == FALSE]))
expected <- tapply(sample$count, INDEX = sample$click, FUN = mean)

residual_f <- function(x) {
  expected <- mean(x)
  
  residual <- (x - expected) / sqrt(expected)
  return (sum(residual^2))
}
residual <- tapply(sample$count, INDEX = sample$click, FUN = residual_f)

diff <- sum(residual)

# to perform a permutation test for the difference between expected and observed bin counts
# (i)     permute the observed data points to get an r*c contingency matrix with the same size of each bin as in the original one and record the squared sum of residuals, i.e. Pearson's Chi statistic
# (ii)    repeat step (i) R times where R is rather big
# (iii)   compare the distribution of Chi produced at step (ii) with the one originally observed
# (iii-a) if the original value of Chi statistic is out of the produced distribution or is close to its border, it is statistically significant, i.e. could not have been (easily) brought up by chance
# (iii-b) otherwise, it is not statistically significant

# TODO a permutation function for Pearson's Chi-Square Test

# Cf. Chi-Square results
chisq.test(sample_t, simulate.p.value = TRUE)

# Cf. Fisher's exact test that is useful when some counts are extremely small
fisher.test(sample_t)