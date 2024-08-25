# this file contains the worked examples and solutions to the problems from Chapter 1 of
# Bruce, P., Bruce, A. 2017. "Practical Statistics for Data Scientists", O'Reilly.

# ---------------
# pages [1;41)
# basic statistics for numeric data

setwd("D:/r_projects/practical_statistics_for_data_scientists")
state <- read.csv(file = "data/state.csv", stringsAsFactors = FALSE)

is.data.frame(state) # TRUE
summary(state)

# mean, trimmed mean, weighed mean
mean_ <- function(x, weights = c(), trim = 0.0) {
  n <- NROW(x)
  if (missing(weights))
    weights <- rep(1, n)
  
  if (trim <= 0)
    return (sum(x * weights) / sum(weights))
  else {
    t <- floor(trim * n)
    
    if (missing(weights)) {
      return (sum(sort(x)[(t+1) : (n-t)]) / (n - 2*t))
    }
    else {
      x <- x * weights
      o <- order(x, decreasing = FALSE)
      x <- x[o]
      weights <- weights[o]
      
      return (sum(x[(t+1) : (n-t)]) / sum(weights[(t+1) : (n-t)]))
    }
  }
}


# TODO implement weighed median and compare the results with matrixStats::weightedMedian
# the weighed quantile of variable X is a value Xq
# such that in sorted X the sum of weights of Xi less than Xq (Xi < Xq) equals q
# { Xq : Sum(Xi) = q, Xi < Xq, X is sorted in ascending order }

quantile_ <- function(x, q, type = 7) {
  # TODO compare with base R implementation
  # test for various sample sizes
  # graph discontinuous (t = 1...3) and continuous (t = 4...9) to see the difference
  check_range <- function(idx) {
    idx[idx < 1] <- 1
    idx[idx > n] <- n
    return (idx)
  }
  # recall that floating-point arithmetic comes with rounding errors
  # hence, set the minimal difference between x and round(x), floor(x) etc. to take into account
  fp_tolerance <- 1e-15
  
  n <- NROW(x)
  t <- q * n
  
  x <- sort(x)
  
  if (type == 1) {
    # if nq is integer, return the point Xj at nq-th position in sorted vector ob observations X
    # otherwise, return the next greater point, i.e. X(j+1)
    shift <- as.integer(t - floor(t) > fp_tolerance)
    idx <- floor(t) + shift
    
    return (x[check_range(idx)])
  }
  else if (type == 2) {
    # if nq is integer, return the mean of the point Xj found at nq-th position in sorted vector of observations X and the next greater point, i.e. X(j+1)
    # otherwise, return the next greater point, i.e. X(j+1)
    shift <- t - floor(t) > fp_tolerance
    idx <- floor(t)
    
    retval <- rep_len(0, length(idx))
    retval[!shift] <- (x[check_range(idx[!shift])] + x[check_range(idx[!shift ] + 1)]) / 2
    retval[shift] <- x[check_range(idx[shift] + 1)]
    
    return (retval)
  }
  else if (type == 3) {
    # if (nq - 0.5) is even integer, return the point Xj at nq-th position in sorted vector ob observations X
    # otherwise, return the next greater point, i.e. X(j+1)
    # NB: in this implementation, we first select the elements X(j+1) and then subtract the shift of 1 where necessary
    
    t <- t - 0.5
    floor_t <- floor(t)
    shift <- (floor_t %% 2 == 0) & (t - floor_t < fp_tolerance)
    idx <- floor_t + 1 - shift
    
    return (x[check_range(idx)])
  }
  else if (type == 4) {
    # if nq is integer, return the point Xj at nq-th position in sorted vector ob observations X
    # otherwise, use linear interpolation to compute a value between Xj and return the next greater point, i.e. X(j+1)
    idx = floor(t)
    # equivalently,
    # gamma = t - floor(t)
    gamma = t - idx
    
    next_idx <- check_range(idx + 1)
    idx <- check_range(idx)
    
    return ((1 - gamma) * x[idx] + gamma * x[next_idx])
  }
  else if (type == 7) {
    t <- t + (1 - q)
    
    idx = floor(t)
    # equivalently,
    # gamma = t - floor(t)
    gamma = t - idx
    
    next_idx <- check_range(idx + 1)
    idx <- check_range(idx)
    
    return ((1 - gamma) * x[idx] + gamma * x[next_idx])
  }
}

variability_metric_ <- function(x, name) {
  switch (name,
    min =, MIN = quantile_(x, 0),
    max =, MAX = quantile_(x, 1),
    median =, MEDIAN = quantile_(x, 0.5),
    iqr =, IQR = diff(quantile_(x, c(0.25, 0.75))),
    variance = sum((x - mean_(x))^2) / (NROW(x) - 1),
    sd = , SD = sqrt(sum((x - mean_(x))^2) / (NROW(x) - 1)),
    # TODO compare with base R implementation
    mad =, MAD = quantile_(abs(x - quantile_(x, 0.5)), 0.5),
    stop("Unknown metric name!")
  )  
}

cor_ <- function(x, y, method = "pearson") {
  # TODO implement kendall and spearman correlation quotients and compare to base R implementation
  de_mean <- function(x) {
    return (x - mean_(x))
  }
  
  inner_product <- function(x, y = x) {
    return (x %*% y)
  }
  
  n <- NROW(x)
  if (NROW(y) != n) stop("Vectors x and y do not have the equal length!")
  
  switch(method,
         pearson = sum((x - mean_(x)) * (y - mean_(y)) / (n - 1) / variability_metric_(x, "sd") / variability_metric_(y, "sd")),
         kendall = ,
         spearman = ,
         vector = inner_product(de_mean(x), de_mean(y)) / sqrt(inner_product(de_mean(x)) * inner_product(de_mean(y))),
         stop("Unknown method name!"))
}


# arithmetic mean
mean(state$Population) == mean_(state$Population) # TRUE

res <- rep_len(FALSE, NROW(state$Population))
for (i in seq_along(state$Population))
  res[i] <- mean(state$Population[1:i], t = 0.1) == mean_(state$Population[1:i], trim = 0.1)
sum(!res) == 0 # TRUE

# weighed mean
res <- rep_len(FALSE, NROW(state$Population))
for (i in seq_along(state$Population))
  res[i] <- weighted.mean(state$Murder.Rate[1:i], state$Population[1:i]) == mean_(state$Murder.Rate[1:i], weights = state$Population[1:i])
sum(!res) == 0 # TRUE

# weighed trimmed mean
mean_(state$Murder.Rate, weights = state$Population, trim = 0.1)

# quantiles
probs <- seq(0, 1, 0.1)
quantile(state$Population, probs = probs, type = 1) == quantile_(state$Population, q = probs, type = 1)

quantile(state$Population, probs = probs, type = 2) == quantile_(state$Population, q = probs, type = 2)

quantile(state$Population, probs = probs, type = 3) == quantile_(state$Population, q = probs, type = 3)

quantile(state$Population, probs = probs, type = 4) == quantile_(state$Population, q = probs, type = 4)

quantile(state$Population, probs = probs, type = 7) == quantile_(state$Population, q = probs, type = 7)

# variability metrics
variability_metric_(state$Population, "min") == min(state$Population)
variability_metric_(state$Population, "max") == max(state$Population)
variability_metric_(state$Population, "median") == median(state$Population)
variability_metric_(state$Population, "IQR") == IQR(state$Population)
variability_metric_(state$Population, "MAD") == mad(state$Population)
variability_metric_(state$Population, "sd") == sd(state$Population)
variability_metric_(state$Population, "variance") == var(state$Population)

quantile(state$Murder.Rate, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
boxplot(state$Population / 1e6, ylab = "Population, million")

# frequency table
breaks <- seq(from = min(state$Population), to = max(state$Population), length = 11)
freq <- cut(state$Population, breaks = breaks, include.lowest = TRUE, right = TRUE)
table(freq)
hist(state$Population, breaks)

# histogram and density plot
hist(state$Murder.Rate, freq = FALSE)
lines(density(state$Murder.Rate), lwd = 3)

# ---------------
# pages [41;46)
# basic statistics for categorical data

delay <- read.csv("data/dfw_airline.csv")
delay_pct <- delay / sum(delay)
barplot(as.matrix(delay) / 1e3, cex.axis = 1)
mode(delay) == names(delay_pct)[[which.max(delay_pct)]]

# to find the mode of a dataset simply use table()
ds <- round(runif(100, min = 0, max = 10))

mode_ <- function(x) {
  t <- table(x)
  return (names(t)[[which.max(t)]])
}
mode_(ds)

# ---------------
# pages [46;63)
# correlation of univariate and multivariate data
library(corrplot)
library(ggplot2)

sp <- read.csv("data/sp500_data.csv", stringsAsFactors = FALSE)
sp_sym <- read.csv("data/sp500_sectors.csv", stringsAsFactors = FALSE)

table(sp_sym$sector)
etfs <- sp[row.names(sp) > "2012-07-01", sp_sym[sp_sym$sector == "etf", 'symbol']]

corrplot(cor(etfs), method = "circle", type = "upper")

plot(sp$VZ, sp$TEL)

# visualising bivariate and multivariate numeric data
kc <- read.csv("data/kc_tax.csv", stringsAsFactors = FALSE)
trimmed_kc <- subset(kc, kc$TaxAssessedValue < 750000 & kc$SqFtTotLiving > 100 & kc$SqFtTotLiving < 3500)

# hexagon binning plot
ggplot(trimmed_kc, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) +
  stat_binhex(colour="white") +
  theme_bw() +
  scale_fill_gradient(low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")

# contour plot (à la topographical map)
ggplot(trimmed_kc, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() +
  geom_point(alpha=0.1) +
  geom_density2d(colour="white") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")

# the above plots (that capture the relationship between 2 variables) can be split into separate facets for particular values of some 3rd variable
ggplot(subset(trimmed_kc, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) +
  stat_binhex(colour="white") +
  theme_bw() +
  scale_fill_gradient( low="white", high="blue") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")

# visualising bivariate categorical data
loan <- read.csv("data/lc_loans.csv", stringsAsFactors = FALSE)
t_loan <- table(loan)
tp_loan <- proportions(t_loan, margin = 2)
row.names(tp_loan) <- paste(row.names(tp_loan), ", %", sep = "")
tt_loan <- rbind(t_loan, tp_loan)

print.table(tt_loan[sort(row.names(tt_loan)),], digits = 2, zero.print = ".")

# NB: external packages provide utilities to print tables, e.g. Matrix::Matrix(), descr::CrossTable

airline <- read.csv("data/airline_stats.csv", stringsAsFactors = FALSE)

# boxplot for multiple categorical variables
boxplot(pct_carrier_delay ~ airline, data = airline, ylim=c(0, 50))

# violin plot for multiple categorical variables
# the plot's curve is the density distribution graph of a variable (with its mirror-image appended below it) rotated counter-clockwise by 90 degrees
ggplot(data=airline, aes(airline, pct_carrier_delay)) +
  ylim(0, 50) +
  geom_violin() +
  labs(x="", y="Daily % of Delayed Flights")