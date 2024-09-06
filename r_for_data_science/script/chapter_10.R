# this file contains worked examples and solutions to the problems from Chapter 10 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/eda

library(tidyverse)

# EDA is an iterative statistical practice which seeks to:
# (i)  determine the distribution of individual variables present in data and their statistical moments
# (ii) determine the covariation of groups of variables, i.e. the mixed statistical moments of variables

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.25)

smaller_diamonds <- diamonds |> 
  filter(carat < 3.)

ggplot(smaller_diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

# take care to read the scale of a plot, as it may hint, e.g., at outliers
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.25)

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.25) +
  coord_cartesian(ylim = c(0, 50))

unusual_diamods <- diamonds |> 
  filter(y < 3| y > 20) |> 
  select(price, x, y, z) |> 
  arrange(y)

# in this case, unusual cases are most likely erroneous entries
unusual_diamods

# exercises 10.3.3
# TODO

# rather than filtering the suspicious  entries out, mark them with NAs
diamonds_cleared <- diamonds |> 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(diamonds_cleared, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth()

# also take care to set a suitable scale when displaying a few variables with different ranges
# following is a poor example
nycflights13::flights |> 
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + (sched_min / 60.)
  ) |> 
  ggplot(aes(x = sched_dep_time)) +
  geom_freqpoly(aes(colour = cancelled), binwidth = 1/4.)

# exercises 10.4.1
# TODO

ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(colour = cut), binwidth = 500, linewidth = 0.75)

# when comparing probability distributions, one can simply turn from a frequency plot to a density plot
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(colour = cut), binwidth = 500, linewidth = 0.75)

# the diamonds of poorer quality have a higher median price
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

# when the factor is not ordered, fct_reorder() can be used
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()

ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

# exercises 10.5.1.1
# TODO


# covariation of two variables --------------------------------------------
# contingency tables and density plots are used for two categorical variables
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

diamonds |> 
  count(color, cut) |> 
  ggplot(aes(x = cut, y = color)) +
  geom_tile(aes(fill = n))

# exercises 10.5.2.1
# TODO

# scatterplot and its modifications are used for two numerical variables
ggplot(smaller_diamonds, aes(x = carat, y = price)) +
  geom_point()

# however, as the number of data point soars, points overlap each other and the density cannot be estimated correctly
# the possible workarounds are:
# (i)   to use transparent points
# (ii)  use a 2D analogue of barchart, that is bin different ranges of the explored variables
# (iii)  bin one of the continuous variables and leave another one unchanged, use a boxplot chart for multiple categories

# (i)
ggplot(smaller_diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 1 / 100)

# (ii)
ggplot(smaller_diamonds, aes(x = carat, y = price)) +
  geom_bin2d()

# install.packages("hexbin")
ggplot(smaller_diamonds, aes(x = carat, y = price)) +
  geom_hex()

# (iii)
ggplot(smaller_diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

# exercises 10.5.3.1
# TODO

install.packages("tidymodels")
library(tidymodels)

diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()

ggplot(diamonds_aug, aes(x = cut, y = .resid)) + 
  geom_boxplot()

# it might be useful to overlay the density plots of different groups of observations, e.g. those made on different days, to quickly check if they are similar
nycflights13::flights |> 
  filter(dep_delay < 120) |> 
  ggplot(aes( x = dep_delay, group = interaction(day, month))) +
  geom_freqpoly(binwidth = 5, alpha = 1 / 5.)