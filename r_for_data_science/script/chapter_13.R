# this file contains worked examples and solutions to the problems from Chapter 13 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/numbers

library(tidyverse)
library(nycflights13)

# readr has functions to parse strings with numbers
# (i) parse_double() for strings with numbers but without extra symbols
# (ii) parse_number to extract numbers from strings of texts
# etc. cf. ?parse_atomic

# (i)
x <- c("1.2", "5.6", "1e3", "59%", "123 C")
parse_double(x)

# (ii)
x <- c("$1,234", "USD 3,513", "59%", "some tex@#t_ 1234.033 with a number")
parse_number(x)

x <- c("some tex@#t_ 1234.033 with a number or 2", "plain text, no numbers.")
parse_number(x)

# counting observations, i.e. data points in hand, is crucial, the two dplyr:: functions for the purpose are:
# (i)  count() that can be used on its own
# (ii) n_*() that can only be used inside summarise(), mutate(), filter(), group_by() etc.
# (ii-a) n() - count all entries
# (ii-b) n_distinct() count distinct values of some column(s)

flights |> count()
flights |> count(arr_delay == 0 & dep_delay == 0)
flights |> count(dest, sort = TRUE)

# to suppress the default behaviour, i.e. printing a few top lines only, add print(n = Inf) or View() to the pipe
flights |>
  count(dest, sort = TRUE) |> 
  print(n = Inf)

flights |> 
  group_by(dest) |> 
  summarise(n(), avg_arr_delay = mean(arr_delay, na.rm = TRUE)) |> 
  arrange(desc(avg_arr_delay))

flights |> 
  group_by(dest) |> 
  summarise(num_of_carrires = n_distinct(carrier)) |> 
  arrange(desc(num_of_carrires))

# count() has wt argument to specify the weights of counted records
# this allows using count() to compute the sum of values()
# namely, the following line
flights |> count(tailnum, wt = distance, sort = TRUE)
# is equivalent to
flights |> 
  group_by(tailnum) |> 
  summarise(total_dist = sum(distance)) |> 
  arrange(desc(total_dist))

# recall that sum() and mean() can be combined worth logical vectors to count, respectively, the number or the proportion of records that satisfy certain conditions
# don't forget to use na.rm() as well as check the proportion of NAs in data
flights |> 
  group_by(dest) |> 
  summarise(
    total_flights = n(),
    num_cancelled = sum(is.na(dep_time)),
    num_delayed = sum(dep_delay > 0 | arr_delay > 0, na.rm = TRUE),
    prop_delayed = 100 * mean(dep_delay > 0 | arr_delay > 0, na.rm = TRUE)) |> 
  arrange(desc(prop_delayed))

# exercises 13.3.1
# TODO

# note the difference between min() that ranges over a dataframe and pmin() that ranges over a single row; "p" in "pmin" stands for "parallel"

df <- tribble(
  ~x, ~y,
  1,   7,
  -2, -4,
  11,  2,
  1,  NA,
)

min(df, na.rm = TRUE) # -4
pmin(df$x, df$y, na.rm = TRUE) # c(1, -4, 2, 1)

# hence, normally during data analysis, one uses not
df |> 
  mutate(
    min_x = min(x, na.rm = TRUE),
    min_y = min(y, na.rm = TRUE)
  )
# but rather
df |> 
  mutate(
    min_x = pmin(x, na.rm = TRUE),
    min_y = pmin(y, na.rm = TRUE)
  )
# alternatively, to find the minimal value in two columns, provide two vectors as arguments
df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

# modular arithmetic requires two operators
# %/% - integer division
# %%  - the remainder

flights |> 
  mutate(sched_dep_hour = sched_dep_time %/% 100,
         sched_dep_minute = sched_dep_time %% 100) |> 
  select(flight, sched_dep_hour, sched_dep_minute)

flights |>
  # transform & compute
  mutate(sched_dep_hour = sched_dep_time %/% 100,
         sched_dep_minute = sched_dep_time %% 100
         ) |> 
  group_by(sched_dep_hour) |> 
  summarise(
    n = n(),
    prop_cancelled = mean(is.na(dep_time))
  ) |> 
  filter(sched_dep_hour > 1) |> 
  # then plot
  ggplot(aes(x = sched_dep_hour, y = prop_cancelled)) +
    geom_line(colour = "grey50") +
    geom_point(aes(size = n))

# more compactly, mutate and group_by can be merged into one line
flights |>
  # transform & compute
  group_by(sched_dep_hour = sched_dep_time %/% 100) |> 
  summarise(
    n = n(),
    prop_cancelled = mean(is.na(dep_time))
  ) |> 
  filter(sched_dep_hour > 1) |> 
  # then plot
  ggplot(aes(x = sched_dep_hour, y = prop_cancelled)) +
  geom_line(colour = "grey50") +
  geom_point(aes(size = n))


# rounding numbers --------------------------------------------------------
# a few hints on rounding in R
# (i)   R utilises the rule "round half to even", that is x.5 is rounded to x, if x is even, and to x + 1, if x is odd; the strategy is useful to keep rounding of big datasets unbiased(half of the times the rounding reduces the recorded value, half of the times it increases the recorded value)
# (ii)  base R round() has digits argument to specify precision, viz. 10^(-digits); however, digits can be negative, to round to tens, thousands etc.
# (iii) floor() and ceiling() do not have digits argument, yet precision can be specified by first dividing and then multiplying it by the required precision
# (iv)  round() can be used to round to any fraction (or whole number) that is not power of 10, by using the same trick as in (iii)

# (i)
round(c(1.51, 1.5, 1.49, 2.51, 3.51)) # c(2, 2, 3, 1, 4)

# (ii)
round(1233, digits = -2) # 1200
round(1233, digits = -3) # 1000
round(1500, digits = -3) # 2000

# (iii)
floor(0.24) # 0
floor(0.24 / 0.1) * 0.1 # 0.2

# (iv)
round(900 / 33) * 33 # 27 * 33 = 891
round(908 / 33) * 33 # 28 * 33 = 924


# binning vectors ---------------------------------------------------------
# cut() is used to count the number of observations within a particular range of values, that it to bin the observations
bins <- cut(
  x = round(runif(100, 0, 100)),
  breaks = c(0, 5, 10, 100)
  )

is.factor(bins) # TRUE
table(bins)

# cut can take custom labels
bins <- cut(
  x = round(runif(100, 0, 100)),
  breaks = c(0, 5, 10, 100),
  labels = c("small", "medium", "large")
)
table(bins)

# cf. other arguments of cut()


# cumulative, or rolling, aggregates --------------------------------------
# base R has the following functions to compute running totals
# (i)   cumsum()
# (ii)  cumprod()
# (iii) cummin()
# (iv)  cummax()
# dplyr introduces another one
# (v)   cummean()
#
# to do more complex computations with sliding windows, however, use install.packages("slider")

x <- 1:1000

cumulative_x <- tibble(
  x,
  sum = cumsum(x),
  prod = cumprod(x),
  min = cummin(x),
  max = cummax(x),
  mean = cummean(x)
)

ggplot(cumulative_x, aes(x = x)) +
  #geom_line(aes(y = sum), colour = "green") +
  #geom_line(aes(y = prod), colour = "red") +
  geom_line(aes(y = min), colour = "blue") +
  geom_line(aes(y = max), colour = "black") +
  geom_line(aes(y = mean), colour = "purple")

# exercises 13.4.8
# TODO

# general transformations -------------------------------------------------
# ranking can be done in multiple ways
df <- tibble(x = c(0, 1, 2, 2, 3, 5, 7, 7, 3, 10, NA))

df |> 
  mutate(
    row_number = row_number(x),
    min_rank = min_rank(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )

# NB: row_number() can be used without arguments to count the number of rows in a dataframe passed to pipe

# dplyr::lead(n) and dplyr::lag(n) apply a sliding window with the offset of n to a vector
x <- round(runif(10, 0, 5))

# x'[i] = x[i + 3]
dplyr::lead(x, n = 3)

# x'[i] = x[i - 3]
dplyr::lag(x, n = 3)

# to check if the values at two successive positions differ, use x == dplyr::lag(x)
x == dplyr::lag(x)
x - dplyr::lag(x)

# additionally, the function can be used to break a set of observations into subsets
events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)

events <- events |> 
  mutate(
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5,
    session = cumsum(has_gap)
  )

events

# another useful function is dplyr::successive_id() that increases the counter every time the iterated value changes
df <- tibble(
  x = letters[round(runif(n = 10, min = 1, max = 3))],
  y = runif(n = 10)
)

df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1) |> 
  select(id, x, y)

# exercises 13.5.4
# TODO

# exercises 13.5.4
# TODO