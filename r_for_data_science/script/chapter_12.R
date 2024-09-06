# this file contains worked examples and solutions to the problems from Chapter 12 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/logicals

library(tidyverse)
library(nycflights13)

# using filter() is equivalent to using mutate() for intermediate computations
flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

flights |> 
  mutate(dep_during_daytime = dep_time > 600 & dep_time < 2000,
         arr_approx_in_time = abs(dep_delay) < 20,
         .keep = "used") |> 
  filter(dep_during_daytime == TRUE & arr_approx_in_time == TRUE)

# like in many programming languages, the floating-point arithmetic in R gives approximate results; that is there are rounding errors, albeit tiny
x <- c(1 / 49 * 49, sqrt(7)^2)
x == c(1, 7)
print(x, digits = 16)

# rather than check if two numbers are equal, compare their absolute difference to a small epsilon, viz. the tolerable rounding error
abs(x - c(1, 7)) <= c(1e-8, 1e-8)
# dplyr::near() encapsulates the comparison
dplyr::near(x, c(1, 7))

# recall that R has a ternary logic, use is.na() to check for NAs
# it is useful to put NAs at the top of a studied table
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

# exercises 12.2.4
# TODO

# to avoid writing long sequences of logical conditions pertaining to one variable, use value %in% range

# note that %in% makes a proper check for NAs
# thus the below code
flights |>
  filter(dep_time %in% c(NA, 0800)) |> 
  count()
# is equivalent to
flights |> 
  filter(dep_time == 0800 | is.na(dep_time)) |> 
  count()

flights |> 
  group_by(year, month, day) |> 
  summarise(
    total_in_time = all(dep_delay == 0, na.rm = TRUE),
    total_short_delay = all(dep_delay > 0 & dep_delay <= 30, na.rm = TRUE),
    any_long_delay = any(dep_delay > 300, na.rm = TRUE),
    .groups = "drop"
  )

# a numerical summary can be easily given by applying sum() and mean() to logical vectors
flights |> 
  group_by(year, month, day) |> 
  summarise(
    pct_in_time = 100 * mean(dep_delay == 0, na.rm = TRUE),
    pct_short_delay = 100 * mean(dep_delay > 0 & dep_delay <= 30, na.rm = TRUE),
    num_long_delay = sum(dep_delay > 300, na.rm = TRUE),
    .groups = "drop"
  )

# combined with R's versatile subsetting, an informative summary is a matter of a few lines of code
flights |> 
  group_by(year, month, day) |> 
  summarise(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# exercises 12.4.4
# TODO

# recall base R vectorised ifelse()
# dplyr::if_else() extends the base feature by adding an option for NAs
x <- c(-5:3, NA, 4:5)
y <- 1:3

# thus
ifelse(x < 0, -(x^2), x^2)
# is equivalent to
dplyr::if_else(x < 0, -(x^2), x^2)

# however, the latter has the fourth argument to handle NAs
dplyr::if_else(x < 0, -(y^2), y^2, 0)

# dplyr::if_else() also ensures that the vector arguments all have the same length, while base R silently recycles them
y <- 1:3

# thus, while the following line works
ifelse(x < 0, -(y^2), y^2)
# its dplyr analogue fails with an error
dplyr::if_else(x < 0, -(y^2), y^2)

# dplyr::case_when() is straightforward but always use is.na() and .default cases
dplyr::case_when(
  x == 0   ~ "0",
  x < 0    ~ "neg",
  x > 0    ~ "pos",
  is.na(x) ~ "na"
)

# only the first matching condition is used
dplyr::case_when(
  x == 0   ~ "0",
  x < 0    ~ "neg",
  x > 0    ~ "pos",
  x > 2    ~ ">2",
  is.na(x) ~ "na"
)

# there is no fall-through behaviour like in C or SQL and base R switch()
dplyr::case_when(
  x == 0   ~ "0",
  x < 0    ~ "neg",
  x > 0,
  x > 2    ~ ">2",
  is.na(x) ~ "na"
)

switch("c",
       a = "A",
       b = "B",
       c = ,
       d = "D",
       stop("Invalid `x` value!"))

# another handy feature of dplyr::if_else() and dplyr::case_when is that both are strict with regard to implicit type coercion; only the following pairs of types are compatible:
# (i)   logical and numeric
# (ii)  factor and string
# (iii) date and datetime
# (iv)  NA and any other type

# exercises 12.5.4
# TODO
