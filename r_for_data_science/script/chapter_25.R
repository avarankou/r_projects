# this file contains worked examples and solutions to the problems from Chapter 25 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/functions

library(tidyverse)
library(nycflights13)

# in the following, three "types", or common use-cases, of funcitons are considered
# (i)   vector functions
# (ii)  dataframe functions
# (iii) plot funcitons


# vector functions --------------------------------------------------------
# are functions with vector arguments and, optionally, a vector return value

rescale <- function(vec) {
  rng <- range(vec, na.rm = TRUE, finite = TRUE)
  
  out <- (vec - rng[1]) / (rng[2] - rng[1])
}

z_score <- function(vec) {
  mean <- mean(vec, na.rm = TRUE)
  sd <- sd(vec, na.rm = TRUE)
  
  out <- (vec - mean) / sd
}

df <- data.frame(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5)
)

df |> 
  mutate(across(a:d, rescale)) |> 
  print()

df |> 
  mutate(across(a:d, z_score)) |> 
  summarise(
    mean(a), sd(a),
    mean(b), sd(b),
    mean(c), sd(c),
    mean(d), sd(d)) |> 
  print()

# exercises 25.2.5
# TODO

# dataframe functions -----------------------------------------------------
# are functions with a dataframe argument and a dataframe return value

# tidyverse functions use so-called data-masking and tidy-selection
# data-masking allows referring to dataframe variables without escaping their names what is useful for data analysis
# that is, a function's argument name is interpreted as the dataframe's column name
# cf. https://rlang.r-lib.org/reference/topic-data-mask.html

# when writing functions straight ahead it results in a problem
group_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by(group_var) |> 
    summarise(mean_value = mean(mean_var))
}

diamonds |> group_mean(cut, carat)

# here is what is happening
df <- tibble(
  mean_var = 1,
  group_var = "g",
  group = 1,
  x = 10,
  y = 100
)

# note that the dataframe is grouped by df$group_var and not by df$group, whatever the latter is
df |> group_mean(group, x)
df |> group_mean(group, y)

# this is due to data-masking of arguments in summarise()
# to state that a token is an argument's name, as opposed to a dataframe's column name, embrace this token in {{}} in the function's body

group_mean_fixed <- function(df, group_var, mean_var) {
  df |> 
    group_by( {{ group_var }} ) |> 
    summarise(
      mean_value = mean( {{ mean_var }} ),
      .groups = "drop")
}

df |> group_mean_fixed(group, x)
df |> group_mean_fixed(group, y)

diamonds |> group_mean_fixed(cut, price)

# however, it is impossible to provide a vector of arguments in this way
diamonds |> group_mean_fixed(c(cut, color), price)

# use pick() instead
group_mean_pick <- function(df, group_vars, mean_var) {
  df |> 
    group_by( pick({{ group_vars }}) ) |> 
    summarise(
      mean_value = mean( {{ mean_var }} ),
      .groups = "drop")
}

diamonds |> group_mean_pick(c(cut, color), price)

# another example of using pick()
count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}

diamonds |> count_wide(c(clarity, color), cut)

# is equivalent to
diamonds |> 
  count(clarity, color, cut) |> 
  pivot_wider(
    names_from = cut, 
    values_from = n,
    names_sort = TRUE,
    values_fill = 0
  )

# due to data-masking in summarise(), the following function can be used with (i) group_by() verb or (ii) a transformed column as input
stat_sum <- function(df, var_name) {
  df |> 
    summarise(
      min = min( {{ var_name }} ),
      mean = mean( {{ var_name }} ),
      max = max( {{ var_name }} ),
      sd = sd( {{ var_name }} ),
      cv = sd( {{ var_name }}) / mean( {{ var_name }} ),
      n = n(),
      na_values = sum(is.na( {{ var_name }})),
      .groups = "drop"
      )
}

diamonds |> stat_sum(carat)

diamonds |> 
  group_by(cut, color) |> 
  stat_sum(log10(carat)) |> 
  print()
  

count_prop <- function(df, var_name, sort = FALSE) {
  df |> 
    count( {{ var_name }}, sort = sort) |> 
    mutate(pror = n / sum(n))
}

diamonds |> 
  count_prop(cut)


# exercises 25.3.5
# TODO


# plot functions ----------------------------------------------------------
# are functions that create and return a plot

# ggplot2::aes() is a data-masking function, thus passing the variables' names must be embraced in {{}} to suppress their default interpretation as the dataframe's columns

lin_reference_lines <- function(df, x, y) {
  df |> 
    ggplot(aes(x = {{ x }}, y = {{ y }} )) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, colour = "red", se = FALSE) +
    geom_smooth(method = "loess", formula = y ~ x, colour = "blue", se = FALSE)
}

mtcars |> 
  lin_reference_lines(cyl, mpg)

# note a non-standard operator ':=' (aka the "walrus operator")
# it is necessary to assign a value to the user-supplied variable name (which cannot be done with base R '=' operator)
sorted_bars <- function(df, var) {
  df |> 
    mutate( {{ var }} := fct_rev(fct_infreq( {{ var }})) ) |> 
    ggplot(aes(y = {{ var}} )) +
    geom_bar()
}

diamonds |> sorted_bars(clarity)

# cf. rlang:: a low-level package of the tidyverse
# e.g., rlang::englue() works both as a template system for strings as well as for variable names
histogram <- function(df, var, binwidth) {
  label = "A histogram of {{var}} with binwidth {binwidth}"
  
  df |> 
    ggplot(aes(x = {{ var }} )) +
    geom_histogram( binwidth = {{ binwidth }} ) +
    labs(title = label)
}

diamonds |> histogram(carat, 0.1)

# exercises 25.4.4
# TODO

# exercises 25.5.1
# TODO
