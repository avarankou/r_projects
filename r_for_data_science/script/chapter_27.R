# this file contains worked examples and solutions to the problems from Chapter 27 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/base-r

library(tidyverse)

# note the difference between base R subsetting with logical indexes and dplyr::filter()

df <- tibble(
  x = c(1, 2, 4, 3, NA),
  y = letters[1:5],
  z = runif(5)
)

# dplyr::filter() drops NAs
df |> 
  filter(x > 1)

# base R subsetting keeps NAs
df[df$x > 1, ]

# thus, to make the latter equivalent to the former, a conjunction is needed
df[df$x > 1 & !is.na(df$x), ]

# base R order() is similar to dplyr::arrange()
df[order(df$x, df$y, decreasing = TRUE), ]

df |> 
  arrange(desc(x), desc(y))

# base R subsetting with [] or subset() is similar to dplyr::select() and dplyr::relocate()
df[, c("x", "z")]

df |> select(x, z)

subset(df, x > 1, c(y, z))

df |> 
  filter(x > 1) |> 
  select(y, z)

# exercises 27.2.4
# TODO

# base R subsetting of a single column with [[]] or $ is equivalent to dplyr::pull()
max(diamonds$price)

diamonds |> pull(price) |> max()

l1 <- list(
  a = 1:5,
  b = letters[c(1:20, 24)],
  c = "some string",
  d = list(runif(5))
)

# recall that subsetting a list with [] returns a list, while subsetting it with [[]] returns the stored object
is.list(l1[1]) # TRUE
is.list(l1["a"]) # TRUE
is.list(l1[["a"]]) # FALSE
is.vector(l1[["a"]]) # TRUE

# exercises 27.2.4
# TODO

# base R *apply() functions are similar to dplyr::map() and dplyr::across(), respectively
df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)

num_cols <- sapply(df, is.numeric)
df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], \(x) x * 2)
df

tapply(diamonds$price, diamonds$cut, mean)
# is equivalent to
diamonds |> 
  group_by(cut) |> 
  summarise(mean(price))