# this file contains worked examples and solutions to the problems from Chapter 18 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/missing-values

library(tidyverse)


# LOCF: last observation carried forward ----------------------------------
# quite often, especially when data is entered manually, the missing values in a successive row signal that the values are the same as in the previous row
# tidyr::fill() fills the values in and also allows setting the direction of filling

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |> fill(everything(), .direction = "down")


x <- c(1, 3, 3, 7, 92, NA, 12, NA)

# dplyr::coalesce() is used to replace NAs
coalesce(x, 0)

# dplyr::na_if() is used for the inverse problem
x <- na_if(x, 0)

# recall there are also NAN, which is a numeric constant
is.numeric(NaN) # TRUE
logical(NaN) # FALSE

is.numeric(NA) # FALSE
is.logical(NA) # TRUE

# dplyr::complete() is used to fill in the missing rows or columns that appear after pivoting

# exercises 18.3.4
# TODO

# sec. 18.4
# TODO

?purrr
