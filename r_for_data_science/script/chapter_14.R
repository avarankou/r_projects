# this file contains worked examples and solutions to the problems from Chapter 14 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/strings

library(tidyverse)
library(babynames)

# like C++, R supports raw strings
raw_string <- r"(some string without escape \ sequences " ' / / \ but with special charactes)"

#prints escapes as well as puts the string in quotes
print(raw_string)

# prints the string as it is normally displayed
str_view(raw_string)
writeLines(raw_string)

# exercises 14.2.4
# TODO

# tidyverse comes with its own utilities for string manipulations that are designed to work with dplyr::mutate()
# thus
str_c("Today is ", c("Monday", "Tuesday", "Wednesday"))
# is similar to base R paste0
paste0("Today is ", c("Monday", "Tuesday", "Wednesday"))

df <- tibble(day = c("Monday", "Tuesday", "Wednesday", "Thursday", NA))
df |> 
  mutate(
    greeting_1 = str_c("Today is ", day),
    greeting_2 = coalesce(str_c("Today is ", day), "Today is some day"),
    greeting_3 = str_c("Today is ", coalesce(day, " some day"))
  )

# str_glue() works like a template system, i.e. it simplifies the way evaluated expressions are included in strings

df <- tibble(
  day = c("Monday", "Tuesday", "Wednesday", "Thursday", NA),
  daydate = c(12, 13, 14, 15, 16),
  monthdate = c(10, 10, 10, 10, 10)
)

df |> 
  mutate(
    greeting = str_glue("Today is {day}, {daydate}.{monthdate}"))

# str_flatten() concatenates a string vector into one string, thus it is useful when combined with summarise()
df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)

df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

# exercises 14.3.4
# TODO


# separating a string into distinct values --------------------------------
# the following functions are used to separate a single string into several values
# just like pivot_longer and pivot_wider they can put the new values either in new rows or new columns
# (i)   separate_longer_delim()
# (ii)  separate_longer_position()
# (iii) separate_wider_delim()
# (iv)  separate_wider_position()
# (v)   separate_wider_regex()

# they come with a bunch of useful arguments, viz. too_few and too_many, each of which can take a number of values

df <- tibble(record = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))

df |> 
  mutate(x = record) |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )


# letter-wise functions ---------------------------------------------------
# there is a bunch of C-style functions to manipulate strings
# sometimes, it comes in handy for analysis

babynames |> 
  count(length = str_length(name), wt = n) |> 
  arrange(desc(length))

babynames |> 
  filter(str_length(name) >= 14) |> 
  count(name, wt = n, sort = TRUE)

# exercises 14.5.3
# TODO

# NB:str_equal() compares strings not by their character codes but by their appearance
