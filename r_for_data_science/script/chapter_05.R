# this file contains worked examples and solutions to the problems from Chapter 5 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-tidy

library(tidyverse)

# tidy datasets satisfy the structure of relations in relational algebra, viz.
# (i)   each variable is a column, each column is a variable
# (ii)  each row is an observation (a record, a tuple) and vice versa
# (iii) each cell is a single value and vice versa
#
# this is so, because the purpose of analysis is, basically, to establish functional relationships between the observed values

table <- tibble(
  country = c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
  year = c(1999, 2000, 1999, 2000, 1999, 2000),
  cases = c(745, 2666, 37737, 80488, 212258, 213766),
  population = c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583)
)

table <- table |> 
  mutate(infection_rate_per_1k = cases / population * 1000) |> 
  arrange(country, year)

ggplot(table, aes(x = year, y = infection_rate_per_1k)) +
  geom_line(aes(group = country, colour = "grey50")) +
  scale_x_continuous(breaks = c(1999, 2000))

# exercises 5.2.1
# TODO


# pivoting data -----------------------------------------------------------
# pivot_linger() is used to reshape wide data into tall

billboard <- tidyr::billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(week = parse_number(week))

billboard |> 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

# "tribble" stands for "transposed tibble"
# it facilitates manual data input, by making it row-wise
df <- tribble(
  ~id, ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  130,  135
)

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# pivot_longer() can split a single column into several ones with names_sep or names_pattern argument and a vector value of names_to argument
who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  ) |> 
  filter(is.na(count) == FALSE)

# what is more, pivot_longer() can split a column header into values with ".value" key passed to names_to argument
household |> 
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )

# pivot_wider() is used to reshape tall data into wide

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with ("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)

df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

# Cf.vignette("pivot", package = "tidyr")