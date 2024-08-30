# this file contains worked examples and solutions to the problems from section 3 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-transform

library(tidyverse)
library(nycflights13)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarise(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    sd = sd(arr_delay, na.rm = TRUE),
    max = max(arr_delay, na.rm = TRUE),
  )

# basic functions for
# r o w s
# (i)   filter()
# (ii)  arrange()
# (iii) distinct()
# (iv)  count()

# filter() is frequently used with %in%, viz. to specify a filtered range of some variable
# thus
flights |>
  filter(day == 1 | day == 2 | day == 3)
# is equivalent to
flights |>
  filter(day %in% 1:3)

# arrange() sorts data in ascending order by default
flights |>
  arrange(month, desc(arr_delay))

# distinct() can be called with columns specified or not
flights |>
  distinct(year, month) |>
  arrange(month)
# .keep_all signals that unspecified columns must be left in the output
flights |>
  distinct(year, month, .keep_all = TRUE) |>
  arrange(month)

# count() counts the number of rows instead
flights |>
  count(month, origin, dest, sort = TRUE)

# exercises 3.2.5
# TODO

# basic functions for
# c o l u m n s
# (i)   mutate()
# (ii)  select()
# (iii) rename()
# (iv)  relocate()

# mutate() is used to create a computed column
flights |>
  mutate(
    gain_min = dep_delay - arr_delay,
    speed_mph = distance / air_time * 60.,
    .before = 1)
# while .before and .after arguments specify the position of the computed columns,
# .keep argument specifies the columns to be left in the output: all, used, unused, none
flights |>
  mutate(
    gain_min = dep_delay - arr_delay,
    .keep = "used")

# select() is used to specify the columns of the output
# remember to use R's powerful subsetting
flights |>
  select(where(is.character) | where(is.numeric.Date))

flights |>
  select(1:3)

flights |>
  select(!year:day)

# rename() is used to rename a few columns while keeping all in the output
# Cf. janitor()::clean_names()
flights |>
  rename(Y = year, M = month, D = day)

# relocate() is used to change the order of columns in the output
flights |>
    relocate(starts_with("arr"), .before = dep_time)

flights |>
  rename(Y = year, M = month, D = day) |>
  relocate(D, M, Y, .before = dep_time)


# exercises 3.3.5
# TODO

# other basic functions are
# (i)   group_by()
# (ii)  sumamrise()
# (iii) slice_*()

# group_by() groups the rows by a specified variable
# it is normally followed by summarise()
flights |> 
  group_by(month) |> 
  summarise(
    n = n(),
    avg_delay_min = mean(dep_delay, na.rm = TRUE),
    sd_min = sd(dep_delay, na.rm = TRUE)
  )

# slice_*() allows selecting a number or a fraction of rows ...
# (i)    slice_head()   - from the beggining of the dataframe
# (ii)   slice_tail()   - from the end of the dataframe
# (iii)  slice_min()    - with minimal value(s), specify with_ties = FALSE if necessary
# (iv)   slice_max()    - with maximal value(s), specify with_ties = FALSE if necessary
# (v)    slice_sample() - randomly
flights |>
  group_by(dest) |> 
  summarise(
    max_arr_delay_min = max(arr_delay),
    max_dep_delay_min = max(dep_delay))

# is similar to the following code
# but slice_max() keeps all columns in the output, can keep ties, and allows specifying N rows
flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 1, with_ties = FALSE) |> 
  relocate(dest)

flights |> 
  group_by(dest) |> 
  slice_max(arr_delay, n = 3, with_ties = TRUE) |> 
  relocate(dest)

# note that the last grouping column is ignored
daily <- flights |> 
  group_by(year, month, day) |> 
  summarise(n = n())


# exercises 3.5.7
# TODO

# never forget to include n() when working with summary statistics
install.packages("Lahman")
library(Lahman)

batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarise(
    n = sum(AB),
    perfomance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE)
  ) |> 
  arrange(desc(n))

batters |> 
  filter(n>100) |> 
  ggplot(aes(x = n, y = perfomance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)

