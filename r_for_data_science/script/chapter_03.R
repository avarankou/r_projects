# this file contains worked examples and solutions to the problems from Chapter 3 of
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


# basic functions for r o w s  --------------------------------------------
# (i)   filter() is equivalent to sigma in relational algebra
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


# exercises 3.2.5 ---------------------------------------------------------

# 3.2.5.1
flights_no_na <- flights |> 
  filter(if_all(where(is.numeric), \(x) !is.na(x) )) |> 
  mutate(
    dep_dtime = lubridate::make_datetime(year = year, month = month, day = day, hour = dep_time %/% 100, min = dep_time %% 100),
    arr_dtime = lubridate::make_datetime(year = year, month = month, day = day, hour = arr_time %/% 100, min = arr_time %% 100),
    sched_dep_dtime = lubridate::make_datetime(year = year, month = month, day = day, hour = sched_dep_time %/% 100, min = sched_dep_time %% 100),
    sched_arr_dtime = lubridate::make_datetime(year = year, month = month, day = day, hour = sched_arr_time %/% 100, min = sched_arr_time %% 100)) |> 
  select(!c(year, month, day, hour, minute, dep_time, arr_time, time_hour, sched_dep_time, sched_arr_time))

flights_no_na |> 
  filter(arr_delay >= 2 * 60)

flights_no_na |> 
  filter(dest %in% c("IAH", "HOU"))

flights_no_na |> 
  filter(carrier %in% c("UA", "DL", "AM"))

flights_no_na |> 
  filter(month >= 6 & month <= 8)

flights_no_na |> 
  filter(arr_delay > 2 * 60 & dep_delay <= 0)

flights_no_na |> 
  filter(
    arr_delay >= 1 * 60 &
    air_time - (sched_arr_dtime - sched_dep_dtime) > 30)


# 3.2.5.2
flights_no_na |> 
  arrange(desc(dep_delay)) |> 
  mutate(n = row_number()) |> 
  filter(n <= 10)

# or, better
flights_no_na |> 
  slice_max(dep_delay, n = 10)

flights_no_na |> 
  arrange(dep_dtime) |> 
  mutate(n = row_number()) |> 
  filter(n <= 10)

flights_no_na |>
  slice_min(dep_dtime, n = 10)

# 3.2.5.3
tmp <- flights_no_na |> 
  mutate(speed_mph = 60. * distance / air_time ) |> 
  slice_max(speed_mph, n = 10, with_ties = FALSE) |> 
  select(flight, tailnum, origin, dest, distance, air_time, speed_mph)

# 3.2.5.4
flights_in_2013 <- flights |> 
  filter(year == 2013) |> 
  distinct(year, month, day) |> 
  count()

flights_in_2013 == 365 # TRUE

# 3.2.5.5
flights_no_na |>
  arrange(desc(distance))


# basic functions for c o l u m n s ---------------------------------------
# (i)   mutate()
# (ii)  select() is equivalent to pi in relational algebra
# (iii) rename() is equivalent to rho in relational algebra
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


# exercises 3.3.5 ---------------------------------------------------------

# 3.3.5.1
# the differences are due to overnight flights
flights_no_na |> 
  mutate(dep_delay_sec = int_length(interval(sched_dep_dtime, dep_dtime)),
         eq = dep_delay_sec / 60 == dep_delay,
         .keep = "used") |> 
  filter(eq == FALSE)

# 3.3.5.3
# the column selected twice appears in the output one time only
flights_no_na |> 
  select(tailnum, tailnum, ends_with("time")) |> 
  View()

# 3.3.5.4
# all_of() and any_of() are selector functions that are useful within a select() statement
# while all_of() requires that all column names specified in its argument are present in the target dataframe, any_of() allows for missing columns
variables <- c("year", "month", "day", "dep_delay", "arr_delay", "air_time_min")

flights |> 
  select(all_of(variables))

flights |> 
  select(any_of(variables))

# 3.3.5.5
flights |> 
  select(contains("TIME", ignore.case = FALSE))

# 3.3.5.6
flights_no_na |> 
  rename(air_time_min = air_time) |> 
  select(air_time_min, !air_time_min)

# 3.3.5.7
flights_no_na |> 
  select(tailnum, arr_delay) |> 
  arrange(desc(arr_delay))

# other basic functions of dplyr:: ----------------------------------------

# (i)   group_by() is equivalent to gamma in relational algebra
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
# (i)    slice_head()   - from the begining of the dataframe
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



# exercises 3.5.7 -------------------------------------------------------

# 3.5.7.1

# 3.5.7.2
flights_no_na |> 
  group_by(dest) |> 
  slice_max(dep_delay, n = 1, with_ties = TRUE)

# 3.5.7.3

# 3.5.7.4
# with a negative value of n argument, slice_* sorts the tibble by the sliced variable but preserves all rows
flights_no_na |> 
  slice_min(dep_delay, n = -10)

flights_no_na |> 
  slice_max(dep_delay, n = -10)

# 3.5.7.5
flights_no_na |> 
  count()

flights_no_na |> 
  mutate(rowid = n()) |> 
  summarise(count = max(rowid))

# 3.5.7.6
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

# note that group_by() does not change the way tibble looks like, although it adds grouping to it
df |> 
  group_by(y)

# to see the effect of grouping, other verbs must be added
df |> 
  group_by(y) |> 
  summarise(mean_x = mean(x))

df |> 
  group_by(y) |> 
  distinct(z)

df |> arrange(y)

dg <- df |> 
  group_by(y, z) |> 
  summarise(mean_x = mean(x))
group_vars(dg)

# the output looks the same way, however the original dataframe cannot be restored any longer as the grouping metadata is removed
dg <- df |> 
  group_by(y, z) |> 
  summarise(mean_x = mean(x), .groups = "drop")
group_vars(dg)

df |> 
  group_by(y, z) |> 
  summarise(mean_x = mean(x))

# note that mutate() verb is not affected by the grouping
df |>
  group_by(y, z) |> 
  mutate(mean_x = mean(x))


# summary -----------------------------------------------------------------
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

