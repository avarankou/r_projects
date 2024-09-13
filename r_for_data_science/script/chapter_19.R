# this file contains worked examples and solutions to the problems from Chapter 19 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/joins

library(tidyverse)
library(nycflights13)

# if some variable of a dataframe is likely to be a primary key, then check
# (i)  that its values in the dataframe are indeed unique
# (ii)  there are no NAs among its values in the dataframe

planes |> 
  count(tailnum) |> 
  filter(n > 1)

planes |> 
  count(is.na(tailnum) == TRUE)

weather |> 
  count(origin, time_hour) |> 
  filter(n > 1)

weather |> 
  count((is.na(origin) | is.na(time_hour)) > 0)

# conversely, if some variable's values (or a tuple) have neither duplicates nor NAs, then it can be short-listed as a candxate for being a primary key; however, whether some variable is a key or not is a theoretical, or modelling, property, hence, inferring it form data alone is impossible
airports |> 
  count(alt, lat) |> 
  filter(n > 1)

# it might be simpler to create an index column and use it as primary key; such a column is definetely useful for communication
flights2 <- flights |> 
  mutate(x = row_number(), .before = 1)

# exercises 19.2.4
# TODO


# equi joins --------------------------------------------------------------
# building on usual relational algebra, dplyr:: has several types of join operators
#
# m u t a t i n g   j o i n s (used to add new columns to a dataframe)
# (i)   inner_join()
# (ii)  left_join()
# (iii) right_join()
# (iv)  full_join()
# f i l t e r i n g   j o i n s (used to filter the rows of a dataframe)
# (v)   anti_join()
# (vi)  semi_join()

# note that dplyr:: automatically finds the columns to join on and does not duplicate them in the output
# obviously, it functions as natural join, i.e. joins on all columns with matching names

# set keep argument to TRUE to preserve the key columns of both dataframes
flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  left_join(airlines)

flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  left_join(planes |> select(tailnum, type, engines, seats)) |> 
  filter(is.na(type) | is.na(engines) | is.na(seats))

# the columns to join on can be specified explicitly with join_by argument
flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  left_join(planes, join_by(tailnum))

flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  left_join(airports, join_by(origin == faa))

# semi_join() filters the rows of a dataframe that have a match in the joined one
airports |> 
  distinct(faa) |> 
  count()

# thus, only 3 or 101 of 1458 airports have, respectively, a departure or an arrival recorded in flights dataframe
airports |> 
  semi_join(flights2, join_by(faa == dest)) |> 
  count()

airports |> 
  semi_join(flights2, join_by(faa == origin)) |> 
  count()

# conversely, anti_join() filters the rows of a dataframe that have no match in the joined one

# thus, 4 airports appear as destinations in flights dataframe but are not listed in airports, while all origin airports are there
flights2 |> 
  select(x, dest) |> 
  anti_join(airports, join_by(dest == faa)) |> 
  group_by(dest) |> 
  count()

flights2 |> 
  select(x, origin) |> 
  anti_join(airports, join_by(origin == faa)) |> 
  group_by(origin) |> 
  count()

# similarly, 722 planes have a record in flights but not in planes
flights2 |> 
  select(x, year, time_hour, origin, dest, tailnum, carrier) |> 
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

# exercises 19.3.4
# TODO

# recall that a join results in multiplication of certain rows of a datarame when they have several matching entries in the joined dataframe
# dplyr:: allows specifying the relationship between dataframes and gives a warning or an error on such occasions
df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))

# the default behaviour is a warning
df1 |> 
  inner_join(df2, join_by(key))

# if the relationship is M:M, then such situations are expected and thus permitted
df1 |> 
  inner_join(df2, join_by(key), relationship = "many-to-many")

# if the relationship is 1:1, then such situations are not expected and thus join throws an error
df1 |> 
  inner_join(df2, join_by(key), relationship = "one-to-many")


# non-equi joins ----------------------------------------------------------
# whenever a join is done by a condition other than the equality of keys, it is a non-equi join
# more specifically, the following types are most common:
# (i)   cross joins, i.e. Cartesian products
# (ii)  inequality joins, i.e. joins on conditions of (strict) inequality
# (iii) rolling joins, i.e. inequality joins that only keep the closest match
# (iv)  overlap joins, i.e. inequality joins designed for ranges

# note that the Cartesian product of x and y is basically a set of all permutations of x and y
# thus, taking a several cross joins in a row, one generates the set of all permutations of several variables
df <- tibble(x = 1:5)
df |> cross_join(df) |> cross_join(df)

# turning to the strict inequality join of x and y, one gets the set of all combinations of the variables
df |> inner_join(df, join_by(x < x)) |> inner_join(df, join_by(x.y < x)) |> print(n = Inf)

# rolling joins can be expressed as inequality joins with subsequent filtering
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)

set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)

employees |> 
  inner_join(parties, join_by(closest(birthday <= party))) |> 
  arrange(name)

# or, with filtering
employees |> 
  inner_join(parties, join_by(birthday <= party)) |>
  group_by(name, birthday) |> 
  summarise(min(party)) |> 
  arrange(name)

# overlap joins, i.e. joins with join_by argument set to between() or overlaps() serve as shortcut for specifying intervals

# another useful argument is unmatched, which is used to signal about entries with no matches
employees |> 
  inner_join(parties, join_by(closest(birthday <= party)), unmatched = "error") |> 
  arrange(name)

# alternatively, one can check if there are rows with no matches with an anti join
# note that anti_join() can be a "rolling join" as well
employees |> 
  anti_join(parties, join_by(closest(birthday <= party)))

# exercises 19.5.4
# TODO

