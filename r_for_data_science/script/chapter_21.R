# this file contains worked examples and solutions to the problems from Chapter 21 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/databases

library(DBI)
library(dbplyr)
library(tidyverse)

# DBI::duckdb is an in-memory database for R
# the interface for working with it is exactly the same as for other SQL DBMSs

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

# create sample tables in the DB
dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

# it is possible to import data into DB right from .csv files
# Cf. duckdb_read_csv(), duckdb_register_arrow()

# list tables in the current DB
dbListTables(con)

# read a table into a dataframe
con |> 
  dbReadTable("diamonds") |> 
  as_tibble()

# optionally, with a query
df <- dbGetQuery(con, "select * from diamonds where price > 1e4")
is.data.frame(df)
is_tibble(df)

df <- as_tibble(df)

# note that dplyr is a fronted for working with data in R
# it supports multiple non-standard backends, among which are
# (i)   dbplyr - for working with data stored in DBs
# (ii)  dtplyr - for working with data stored in data.table instead of data.frame; the former have indexes and are thus much faster
# (iii) multidplyr - for running the code on multiple cores

# dbplyr translates R code into SQL
diamonds_db <- tbl(con, "diamonds")
diamonds_db_filtered <- diamonds_db |> 
  filter(price > 15000 & carat > 0.4) |> 
  select(carat:clarity, price)

show_query(diamonds_db_filtered)

# note that the number of rows is unspecified; dbplyr only executes the code when necessary, i.e. is performs lazy evaluation
diamonds_db_filtered

# to execute the query and store its result in a dataframe, call collect()
diamonds_df <- collect(diamonds_db_filtered)

# cf. https://dm.cynkra.com/articles/dm.html 
# dm:: is a package that can be usful for working with complex relational schemas, e.g. visualising the relationship between tables

# exercises 21.5.10
# TODO

