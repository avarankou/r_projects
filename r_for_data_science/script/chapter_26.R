# this file contains worked examples and solutions to the problems from Chapter 26 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/iteration

library(tidyverse)


# modifying multiple columns with dplyr::across() -------------------------
# dplyr::across() is used to apply the same function to several columns of a dataframe
df <- tibble(
  grp = sample(2, 5, replace = TRUE),
  a = runif(5),
  b = runif(5),
  c = runif(5),
  d = runif(5),
  e = letters[14:18]
)

df |>
  summarise(
    n = n(),
    across(.cols = a:d, .fns = median)
  )

df |> 
  group_by(grp) |> 
  summarise(
    n = n(),
    across(.cols = everything(), .fns = max)
  )


# .cols argument can be used with functions like everything(), where(is.numeric), where(!is.Date) etc. as well as with starts_with() or end_with()
df |> 
  group_by(grp) |> 
  summarise(
    n = n(),
    across(.cols = where(is.numeric), .fns = max)
  )

rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  out <- rnorm(n - n_na, mean = mean, sd = sd)
  out <- c(out, rep(NA, n_na))
  out <- out[sample(n, replace = FALSE)]
}

df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)

# recall that many functions, e.g. max(), propagate NAs
# they normally have na.rm or a similar argument which, however, cannot be passed to across() directly
df_miss |> 
  summarise(
    n = n(),
    across(everything(), max)
  )

# to pass arguments to across it is necessary to use a helper function which can be anonymous
median_excl_na <- function(x) {
  median(x, na.rm = TRUE)
}

df_miss |> 
  summarise(
    n = n(),
    across(where(is.numeric), median_excl_na)
  )

# alternatively, with an anonymous function
# base R syntax for anonymous functions: \(arg1...) func_call(arg1)
# (deprecated) old tidyverse syntax for anonymous functions: ~ func_call(.arg1)

# note that across() can take a list of functions as an argument and that the resulting column names can be specified with .names which uses glue:: syntax
df_miss |> 
  summarise(
    across(
      where(is.numeric),
      list(
        median = \(x) median(x, na.rm = TRUE),
        n_nas  = \(x) sum(is.na(x))
      ),
      .names = "{.fn}_{.col}"
  )
)

# note also that by default across() uses the modified columns' names
# hence, when across() is used inside mutate(), it overrides the mutated columns
df_miss |> 
  mutate(across(where(is.numeric), \(x) coalesce(x, 0)))

# to suppress this behaviour, use .names argument
df_miss |> 
  mutate(
    across(
      where(is.numeric),
      \(x) coalesce(x, 0),
      .names = "{.col}_no_nas"
      )
    ) |> 
  select(starts_with("a"))

# consider the following use case
df_miss |> 
  mutate(nas = is.na(a) | is.na(b) | is.na(c) | is.na(d)) |> 
  filter(!nas) |> 
  select(!nas)

# to make the code succinct - as well as benefit from other features of across() - use dplyr::if_any() and dplyr::if_all()
df_miss |> 
  filter(if_all(where(is.numeric), \(x) !is.na(x) ))

# across() is a powerful and flexible programming tool
# consider the following example
expand_dates <- function(df) {
  df |> 
    mutate(
      across(where(is.Date), list(year = year, month = month, day = mday))
    )
}

df_date <- tibble(
  name = c("Amy", "Bob"),
  date = ymd(c("2009-08-03", "2010-01-16"))
)

df_date |> 
  expand_dates()

# and another one
coef_var <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

summarise_stats <- function(df, summary_vars = where(is.numeric)) {
  df |> 
    summarise(
      across(
        .col = {{ summary_vars }},
        .fns = list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd   = \(x) sd(x, na.rm = TRUE),
          coef_var = coef_var
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}

diamonds |> 
  group_by(color) |> 
  summarise_stats()

# it is easy to note, that across() resembles pivoting
# and, in some cases, the goal can only be achieved by sequentially pivoting and modifying the dataframe in hand
# e.g., consider this dataframe of 4 pairs of variables, each of which contains values and weights of some observations
# although the pattern of columns is "simple", it requires computing with two columns which cannot be currently done with across()
df_paired <- tibble(
  a_val = rnorm(10),
  a_wts = runif(10),
  b_val = rnorm(10),
  b_wts = runif(10),
  c_val = rnorm(10),
  c_wts = runif(10),
  d_val = rnorm(10),
  d_wts = runif(10)
)

# with pivoting, however, the task is straightforward
df_paired |> 
  pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_") |> 
  group_by(variable) |> 
  summarise(wmean = weighted.mean(x = val, w = wts))

# exercises 26.2.8
# TODO

# reading multiple files --------------------------------------------------
# purr::map() applies a specified function to each element of a specified vector and returns a list (since lists in R can store elements of different datatypes)
map(list.files(path = "dir", pattern = "[.]xlsx$"), readxl::read_excel)



# an anonymous function can be used to provide arguments to the map() function argument and the pipe opeartor can be used to simplify the code
df <- list.files(path = "dir", pattern = "[.]xlsx$") |> 
  map(\(path) readxl::read_excel(path, n_max = 1)) |>
  list_rbind()

# more complex cases can be handled with set_names() and basename() functions
# cf. separate_wider_delim()
# cf. install.packages("crew")
paths <= list.file(path = "dir", pattern = "[.]xlsx$")

df <- paths |> 
  set_names(basename) |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  mutate(year = parse_number(year))

# working with purr::map(), think functional
# that is, rather then writing a procedure and iterating over files
# map all files to a dataframe and process the latter

# thus, avoid
process_file <- function(path) {
  df <- read_csv(path)
  
  df |> 
    filter(!is.na(id)) |> 
    mutate(id = tolower(id)) |> 
    pivot_longer(jan:dec, names_to = "month")
}

paths |> 
  map(process_file) |> 
  list_rbind()

# and prefer
paths |> 
  map(read_csv) |> 
  list_rbind() |> 
  filter(!is.na(id)) |> 
  mutate(id = tolower(id)) |> 
  pivot_longer(jan:dec, names_to = "month")

# to explore why some file is not loaded properly, it is useful to check the datatypes entry-wise
df_types <- function(df) {
  tibble(
    col_name = names(df), 
    col_type = map_chr(df, vctrs::vec_ptype_full),
    n_miss = map_int(df, \(x) sum(is.na(x)))
  )
}

files |> 
  map(df_types) |> 
  list_rbind(names_to = "file_name") |> 
  select(-n_miss) |> 
  pivot_wider(names_from = col_name, values_from = col_type)

# cf. map_if() and map_at() to select the elements of list to map by their values and names, respectively

# purr::possibly() is a function for replacing errors with a specified value
possibly(readxl::read_excel(path), NULL)

# writing files -----------------------------------------------------------
# writing to databases

# DBI::dbCreateTable() creates a table based on the column names and types of a specified dataframe
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbCreateTable(con, "diamonds", diamonds)

# DBI::dbAppendTable() appends data to an existing table
DBI::dbAppendTable(con, "diamonds", diamonds[diamonds$price > 1e4, ])

con |> 
  tbl("diamonds") |> 
  filter(cut == "Fair") |> 
  arrange(desc(price))

# it is, obviously, possible to do this with purr::map() or purr::walk(), the latter of which does not return a value

# writing to .csv files
# group_nest() splits a dataframe into a dataframe of its subsets by some grouping variable(s)
NCOL(diamonds)

# TODO
# 26.4.2, 26.4.3
