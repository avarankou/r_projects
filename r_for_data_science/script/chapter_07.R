# this file contains worked examples and solutions to the problems from Chapter 7 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-import

library(tidyverse)


# reading csv files -------------------------------------------------------
# read_csv() allows specifying the strings that must be interpreted as NA values
students <- read_csv("https://pos.it/r4ds-students-csv", na = c("N/A", ""))

students <- students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`,
    meal_plan = mealPlan,
    age = AGE
  ) |> 
  mutate(
    meal_plan = as.factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

# read_csv() can be used with character strings and has a number of handy arguments:
# (i)   skip = N - to skip N first rows
# (ii)  col_names = FALSE - no to treat the top row as headers
# (iii) col_names = c(...) - to specify custom column names
# (iv) comment = "#" - to specify the comment symbol



# exercises 7.2.4 ---------------------------------------------------------

# 7.2.4.1
read_delim(delim = "|")

# 7.2.4.2
arg_read_csv <- names(formals("read_csv"))
arg_read_tsv <- names(formals("read_tsv"))

intersect(arg_read_csv, arg_read_tsv)
length(setdiff(arg_read_csv, arg_read_tsv)) == 0 # TRUE; all 20 arguments are the same

# 7.2.4.3
# the most important arguments are col_positions and col_types
?read_fwf


# 7.2.4.4
csv_content <- "x,y\n1,'a,b'"
df <- read_csv(csv_content, quote = "'")

# 7.2.4.5
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")

# 7.2.4.6
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)


# specifying column types -------------------------------------------------
# read_csv() can fail to parse a column, if it contains erroneous entries
# col_types argument is used to specify the column types explicitly and problems() is used to explore the type casting errors

simple_csv <- "
 x
 10
 20
 30
 .
 50"

df <- read_csv(
  simple_csv,
  col_types = list(x = col_double())
)

problems(df)

# in case of simple problems, na argument will do
df <- read_csv(
  simple_csv,
  col_types = list(x = col_double()),
  na = "."
)

# to read a list of files, a vector argument can be provided to read_csv
# bear in mind, that list.files(...) allows selecting files from a directory by pattern
sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
# id arguments adds an extra column to the output dataframe
read_csv(sales_files, id = "file")


# writing files -----------------------------------------------------------
# consider a few options of saving a dataframe
# (i)   write_csv() or write_tsv() - save it as a text file and loose datatype information
# (ii)  write_rds() and read_rds() - save & load it in R native binary format with no loss of datataype information
# (iii) arrow::write_parquet() and arrow::read_parquet() - save % load in efficent .parquet binary format independent from R