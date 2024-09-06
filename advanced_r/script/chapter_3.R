# this file contains the worked examples and solutions to the problems from Chapter 3 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/vectors-chap.html

# type coercion rules are
# character > double (derives from numeric) > integer (derives from numeric) > logical
# or, an extended variant:
# expression > list > character > complex > double > integer > logical > NULL
# since logical is the most basic datatype, NA is a logical value
# so R runs on ternary logic
# (different from that of SQL, viz. some binary operations with NA as an operand return a non-NA result)
typeof(NA)
typeof(NA_integer_)
typeof(NA_real_)
typeof(NA_character_)

TRUE & NA # NA
FALSE & NA # FALSE

TRUE | NA # TRUE
FALSE | NA # NA

NA | NA # NA
NA & NA # NA

NA + 1 # NA
NA * 1 # NA
1 / NA # NA
sqrt(NA) # NA
NA ^ NA # NA
NA ^ 0 # 1

typeof(c(TRUE, 1L)) # integer
typeof(c(TRUE, 1)) # double
typeof(c(TRUE, "1")) # character
typeof(c(1L, 1)) # double
typeof(c(1L, "1")) # character
typeof(c(1, "1")) # character

c(1, FALSE) # c(1, 0)
c("a", 1) # c("a", "1")
c(TRUE, 1L) # c(1L, 1L)

-1 < FALSE # the right-hand side argument is coerced to numeric and the expression is evaluated to TRUE
1 < "two" # the left-hand side argument is coerced to character and the expression is evaluated to TRUE
"one" < 2 # the right-hand side argument is coerced to character and th expression is evaluated to FALSE


# for explicit type coercion use as.numeric() etc.
# explicit type coercion can also cast character vectors into other types
as.logical(c(1, 0, 1L, 2L, 0.0001, "0", "falSE", "sdfsdf", "TRUE", TRUE))
as.numeric(c("1.1", "0.32423", "1e32", "Inf", "boo"))

# two datatypes that are seldom used: complex and raw
# 1. complex (for complex numbers)
z <- complex(real = stats::rnorm(100), imaginary = stats::rnorm(100))

# 2. raw (for binary data, supports bitwise operations and shifts)
raw_vector <- raw(16)
for (i in seq_along(raw_vector))
  raw_vector[[i]] <- as.raw(i)
raw_vector

# TODO
# is.atomic(), is.numeric(), and is.vector()

# attributes are key-value pairs of arbitrary type that could be added to any vector
# most operations ignore attributes with the exception of the default attributes 'dim' (for 'dimensions') and 'names'
x <- 1:5

attr(x, "a1") <- "abcdef"
attr(x, "a2") <- 6:10
str(x) # vector with attributes
str(attributes(x)) # list of 2

# alternatively, attributes can be set via a constructor call
y <- structure(
  1:5, 
  a1 = "abcdef",
  a2 = 6:10
)

# a reserved attribute name is "comment"
structure(1:5, comment = "unlike other attiributes, this one is not printed by default")
structure(1:5, attribute_key = "attribute value")

x == y # rep(TRUE, times = 5)
for (i in seq_along(min(length(attributes(x)), length(attributes(y)))))
  print(attributes(x)[[i]] == attributes(y)[[i]])

# "names" attribute gives names to the vector entries, some names may be missing or duplicated
x <- c(a = 1, b = 2, c = 3)
x
attributes(x)

y <- 1:3
names(y) <- c("a", "b", "b")
y

z <- setNames(1:4, c("a", "b", "c"))
z

zz <- 1:3
names(zz) # NULL

# with "dim" attribute specified, a vector can be considered as a matrix or an array, although all these are different datatypes
x <- c(1:10)
x
dim(x) <- c(2,5) # 2*5 matrix
x
dim(x) <- c(5, 2) # 5*2 matrix
x
typeof(x) # integer
y <- matrix(1:10, nrow = 5, ncol = 2)
y == x # matrix(TRUE, nrow = 5, ncol = 2)
typeof(y) # integer
str(x)
str(y)

# the difference between vectors and matrices or arrays pertains to one-dimensional structures
# namely, vectors are zero-dimensional while one-dimensional matrices are row- or column-vectors proper
# the same is true for vectors vs one-dimensional arrays
str(1:3) # 1d vector, dim == NULL
str(matrix(1:3, ncol = 1)) # column vector, dim == c(3, 1)
str(matrix(1:3, nrow = 1)) # row vector, dim == c(1, 3)
str(array(1:3, 3)) # "array" vector, i.e. 1d array with dim == 3
str(matrix(1:6, nrow = 2, ncol = 3)) # dim = c(2, 3)
str(array(1:6, dim = c(2, 3))) # "array" matrix, i.e. 2d array, dim == c(2, 3)

# note that NROW and NCOL returns the "dimensions" of a 0d vector without casting it into 1d vector,
# while nrow and ncol returns NULL
# by default, a 0d vector is cast into a 1d column-vector
NROW(1:3)
NCOL(1:3)
nrow(1:3)
ncol(1:3)
# casting can be performed explicitly by calling cbind()
dim(cbind(1:3))

# note the difference between the following arrays
x1 <- array(1:5, c(1, 1, 5)) # an array of scalars with 5 entries
x2 <- array(1:5, c(1, 5, 1)) # an array of 5*1 row-vectors with one entry
x3 <- array(1:5, c(5, 1, 1)) # an array of 1*5 column-vectors with one entry

# "class" attribute turns an object into an S3 class
# "factor" is an S3 class available in base R
# it derives from integer and extends it with the attribute "levels"
x <- factor(c("a", "b", "b", "b", "a", "b", "a"), levels = c("a", "b", "c"))
x
typeof(x)
levels(x)
attributes(x)

table(x) # tabulates all the specified levels, i.e. "a", "b", "c", although "c" is not present in the factor vector
table(c("a", "b", "b", "b", "a", "b", "a")) # only tabulates the values present in the character vector, i.e. "a" and "c"

# factors can be ordered
x <- factor(c("a", "b", "b", "b", "a", "b", "a", "d", "e"), ordered = TRUE, levels = c("f", "e", "d", "c", "b", "a"))
x
# table is another S3 class to store N-dimensional contingency tables
t <- table(x)
typeof(t) # integer
dim(t) # 6
attributes(t) # "dim", "dimnames", "class"

# when the levels of a factor are changed, the factor is copied
cat(tracemem(x), "\n")
levels(x) <- rev(levels(x))
untracemem(x)

f1 <- factor(letters)
f1[1] == "a" # TRUE
levels(f1)[1] == "a" # TRUE
levels(f1) <- rev(levels(f1)) # reverses both the entries and the levels
f1[1] == "z" # TRUE
levels(f1)[1] == "z" # TRUE

f2 <- rev(factor(letters)) # reverses the entries only
f2[1] == "z" # TRUE
levels(f2)[1] == "a" # TRUE
f3 <- factor(letters, levels = rev(letters)) # reverses the levels only
f3
f3[1] == "a" # TRUE
levels(f3)[1] == "z" # TRUE
# by comparison of factors the entries - and not the levels - are compared
f2 == f3 # rep(FALSE, times = 26)
f2 == f1 # rep(FALSE, times = 26)
f3 == f1 # rep(TRUE, times = 26), as.integer(f3) == c(26:1)
as.integer(f3) == as.integer(f1) # rep(FALSE, times = 26), c(26:1) != c(1:26)


# NOTE
# avoid using implicit coercion of character strings into factors, e.g. by read.csv() and data.frame()
# that is, set stringsAsFactors = FALSE
# (1) factors derive from integer, not character
# (2) the functions like read.csv() cannot "know" the full list of categorical variables nor their order and is limited to the strings present in the data
#     that is, the levels and their ordering is a theoretical (modelling) property and not an empirical (recorded in the data) one

# Date is another S3 class available in base R; it derives from double
today <- Sys.time()
today
typeof(today) # double
attributes(today) # class, tzone
# date is UNIX-style
unclass(as.Date("1970-02-01")) # 31 days
as.date(31) # 1970-Feb-01

# POSIXct and POSIXlt are datetime formats
today <- as.POSIXct(x = "2024-05-20 13:01:00", tz = "Europe/Minsk")
today
typeof(today)
attributes(today)
unclass(today) # the number of seconds since Unix epoch
structure(today, tzone = "UTC") # change the values of "tzone" attribute

# difftime is a type for datetime differences
# which inherits from double and extends it with "units" attribute
week_1 <- as.difftime(1, units = "weeks")
week_1
typeof(week_1) # double
attributes(week_1) # "class", "units"

week_2 <- as.difftime(7, units = "days")
week_2
typeof(week_2) # double
attributes(week_2)

week_1 == week_2 # TRUE

# list stores references to entities (vectors) of any type
# unlike vectors which are atomic, lists are recursive, i.e. lists can store other lists
l1 <- list(
  1:3,
  list(4:6),
  c("7", "8", "9")
)

typeof(l1)
str(l1)

# c() silently coerces vectors to lists (of atomic values) if there is a list among its arguments
vec_c <- c(c(1, 2, 3), 4:6, c(7:10))
typeof(vec_c) # double
list_c <- c(c(1, 2, 3), 4:6, list(7:10))
typeof(list_c)
length(list_c) # 7

list_cc <- c(list(1:3), list(4:6))
length(list_cc) # 2

# unlist() turns a list back into a vector
typeof(unlist(list_c)) # double vector of length 10
unlist(list_c) == 1:10 # rep(TRUE, times = 10)

vec_d <- as.vector(list_c, mode = "list")
typeof(vec_d) # list

date <- as.Date("2024-05-20")
datetime <- as.POSIXct(x = "2024-05-20 13:01:00", tz = "Europe/Minsk")

date_vec <- c(date, datetime)
date_vec # 2 dates

datetime_vec <- unlist(list(date, datetime))
datetime_vec # 2 integers

# dataframes are lists of vectors of equal length
# hence, they have a rectangular shape cols * rows
# where columns represent variables and rows represent observations
df1 <- data.frame(x = letters[1:3], c(1:3))
attributes(df1)
typeof(df1) # list

# it is recommended to suppress the default conversion of character vectors to factors when creating a dataframe
df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

df1 <- data.frame(row.names = c("FIRST", "SECOND", "THIRD"))
str(df1) # 3 observations of 0 variables

df1 <- data.frame()
str(df1) # 0 observations of 0 variables

df1 <- data.frame(xyz = NULL)
str(df1) # 0 observations of 0 variables


# although the number of rows must be the same in each column,
# a dataframe will fill in the values of a shorter column by recycling the given rows a whole number of times
# if the necessary number of rows is greater than the number of given rows by a non-integer factor, there will be an error
df2 <- data.frame(
  x = 1:6,
  y = c("a", "b") # y = rep(c("a", "b"), times = 3)
)

df3 <- data.frame(
  x = 1:5,
  y = c("a", "b")
) # error, arguments imply differing number of rows: 5, 2

# unlike matrices, dataframes are not transposable, because observations (rows) and variables (columns) are not interchangeable
df3 <- data.frame(
  x = 1:5,
  y = 6:10
)
df3
typeof(df3) # list

# when transposed, a dataframe is silently coerced to a matrix
df3 <- t(df3)
df3
typeof(df3) # integer
df3 <- t(df3)
df3

# if the original columns have different datatypes, typecasting will be performed
df3 <- data.frame(
  x = 1:10,
  y = rep("STRING", times = 10)
)
df3 <- t(df3)

df3 <- data.frame(
  x = 1:10,
  y = rep("STRING", times = 10)
)

# a dataframe can be cast into a matrix implicitly via as.matrix()
df3_mat <- as.matrix(df3)
typeof(df3_mat) # character

# a dataframe can be cast into a numeric matrix via data.matrix()
data.matrix(df3)
typeof(data.matrix(df3)) # integer

df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blond", "brown", "black")
#  row.names = c("Bob", "Stan", "Susan") # row names can be specified in a constructor
)

row.names(df3) <- c("Bob", "Stan", "Susan") # row names can be specified later
row.names(df3) <- c("Bob", "Stan", "Bob") # error, duplicate 'row.names' are not allowed
df3[c("Bob", "Stan"), ]

# subsetting, i.e. indexing, columns of dataframes is not straightforward
# it is recommended to use the most explicit way, viz df[["varname"]]
df3[["age"]] # ok
df3[["a"]] # NULL
df3$age # ok
df3$a # returns the first variable (column) which names starts with "a"

# since dataframes are lists of vectors, a particular column can be a list, an array or a matrix
# when specifying a list column in a constructor, it must be placed in a wrapper I() to suppress type coercion
df4 <- data.frame(
  id = 1:4,
  grades = I(list(1:3, 4:6, 7:10, 11:25))
)

# alternatively, a list column can be added after construction as any other type of column
df4$marks <- list(6:10, 10:2, 3:12, 9:12)
df4$names <- c("Joe", "Jill", "Kent", "Kate")

# tibble is a wrapper over a dataframe
library("tibble")

tib1 <- tibble(
  x = 1:3,
  y = c("a", "b", "c"),
)
typeof(tib1) # list
attributes(tib1)

# unlike dataframes, tibbles do not converse strings to factors implicitly
typeof(tib1$y) # character

# another difference from dataframes is that tibbles only recycle vectors of length one
tib2 <-tibble(
  x = 1:10,
  y = 1:2
) # error, only values of size one a recycled

tib2 <- tibble(
  x = 1:10,
  y = 999
) # ok

tib2$y # rep(999, times = 10)

# the previously specified attributes in a constructor can be referenced by the later ones
tib2 <- tibble(
  x = 1:10,
  y = 1 / x,
  z = 10 * y
)

# tibble inherrits from dataframe, hence
is.data.frame(tib2) # TRUE
is.data.frame(data.frame(1:10)) # TRUE

is_tibble(tib2) # TRUE
is_tibble(data.frame(1:10)) # FALSE

# conversion from dataframe into tibble and vice versa is possible with
# as.data.frame() and as_tibble()

# NULL is a special datatype to represent empty vector
typeof(NULL) # NULL
length(NULL) # 0
str(NULL) # NULL
attributes(NULL) # NULL
is.null(NULL) # TRUE
is.null(c()) # TRUE
is.null(0) # FALSE
is.null(FALSE) # FALSE
