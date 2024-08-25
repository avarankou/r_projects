# this file contains the worked examples and solutions to the problems from Section 2 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/subsetting.html

# there are 3 subsetting operators in R: [], [[]], $

# [] is used to select multiple objects from a vector, list, df etc.
# the possible arguments are

# (I) positive integer indexes select entries at the specified positions, ranging from 1 to length(x) inclusive
x <- seq(from = .1, to = 1., by = .1)
x[c(1, 2, 7)]
x[8:0] # returns elements in the reverse order
x[rep(1, times = 5)] # returns duplicate values
x[c(1., 1.9)] # returns the first element twice, since double indexes are truncated to integers

# (II) negative integer indexes select all entries except the ones at the specified positions
x[-5]
x[c(-1, -4, -5.5)]

x[c(1:3, -2)] # error, only 0's may be used with negative subscripts

# (III) logical indexes select the entries corresponding to TRUE values
# logical indexing can be recursive
x[x < 4]
x[x < 0.4]

# if the logical vector of indexes differs in size from the indexed vector, the shorter one is recycled
# it is recommended to avoid recycling except straightforward cases, e.g. where one of the vectors has size 1
x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE)] # by contrast with the recycling of column values of dataframes, no error here
y <- 1:3
y[c(TRUE, TRUE, FALSE, FALSE, TRUE)] # c(1, 2, NA)

# NA in index results in NA in output
y[c(TRUE, NA, TRUE)] # c(1, NA, 3)

# (IV) zero index returns an empty vector
str(x[0]) # num(0)
z <- c()
str(z) # NULL

# (V) empty indexing operator, i.e. [], returns the original vector; this is useful for multidimensional datastructures
x[]
y[]

# (VI) if the indexed vector has names, a character vector of names can be used for indexing
y <- setNames(y, letters[1:length(y)])
y[c("c", "a", "a", "b")]
# unlike $ operator that allows partial matching (matching by first letters), indexing by names with [] requires exact matching
y[c("A", "aa", "a")] # c(NA, NA, 1)

# since factors inherit from integer, the underlying integer vector is used for subsetting and not string labels
# hence, it is not recommended to use factor for subsetting
f <- factor(letters[1:3], levels = rev(letters))
f
y[f] # c(NA, NA, NA)
as.integer(f) # c(26, 25, 24)

# subsetting a list with [] always returns a list, while [[]] and $ return single entries
l1 <- list(1:10)
typeof(l1[1:2]) # list
typeof(l1[1]) # list
typeof(l1[11]) # list
l1[11] # NULL


typeof(l1[[1]]) # int
typeof(l1[[11]]) # error, subscript out of bounds

# subsetting multidimensional datastructures with [] is similar to subsetting of vectors
# in this case, [] contains a comma-separated list of vectors that specify the indexes of elements to subset in each dimension
a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ] # first two rows and all columns
a[3:2, 1] # the 3rd and the 2nd row of the 1st column

a[c(TRUE, FALSE, TRUE), c("B", "A")] # the 1st and the 3rd rows of columns "B" and "A"

a[0, -2] # no rows, all columns but the 2nd

# recall that matrices and arrays are simply vectors with the dim attribute specified; hence, one can use an integer index to subset any entry in a matrix or an array, both of which are stored in column-major order
m1 <- matrix(1:10, nrow = 2, ncol = 5)
m1[9] == m1[1,5] # TRUE

m2 <- outer(1:5, 1:5, FUN = paste, sep = ",")
m2[c(7, 24:26)] # "2,2", "4,5", "5,5", NA

# one can also use an N*2 matrix to subset N entries from a matrix, a N*3 matrix to subset N entries from a 3d array etc.
selector <- matrix(data = c(
  1,1,
  2,2,
  3,3,
  4,4,
  5,5),
  byrow = TRUE,
  ncol = 2
)

m2[selector]

# a dataframe is a list of vectors, so when subset with [] and a single index it behaves like a list
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[1:3] # first thre columns
# a dataframe is a 2d matrix of data, so when subset with [] and two indexes it behaves like a matrix
df[2,3] # "b"
df[df$x == 2,]
# hence, there are two syntactically different ways to select columns of a dataframe
# (i) by specifying column names
df[c("x", "z")]
# (ii) by specifying no row indexes and the necessary column indexes
df[, c(1, 3)]
# however, when selecting a single column, the first method does not cast a column vector into a vector
typeof(df[c("x")]) # list
str(df[c("x")])
# while the second method casts a column vector into a 0d vector implicitly
typeof(df[, 1]) # integer
str(df[, 1])

# to suppress the implicit type cast, use "drop = FALSE"
typeof(df[, 1, drop = FALSE]) # list
str(df[, 1, drop = FALSE])

# tibbles avoid this subtle difference: subsetting a tibble with [] always returns a tibble
df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])

str(df["x"]) # tibble [3 × 1] (S3: tbl_df/tbl/data.frame)
str(df[, "x"])  # tibble [3 × 1] (S3: tbl_df/tbl/data.frame)

# working with factors, one can provide a "drop" argument as well
# however, it is set to FALSE by default and it drops (or not) levels but not the dimensions
# that is said, using a factor with "drop = TRUE" is a signal that a character vector shall be used instead
fac <- factor(x = c("a", "h", "l", "b"), levels = rev(letters))
fac[2] # one entry, 26 levels
fac[2, drop = TRUE] # one entry, 1 level

# exercises 4.2.6

# 4.2.6.1
mtcars[mtcars$cyl == 4 & mtcars$gear == 4,]
mtcars[1:4, ]
mtcars[mtcars$cyl <= 5,]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]

# 4.2.6.2
x <- 1:5
x[NA] # since NA is logical, NA & 1:5 is returned
x[NA_real_] # a single integer index is specified, hence a single entry is returned

# 4.2.6.3
x <- outer(1:5, 1:5, FUN = "*")
dim(upper.tri(x))
x[upper.tri(x)] # returns the entries in the upper-triangular part of matrix as a vector
# so it can be handy to subset a matrix with a logical matrix indexes with the same size
# however, the matrix of indexes is processed as an integer vector that specifies the column-major indexes of entries of the subset matrix; hence, when the matrix of indexes has a size other than that of the subset matrix, the vector of indexes is recycled and the result is no longer easy to grasp
y <- matrix(1:49, nrow = 7, ncol = 7)
y[upper.tri(x)]

# 4.2.6.4
dim(mtcars) # 32 rows, 11 columns
mtcars[1:11] # is equivalent to mtcars[]
mtcars[1:20] # tries to select 20 columns, of which there are only 11, and fails
mtcars[1:20,] # selects the first 20 rows and all their columns

# 4.2.6.5
diag_ <- function(mat) {
  mindim = min(nrow(mat), ncol(mat))
  toprow = seq(from = 1, to = mindim * nrow(mat), by = nrow(mat))
  shift = 0:(mindim-1)
  mat[toprow + shift]
}

m1 <- matrix(1:16, nrow = 4, ncol = 4)
m2 <- matrix(1:16, nrow = 8, ncol = 2)
m3 <- matrix(1:16, nrow = 2, ncol = 8)
diag_(m1)
diag_(m2)
diag_(m3)

# 4.2.6.6
df <- data.frame(
  height = trunc(runif(25, min = 160, max = 180)),
  weight = trunc(runif(25, min = 60, max = 80)))
df[1, 2] <- NA
df
df[is.na(df)] <- 0 # replaces NAs with 0s in a dataframe
df

# [[]] is a subsetting operator that extracts a single value from the indexed datastructure
# thus, when used with a list, [[]] returns the value of the indexed entry
# by contrast, [] with a single argument returns a list with one entry
l1 <- list(1, 2, 3, 4:10)
typeof(l1[3]) # list
typeof(l1[[3]]) # double
typeof(l1[4]) # list
length(l1[4]) == 1 # TRUE
length(l1[[4]]) == 7 # TRUE
l1[[4]] == 4:10 # rep(TRUE, 7)

# for the sake of consistency and ease of use, it is better to subset single entries from vectors with [[]] as well, although the result in this case is equivalent to that of []
v1 <- 1:25
v1[2] == v1[[2]] # TRUE
v1[2:3] == v1[[2:3]] # error in v1[[2:3]] : attempt to select more than one element in vectorIndex, i.e. [[]] only works with a single index

# [[]] can also be used with a string argument to subset a named column
mtcars[["cyl"]]
index = "cyl"
mtcars[[index]]

# $ is a shorthand operator for [["index"]] that is only used to subset a named column from a dataframe or a tibble
# however, unlike [["index"]], x$ind does left-to-right partial matching (with dataframes but not with tibbles)
mtcars[["mp"]] # NULL
mtcars$mp == mtcars$mpg # rep(TRUE, 32)
# to avoid confusion, set the following global setting
options(warnPartialMatchDollar = TRUE)
mtcars$cy

# recall that tibbles do not allow partial matching when subsetting columns
tib1 <- tibble(mtcars)
tib1$cy # NULL

# when trying to index an element that does not exist, [[]] can return NULL or throw an error
# to avoid this inconsistent behaviour, one can use
# (i)  purrr:pluck() that always returns NULL if the element is not found
# (ii) purrr:chuck() that always throwa an error if the element is not found
# note that neither function alows partial mathcing like $
# these functions are quite useful for nested datastructures

purrr::pluck(mtcars, "mpg", 10) == mtcars$mpg[[10]] # TRUE
is.null(purrr::pluck(mtcars, "mpggg")) # TRUE
is.null(purrr::pluck(NULL, NA_integer_, NA_integer_, 199)) # TRUE


# exercises 4.3.5

# 4.3.5.1
# (i-a..b) subsetting the third row (observation) and and the second column (variable)
mtcars[3, 2]
mtcars[[3,2]] # avoid using it
# (ii-a...f) subsetting the second variable and, then, its third element
# "cyl" can also be stored in a varable
# "cb"..."cy" can be used for partial matching
mtcars[[2]][3]
mtcars[[2]][[3]]
mtcars$cyl[3]
mtcars$cyl[[3]]
mtcars[["cyl"]][3]
mtcars[["cyl"]][[3]]
# (iii) using purrr::pluck or purrr::chuck
purrr::pluck(mtcars, 2, 3)
purrr::chuck(mtcars, 2, 3)

# 4.3.5.2
mod <- lm(mpg ~ wt, data = mtcars)
summary(mod)
mod$df.residual
summary(mod)$r.squared

# subassignment
# subassignment is subsetting followed by assignment, i.e. x[i] <- value
# to avoid the complications crated by R recycling rules, it is better to limit subassignment to the cases when length(i) == length(value)
a <- 1:20
a[2:10] <- -2:-4

b <- 1:20
b[c(2, 1:3)] <- c("string", "another string")
b

# use list_var[i] <- NULL to remove i-th node from a list
# use list_var[i] <- list(NULL) to add a node containing NULL to the specified position of a list
l1 <- list(1:3, 4:6, 7:10)
l1[3] <- list(NULL)
l1[5] <- list(NULL) # l1[4] is silently created and set to NULL

l1[3:5] <- NULL
length(l1) == 2

# recall that [] with no argument returns the data stored in a datastructure; hence, it can be combined with assignment
typeof(mtcars[]) # list
mtcars[] <- lapply(mtcars, as.integer) # cast all entries of mtcars dataframe to integers
is.data.frame(mtcars) == TRUE # TRUE
rm(mtcars)

mtcars <- lapply(mtcars, as.integer) # cast lists of mtcars to integer vectors
mtcars
str(mtcars) # list of 11
is.data.frame(mtcars) == FALSE # TRUE
rm(mtcars)

# below are some common applications of subsetting

# (i) lookup tables
lookup <- c("Male", "Female", NA)
names(lookup) <- c("m", "f", "u")
# equivalently, the names can be specified in constructor
# lookup <- c(m = "Male", f = "Female", u = NA)

data <- c("f", "m", "f", "f", "u", "u", "m", "f")
lookup[data]
# use unname() to remove the header with names
unname(lookup[data])

# (ii) integer lookup tables
lookup <- data.frame(
  grade = 5:1,
  mark = letters[5:1],
  description = c("Excellent", "Good", "Passed", "Bad", "Untolerable"),
  passed = c(TRUE, TRUE, TRUE, FALSE, FALSE))

data <- sample(1:5, size = 10, replace = TRUE)

index = match(data, lookup$grade)
lookup[index, ]

# (iii) random sampling
index = sample(1:length(mtcars), 5, replace = FALSE)
mtcars[index, ]

# (iv) ordering
vec <- sample(1:10, 10, replace = FALSE)
vec
order(vec) # returns the order in which to pick elements of vec in the ascending order
vec <- vec[order(vec)]

# order() is useful for ordering a dataframe by some column
df <- data.frame(
  z = sample(1:20, 20, replace = TRUE),
  y = sample(1:20, 20, replace = TRUE),
  x = sample(1:20, 20, replace = TRUE),
  row.names = letters[1:20]
)

df[order(df$z),] # order df by z in the ascending order
df[order(rownames(df)),] # order df by rownames in the ascending order
df[, order(colnames(df))] # order df's columns in the ascending order in output

# sort() can be use to sort vectors and factors in-place
sort(1:25, decreasing = TRUE) # 25:1

# (v) expanding aggregated counts
# in some datasets, the rows with same values are aggregated into one, while their number before aggregation is specified in "count" column (unique rows have 1 in this column)
# it might be necessary to expand them back into distinct rows and thus restore the original datastructure
df <- data.frame(
  x = sample(20:25, size = 10, replace = TRUE),
  y = sample(35:75, size = 10, replace = FALSE),
  count = sample(1:3, size = 10, replace = TRUE)
)

# recall that rep(value, times) is vectorised; i.e. when value and times are vectors, v[i] is repeated times[i] times
rep(letters[1:10], df$count)

# hence, to expand the aggregated rows, use
df[rep(1:nrow(df), df$count),]

# (vi) removing or skipping columns
mtcopy <- mtcars
mtcopy$cyl <- NULL # remove a column
mtcopy[c("mpg", "hp", "carb")] # display necessary columns
mtcopy[setdiff(names(mtcopy), c("mpg", "hp", "carb"))] # display all columns but the unnecessary ones

# (vii) logical subsetting, or conditional selection of rows
# recall that !, & and | are vectorised operators, i.e. work with vectors element-wise, while && and || are usual binary Boolean operators
# hence, use & and | for complex conditions when substetting rows from a dataframe
mtcars[mtcars$cyl > 4 & mtcars$mpg < 15,]

# always simplify complex conditions with De Morgan's rules
# !(X & Y) <-> !X | !Y
# !(X | Y) <-> !X & !Y

# (viii) Boolean algebra and set operations
# since subsetting can be done with either integer or logical indexes, it is worthwhile to recall that they are synonymous
vec <- sample(10, replace = FALSE) > 7
# which(x) returns the indexes of TRUE entries in the logical vector x
which(vec)

# its inverse function would be
unwhich <- function(x, len) {
  out <- rep_len(FALSE, len)
  out[x] <- TRUE
  out
}

x <- 1:15 %% 2 == 0
y <- 1:15 %% 3 == 0

# (i) & is equivalent to intersection
which(x & y) == intersect(which(x), which(y))

# (ii) | is equivalent to union
sort(which(x | y)) == sort(union(which(x), which(y)))

# (iii) xor is equivalent to union minus intersection
sort(which(xor(x, y))) == sort(setdiff(union(which(x), which(y)), intersect(which(x), which(y))))

# (iv) a & !b is equivalent to set difference
which(x & !y) == setdiff(which(x), which(y))

# recall that logical datatype has three possible values, viz. TRUE, FALSE, NA
# when a logical vector contains NAs, which() ignores this entries
# hence it is better to check for NAs explicitly
z <- sample(10) > 2
z[c(1, 2, 12)] <- NA

length(intersect(which(z), which(is.na(z)))) == 0 # TRUE

# because of this, !x and -which(x) are not equivalent
# !x and -which(x) also diverge when x is all FALSE
# while !x will be all TRUE, -integer(0) will be integer(0)

# exercises 4.5.9

# 4.5.9.1
# base R sample(x) is also used for random permutations of a given vector x
colindex = sample(1:ncol(mtcars))
df[, colindex]
# alternatively, column names can be used
colindex = sample(colnames(mtcars))
df[, colindex]

# hence, permuting both rows and columns is easy
rowindex = sample(1:nrow(mtcars))
colindex = sample(1:ncol(mtcars))
df[rowindex, colindex]

# 4.5.9.2
# sampling from a dataframe is a straightforward use case of sample()
rowindex = sample(1:runif(1, min = 1, max = nrow(mtcars)))
mtcars[rowindex,]

# a contiguous sample can be obtained in a similar manner
rowindex = sort(runif(2, min = 1, max = nrow(mtcars)))
mtcars[rowindex[[1]]:rowindex[[2]],]

# 4.5.9.3
# sorting column names is, basically, sorting a vector
colindex = sort(colnames(mtcars))
mtcars[colindex]