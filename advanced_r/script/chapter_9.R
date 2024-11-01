# this file contains worked examples and solutions to the problems from Chapter 9 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/functionals.html

library(rlang)
library(purrr)
library(ggplot2)

# funcitonals are functions that take another function as input and output a vector or a value
# functionals often step in instead of loops in order to make the solution to a routine problem, e.g. that of iterating over some datastructure, reusable and tailored to the goal at hand
# thus, base R apply() functions are functionals


# purrrr:map() ------------------------------------------------------------
# purrr:map() is similar to lapply()
# the input function is applied to every element of the input vector, the result is returned as a list

simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  
  return(out)
}


cube <- function(x) { return(x^3) }

map(1:10, cube)
lapply(1:10, cube)
simple_map(1:10, cube)

# there are versions of map() that return a vector of the specified type, viz. map_chr(), map_lgl(), map_int(), map_dbl()

# there are overloaded versions of map() to subset the input vector by name or index
x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# subset by name
map_dbl(x, "x") # 1 4 8

# subset by index
map_dbl(x, 1) #-1 -2 -3

# subset by both
map_dbl(x, list("y", 1)) # 2 5 9

# map() also allows passing extra arguments to f() which, furthermore, can be named
# this is quite different from passing an function with extra arguments
plus <- function(x, y) x + y
map_dbl(1:10, plus) # error

map_dbl(1:10, plus, y = runif(1)) # the same argument is passed to every call of plus() in map_dbl()
map_dbl(1:10, ~ plus(.x, runif(1))) # map_dbl() is invoked with modified f argument which is now an anonymous wrapper over plus()


# exercises 9.2.6 ----------------------------------------------------------
# 9.2.6.1
# first, consider rlang::as_function() which converts a formula object into an anonymous function
f <- as_function(~ .x + 1)
f(10) # 11
# purrr:as_mapper() does a similar thing but allows passing more than two arguments to a specified funcion
f2 <- as_mapper(~ .x + 1)
f2(10)

# 9.2.6.2
map(1:3, ~ runif(2)) # .f is an anonymous function that returns a random vector of length 2
map(1:3, runif(2))  # .f is not a function at all, consider the below code; map treats it as an index to subset

f <- runif(2)
is.function(f) # FALSE
map(1:3, f)

# 9.2.6.3
# (a) compute the sd of every column of a numeric df
df <- data.frame(
  x = 1:10,
  y = c(rep(5, 5), 6:10),
  z = runif(10)
)

map_dbl(df, function(x) sd(x))
map_dbl(df, ~ sd(.x))

# (b) compute the sd of every numeric column of a mixed df
# one way
df <- data.frame(
  w = letters[1:10],
  x = 1:10,
  y = c(rep(5, 5), 6:10),
  z = runif(10)
)

f <- function(x) {
  if (is.numeric(x)) {
    return(sd(x))
  }
  else return(x)
    
}

map(df, f)

# another way
idx <- map_lgl(df, is.numeric)
map_dbl(df[idx], sd)

# (c) compute the number of levels of every factor
idx <- map_lgl(diamonds, is.factor)
map_int(diamonds[idx], nlevels)

# 9.2.6.4
trials <- map(1:100, ~t.test(rpois(10, 10), rpois(7, 10)))
tibble::tibble(
  val = map_dbl(trials, "p.value")
) |>
  ggplot(mapping = aes(x = val)) +
  geom_histogram()

# 9.2.6.5
x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, ~ map(.x = .x, .f = triple))

# 9.2.6.6
# fit a linear regression model for each formula in the list
# see ?I
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# for a single formula, the task is straightforward
mdl <- lm(formulas[[1]], mtcars)
mdl_summary <- summary(mdl)
mdl_summary$r.squared

# so it is for a list of formulas with map_dbl()
map_dbl(formulas, ~ (summary(lm(.x, mtcars)))$r.squared)

# 9.2.6.7
# fit a model to bootstrap samples
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

map_dbl(bootstraps, ~ (summary(lm(formulas[[1]], .x)))$r.squared)


# purrr style -------------------------------------------------------------
# compare a C-style solution to its functional counter-part
by_cyl <- split(mtcars, mtcars$cyl)
slopes <- double(length(by_cyl))

for (i in seq_along(slopes)) {
  mdl <- lm(mpg ~ wt, data = by_cyl[[i]])
  slopes[[i]] <- coef(mdl)[[2]]
}

slopes

# a functional one
split(mtcars, mtcars$cyl) |> 
  map(~ lm(mpg ~ wt, data = .x)) |> 
  map(coef) |> 
  map_dbl(2)


# map variants ------------------------------------------------------------
# apart from map_lgl() and the like, there are 5 other families of map functions
# (i)   modify()
# (ii) walk()
# (iii)  imap()
# (iv)  map2()
# (v)   pwalk()

# (i) modify() returns the object of the same type with its input; note that modify() creates a copy and d o e s  n o t  modify in place
df <- data.frame(
  x = rpois(10, 1),
  y = rpois(10, 1)
)

md <- modify(df, sqrt)
mp <- map(df, sqrt)

is.data.frame(md) # TRUE
is.list(md) # TRUE

is.data.frame(mp) # FALSE
is.list(mp) # FALSE

# modify(), just as map(), has a variant that accepts a predicate formula wrapped in a function; this allows modifying objects iff they satisfy the specified condition
df <- data.frame(
  x = rpois(10, 1),
  y = rpois(10, 1),
  z = letters(1:10)
)

modify(df, sqrt) # error: non-numeric column z is passed to a numeric function sqrt()
modify_if(df, is.numeric, sqrt) # ok

# (ii) walk() and walk2() return "invisible" output and are thus useful for iteratively calling the functions like print() and write.csv()  which are called for their side-effect (rather than output)
temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)

# (iii) imap() iterates the input by indexes and not by elements, like 2) and 3) in the list below
# recall that there ate three ways of calling for
# 1) for(x in xs) to iterate over elements of xs
# 2) for(x in seq_along(x)) to iterate over the indeces 1..lenth(x)
# 3) for(nm in names(x)) to iterate over xs by the elements' names

# the first argument, accessed with .x, is the object's value and the second, accessed with .y, is its name
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))

x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The first value of ", .y, " is ", max(.x)))

# (iv) map2() has two vectorised arguments rather than one
# to appreciate the difference, consider computing a weighed mean with map()
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

map(xs, mean, na.rm = TRUE)
map(xs, weighted.mean, na.rm = TRUE, w = ws) # error
map(xs, weighted.mean, na.rm = TRUE, w = ws[[1]]) # ok, but the same weights are passed to each call of weighted.mean()

# one possible workaround is to tweak the data structure, e.g. to pass a list of two values to map and a modified version of weighed.mean()
# this, however is ad hoc
pairs  <- map(1:8, ~ list(x = runif(10), w = runif(10)))
map(pairs, ~ weighted.mean(x = .x$x, w = .x$w), na.rm = TRUE)

# on the contrary, map2 is general and succint
map2(xs, ws, weighted.mean, na.rm = TRUE)

# note that there are map2_dbl(), map2_lgl() etc.

# (v) pmap() is a "template" version map2 which passes an arbitrary number of parameters to the mapping function
# thus, the code below is similar to a call to map2()
pmap(list(xs, ws), weighted.mean, na.rm = TRUE)


# exercises 9.4.6 ---------------------------------------------------------

# 9.4.6.1
# in this case, argument f is not a function but an index
# the result is a dataframe with the first observation copied to each row of the output dataframe
modify(mtcars, 1)
mtcars[1, ]

# 9.4.6.2
cyls <- split(mtcars, mtcars$cyl)
temp <- tempfile()
dir.create(temp)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

# alternatively, with iwalk() instead of walk2()
savefile <- function(x, name, dir) {
  filename <- file.path(temp, paste0("cyl-", name, ".csv"))
  write.csv(x, filename)
}

iwalk(cyls, savefile, temp)

# 9.4.6.3
# note that the list items are named after the target dataframe's columns
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
# modify the target columns by calling f(var) for each f in trans and var in mtcars
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))

# alternatively, with map()
# note, that in this case mtcars, is accessed from each function, whereas with map2() above only the modified column passed to the transformation functin
mtcars[nm] <- map(nm, ~ trans[[.x]](mtcars[[.x]]))

# 9.4.6.4
# write.csv returns NULL
tf <- tempfile(fileext = ".csv")
str(withVisible(write.csv(mtcars, tf)))


# reduce family -----------------------------------------------------------
# map() is coupled with reduce()
# while the former iteratively applies a function to a list of inputs and returns a list of the same length with the input one, the latter iteratively applies some aggregate function to a list of inputs and returns a single value which could have been also obtained by applying the given aggregate function to the input concatenated into a single vector
# obviously, the benefit of turning loops into map-reduce calls is that the latter can be easily shared among several processing units

# thus, similarly to map(), a naive implementation of reduce() is nothing but a wrapper over a for-loop
simple_reduce <- function(x, f, ...) {
  if (is.null(x) || length(x) == 0) {
    stop("Invalid input! `x` must have a length more than 1", call. = FALSE)
  }

  aggr <- x[[1]]
  for (i in seq(2, length(x), by = 1)) {
    aggr <- f(x[[i]], aggr, ...)
  }
  
  return(aggr)
}

l <- map(1:4, ~ sample(1:10, 15, replace = TRUE))
manual_reduce <- (l[[1]] |> 
  intersect(l[[2]]) |> 
  intersect(l[[3]]) |> 
  intersect(l[[4]])
)

reduce(l, intersect) == manual_reduce

# observe that base sum(x) can be thought of as reduce(x, `+`), as indeed any composite function of transitive relation can be
sum(1:321) == reduce(321:1, `+`)

# accumulate() is a version of reduce() that returns the results of all intermediate steps along the main one; it is thus analogous to cumulative sum
accumulate(1:10, `*`)

# in the wild, reduce() must be supplied with .init argument which sets the first aggregate value and allows checking that the input type is valid
reduce("a", `+`) # ok, since `+` is never called
reduce(letters[1:10], `+`) # error: a non-numeric argument passed to a numeric function
reduce("a", `+`, .init = 0) # error: a non-numeric argument passed to a numeric function; that is so because this time `+` is called for (.init, x[[1]]) pair of values

# reduce2() is used with aggregate functions which take two arguments
# note that, if no initial value is supplied as .init argument, .f is called length(.x) - 1 times
# hence, argument .y contains one element less than .x
# vice versa, if .init is there, .x and .y have the same length as .f is called length(.x) times


# 9.6 predicate functionals -----------------------------------------------
# for the sake of code's brevity and to make its lexical structure closer to that of natural languages, purrr:: provides a wrappers around common vectorised logical operators
# purr::some(.x, .p), every(), none() are similar to any(.p(.x)), all(.p(.x)) and all(negate(.p)(.x))
# purr::detect(.x, .p) and detect_index() are similar to match()
# purr::keep(.x, .p) and discard() are similar to logical subsetting
# however, the purrr:: variants work like maps and are thus applicable not only to vectors but to lists (dataframes) as well

df <- data.frame(x = 1:3, y = c("a", "b", "c"))
detect(df, is.factor) # NULL
detect_index(df, is.factor) # 0

str(keep(df, is.factor)) # 'data.frame':    3 obs. of  0 variables
str(discard(df, is.factor)) # 'data.frame':    3 obs. of  2 variables

# thus the following two lines do the same comparison
purrr::some(1:5, ~ .x %% 2 == 0)
any(1:5 %% 2 == 0)

# the following line, however, is not expressible with base R any() without explicitly iterating over l
l <- list(1:5, 25:35)
purrr::some(l, ~ some(.x, ~ .x %% 2 == 0))


# exercises 9.6.3 ---------------------------------------------------------

# 9.6.3.1
# recall that predicate functionals return a single logical value when applied to a vector, like is.character(), is.null() or all() do
all(1:5) # TRUE
# since is.na is vectorised, i.e. returns a vector, it is not a predicate functional
is.na(1:5) # rep(FALSE, 5)
any(is.na(1:5)) # FALSE

# 9.6.3.2
simple_reduce_init <- function(x, f, init, ...) {
  if (is.null(x) || length(x) == 0) {
    error("Invalid input! `x` must have a length more than 1")
  }
  aggr <- init
  for (i in seq_along(x)) {
    aggr <- f(x[[i]], aggr, ...)
  }
  
  return(aggr)
}

simple_reduce(c(), `+`)
simple_reduce_init(1:10, `+`, 125)

# 9.6.3.3
# span(x, f) returns the location of the longest sequential run of elements where the predicate is true

run_counter <- function(a, b) {
  ifelse(a & b, a + 1, int(b))
}

span <- function(.x, .p) {
  lgl <- map_lgl(.x, .p)
  runs <- accumulate(lgl, run_counter, .init = 0)
  
  if (any(runs > 0)) {
    return( which.max(runs) - max(runs) + 1 )
  }
  else return(0)
}

x = c(1:3, rep(2, 4), 6:10, rep(7, 7))

span(x, ~ .x %% 2 == 0)
span(x, ~ .x %% 2 == 1)
span(x, ~ .x %% 48 == 0)

# 9.6.3.4
# arg_max() takes a function and a vector of inputs, and returns the elements of the input where the function returns the highest value
# e.g., arg_max(-10:5, function(x) x ^ 2) should return -10. arg_max(-5:5, function(x) x ^ 2) should return c(-5, 5)
arg_m <- function(m = "ax", x, f) {
  if (m %in% c("ax", "in") == FALSE) {
    stop("Invalid value `m` = ", m, " The value must be 'ax' or 'in'." , .call = FALSE)
  }
  
  vals <- map_dbl(x, f)
  key = ifelse(m == "ax", max(vals), min(vals))
  idx = which(vals == key)

  return( x[idx] )
}

arg_max <- function(x, f) arg_m("ax", x, f)
arg_min <- function(x, f) arg_m("in", x, f)

arg_max(-10:5, ~ .x^2)
arg_max(-5:5, ~ .x^2)

arg_min(-10:5, ~ .x^2)
arg_min(-5:5, ~ .x^2)

# 9.6.3.5
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

mod_df <- modify_if(mtcars, is.numeric, scale01)


# exercises 9.7.3 ---------------------------------------------------------
# TODO
