# this file contains worked examples and solutions to the problems from Chapter 13 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/s3.html

library(sloop)
library(vctrs)

# basics ------------------------------------------------------------------
# an S3 object is a base type (integer or double vector, list, string etc.) with class attribute
# S3 objects can be used in generic functions which dispatch their arguments to the specific implementations
# thus, a factor is an S3 object inheritting from integer

fctr <- factor(rep(c("ONE", "THREE"), 3), levels = c("ONE", "TWO", "THREE"))
typeof(fctr) # integer
attributes(fctr)$levels == levels(fctr)
attributes(fctr)$class == class(fctr)

# simply by calling base R unclass(), one can transform an S3 object into a base object
unclass(fctr)

# to find out if a function is generic, sloop::ftype() can be used
sloop::ftype(print) # "S3" "generic"
sloop::ftype(max) # "primitive" "generic"
sloop::ftype(unclass) # "primitive"

# to see the source code of an S3 function, sloop::s3_get_method() is used
# printing the source in a usual way, viz. by calling its name without parenthesis, does not work for S3 objects which are not exported


# exercises 13.2.1 --------------------------------------------------------
# 13.2.1.1
sloop::ftype(t.test) # "S3" "generic"
t.test(x = runif(10, min = 0, max = 0.9), y = runif(10, min = 0.1, max = 1.0))

# most often, methods' names contain a dot which separates the name of a generic function from that of its implementation for a specific datatype
sloop::ftype(t) # "S3" "generic"
sloop::ftype(t.data.frame) # "S3" "method"
df <- data.frame(x = 1:1000, y = 1000:1)
t(df) # t.data.frame() is called inside t()

t.data.frame(df)

# note, that calling t.data.frame() directly is a bad idea as it avoids the type check
t(1:10) # ok
t.data.frame(1:10) # error

# 13.2.1.2
# t.test
# is.infinite, is.na, is.null, is.integer etc.
# as.integer, as.character etc.
# file.remove, file.create etc.

# 13.2.1.3
sloop::ftype(as.data.frame) # "S3" "generic"
sloop::ftype(as.data.frame.integer) # "S3" "method"
sloop::ftype(as.data.frame.data.frame) # "S3" "method"

# so, as.data.frame.data.frame is the implementation of generic as.data.frame() for the dataframe argument; in general, the methods are not called directly but rather the generic interface

# 13.2.1.4
set.seed(1014)
some_days <- as.Date("2017-01-31") + sample(10, 5)

sloop::ftype(mean) # "S3" "generic"
mean(some_days) # the mean of Date objects is returned
mean(unclass(some_days)) # the mean of integers behind the dates is returned
unclass(mean(some_days)) # the result, however, is numerically the same in this case

# 13.2.1.5
points <- rpois(100, 10)
x <- ecdf(points)
plot(x)

sloop::otype(x)
sloop::s3_class(x) # ecdf inherits from stepfun which inherits from function

# 13.2.1.6
x <- table(rpois(100, 5))
sloop::otype(x)
sloop::s3_class(x) # table


# classes -----------------------------------------------------------------
# to reiterate, classes in R are distinguished from base objects only by class attribute
# this is a lenient way of defining classes unlike that of, e.g., C++ or Java
x <- structure(list(), class = "some_class")
class(x)
sloop::otype(x) # "S3"
x <- unclass(x)
sloop::otype(x) # "base"

class(x) <- "some_new_class"
inherits(x, "some_class") # FALSE
inherits(x, "some_new_class") # TRUE

# thus, although class attribute can be set when creating the object, it can be freely set or modified later; consequently, there is no way to ensure that a particular object will retain its original class
model <- lm(log(mpg) ~ log(disp), data = mtcars)
class(model) # "lm"
class(model) <- "Date"
print(model) # Error
class(model) <- "lm"
print(model) # OK

# to avoid the likely issues when designing custom S3 classes, it is recommended to add three different types of helper functions as interface for users of the class 
# (i)   a constructor to create an instance of the class fromother functions
# (ii)  a validator to ensure, when necessary, that a previously created object is correct
# (iii) a helper-constructor with a simple interface to create an instance of the class in scripts

# (i) a constructor
# from the design perspective, the following three features of constructors are desirable
# (a) unified naming of constructors, e.g. new_classname()
# (b) one argument for the base object and one argument for each attributes, possibly with default values
# (c) validation that the arguments' types and values are correct
new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}


new_Date(c(-1, 0, 1))

new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  return(
    structure(x,
      class = "difftime",
      units = units)
  )
}

new_difftime(c(2, 3), units = c("days", "secs")) # error, units argument is not vectorised
new_difftime(c(2, 3), units = "secs") # ok

# (ii) a validator
# for a complicated object, the number of checks necessary to ensure that a newly created object is valid can be big; furthermore, as object is modified by external code, it might be useful to double-check its validity later on
# hence, it is useful to move more intricate checks in a separate function, a validator function
new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  
  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a") # Error in as.character.factor(x): malformed factor
new_factor(0:1, "a") # Error in as.character.factor(x): malformed factor
new_factor(rep(1L, 10), "a") # ok, since 1 stands for the first (and the only) level
new_factor(1:2, c("a", "c")) # ok, since 1 stands for the first level, 2 stands for the second level etc.

# a validator can additionally check the number of levels
validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing positive integers.",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  return( x )
}

validate_factor(new_factor(1:5, "a")) # Custom error
validate_factor(new_factor(0:1, "a")) # Custom error
validate_factor(new_factor(rep(1L, 10), "a")) # ok, since 1 stands for the first (and the only) level

# (iii) a helper-constructor
# from the design perspective, the following three features of helper-constructors are desirable
# (a) the same name as that of the target class
# (b) the call to the respecitve constructor and, if exists, the validator function at the end of body
# (c) informative error messages and a convenient interface for the end-user
# (d) useful and documented data conversions, default values etc.

# for instance, new_difftime() constructor above does not work with integer inputs, what is confusing
new_difftime(1:10) # Error: is.double(x) is not TRUE
new_difftime(100L) # Error: is.double(x) is not TRUE

difftime <- function(x = double, units = "secs") {
  x <- as.double(x)
  return( new_difftime(x, units) )
}

difftime(1:10) # ok
difftime(100L) # ok


# exercises 13.3.4 --------------------------------------------------------
# 13.3.4.1
# a primitive constructor for data.frame
new_data.frame <- function(...) {
  # transform the input into a list
  x <- list(...)
  n <- length(x)
  
  vec_len <- integer(n)
  
  for (i in seq_len(n)) {
    vec_len[[i]] <- length(x[[i]])
  }
  
  if (length(unique(vec_len)) != 1) {
   stop("The arguments have of differenet length!") 
  }
  
  # return
  structure(
    x,
    row.names = character(ifelse(n == 0, 0, length(x[[n]])) ),
    class = "data.frame"
  )
}


new_data.frame(l1 = 1:3, l2 = 4:6)
new_data.frame(l1 = 1:3, l2 = 4:7)
df <- new_data.frame(NULL, NULL, c())
is.data.frame(df) # TRUE

# 13.3.4.2
# consider a simple helper-constructor to create a factor from a character vector
simple_factor <- function(x = character(), levels = unique(x)) {
  # the non-matching entries are set to NA
  # the matching entries are set to the indexes of their respective levels
  # the contructor function new_factor(), however, throws an exception if there are any NAs
  ind <- match(x, levels)
  validate_factor(new_factor(ind, levels))
}

# consider all funny cases
# (i) NULLs
simple_factor(NULL)
simple_factor(NULL, NULL)
simple_factor(NULL, c("a", "b", "c"))
# (ii) more values than levels
simple_factor(c("a", "b", "c"), NULL)
simple_factor(c("a", "b", "c", "b"), c("a", "b"))
simple_factor(letters[1:26], c("a", "b", "c"))
# (iii) more levels than values
simple_factor(rep("a", 10), c("a", "b", "c"))

# the problem with simple_factor() is that it throws an exception if there are missing levels
# while several alternative responses are possible, e.g.
# (i)   to drop such values
# (ii)  to substitute such values with a replacement value, if the latter is specified
# (iii) to add these values to the levels
# (iv)  to substitutes the values with no matching levels with NAs, as base R factor() does
# let us implement only one of them, viz. (iii)

factor_ <- function(x = character(), levels = unique(x)) {
  ind <- match(x, levels)
  missing_levels <- unique(x[is.na(ind)])

  lml <- length(missing_levels)
  if (lml > 0L) {
    warning(lml, " distinct values were missing in `levels` while present in `x`. These were added to `levels`, so that no element of `x` is lost.\n The following levels were added: ", paste(missing_levels))
    levels <- c(levels, missing_levels)
    ind <- match(x, levels)
  }
  
  # return
  validate_factor(new_factor(ind, levels))
}

l <- letters[1:3]
factor_(letters[1:10], l)

# 13.3.4.3
# compare simple_factor() with base R factor() to see that the latter is quite more general; namely, rather than simply adding class attribute and doing basic datatype validation, it
# (i)   silently replaces NULL input with an empty vector, to eventually reutrn an empty factor
# (ii)  silently replaces missing levels with the ordered set of unique values of x
# (iii) silently casts input to character, if it is of other type
# (iv)  substitutes the values with no matching levels with NAs
# (v)   distinguishes between "ordered" and "factor" classes for ordered and unordered factors
# (vi)  optionally, removes the unwanted levels, if any are specified in exclude argument
# (vii) optionally, replaces unwanted levels with the proper ones

# 13.3.4.4
# TODO

# 13.3.4.5
# TODO


# generics and methods ----------------------------------------------------
# S3 generic performs method dispatch, that is it invokes an appropriate implementation of the generic function depending on the class of its argument
# method dispatch is done by base R UseMethod()
sloop::is_s3_generic("mean") # TRUE
sloop::ftype(mean) # "S3" "generic"
# normally, the body of a generic consists of a single line which simply calls UseMethod()
# note, that the arguments of a generic are not passed to UseMethod() in a regular way; this issue is handled by base R in some complicated way
# sloop::s3_dispatch mimics some aspects of the dispatch process
# whereas an implementation for some class may be missing, there is always a generic.default implementation which serves as a fallback option for all types of input
x <- matrix(1:10, nrow = 2)
sloop::s3_dispatch(mean(x))
sloop::s3_dispatch(mean(1:3))

# sloop::s3_methods_generic() and sloop::s3_methods_class() list all methods associated with a particular generic or class
sloop::s3_methods_generic("mean")
sloop::s3_methods_generic("sum")
sloop::s3_methods_class("ordered")
sloop::s3_methods_class("data.frame")


# exercises 13.4.4 --------------------------------------------------------
# 13.4.4.1
# recall that both t() and t.test() are S3 generics, albeit they have nothing to do with each other
# thus, their source is exhausted by a single line which simply calls UseMethod("*")
t
t.test
# t() is likely to be overloaded for a view array-like datatypes
sloop::s3_methods_generic("t")
# t.test(), on the contrary, has default version and one to work with symbolic formulae
sloop::s3_methods_generic("t.test")
# interestingly, because method dispatch in R has to rely on the value of class attribute which is simply volatile strings, one can perplex it by overloading t() for a class "test"
x <- structure(1:10, class = "test")
# note that method dispatch has looked for a function t.test() but, failing to find it, has fallen beck to t.default()
# "t.test()" which is looked for, however, has little to do with t.test.default() and t.test.formula() which are implementation of t.test() generic
sloop::s3_dispatch(t(x))

# 13.4.4.2
sloop::s3_class("table")
sloop::s3_methods_class("table")

# 13.4.4.3
sloop::s3_class("ecdf")
sloop::s3_methods_class("ecdf")

# 13.4.4.4
# to list all base functions, see exercise #6.2.5.5
base_objects  <- mget(ls("package:base", all = TRUE), inherits = TRUE)
base_funs     <- Filter(is.function, base_objects)
base_generics <- Filter(is_s3_generic, names(base_funs))
num_implementations <- Map(function(x) { nrow(sloop::s3_methods_generic(x)) }, base_generics)
num_implementations[which.max(num_implementations)] # print() has 262 implementations and, unsurprisingly, is the winner

# 13.4.4.5
# TODO
# interestingly, Hadley Wickham says the output is x = 1, y = 10 what suggests that UseMethod() first takes the values of its function from the function's environment and not from the calling one
# so, it is worthwile reading the docs of UseMethod() later
g <- function(x) {
  x <- 10
  y <- 10
  UseMethod("g")
}

sloop::is_s3_generic("g") # TRUE
sloop::s3_methods_generic("g") # an empty tibble
g(123) # Error: no appropriate method defined

g.default <- function(x) c(x = x, y = y)
g(123) # x = 123, y = 1

x <- 1
y <- 1
g(x) # x = 1, y = 1

# 13.4.4.6
# the subsetting operator `[` is a generic with implementations for 17 base classes and numerous classes coming from external packages (currently, 33 more implementations in this local R setup)
sloop::s3_methods_generic("[")$source == "base"


# object styles -----------------------------------------------------------
# not all objects are vectors; the three important alternatives are:
# (i)   record style objects, similar to C structures, which use vectors - which are the primitives of R either way - to store different components of a single observation; these vectors can be bound in a list, e.g. as POSIXlt does
# (ii)  multi-dimensional arrays, like dataframes and tibbles; lthough these are also used to store the results of observation, the structure is multi-dimensional
# (iii) scalar objects, like lm, which can represent any complex entity and not an observation


# exercises 13.5.1 --------------------------------------------------------
# 13.5.1.1
# TODO check https://vctrs.r-lib.org
# (0)   vector style objects: factor(), ordered(),
# (i)   record style objects: as.Date(), as.POSIXct()
# (ii)  multi-dimensional arrays:
# (iii) scalar objects: I(), lm(), ecdf()

df <- data.frame(x = 1:10, y = 31:40)
obj <- I(df)
class(obj)

a <- 1
b <- 2
formula <- I(a + b)

cummulative_dist <- ecdf(rpois(25, 5))
class(cummulative_dist)
print(cummulative_dist)
unclass(cummulative_dist)

t <- table(sample(1:10, 500, replace = TRUE))
class(t)
View(t)
unclass(t)

d <- as.Date(42)
class(d)

# 13.5.1.2
# TODO


# inheritance -------------------------------------------------------------
# recall how lenient S3 object system in R is: a class is defined simply by setting the value of `class` attribute of an object
# furthermore, as R is vectorised, `class` attribute is a vector of values, thus an object can have multiple classes
# (multiple) inheritance in R works precisely in this way: a subclass has at least two elements in `class` attribute, viz. one for a super-class and one for a sub-class
letters[sample(1:12, 50, replace = TRUE)] |> 
  ordered(letters[1:10]) |> 
  class() == c("ordered", "factor") # rep(T, 2)

# UseMethod() first looks for a method implementation in the subclass, then in a superclass etc.

new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  
  # return
  structure(
    x,
    class = "secret"
  )
}

print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  
  # return
  invisible(x)
}

secret <- new_secret(c(1, 12, 123, 1234, 12345))
print(secret)

# note that, since `[` is not overloaded for secret class, the subsetting works as it does for double
sloop::s3_dispatch(secret[1:2])

# implementing `[.secret`, one shall use NextMethod() to signal the dispatch system which method to use
`[.secret` <- function(x , i) {
  # a naive approach results in an infinite loop
  # new_secret(x[i])
  
  # alternatively, one can unclass x, call the default version of `[` and then create x again via a constructor call; this, however, means that a superfluous copy of x is created
  # x <- unclass(x)
  # new_secret(x[i])
  
  # using NextMethod() is the way; it passes the function's arguments to an appropriate method of x's superclass
  new_secret(NextMethod())
}

sloop::s3_dispatch(secret[1:6])

# to make room for future inheritance from a custom class, a few amendments must be made in a typical constructor function, namely
# (i)  it must set the value of `class` attribute with a vector of class names
# (ii) it must take an arbitrary number of extra arguments
new_secret <- function(x , class = character(), ...) {
  stopifnot(is.double(x))
  
  # return
  structure(
    x,
    ...,
    class = c(class, "secret")
  )
}

new_super_secret <- function(x) {
  # return
  new_secret(x, "super_secret")
}

super <- new_super_secret(c(1, 2, 3, 4 , 5))
print(super) # the base class implementation is called
super[1] # the superclass implementation is called, as there is no implementation for a subclass
sloop::s3_dispatch(super[1])

print.super_secret <- function(x, ...) {
  print(rep("xxx", length(x)))
  
  # return
  invisible(x)
}

print(super)
# however, `[.secret` does not work with `print.super_secret`, since the former explicitly calls the base class constructor but does not pass the name of subclass to it
super[1]

# according to Hadley Wickham, there is no simple workaround in base R; his package vctrs, however, solves the issue alongside a few others
vec_restore.secret <- function(x, to, ...) new_secret(x)
vec_restore.super_secret <- function(x, to, ...) new_super_secret(x)

`[.secret` <- function(x, ...) {
  vctrs::vec_restore(NextMethod(), x)
}

super[1:6]


# exercises 13.6.3 --------------------------------------------------------
# 13.6.3.1
# on the face of it, `[.Date` does a trick similar to that of vec_restore by calling oldClass() as far as `class` attribute is concerned; i.e. it sets the value back to that of its `x` argument
# unlike vec_restore, however, it calls .Date constructor and not that of `x` which might be inheriting from Date
`[.Date`

# 13.6.3.2
# TOD

# 13.6.3.3
# consider a more complicated example of class inheritance
generic2 <- function(x) UseMethod("generic2")
generic2.a1 <- function(x) "a1"
generic2.a2 <- function(x) "a2"
generic2.b <- function(x) {
  class(x) <- "a1" # here, `class` of `x` is overwritten
  NextMethod() # supposedly, generic.a1 is called
  # however, exhibiting fool-proof design, R's NextMethod() evaluates `class(x)` when the method is called
  # from ?NextMethod:
  # Note that .Class is set when the generic is called, and is unchanged if the class of the dispatching argument is changed in a method. It is possible to change the method that NextMethod would dispatch by manipulating .Class, but ‘this is not recommended unless you understand the inheritance mechanism thoroughly’ (Chambers & Hastie, 1992, p. 469).
}

obj <- structure(
  list(),
  class = c("b", "a2")
)

obj_2 <- structure(
  list(),
  class = "a1"
)

sloop::s3_dispatch(generic2(obj))
class(obj)

sloop::s3_dispatch(generic2(obj_2))

?NextMethod


#  13.7 -------------------------------------------------------------------
# TODO

# exercises 13.7.5 --------------------------------------------------------
# TODO