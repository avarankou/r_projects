# this file contains worked examples and solutions to the problems from Chapter 6 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/functions.html

double <- function(x) {
   x <- 2*x
}

# apart from primitive functions written in C, that are predominantly used in base R, each function has 3 components
# (i) the list of arguments
formals(double)
# (ii) the body
body(double)
# (iii) the environment in which the function is defined
environment(double)


# srcref attribute contains the function's source code incl. formatting and comments
attributes(double)

typeof(double) # closure
# primitive functions are either builtin or special
typeof(sum) # builtin
typeof(`[`) # special
# primitive functions have neither formals, nor body, nor environment
is.null(formals(sum)) # TRUE
is.null(body(sum)) # TRUE
is.null(environment(sum)) # TRUE

# in R, functions are o-b-j-e-c-t-s
# this language property is colloquially called first-class functions

# anonymous functions can be useful, especially as arguments of frequent routines or when nested in a bigger function
lapply(mtcars[1:5,], function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

# functions, being objects, can be stored in a list
func_list <- list(
  double = function(x) x * 2,
  half = function(x) x / 2
)

typeof(func_list[[2]]) # closure
func_list$half(1:10)
func_list[[1]](1:10)

# do.call(func, args) is a wrapper that allows calling a function with the arguments collapsed into a list
args <- list(
  x = 1:10,
  rm.na = FALSE,
  trim = 1
)

do.call(mean, args) == mean(1:10, trim = 1) # TRUE

# exercises 6.2.5

# 6.2.5.1
# the function is either named or unnamed, i.e. anonymous, the question pertains only to the former functions
# since functions are objects and the only way to refer to an object in R is by its name (and not, for example, with a pointer to its location in memory), the programmer must already use a function's name to refer to it in code
# hence, if there were a function inverse to match() that returned the name corresponding to the specified function, it would simply duplicate the name of specified function and thus would be completely useless

# 6.2.5.2
# the correct way to call an anonymous function is
(function(x) 3*x)(9) # the anonymous function is enclosed in parentheses and is invoked when specified

# without parentheses, "x(9)" is parsed as a part of the anonymous function's body, i.e. if it were called x(9) would be called and throw an error
function(x) 3*x(9)
(function(x) 3*x(9))(1) # error: could not fund function x

# 6.2.5.4
is.function(mean)
is.primitive(sum)

# 6.2.5.5
objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
num_args <- lapply(funs, formals)
num_args <- lapply(num_args, length)
num_args <- unlist(num_args)
names(funs)[[which.max(num_args)]]

no_args <- num_args == 0
sum(no_args)
funs[num_args == 0]

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
prim_funs <- Filter(is.primitive, objs)


# magrittr library introduces the pipe operator %>% (and its modifications: %T>% %$>% %<>%) that simplifies the syntax of passing one function's result to another function multiple times in a row
library(magrittr)
car_data <- 
  mtcars %>%
  subset(hp > 100) %>%
  aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
  transform(kpl = mpg %>% multiply_by(0.4251)) %>%
  print

# the scoping rules in R are applied to the parse-time structure of the program and not to its run-time structure
# these rules are applied at run time, however; see (iv) below
# this is called lexical scoping
# in R, it has the four following features
# (i)   name masking
# (ii)  functions versus variables
# (iii) a fresh start
# (iv)  dynamic lookup

# (i) name masking means that the specified name is first looked for the current function's scope, then in its parent function's scope, then in the latter's parent function's scope etc. until the name is looked for in the global scope
# thus a variable with a certain name masks all other variables with the same name, if any, defined in higher-level functions
# once the function's scope is left, the in-scope variable is no longer accessible by its name
# note the difference from C/C++ where a scope is limited by parentheses {} and not only by a function's body

x <- 1:20
for (i in 1:3)
{
  # the same x as that outside the loop
  x <- 1:i
}
x # 1:3

x <- 1:20
(function() {
  for (i in 1:3)
  {
    # x inside the function (in the current funtion's scope) is different from that outside the function
    x <- 1:i
  }
}) ()
x # 1:20

# (ii) recall that functions are also objects in R
# therefore, the name masking also applies to functions
sum(1:10) # 55
(function () {
  sum <- function(x) base::sum(x[sample(c(TRUE, FALSE), size = length(x), replace = TRUE)])
  sum(1:10) # fuzzy sum is called, because of the name masking
})()
sum(1:10) # 55

# however, functions are special kind of objects and only function objects are looked up when some function is called
# yet, it is better no to use same names for functio and non-function objects

# (iii) every time a function is called, its environment is created; the latter is normally destroyed after the function terminates
g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11() # 1
g11() # 1

# (iv) dynamic lookup means that some named object's value is looked up at run time and thus the name can refer to an object outside the current scope
# in other words, it is not only the current function's environment that defines the values of objects it uses
# although useful, this feature might lead to unexpected results as code gets more complicated
f <- function() {
  x <- x*2
  print(x)
}
x <- 1:5
f()

# in R, the  scoping rules described above apply to a-l-l objects and functions. including the lookup for "+", which is actually a function, c(), mean() etc.
# hence, setting a function's environment to the empty environment is seldom an option
environment(f) <- emptyenv()
f() # error, could not find function "{"

# by contrast, checking a function's dependencies is useful when things get complicated
codetools::findGlobals(f)

# exercises 6.4.5

# 6.4.5.1
c <- 10:1 # create a vector c
c <- c(c = c) # create a vector with the name(s) "c" and the values given by c, save it to the variable c
# if the default value of use.names argument is changed to FALSE, the second line of the below  three achieves nothing
c <- 10:1
c <- c(c = c, use.names = FALSE)
c

# 6.4.5.3
f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10) == 202 # TRUE

# R also uses lazy evaluation, i.e. any expression is only evaluated when it is used; in other words, many chunks of code, e.g. those created by logical branching, remain not evaluated at all
# this applies to default arguments of functions as well

h01 <- function(x) {
  10
  # since x is not evaluated, stop() is not called
}
h01(stop("This is an error!"))


h02 <- function(x) {
  10
  y <- x* 2 # as x is evaluated, stop() is called
}
h02(stop("This is an error!"))

# promise is a concept closely related to lazy evaluation; basically, the former is the backbone of the latter
# a promise consists of
# (i)   the expression to be evaluated
# (ii)  the environment in which the expression is to be evaluated
# (iii) the evaluated value, i.e. the result of evaluating the expression, that is computed at most once (when first utilised) and then cached
# promises cannot be manipulated in R code

# if a function's argument is a promise, i.e. an expression that must be evaluated at some point, then the evaluated value is bound outside the function and not inside of it, i.e. not in the function's environment but in its parent environment
y <- 100
f <- function(x = 10) {
  x <- x *2
}
f()
y == 100 # TRUE
f(y <- 101)
y == 101 # TRUE

# the same applies to default arguments
# a function's default arguments are evaluated, if necessary, in its scope
# if a default argument is changed to a promise, the latter is evaluated outside the function's scope
f <- function(arg = ls()) {
  in_f_x <- 1
  in_f_y <- 2
  arg
}

f() # "arg", "in_f_x", "in_f_y" are the objects in f's environment
f(ls()) # whatever the objects in f's parent environment are, in_f_x and in_f_y are not among them

# due to promises, default arguments can be specified with references to other arguments and even with references to variables defined later in the function's body
# obviously, it is better to use this feature sparingly and limit it to straightforward cases

# missing() checks if a default argument has been passed to the function
f <- function(x = NULL) {
  if (missing(x)) x <- runif(1)
  x <- x ^ 2
  print(x)
}
f()
f(2)

# exercises 6.5.4

# 6.5.4.1
x_ok <- function(x) {
  # && is lazily evaluated evaluated left-to-right
  # if the left-hand sie argument is FALSE, the right-hand side argument is not evaluated
  !is.null(x) && length(x) == 1 && x > 0
}

x_ok_tick <- function(x) {
  # the function will fall for vector arguments with the length above 1
  x > 0 && !is.null(x) && length(x) == 1
}

x_ok(NULL) # FALSE
x_ok(1) # TRUE
x_ok(1:3) # FALSE 

x_ok_tick(NULL) # FALSE
x_ok_tick(1) # TRUE
x_ok_tick(1:3) # error, length = 3 in coercion to logical (when calling && with a vector of length three as the left-hand side argument)

# 6.5.4.2
f2 <- function(x = z) {
  z <- 100
  x
}
f2() == 100 # TRUE, x's default value is lazily evaluated within the scope of f2()

# 6.5.4.3
y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
  # when the above line is executed:
  # first, x is evaluated
  # as x is evaluated,f1::y (and not global::y) is set to 1
  # then, f2::y is evaluated
}

f1() == 2:1 # TRUE TRUE
y == 10 # TRUE

# 6.5.4.4
show_time <- function(x = stopp("Error!")) {
  stopp <- function(...) Sys.time()
  print(x)
  # when the above line is executed:
  # first, x is set to its default value
  # second, its default value is evaluated
  # thus show_time::stopp() is called, its argument is not used and Sys.time() is returned
}
show_time() # Sys.time()

# 6.5.4.6
library() # can be invoked without arguments to list all available packages

# ... in the lists of arguments signals that the function can take an arbitrary number of arguments
# these arguments can be further passed to another funciton
f_sum <- function(...) {
  l1 <- list(...) # list(...) evaluates the arguments and stores them in list
  str(l1)
  print(sum(...))
  print(..3:5) # ..N refers to the Nth argument of ...
}

f_sum (a = 1, 2, 3, d = 4, 5, 6)

# exercises 6.6.1

# 6.6.1.1
sum(1, 2, 3)
sum(1, 2, 3, na.omit = TRUE, na.na = rep(TRUE, 3)) # as sum takes a variable number of arguments, na.omit and na.na are cast to numeric

# 6.6.1.2
plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")
# col.lab is one of variadic arguments of plot(); their full list can be checked in par()

# 6.6.1.3
# colours of the labels and axes are specified with other arguments, viz. col.lab and col.axis
plot(1:10, col = "red", col.lab = "blue", col.axis = "green")

# normally, a function either (i) returns a value or (ii) throws an error
# (there are a few more exotic options)
# a function can return
# (i-i)  implicitly, when the result of the last expression is returned
# (i-ii) explicitly, by calling return()
# apart from this, a function returns its value either
# (i-a) visibly
# (i-b) invisible, by calling invisible()
# the latter option is hande if a function is used for its side effect, e.g. <-, plot(), print() etc.

f <- function() 1:10
f_invisible <- function() invisible(1:10)
f() # 1:10
f_invisible() # nothing is printed to console
(f_invisible()) # 1:10

# when a function (ii) terminates its execution by calling stop(), it is sometimes necessary to perform some actions nonetheless
# for this purpose, exit handlers are specified in the function's body
# it is better to put an exit handler next to the code that modifies the same variables
# on.exit(add = TRUE) means that the exit handler is added to previously specified ones rather than overwrites them; this is almost always the expected behaviour
f_eh <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
    on.exit("Goodbye again!\n", add = TRUE) # this error handler is futile
  }
}

f_eh(TRUE)
f_eh(FALSE)

# exercises 6.7.5

# 6.7.5.1
# save R variables with save()
save(list = ls(all.names = TRUE), file = "data/all.rda")
# load() returns a list of loaded values invisibly
l <- load("data/all.rda")
l
# it is better to use attach(), though, to avoid overriding object in Global Environment
attach("data/all.rda")

# 6.5.7.2
v <- write.table(c(name = 1:10), file = "data/write.csv")
v # write.table() invisibly returns NULL; returning the success status would be more useful

# 6.5.7.4
graph <- function(code) {
  if(dev.cur() == 1) dev.new()
  
  on.exit(dev.off(), add = TRUE)
  force(code)
}

graph(plot(1:10, col = "pink"))
graph(stop("Some error!"))

# 6.5.7.5
# TODO

# recall that in R, first, everything (incl. functions) is and object
# and, second, every computation is a function call
# a function have one of four forms
# (i)   prefix, e.g. sum(a, b)
# (ii)  infix, e.g. a + b
# (iii) replacement, e.g. names(x) <- c("a", "b", "c")
# (iv)  special built-in functions that come in various guises, e.g. [[]], for, if

# any function of type (ii), (iii), (iv) can be rewritten in prefix form
# NB: `+`, and `for` are functions indeed (hence, if necessary, they and other default functions can be overwritten)
x + y
`+`(x, y)

names(df) <- c("x", "y", "z")
`names<-`(df, c("x", "y", "z"))

for(i in 1:10) print(i)
`for`(i, 1:10, print(i))

# (i) functions in prefix forms match their arguments (in order of precedence)
#   (1) by full name
#   (2) by first letters of the name, if there are no ties
#   (3) by position
k <- function(abcd, efghi, jkl = 3) {
  list(first = abcd, second = efghi, third = jkl)
}

str(k(jkl = 4, efghi = 5, abcd = 6))
str(k(1,2))
str(k(a = 10, j = 30, e = 20))

# it is better not to rely on partial matching, so the following option can be set
options(warnPartialMatchArgs = TRUE)
str(k(a = 10, j = 30, e = 20))

# (ii) user-defined functions in infix form start and and with '%' (recall the pipe operator, %>%)
# the name of such a function (unlike that of a prefix function) can contain any character but '%'; the escape characters come with an additional backslash in funciton definition only
# infix functions are evaluated left to right
`%/\\%` <- function(a, b) paste(a, b)
"call" %/\% "function"

# (iii) replacement functions modify their first argument, although this modification is actually a silent copy
`modify<-` <- function(x, value) {
  x <- value
  x
}
x <- 5
tracemem(x)
modify(x) <- 10
x # 10


`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
# additional arguments go to the left part of the function call, on the right-hand side there is only value argument
modify(x, 1) <- 10

x <- c(a = 1, b = 2, c = 3)
names(x)[[2]] <- "two"
x
# its equivalent in prefix form
`*tmp*` <- x
x <- `names<-`(`*tmp*`,
               `[<-`(names(`*tmp*`), 2, "twooo")
               )
rm(`*tmp*`)
x

# exercises 6.8.6

# 6.8.6.1
#1 + 2 + 3
`+` (`+` (1, 2), 3)
# 1 + (2 + 3)
`+` (1, `+` (2, 3))
# if (length(x) <= 5) x[[5]] else x[[n]]
x <- 1:5
n <- 6
`if`(`<=` (length(x), 5), `[[`(x, 5), `[[`(x, n))

# 6.8.6.2
# x <- sample(replace = TRUE, 20, x = c(1:10, NA))
x <- sample(x = c(1:10, NA), size = 20, replace = TRUE)

# y <- runif(min = 0, max = 1, 20)
y <- runif(n = 20, min = 0, max = 1)

# cor(m = "k", y = y, u = "p", x = x)
cor(x = x, y = y, method = "kendall", use = "pairwise")

# 6.8.6.3
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
# the following function call works
modify(x, 1) <- 10
# the following function call throws an error, viz. target of assignment expands to non-language object
modify(get("x"), 1) <- 10
# the same error
get("x")[1] <- 10
assign("new_x", get("x"))[1]
# the call of modify above is parsed into
# line 1: copy <- x
# line 2: modify(copy, position, value)
# line 3: x <- copy
# since x is a result of function call, its value cannot be modified in line 3

# 6.8.6.4
`vec_mod<-` <- function(x, value) {
  if (!is.vector(x)) stop("x is not a vector!")
  pos <- ceiling(runif(1, min = 0, max = length(x)))
  x[[pos]] <- value
  return(x)
}

x <- 1:10
vec_mod(x) <- 193
x

# 6.8.6.5
`%+%` <- function(lhs, rhs) {
  if (is.character(lhs)) {
    if (!is.character(rhs))
      stop("Right-hand side operand is not a character vector!")
    else
      return (paste(lhs, rhs, sep = " "))
  }
  else if (is.character(rhs)) {
    stop("Left-hand side operand is not a character vector!")
  }
  else
    return (lhs + rhs)
}

"abc" %+% "def"
"abc" %+% 456
123 %+% "def"
123 %+% 456
c("a", "b", "c") %+% c("def", "ghi", "J k l")
c("a", "b", "c") %+% c("def", "ghi", "J k l", "mnop")

# 6.8.6.6
l1 <- apropos("<-")
l2 <- mget(ls("package:base", all.names = TRUE), inherits = TRUE)

l1 <- intersect(l1, names(l2))
l1_primitive <- Filter(is.primitive, l2[l1])

# 6.8.6.8
`%xor%` <- function(lhs, rhs) {
  return (xor(lhs, rhs))
}

a <- -10:10
b <- 15:-5
xor(a, b) == a %xor% b # rep(TRUE, 21)

# 6.8.6.9
`%n%` <- function(lhs, rhs) {
  return(intersect(lhs, rhs))
}


`%u%` <- function(lhs, rhs) {
  return(union(lhs, rhs))
}


`%\\%` <- function(lhs, rhs) {
  return(setdiff(lhs, rhs))
}

intersect(a, b) == a %n% b # rep(TRUE. 16)
union(a, b) == a %u% b # rep(TRUE, 26)
setdiff(a, b) == a %\% b # rep(TRUE, 5)
setdiff(b, a) == b %\% a # rep(TRUE, 5)