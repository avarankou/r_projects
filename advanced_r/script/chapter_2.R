# this file contains the worked examples and solutions to the problems from Chapter 2 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/names-values.html

x <- c(1, 2, 3)
y <- x

# originally, both x and y point to the same object
lobstr::obj_addr(x) == lobstr::obj_addr(y)

# however, consider modifying x or y
# copy on modify
x[[3]] = 4
x
y
lobstr::obj_addr(x) == lobstr::obj_addr(y)

y <- x
y[3] = 3
y
x
lobstr::obj_addr(x) == lobstr::obj_addr(y)

# the copying can be made explicit by calling base::tracemem()
cat(tracemem(x), "\n")
y <- x
y[[3]] = 5
# no copy is made here
y[[3]] = 6
# nor here
y[[4]] = 4
untracemem(x)

# the following functions all point to the same function object
lobstr::obj_addr(mean)
lobstr::obj_addr(base::mean)
lobstr::obj_addr(get("mean"))
lobstr::obj_addr(evalq(mean))
lobstr::obj_addr(match.fun("mean"))

# the function argument is also first copied on modify
f <- function(a) {
  a
}

b <- c(1, 2, 3)
cat(tracemem(b), "\n")
z <- f(b)

lobstr::obj_addr(z) == lobstr::obj_addr(b)
untracemem(b)

# now consider a function that modifies its argument
f_mod <- function(a) {
  a <- a * 2
}

b <- c(1, 2, 3)
cat(tracemem(b), "\n")
z <- f_mod(b)

b
z
lobstr::obj_addr(z) == lobstr::obj_addr(b)
untracemem(b)

# lists perform a shallow copy on modify, i.e. only the changed node is copied
# to check what object the list entries point to use lobstr:ref()
l1 <- list(1, 2, 3)
l2 <- l1
lobstr::ref(l1, l2)

l2[[3]] = 4
lobstr::ref(l1, l2)

# dataframes are lists of vectors; hence,
# (i)  when a column is modified, only one list node is copied
# (ii) when a row is modified, all list nodes are copied

df1 <- data.frame(c(1, 2, 3), c(4, 5, 6))
df2 <- df1
lobstr::obj_addr(df1) == lobstr::obj_addr(df2)
df2[[2]][1] = 0
lobstr::obj_addr(df1) == lobstr::obj_addr(df2)
lobstr::obj_addr(df1[[1]]) == lobstr::obj_addr(df2[[1]])
lobstr::obj_addr(df1[[2]]) == lobstr::obj_addr(df2[[2]])

df2 <- df1
lobstr::obj_addr(df1) == lobstr::obj_addr(df2)
# the first row is modified, so the whole list of vectors is copied
df2[1,] <- df2[1,] * 2
lobstr::ref(df1, df2)

# two things to note about character vectors
# (i)  character vectors are actually vectors of strings and not of individual characters
# (ii) R uses Global String Pool, so there are no duplicate strings in the environment

# note that strvec[1] and strvec[4] point to the same object
strvec <- c("1", "abc", "!@#", "1")
lobstr::ref(strvec, character = TRUE)

# so do strvec[2] and strvec2[4]
strvec2 <- c("1", "2", "3", "abc")
lobstr::ref(strvec, character = TRUE)
lobstr::ref(strvec2, character = TRUE)

# exercises 2.3.6

# 2.3.6.1
# tracemem(1:10) is not useful, while it returns the address of an unnamed object
# the unnamed object can only be referenced once, viz. when it is created and utilised
tracemem(1:10)

# 2.3.6.2
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)

rm(x)
x <- list(1:10)
x[[2]] <- x
lobstr::ref(x)
x[[2]][1] <- 2
lobstr::ref(x)

?object.size

y <- rep(list(runif(1e4)), 100)

# estimates the object size based on its dataype and length
object.size(y)
# makes a more accurate estimated taking references into account
lobstr::obj_size(y)
lobstr::ref(y)

funs <- list(mean, sd, var)
lobstr::obj_size(funs)

a <- runif(1e6)
lobstr::obj_size(a) # 8 bytes * 10e6 is around 8 MB

b <- list(a, a)
lobstr::obj_size(b) # still only 8 MB due to references
lobstr::obj_size(a, b) # still only 8 MB due to references

b[[1]][[1]] <- 10
lobstr::obj_size(b) # 16 MB, since b[[1]] and b[[2]] are different objects now
lobstr::obj_size(a, b) # 16 MB

b[[2]][[1]] <- 10
b[[1]] == b[[2]] # rep(TRUE, 10e6)
lobstr::ref(b) # although b[[1]] and b[[2]] have the same content, these are two different objects
lobstr::obj_size(b) # hence, the size of b doubles and is now 16 MB
lobstr::obj_size(a, b) # 24 MB
