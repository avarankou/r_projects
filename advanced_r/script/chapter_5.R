# this file contains worked examples and solutions to the problems from Chapter 5 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/control-flow.html

# if() coerces its argument to logical
# hence if("string") throws an error
if ("condition") print("satisfied") else print("unsatisfied")
# a zero-length logical vector passed as an argument leads to the error
if(logical()) print("satisfied") else print("unsatisfied")
# so does an NA argument
if(NA) print("satisfied") else print("unsatisfied")
# by default, an array of length greater than 2 results in the warning
# for the sake of consistency and to avoid slips, set the following global setting
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
if (c(TRUE, FALSE)) print("satisfied") else ("or not satisfied?")

# ifelse() is a vectorised function
# avoid writing too complex checks and do not use the ifelse::yes and ifelse::no arguments of different datatypes
x <- sample(10)
result<- ifelse(x %% 2 == 0, "even", odd)
result
# also bear in mind that vectors are recycled and avoid the usage like
result<- ifelse(x %% 2 == 0, c("even", "EVEN"), c("odd", "ODD"))
result

# switch() is typically wrapped in a function
# switch() coerces its first argument to integer or string; however, the evaluation rules for integer arguments are bothersome, so limit the use of switch() to string argument if possible
test_option <- function(x) {
  switch(x,
         a = "A",
         b = "B",
         c = "C",
         stop("Invalid `x` value!"))
    
}

test_option("a")
test_option("b")
test_option("z")

# dplyr::case_when() is similar to switch()
x <- sample(10)

dplyr::case_when(
  x %% 10 == 0 ~ "round",
  x %% 2 == 0 ~ "even",
  is.na(x) == TRUE ~ "NA",
  TRUE ~ "odd"
)

# exercises 5.2.4

# 5.2.4.1
x <- ifelse(TRUE, 1L, "no") # returns a numeric vector of length one
is.integer(x) && length(x) == 1

x <- ifelse(FALSE, 1, "no") # returns a character vector of length one
is.character(x) && length(x) == 1

x <- ifelse(NA, 1, "no") # returns a logical vector of length one
is.logical(x) && length(x) == 1
# NB: ifelse() propagates an NA input into output, while if() throws an error
x <- if(NA) "yes" else "no" # error: missing value where TRUE / FALSE needed

# 5.2.4.2
x <- 1:10
if (length(x)) "not empty" else "empty"
as.logical(length(x)) # TRUE

x <- numeric()
if (length(x)) "not empty" else "empty"
as.logical(length(x)) # FALSE

# for iterates through a vector
# for (i in vector)

# note that, unlike in modern C/C++, the iterator variable is always created in the global environment and thus overwrites the existing variable with the same name, if any
i <- 1:25
for (i in 1:3)
  print(i)
i

# use next and break keywords to quit the current iteration or quit the loop altogether
for (i in 1:1000) {
  if (i < 990)
    next
  else if (i >= 995)
    break
  else
    print(i)
}

# mind the following three tips
# (i) preallocate vectors used in loops as output container
means <- vector("double", 10)
for (i in 1:10) {
  s <- sample(1:100, size = 100, replace = TRUE)
  means[[i]] <- mean(s)
}
means

# (ii) avoid using for (i in 1:length(vec)) and use for (i in i:seq_along(vec)) instead
# the cause of issues is the feature that 1:0 == c(1, 0)

# (iii) use [[]] to subset a single element, for otherwise the attributes are dropped and the output might look confusing
xs <- c(Sys.time(), as.Date(c("2020-01-01", "2010-01-01")))
for (x in xs) {
  print(x)
}

for (x in seq_along(xs)) {
  print(xs[[x]])
}

# while loops are straightforward

# repeat is equivalent to while(TRUE) and must contain the exit condition
repeat {
  x <- runif(1, min = 0, max = 1)
  cat(x, " ")
  if (x > .9) break
}

# there are no do...while loops in R

# bear in mind, that functional programming justly favours map(), apply() etc. rather than loops for many typical tasks, see section_9


# exercises 5.3.3

# 5.3.3.1
x <- numeric()
out <- vector("list", length(x))
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2 # in first step, out[1] is added to the list, NA ^ 2 == NA
}
out

# 5.3.3.2
# note that the vector being iterated is copied and is not modified inside the loop
xs <- c(1, 2, 3)
for (x in xs) {
  xs <- c(xs, x * 2)
}
xs # 1 2 3 2 4 6

# 5.3.3.3
# likewise, the index variable is not modified inside the loop
for (i in 1:3) {
  i <- i * 2
  print(i) 
}
i