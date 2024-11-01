# this file contains worked examples and solutions to the problems from Chapter 11 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/function-operators.html

# function operators, or decorators, are functions that take a function or several functions as an argument(s) and return a function as output
# thus, function operators are function factories that take functions as input
# they are frequently used with functionals like purrr::map

library(purrr)
library(memoise)
library(testthat)

# existing function operators ---------------------------------------------
# consider two useful function operators
# (i)  purrr::safely()
# (ii) memoise::memoise()

# (i) purrr:safely() 
# recall that purrr::map() is, basically, a wrapper around a for loop
# now think about catching an error thrown at some step of a for loop and accessing the results of successful step: it is a trivial task, while the motivation for accessing the results instead of simply rerunning the loop comes with costly computations
# this simple case, however, cannot be handled by simply calling map() inside a tryCatch() function, since the latter can only catch the signal and cannot access the results of successful map() sub-calls
#
# that is where purrr::safely() comes in
# the function itself, however, is a general wrapper around tryCatch() and is not limited to the use with map() and variants

# first, a for loop with an error
x <- list(
  runif(5),
  runif(5),
  runif(5),
  "invalid input",
  runif(5),
  "invalid input",
  runif(5)
)

out <- rep(NA_real_, length(x))
for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}

# out vector is OK up to the index at which the error has occured
sum(is.na(out)) == 1 # TRUE

# with map(), there is nothing like out vector with partial results, out_map is not even a bound name
out_map <- map_dbl(x, sum)
env_get(nm = "out_map") # Error: can't find out_map in the environment

# let's wrap sum() in tryCatch() by calling purrr:safely()
# a safely executed function returns a list of $result and $error, one of which is always NULL
safe_sum <- safely(sum)
safe_sum(x[[1]])
safe_sum(x[[4]])

out_safely <- rep(, length(x))
for (i in seq_along(x)) {
  out_safely[[i]] <- safe_sum(x[[i]])
}

str(out_safely)
str(purrr::transpose(out_safely))
some(out_safely, ~ is.null(x$result))
# or, with transpose()
some(transpose(out_safely)$result, is.null)

# thus, safely() takes and returns a function, so it is a function operator by definition
# it can be used with a functional
out_map <- map(x, safely(sum))
out_map <- transpose(out_map)

# finally, out_map contains both the results of successful invocations of the mapped function and the error messages of unsuccessful ones
# note, that unlike the implementation with a for loop, map() does perform N iterations even if some intermediate ones throw errors

# cf. purrr::possibly(), purrr::quietly(), purrr::auto_browser() which are alternatives of purrr::safely()

# (ii) memoise::memoise()
# memoise:: is a package with utilities for caching functions' outputs
# compare recursively computing the 30th entry of the Fibonacci series in a naive way and with caching
fibonacci_naive <- function(n) {
  stopifnot(n >= 0)
  if (n < 2) {
    return( 1 )
  }
  else {
    return( fibonacci_naive(n - 1) + fibonacci_naive(n - 2) )
  }
}

fibonacci_memoise <- memoise::memoise(function(n) { 
  stopifnot(n >= 0)
  if (n < 2) {
    return( 1 )
  }
  else {
    return( fibonacci_memoise(n - 1) + fibonacci_memoise(n - 2) )
  }
})

bm_n <- bench::mark(fibonacci_naive(30))
bm_m <- bench::mark(fibonacci_memoise(30))
rbind(bm_n, bm_m)


# exercises 11.2.3 --------------------------------------------------------
# 11.2.3.1
# Vectorize() creates a wrapper over a non-vectorised function which makes it vectorised
# consider rep() which is not vectorised, i.e. when it takes vector arguments it returns a single value, albeit a vector; vectorised functions, by contrast, return a vector with a separate entry for each element of the input vectors
rep(1:4, 4:1) # a vector of length 10
vrep <- Vectorize(rep.int)
vrep(1:4, 4:1) # a list of 4 entries
map2(1:4, 4:1, rep) # the same as vrep() above

# 11.2.3.2f
# purr::possibly(), just as purrr::safely(), is a wrapper around tryCatch()
# the latter returns a list with $result and $error entries, while the former only returns result 
x <- list(
  ok = runif(5),
  nok = "invalid input"
)

sum(x[["ok"]])
sum(x[["nok"]])

safely(sum)(x[["ok"]])
safely(sum, 0)(x[["nok"]])

possibly(sum)(x[["ok"]])
possibly(sum, 0)(x[["nok"]])


# custom function operators -----------------------------------------------
# consider making a function for fetching the content of several web-pages
urls <- c(
  "adv-r" = "https://adv-r.hadley.nz", 
  "r4ds" = "http://r4ds.had.co.nz/"
)

# a straightforward solution is something like iterating a list of URLS or calling map() or a similar functional
path <- paste(tempdir(), names(urls), ".html")
walk2(urls, path, download.file, quiet = TRUE)

# such a solution, however, is hardly reusable and extensible
delay_by <- function(f, by) {
  force(f)
  force(by)
  
  return( {
    function(...) {
      Sys.sleep(by)
      f(...)
    }
  })
}

# note how dot_every(), which is a functional, works
# it creates an environment in which the argument function will be evaluated, hence it can keep the tally of its invocations and results
dot_every <- function(f, n) {
  force(f)
  force(n)
  
  count <- 0
  function(...) {
    count <<- count + 1
    if (count %% n == 0) { cat(".") }
    else { f(...) }
  }
}

# compare how fast the three below functions are executed
walk(1:100, runif)
walk(1:100, dot_every(runif, 10))
walk(1:100, dot_every(delay_by(runif, 0.2), 10))

# finally, put delay_by() and dot_every() together and call with purrr::walk2()
walk2(
  urls, path,
  download.file |> delay_by(0.1) |> dot_every(1),
  # additional arguemnts of download.file
  quiet = TRUE)


# exercises 11.3.1 --------------------------------------------------------
# 11.3.1.1
# as far as perfomance is concerned, it seems not to make much defference in which order the functions are nested
# nonetheless, the first line is  more straightforward as it says "download a file, wait for some time and update the progress bar", while the second "download a file, update the progress bar, wait for some time"
dot_delay <- bench::mark(walk(1:3000, dot_every(delay_by(runif, 0.01), 100)))
delay_dot <- bench::mark(walk(1:3000, delay_by(dot_every(runif, 100), 0.01)))

rbind(dot_delay, delay_dot)

# 11.3.1.2
# it is better not to memoise or otherwise cash functions like file.download() which access a remote source of data, because the calling code cannot determine whether the data available at some URL have been updated or not
# it can be cached, however, for a short period of time, or when the remote source is unlikely to update the data, or when getting the latest version of data is not crucial etc.

# 11.3.1.3
# below is a simple function which checks if the current working directory has been changed or not
dir_changed_init <- function(verbose = TRUE, ...) {
  files_base <- dir(recursive = TRUE, ...)
  last_check <- Sys.time()
  
  return( function() {
    retval = FALSE
    
    files_current <- dir(recursive = TRUE, ...)
    
    files_deleted <- setdiff(files_base, files_current)
    files_created <- setdiff(files_current, files_base)
    
    if (length(files_deleted) == 0 && length(files_created) == 0) {
      if (verbose) { cat(getwd(), " has not changed since ", format(last_check), "\n") }
      retval <- FALSE
    }
    else {
      if (verbose) {
        cat(getwd(), "has changed since ", format(last_check), "\n")
        
        cat(length(files_created), " new file(s)\n")
        for (i in seq_along(files_created)) {
          cat(i, ": ", files_created[[i]], "\n")
        }
        
        cat(length(files_deleted), " deleted file(s)\n")
        for (i in seq_along(files_deleted)) {
          cat(i , ": ", files_deleted[[i]], "\n")
        }
      }
      
      # it is possible to keep the history of latest N calls of dir_changed() rather than just one
      files_base <<- files_current  
      last_check <<- Sys.time()
      retval <- TRUE
    }
    
    return( retval )
  })
}

dir_changed <- dir_changed_init()
dir_changed()
fn <- paste(getwd(), "data", "tmp.data", sep = "/")
file.create(fn)
dir_changed()
file.remove(fn)
dir_changed()


# 11.3.1.4
# TODO

# 11.3.1.5
# TODO