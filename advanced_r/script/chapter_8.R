# this file contains worked examples and solutions to the problems from Chapter 8 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/conditions.html

library(rlang)
library(lobstr)

# siginalling conditions --------------------------------------------------

# the three types of conditions in R are:
# (i)   errors / raised with stop() or rlang::abort()
# (ii)  warnings / raised with warning() or rlang::warn()
# (iii) messages / raised with message()

# interestingly, warnings are cached when raised and are only printed when control returns to the top level
# unlike warnings, messages are displayed immediately
# when using messages in functions add the option which suppresses them
test <- function(quite = FALSE) {
  cat("1\n")
  warning("It is not advised to print 1!", call. = FALSE)
  if (!quite) { message("Ok, you have print 1") }
  cat("2\n")
  warning("It is not advised to print 2!", call. = FALSE)
}

test()

# to change the default behaviour, set global option warn:
# warn = 0 - default behaviour
# warn = 1 - print warning messages immediately
# warn = 2 - turn warnings into errors, what is useful for debugging
options(warn = 1)
test()

options(warn = 2)
test
traceback()

options(warn = 0)


# exercises 8.2.4 ---------------------------------------------------------

# 8.2.4.1
file_remove <- function(filename) {
  exists <- file.exists(filename)
  if (sum(exists) != length(filename))  {
    stop("File(s) ", filename[!exists], " is (are) not found!")
  }
  else {
    file.remove(filename)
  }
}

file_remove(c("script/chapter_8.R", "some-random-file.txt"))


# ignoring conditions -----------------------------------------------------

# the three types of conditions and the way to ignore and handle them in R are:
# (i)   errors / suppressed with try() / caught with tryCatch()
# (ii)  warnings / suppressed with suppressWarnings() / caught with withCallingHandlers()
# (iii) messages / suppressed with suppressMessages() / caught with withCallingHandlers()

# note that try() does not allow the caller to specify a type of error to handle and simply suppresses all errors
# note also, that try() is a function and has argument "silent" among others

default <- NULL
try(default <- read.csv("some-random-file.csv"), silent = TRUE)

# handling conditions -----------------------------------------------------
# note how different is error handling from C++
# frist, tryCatch is not a block of code but a function with three arguments that correspond to an error handler, executable code and the finally block
# the same applies to withCallingHandlers()
#
# tryCatch(
#  error = function(cnd) {
#   an error handler / catch block
#  }
#
#  executable code / try block
#)

safe_log <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}

log("123") # Error: a non-numeric argument for a mathematical function
safe_log("123") # NA

# second, tryCatch() allows registering a handler for errors, warnings and messages
# regardless of the signal's class, the program exists after the error handler is executed
# thus, the program can exist after a warning or a message, if its handler has been registered in tryCatch()

# in the following example, the error handler is not executed, since it catches error and not messages
tryCatch(
  error = function(cnd) 10,
  {
    message("Hi!")
    1 + 1
  },
  finally = {
    message("Bye!")
  }
)

# in the following example, the error handler is executed right after the message is signalled
# the program quits after that, however
tryCatch(
  message = function(cnd) "There",
  {
    message("Here")
    stop("This code is never run!")
  },
  finally = {
    message("Executing finally is a must.\nNow, bye!")
  }
)

# the same applies to withCallingHandlers() with the main difference that the program does not quits but returns control flow back to the line which raised the warning

# in the following example neither of messages is printed as the error handler catches and suppresses the first one; however, the first one is accessible from the error handler
tryCatch(
  message = function(cnd) cat("Caught a message! It is:\n", cnd$message),
  {
    message("Ping?") # here the error handler will be executed
    message("Pong!")
  }
)

# in the following example both messages are printed (as messages), although the warning handler is executed before either of them
withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"),
  {
    message("Ping?") # here the warning handler will be executed
    message("Pong!")
  }
)

# the handlers - even of different types - can be nested, then a signal is propagated from the nested to the parent handlers

# if in the following example the parent handler is changed to withCallingHandlers(), then control flow eventually returns to the calling code and the message is printed; now it is not
tryCatch(
  # parent handler
  message = function(cnd) cat("Level 2 (parent)\n"),

  {
    print("This code is wrapped in one handler only.")
    
    # nested handler
    withCallingHandlers(
      message = function(cnd) cat("Level 1 (nested)\n"),
      {
        print("This code is wrapped in two handlers.")
        message("Hello")
      }
        
    )
  }
)

# to suppress a signal from propagating to the parent handlers, rlang::cnd_muffle() is used

# in the following example cnd_muffle() muffles the default handler which prints the messages
withCallingHandlers(
  message = function(cnd) {
    cat("Level 2\n")
    cnd_muffle(cnd)
  },
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

# in the following example cnd_muffle() muffles Level 2 handler and the default handler
withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) {
      cat("Level 1\n")
      cnd_muffle(cnd)
    },
    message("Hello")
  )
)


# call stacks -------------------------------------------------------------
# the call stacks of withCallingHandlers() and tryCatch() also differ
# this illustrates why the former can return control flow back to the calling code, while the latter cannot
f <- function() g()
g <- function() h()
h <- function() message("!")

withCallingHandlers(
  message = function(cnd) {
    lobstr::cst()
    #cnd_muffle(cnd)
  },
  f()
)

tryCatch(
  message = function(cnd) {
    lobstr::cst()
  },
  f()
)


# exercises 8.4.5 ---------------------------------------------------------

# 8.4.5.1
# rlang::abort() saves a backtrace of the condition, whilt rlang::stop() does not
catch_cnd(stop("An error"))
catch_cnd(abort("An error"))
?rlang::abort

# 8.4.5.2
# note how ouptup differs if show_condition wraps withCallingHandlers() instead of tryCatch()
show_condition <- function(code) {
  tryCatch(
    error = function(cnd) "error",
    warning = function(cnd) "warning",
    message = function(cnd) "message",
    {
      code
      NULL
    }
  )
}

show_condition(stop("!")) # error
show_condition(10) # NULL
show_condition(warning("?!")) # warning
show_condition({
  10
  message("?")
  warning("?!" )
}) # message

# 8.5.4.3
# the output is: b a b c 
# (i)   the line `message("c")` raises a signal which is caught by the nested handler, the message will not be printed until control flow returns to the calling code
# (ii)  the nested handler raises another signal which is caught by the parent handler
# (iii) the parent handler catches the exception raised in (ii) and prints "b"; control flow returns to the nested handler
# (iv) the nested handler prints "a", which constitutes the signal handling, and the signal raised in (i) is propagated to the parent handler
# (v)  the parent handler prints "a" and returns control to (i)
# (vi) "c" is finally printed as the program execution resumes

withCallingHandlers(
  # parent handler
  message = function(cnd) message("b: ", cnd$message),
  
  # nested handler
  withCallingHandlers(
    message = function(cnd) message("a: ", cnd$message),
    {
      print("start")
      message("c")
      print("finish")
    }
  )
)

# 8.5.4.4
# the following is the source code of rlang::catch_cnd()
# some comments are added to explain its behaviour
#
# catch_cnd <- function (expr, classes = "condition") 
# {
#   stopifnot(is_character(classes)) # check that input is a character vector
#   handlers <- rep_named(classes, list(identity)) # create a named lists of identity functions
#   # finally, wrap the expression passed as expr argument into tryCatch with the newly generated handlers and evaluate expr
#   eval_bare(rlang::expr(tryCatch(!!!handlers, {
#     force(expr)
#     return(NULL)
#   })))
# }

# 8.5.4.5
# to use a signal handler, simply use rlang::catch_cnd() instead of tryCatch()
show_condition_mod <- function(code) {
  catch_cnd(
    {
      code
      NULL
    }
  )
}

show_condition_mod(message("test"))
show_condition_mod(warning("test"))
show_condition_mod(abort("test"))
show_condition_mod(stop("test"))


# custom conditions -------------------------------------------------------
#TODO

# exercises 8.5.4 ---------------------------------------------------------
#TODO

# applications ------------------------------------------------------------
#TODO

# exercises 8.6.6 ---------------------------------------------------------
#TODO

