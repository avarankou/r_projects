# this file contains worked examples and solutions to the problems from Chapter 7 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/environments.html

library(rlang)
library(lobstr)

# environment basics ------------------------------------------------------
# an environment is a special data structure which is used as a bag of variables' names and binds the latter to their values
# hence, an environment is similar to a list; however, it differs from the latter in several important ways:
# (i)   all variables' names in an environment must be unique
# (ii)  an environment has a reference to its parent environment, with the exception of the empty environment
# (iii) an environment is not copied on modification but is modified in place (this behaviour is known as "reference semantics", because an environment's name is a C-style reference to it)
# (iv)  an environment can contain a reference to itself (a list in R cannot)
# (v)   an environment has O(1) access time for its objects, i.e. it is a hashmap of the variables it contains

# two important environments are the global one and the current one
global_env()
current_env()

# to check for the equality of any two envs, base::identical() is used
# the equality operator "==" does not work in this case, since it is a vectorised operator
identical(global_env(), current_env())

e1 <- env(
  a = seq(1, 25, by = 0.33),
  b = function(x) { abs(sd(x) / mean(x)) },
  c = "sample string"
)

# by referring to an environment, one just gets its address
e1 
lobstr::obj_addr(e1)
# rlang::env_print() is used to display its content
env_print(e1)
env_names(e1)

# note that a list in R cannot contain a self-reference, although it contain a copy of itself which is, basically, a nested list
l1 <- list(
  a = "123",
  b = letters[5:7]
)

l1$self_reference = l1
l1$a = "321"
l1$a == l1$self_reference[["a"]] # FALSE
l1$b == l1$self_reference[["b"]] # TRUE
identical(l1, l1$self_reference) # FALSE
l1[[1]] == l1$a # TRUE

# an environment, by contrast, can contain a self-reference
e1$self_reference <- e1
e1$a <- 1:25
e1$a == e1$self_reference$a # TRUE
e1$c == e1$self_reference$c # TRUE
identical(e1$self_reference, e1) # TRUE

# an environment also contains a reference to its parent environment
identical(env_parent(e1), global_env()) # TRUE
identical(env_parent(empty_env()), empty_env()) # Error: The empty environment has no parent
identical(env_parent(global_env()), empty_env()) # FALSE

# to see the list of an environment's ancestors, rlang::env_parents() is used
rlang::env_parents(e1)

# to see the  f u l l  list of parents, which includes the ancestors of the global env, overwrite the default value of last argument with empty_env()
rlang::env_parents(e1, last = empty_env())

# there is a super assignment operator, "<<-", which does not create a variable in the current environment but
# (i)  modifies  one with the same name in a parent environment, if found
# (ii) creates a variable in the global environment
# obviously, <<- can have unexpected side-effects but might be useful for some design patterns

# while subsetting operators "$" and "[[" return NULL if the indexed variable of environment does not exist, rlang::env_get() throws an error
e1$d
e1[["d"]] # note that subsetting with a numerical index does not work with environments, unlike with lists
e1["a"] # Error: an environment cannot be indexed with "["
rlang::env_get(env = e1, nm = "d") # Error: cannot find "d" in environment

# rlang::env_has() checks if the variable exists in the specified environment
rlang::env_has(e1, nms = c("a", "d"))

# rlang::env_poke() and rlang::env_bind() are setters for any and multiple values, respectively
rlang::env_poke(e1, "d", list(da = 1:10, db = "yui"))
e1$d

# rlang::env_unbind() removes a variable from the environment
rlang::env_bind(e1, e = 1:10, f = 30:40)
rlang::env_unbind(e1, "f")

rlang::env_has(e1, nms = c("d", "e", "f"))

# rlang::env_bind_lazy() creates a promise to create a variable which is executed as soon as the variable is accessed for the first time; hence it is called a "lazy binding" by analogy with "lazy evaluation"
rlang::env_bind_lazy(e1, "g" = {Sys.sleep(5); -1})
system.time(env_get(e1, "g"))

# by contrast, rlang::env_bind_active() creates a binding which is re-computed every time it is accessed
rlang::env_bind_active(e1, h = function() runif(1))
e1$h == e1$h # FALSE

# exercises 7.2.7 ---------------------------------------------------------

# 7.2.7.2
env_loop <- env()
env_loop$loop <- env_loop

env_print(env_loop)
identical(env_loop, env_loop$loop) # TRUE

# 7.2.7.3
env_loop <- env()
env_dedoop <- env()
env_loop$loop <- env_dedoop
env_dedoop$dedoop <- env_loop

env_print(env_loop)
identical(env_loop, env_dedoop$dedoop) # TRUE
env_print(env_dedoop)
identical(env_dedoop, env_loop$loop) # TRUE

# 7.2.7.4
e <- env(
  "a" = 1:10,
  "b" = "some string"
)


l <- list(
  "a" = 1:10,
  "b" = "some stirng"
)

# an environment is a bag, i.e. an unordered set, hence indexing it with an integer index does not make sense
# the elements of an environment must be accessed by their names
e1[[1]] # Error
e1[1] # Error
e1$a # OK

l[[1]] # OK
l[c("a", "b", "c")] # OK

# 7.2.7.5
# note that rlang::env_poke overwrites the value of a variable if the latter exists
env_print(e1)
env_poke(e1, "g", 27)

# a modified version of env_poke() can check if the name is already bound and not update an existing binding
env_poke_gently <- function(env = caller_env(), nm, value) {
  if (rlang::env_has(env, nm)) {
    stop("A binding with name `", nm, "` already exists in environment `", env, "`", call. = FALSE)
  }
  else {
    env_poke(env, nm, value, create = TRUE)
  }
}

env_poke_gently(e1, "g", 123)
env_poke_gently(e1, "i", 123)
e1$i

# 7.2.7.6
# `<<-` modifies a variable in a parent environment, if the former is found, or creates it in the global environment
e1$a
e1$a <- 1:5

rm("a")
env_has(global_env(), "a") # FALSE
a <<- letters[1:5]
env_has(global_env(), "a") # TRUE
  
# bind() first tries to modify a binding in the specified environment and then recursively in its parents; it throws an error if the variable is not find in any of the ancestors
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  }
  else if (env_has(env, name)) {
    env_poke(env, name, value)
  }
  else {
    rebind(name, value, env_parent(env))
  }
}

rebind("a", 21:5, e1)
e1$a

rebind("aa", 21:5, e1) # Error
aa <<- 21:5

# recursing over environments ---------------------------------------------
# given the nested structure of environments, recursing over them is a regular task
# consider looking for an environment in which the given variable is defined
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  }
  else if (env_has(env, name)) {
    # Success case
    return(env)
  }
  else {
    # Recursive case
    where(name, env_parent(env))
  }
}

# obviously, this can be done with a while loop as well
where_v2 <- function(name, env = caller_env()) {
  match = FALSE
  
  while(!identical(env, empty_env())) {
    if (env_has(env, name)) {
      match <- TRUE
      break
    }
    else {
      env <- env_parent(env)
    }
  }
  
  if (!match) {
    stop("Can't find ", name, call. = FALSE) 
  }
  else return(env)
}

where_v2("wer")


# exercises 7.3.1 ---------------------------------------------------------

# 7.3.1.1
# it is not difficult to find all environments that contain a binding for the given name
where_v3 <- function(env = caller_env(), name) {
  l <- list()
  
  while(!identical(env, empty_env())) {
    if (env_has(env, name)) {
        l <- append(l, env)
    }
    
    env <- env_parent(env)
  }
  
  if (is_empty(l)) {
    stop("Can't find ", name, call. = FALSE) 
  }
  else return(l)
}

where_v3(e1, "a")

# 7.3.1.2
fget <- function(env = caller_env(), name, inherit = FALSE) {
  if (env_has(env, name, inherit = inherit)) {
    obj <- env_get(env, name, inherit = inherit)
    print(obj)
    if (is.function(obj)) {
      return(obj)
    }
    else return(NULL)
  }
}

fget(name = "mean", inherit = TRUE)


# special environments ----------------------------------------------------
# TODO
