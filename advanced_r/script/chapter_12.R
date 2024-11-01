# this file contains worked examples and solutions to the problems from Chapter 12 of
# Wickham, H. 2019. "Advanced R", Chapman and Hall.
# the text is available at
# https://adv-r.hadley.nz/base-types.html

library(sloop)

# everything in R is an object, including functions and even operators
# some of these objects, viz. b a s e  objects, are not instances of any OOP classes

is.object(1:10)
class(1:10) # or attr(1:10, "class")
sloop::otype(1:10) # base

is.object(data.frame(1:10))
class(data.frame(1:10))
sloop::otype(data.frame(1:10)) # S3
sloop::s3_class(data.frame(1:10)) # data.frame