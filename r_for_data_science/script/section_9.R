# this file contains worked examples and solutions to the problems from section 7 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-import

library(tidyverse)

# we are working with mpg dataset which is about cars
glimpse(mpg)

# among numerical variables are
# (i)   displ - car engine, lit
# (ii)  cyl   - car cylinders, #
# (iii) cty   - car fuel consumption in the city, mpg
# (iv)  hwy   - car fuel consumption on the highway, mpg
# (v)   year  - car model year


# geom_point() ------------------------------------------------------------
# a better one
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) +
  geom_point()

# a poor one, because of chartjunk, cf. the warnings
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

# a poor one, because of a (unordered) categorical variable mapped to an ordered aesthetic
ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

# exercises 9.2.1
# TODO

# not every aestheic works with every geom
# shape aesthetic does not work with geom_smooth() which, basically, draws a smooth curve that approximates data
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_smooth()


# note the difference between aesthetics applied locally, at geom level, and at ggplot level
# (i) colour applied at ggplot level
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) +
  geom_point() +
  geom_smooth()

# (ii) colour applied locally, at geom_smooth level
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes( colour = class)) +
  geom_smooth()

# what is more, additional geoms with custom aesthetics can be added for subsets of data
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    colour = "red"
  ) +
  geom_point(
    data = mpg |> filter(class == "2seater"),
    shape = "circle open", size = 3, colour = "red"
  )

# use diffetent geoms to explore data
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)

ggplot(mpg, aes(x = hwy)) +
  geom_density()

ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()

# use external packages, if useful for the goal at hand
library(ggridges)

ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

# exercises 9.3.1
# TODO