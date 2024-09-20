# this file contains worked examples and solutions to the problems from Chapter 9 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/layers

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


# exercises 9.2.1 ---------------------------------------------------------

# 9.2.1.1
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(fill = "pink", shape = 25)

# 9.2.1.2
# the code below does not set colour because it is specified in aes() parameter of geom_point()
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))

# 9.2.1.3
# stroke sets the width of a marker's border
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)

# 9.2.1.4
# an aesthetic can be specified with a computed value or a computed column
ggplot(mpg, aes(x = displ, y = hwy, colour = displ < 5)) +
  geom_point(shape = 25)


# geometric objects -------------------------------------------------------
# not every aesthetic works with every geom
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
  geom_point(aes(colour = class)) +
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


# exercises 9.3.1 ---------------------------------------------------------

# 9.3.1.1
# geom_line(), geom_boxplot(), geom_histogram(), geom_area()

# 9.3.1.2
# show.legend = FALSE suppresses default behaviour which is to show the legend, if necessary
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = TRUE)

# 9.3.1.3
# se argument of geom_smooth enables and disables the display of the range of confidence interval
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE)

# 9.3.1.4
# top-left
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE)

# top-right
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3) +
  geom_smooth(aes(shape = drv), se = FALSE)

# middle-left
ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, linewidth = 2)

# middle-right
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3, aes(colour = drv)) +
  geom_smooth(se = FALSE, linewidth = 2)

# bottom-left
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3, shape = 16, aes(colour = drv)) +
  geom_smooth(se = FALSE, aes(linetype = drv, linewidth = 2))

# bottom-right
ggplot(mpg, aes(x = displ, y = hwy, fill = drv)) +
  geom_point(size = 3, shape = 21, colour = "white", stroke = 3)

# facets ------------------------------------------------------------------
# facets can be added for a single factor variable or for a combination of two factors

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~drv)

# use scales = { "free", "free_x", "free_y"} to automatically choose a scale for each facet
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv~cyl)

# exercises 9.4.1
# TODO

# some geoms plot not only data but the results of additional computations based on the data
# the default computations can be overwritten using stat_*() functions

# exercises 9.5.1
# TODO


# bar charts --------------------------------------------------------------
# bar charts with multiple categories are normally displayed either:
# (i)   stacked
# (ii)  stacked and normalised
# (iii) dodged

# (i) stacked
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar()

ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar()

# (ii) normalised
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "fill")

# (iii) dodged
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")

# consider a scatterplot with many points overlapping each other
# one cannot sat at glance which observation is prevalent
# the solution is to add a small random offset, called jitter, to the plotted points
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")

# is equivalent to
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_jitter()

# TODO check different values of position argument

# exercises 9.5.1
# TODO

# exercises 9.7.1
# TODO
