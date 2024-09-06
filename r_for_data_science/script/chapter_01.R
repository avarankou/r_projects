# this file contains worked examples and solutions to the problems from Chapter 1 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-visualize

library(tidyverse)
# install.packages("palmerpenguins")
# install.packages("ggthemes")
library(palmerpenguins)
library(ggthemes)

# s c a t t e r p l o t

# start by inspecting the number of variables and their datatypes as well as the number of records
glimpse(penguins)
# pay attention to the factors' levels
levels(penguins$species)
levels(penguins$island)
levels(penguins$sex)
# summarise the numeric variables to get the idea of their range and distribution
summary(penguins$bill_length_mm)
summary(penguins$bill_depth_mm)
summary(penguins$flipper_length_mm)
summary(penguins$body_mass_g)
# mind NAs and develop a strategy for dealing with them

# in order to establish the relationship between two numeric variables, obviously, start with a scatterplot and, optionally, color-code or otherwise mark some categorical variables
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_point(mapping = aes(color = species)) +  #, shape = species )) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length, mm", y = "Body mass, g",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# exercises 1.2.5
# TODO

# d e n s i t y   p l o t   e t c

# a bar chart for categorical data
mean_ = sum(is.na(penguins$species) == FALSE) / nlevels(penguins$species)
ggplot(
  data = penguins,
  mapping = aes(x = fct_infreq(species))
  ) +
  geom_bar() +
  geom_hline(mapping = aes(yintercept = mean_, colour = "red")) +
  labs(
    title = "Number of penguins by species",
    x = "Species", y = "Penguins, #", colour = "Mean, #"
  )

# a histogram for discrete numerical data
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

# a density plot for continuous numerical data
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# exercises 1.4.3
# TODO

# r e l a t i o n s h i p   o f   t w o +   v a r i a b l e s

# a boxplot
ggplot(penguins,
       aes(x = species, y = body_mass_g)) +
  geom_boxplot()

# density plots for separate categories
ggplot(penguins,
       aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.25)

# a stacked bar chart
ggplot(penguins,
       aes(x = island, fill = species)) +
  geom_bar()

# a normalised stacked  bar chart
ggplot(penguins,
       aes(x = island, fill = species)) +
  geom_bar(position = "fill")

# scatterplots with facets
ggplot(penguins,
       aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

# exercises 1.5.5
# TODO


# exercises 1.6.1
# TODO