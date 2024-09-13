# this file contains worked examples and solutions to the problems from Chapter 1 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/data-visualize

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

# scatterplot -------------------------------------------------------------
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
    x = "Flipper length, mm",
    y = "Body mass, g",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()


# exercises 1.2.5 ---------------------------------------------------------

# 1.2.5.3
# for all species, the depth of the beak is linearly proportional to its length
ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, color = species)
  ) +
  #geom_line() +
  geom_smooth(method = "lm") +
  labs(
    title = "Adelie penguins have the deepest beak",
    x = "Beak length, mm",
    y = "Beak depth, mm",
    color = "Species"
  )

lm_beak <- lm(bill_depth_mm ~ bill_length_mm, data = penguins, subset = species == "Adelie")
summary(lm_beak)

# 1.2.5.4
# drawing a scatterplot with a categorical variable on the y-axis is pointless (as indeed on any axis) 
ggplot(penguins, aes(x = bill_depth_mm, y = species)) +
  geom_point()

# a boxplot is a better choice
ggplot(penguins, aes(x = bill_depth_mm, y = species)) +
  geom_boxplot(mapping = aes(color = species)) + 
  labs(
    title = "The distribution of beak's depth is narrow for all species",
    x = "Beak length, mm",
    y = "Beak depth, mm",
    color = "Species",
  )

# 1.2.5.5ff
# the following code fails because x and y aesthetics are not specified for geom_point() neither at the ggplot nor at the geom level
ggplot(data = penguins) +
  geom_point()

# interestingly, geom_point removes NAs whatever na.rm is set to
# na.rm = TRUE supresses the warning, while default na.rm = FALSE preserves it
sum(is.na(penguins$body_mass_g) | is.na(penguins$bill_depth_mm))

ggplot(data = penguins, aes(color = species, shape = species)) +
  geom_point(mapping = aes(x = bill_depth_mm, y = body_mass_g), na.rm = TRUE) +
  labs(
    title = "Penguins with deeper beak tend to weight more",
    caption = "Data from palmarespenguins package",
    x = "Body mass, g",
    y = "Beak depth, mm",
    color = "Species",
    tag = "Plot 1.1"
  )

?ggplot2::labs

# 1.2.5.8
# bill_depth_mm is mapped to color, a continuous color scheme is set automatically
# color can be mapped at either ggplot or geom level, while points' size must be set specifically for geom_point
ggplot(penguins, aes(x = flipper_length_mm, body_mass_g, color = bill_depth_mm)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess")

# 1.2.5.9
# a plot similar to 1.2.5.8 without the range of SE around the smoothed line and with points coloured according to islands (discrete colours)
# however, the smoothed lines are added for each island
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
  ) +
  geom_point() +
  geom_smooth(se = FALSE)

# 1.2.5.10
# two plots look alike because the mappings set at the ggplot level are propagated to the geoms, if the latter do not overwrite them

# density plot etc. -------------------------------------------------------
# a bar chart for categorical data
mean_q_per_species = sum(is.na(penguins$species) == FALSE) / nlevels(penguins$species)

ggplot(
  data = penguins,
  mapping = aes(x = fct_infreq(species))
  ) +
  geom_bar() +
  geom_hline(mapping = aes(yintercept = mean_q_per_species, colour = "red")) +
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


# exercises 1.4.3 ---------------------------------------------------------

# 1.4.3.1
# it is basically the same plot, turned by 90 degrees and with different sorting order
ggplot(penguins, aes(y = species)) +
  geom_bar() +
  geom_vline(aes(xintercept = mean_q_per_species, color = "red")) +
  labs(
    title = "Number of penguins by species",
    x = "Species", y = "Penguins, #", colour = "Mean, #"
  )

# 1.4.3.2
# colour sets colour of border and fill - the colour of bars themselves, border included
# obviously, fill is more usefull and can be set, e.g. according to the categories of the variable plotted
ggplot(penguins, aes(x = species)) +
  geom_bar(colour = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar()

# 1.4.3.3
# bins argument of geom_histogram specifies the number of bins and is thus similar to binwidth and breaks

# 1.4.3.4
# given the range of carat variable, the binwidth can reasonably vary from 0.05 to 1
range(diamonds$carat)

# the binwidth 0.5 gives the general distribution of diamonds
ggplot(diamonds, aes(x = carat, fill = cut)) +
  geom_histogram(binwidth = 0.5)

# the binwidth 0.05 to 0.01 shows that the distribution within the ranges of half-carat is similar,
# namely it is a J-shaped right-skewed distribution, with a markedly high peak on the left side which corresponds to 1.0, 1.5 etc. carat
ggplot(diamonds, aes(x = carat, fill = cut)) +
  geom_histogram(binwidth = 0.01)


# relationship of 2+ variables --------------------------------------------
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


# exercises 1.5.5 ---------------------------------------------------------

# 1.5.5.2
# pertaining to colour aesthetic,
# there is a continuous colour scheme for numeric variables and a descrete on for categorical
#
# pertaining to size asesthetic,
# there is no much difference, however, there is no natural interpretation of different size among categories, hence the warning in output
# shape aesthetic can be used instead
# either way, dealing with more than a handful of categories is not advised
ggplot(mpg, aes(x = displ, y = hwy, color = year, size = cyl)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = manufacturer, shape = trans)) +
  geom_point()

# 1.5.5.3f
# linewidth aesthetic is not applicable to geom_point()
# a single variable can be mapped to multiple secondary aesthetics, although it might be slightly verbose
# mapping a single variable to both x and y is, normally, pointless but possible

# 1.5.5.5
# note that without grouping the observations by species one cannot notice in a scatterplot the strong linear relationship between the variables
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, colour = species)) +
  geom_point()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, colour = species)) +
  geom_point() +
  facet_wrap(~species)

# 1.5.5.6
# it is possible to set the same label for multiple variables, in this case they are grouped under one title as intended
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    colour = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

# 1.5.5.7
# the ratio of species on each island
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

# the number of islands each species inhabit
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

# exercises 1.6.1 ---------------------------------------------------------

# 1.6.1.1f
ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()

# ggsave can save plots in the following formats
# "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf"
ggsave("plot/mpg-plot.pdf")