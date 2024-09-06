# this file contains worked examples and solutions to the problems from Chapter 16 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/factors

library(tidyverse)

x <- c("Jan", "Aug", "Agu", "Mar", "Dec")

# note the difference between base R converts a string vector into a factor and forcats::fct() does
# factor() silently replaces the values that do not correspond to any level with NAs
xf <- factor(x, levels = month.abb)
xf
# forcats::fct() throws an error
xffc <- forcats::fct(x, month.abb)
xffc


# readr::read_csv() also allows specifying the columns to be casted to factors and, furthermore, specify their levels
csv <- "
month, value
Jan,12
Feb,44
Mar,22
Jun,90"

df <- read_csv(csv, col_types = cols(month = col_factor(month.abb)))
is.factor(df$month)
levels(df$month)

# exercises 16.3.1
# TODO

# modifying factor order --------------------------------------------------

# fct_reorder() does the trick
relig_summary <- gss_cat |> 
  group_by(relig) |> 
  summarise(
    n = n(),
    tvhours_avg = mean(tvhours, na.rm = TRUE),
    tvhours_sd  = sd(tvhours, na.rm = TRUE)
  ) |> 
  mutate(
    relig = fct_reorder(relig, tvhours_avg)
  )

ggplot(relig_summary, aes(x = tvhours_avg, y = relig)) +
  geom_point()

# fct_reorder2() does more complicated job, it reorders the levels by the value .x at the highest .y
# simply put, it reorders the levels according to the order of lines at the right edge of a chart

by_age <- gss_cat |> 
  filter(!is.na(age)) |> 
  count(age, marital) |> 
  group_by(age) |> 
  mutate(
    prop = n / sum(n)
  )

# compare the plot without reordering
ggplot(by_age, aes(x = age, y = prop, colour = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

# and with it
ggplot(by_age, aes(x = age, y = prop, colour = fct_reorder2(marital, .x = age, .y = prop))) +
  geom_line(linewidth = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(colour = "marital")

# alternatively, one can move some levels of a factor to the front without reordering the remaining ones
# this is done with fct_relevel()
levels(gss_cat$rincome)
gss_cat$rincome <- fct_relevel(gss_cat$rincome, "Not applicable")
levels(gss_cat$rincome)

# with bar plots, fct_infreq() and fct_rev(fct_infreq()) are used to sort the bars in descending or ascending order respectively
gss_cat |> 
  mutate(marital = fct_infreq(marital)) |> 
  ggplot(aes(x = marital)) +
  geom_bar()

# note, that the pipe can be used within a function call
gss_cat |> 
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |> 
  ggplot(aes(x = marital)) +
  geom_bar()

# exercises 16.3.1
# TODO


# modifying factor levels --------------------------------------------------
# obviously, the most common problems are:
# (i)   renaming levels
# (ii)  merging several levels into one
# (iii) merging several levels into one based on some computed criteria

# (i), (ii) are done with fct_recode()
# note that a few levels are grouped into new "Other" level
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  ) |> 
  count(partyid, sort = TRUE)

# (ii) fct_collapse() is used to do the merging en masse
gss_cat |> 
  mutate(
    partyid = fct_collapse(partyid,
      "Other" = c("Other party", "Don't know", "No answer"),
      "Rep" = c("Strong republican", "Not str republican"),
      "Ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "Dem" = c("Not str democrat", "Strong democrat")
    )
  ) |> 
  count(partyid, sort = TRUE)

# (iii) fct_lump_*() allows regrouping factor levels programmatically, see info


# exercises 16.5.1
# TODO

