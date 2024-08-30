# this file contains worked examples and solutions to the problems from section 4 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/workflow-style

flights |> 
  filter(dest=="IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    n=n(),
    delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  filter(n>10)

flights |> 
  filter(
    carrier == "UA",
    dest %in% c("IAH","HOU"),
    sched_dep_time > 0900,
    sched_arr_time < 2000
  ) |> 
  group_by(flight) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    cancelled = sum(is.na(arr_delay)),
    n=n()
  ) |> 
  filter(n>10)

# Cf. https://styler.r-lib.org/
install.packages("styler")