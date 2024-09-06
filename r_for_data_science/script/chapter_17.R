# this file contains worked examples and solutions to the problems from Chapter 17 of
# Wickham, H. et al 2023. "R for Data Science", O'Reilly.
# the text is available at
# https://r4ds.hadley.nz/datetimes

library(tidyverse)
library(nycflights13)

# R does not have a special datatype for time, only for date and datetime
# cf. install.packages("hms") if you need one

is.Date(today()) # TRUE
is.Date(now()) # FALSE, datetime

# readr::read_csv() recognises ISO8601 time format
# for other formats, use it with col_types(col_date()) or col_types(col_time()) arguments

csv <- "
  date
  01/02/15
"

read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))

# lubridate:: has a series of functions that try to automatically determine the exact format, granted that the order of D, M, Y and H, M is specified
# the order, however, is specified right in the function's name
lubridate::ymd("2017-01-31")
lubridate::mdy("January 31st, 2017")
lubridate::dmy("31-Jan-2017")

lubridate::ymd_hms("2017-01-31 20:11:59")
lubridate::mdy_hm("01/31/2017 08:01")

# lubridate::make_date() and lubridate::make_datetime() are used to put a few variables into a single date(time) object
flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = lubridate::make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  lubridate::make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time),
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt |> 
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 60 * 60 * 24)

# converting between date and datetime can be done with lubridate:: as well
lubridate::as_date(60 * 60 * 10) # that many days from the Unix epoch
lubridate::as_datetime(60 * 60 * 10) # that many seconds from the Unix epoch

# exercises 17.2.5
# TODO


# modifying datetimes -----------------------------------------------------
dt <- now()
lubridate::year(dt)
lubridate::month(dt)
lubridate::month(dt, label = TRUE)
lubridate::yday(dt) # for "year day"
lubridate::mday(dt) # for "month day"
lubridate::wday(dt) # for "week day"
lubridate::wday(dt, label = TRUE, abbr = FALSE) # for "week day"

# these functions can also be used to set the accessed values
year(dt) <- 2030
dt

# alternatively, lubrdidate::update() can be used with multiple arguments simultaneously
update(dt, year = year(now()), hour = 1, minute = 1, second = 59)
# note that out of range values of day, ..., month roll over
update(dt, month = 24, second = 62)


flights_dt |> 
  mutate(wday = lubridate::wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()

# note that the delay time tends to decrease at the mid-hour and at the end of hour
flights_dt |> 
  mutate(minute = minute(dep_time)) |>
  group_by(minute) |> 
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE),
  n = n()
  ) |> 
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()
  
# be sceptical and check that the scheduled departure predominantly rounded to hh:x5, hh:15 and hh:30
# this is typical for data entered manually
flights_dt |> 
  mutate(minute = minute(sched_dep_time)) |>
  ggplot(aes(x = minute)) +
  geom_freqpoly()

# datetime objects can be rounded just as numeric values
lubridate::round_date(today(), unit = "month")
lubridate::floor_date(today(), unit = "month")
lubridate::ceiling_date(today(), unit = "month") # NB, the function returns the first day of next month

# rounding is, obviously, useful for summaries
flights_dt |> 
  count(week = floor_date(dep_time, unit = "week")) |> 
  ggplot(aes(x = week, y = n)) +
  geom_line() +
  geom_point()

flights_dt |> 
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

# exercises 17.3.4
# TODO

# base R difftime() returns the value in specified units
# lubdridate::duration() always returns seconds
difftime(now(), as_date(0))
difftime(now(), as_date(0), unit = "weeks")
lubridate::as.duration(difftime(now(), as_date(0), unit = "weeks"))

# remember DST (day saving time)
one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")

one_am
one_am + ddays(1)

# when the difference is specified as lubridate::period object, there is no confusion
# thus, to reiterate, lubrdiate::duration is for seconds, lubridate::period is for human units of datetime
one_am + days(1)

flights_dt <- flights_dt |> 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + lubridate::days(1),
    sched_arr_time = sched_arr_time + lubridate::days(1)
  )

flights_dt |> 
  filter(arr_time < dep_time) |> 
  count()

# NB: lubridate, being designed by statisticians, has 365.25 days in a year
years(1) / days(1) == 365.25 # TRUE
dyears(1) / ddays(1) == 365.25 # TRUE

# an exact answer is possible for a specific year, this can be done with lubridate::period
y2023 = ymd("2023-01-01") %--% ymd("2024-01-01")
y2023 / days(1) == 365 # TRUE
                          
y2024 = ymd("2024-01-01") %--% ymd("2025-01-01")
y2024 / days(1) == 366 # TRUE

# exercises 17.4.4
# TODO

# timezones --------------------------------------------------------------
# timezones in R only affect the display of datetime objects
Sys.timezone()
head(OlsonNames())

lubridate::with_tz(now(), tzone = OlsonNames()[round(runif(1, 0, 100))])