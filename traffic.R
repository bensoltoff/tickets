# inspired by
# http://adequateman.deadspin.com/places-to-nap-ranked-1752445242
# see Brian's question

require(readr)
require(dplyr)
require(lubridate)
require(magrittr)
require(ggplot2)

# read in data
## ticket data
## https://data.montgomerycountymd.gov/Public-Safety/Traffic-Violations/4mse-ku6q
tickets <- read_csv("data/Traffic_Violations.csv",
                    col_types = c("cc?????????????????????????????????")) %>%
  rename(date = `Date Of Stop`,
         time = `Time Of Stop`) %>%
  mutate(date = mdy(date),
         time = hms(time))

## weather data
require(weatherData)

### separate object for each year - avoids max query limit
weather12 <- getWeatherForDate("KFDK",
                               start_date = "2012-01-01",
                               end_date = "2012-12-31",
                               opt_custom_columns = TRUE,
                               custom_columns = c(2:4, 20))

weather13 <- getWeatherForDate("KFDK",
                               start_date = "2013-01-01",
                               end_date = "2013-12-31",
                               opt_custom_columns = TRUE,
                               custom_columns = c(2:4, 20))

weather14 <- getWeatherForDate("KFDK",
                               start_date = "2014-01-01",
                               end_date = "2014-12-31",
                               opt_custom_columns = TRUE,
                               custom_columns = c(2:4, 20))

weather15 <- getWeatherForDate("KFDK",
                               start_date = "2015-01-01",
                               end_date = "2015-12-31",
                               opt_custom_columns = TRUE,
                               custom_columns = c(2:4, 20))

weather16 <- getWeatherForDate("KFDK",
                               start_date = "2016-01-01",
                               end_date = "2016-01-26",
                               opt_custom_columns = TRUE,
                               custom_columns = c(2:4, 20))

### combine years
weather <- rbind(weather12, weather13, weather14, weather15, weather16)
rm(weather12, weather13, weather14, weather15, weather16)

### clean up weather
weather$Date <- as.character(weather$Date)    # fix error in date formatting
weather %<>%
  tbl_df %>%
  rename(date = Date) %>%
  mutate(date = ymd(date))


# get number of stops for each day
# note that multiple tickets can be written for each stop
stops <- tickets %>%
  mutate(hour = hour(time)) %>%
  group_by(date, hour, Latitude, Longitude) %>%
  summarise(n_vio = n()) %>%
  group_by(date, hour) %>%
  summarise(n = n()) %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  ungroup

# stops by day of the week
stops %>%
  group_by(date, wday) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(wday, n)) +
  geom_point() +
  geom_boxplot()

# stops by hour of the day
stops %>%
  group_by(hour) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(hour, n)) +
  geom_bar(stat = "identity")

# compare daily precipitation to number of stops per day
stops_day <- stops %>%
  group_by(date) %>%
  summarise(n = sum(n)) %>%
  left_join(weather)

ggplot(stops_day, aes(Mean_TemperatureF, n)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  labs(x = "Average Daily Temperature (in F)",
       y = "Number of Initiated Traffic Stops") +
  theme_bw()

ggplot(stops_day, aes(PrecipitationIn, n)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  scale_x_sqrt(breaks = c(0, .25, .5, 1, 2)) +
  labs(x = "Total Daily Precipitation (in Inches)",
       y = "Number of Initiated Traffic Stops") +
  theme_bw()






