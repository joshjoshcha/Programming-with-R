# Problem 1 
library(nycflights13)
library(tidyverse)
library(lubridate)
library(ggplot2)

#a
flights$hour <- flights$sched_dep_time %/% 100
avg_delay <- aggregate(dep_delay ~ origin + year + month + day + hour, 
                       data = flights, 
                       FUN = function(x) mean(x, na.rm = TRUE))
worst_48 <- avg_delay[order(-avg_delay$dep_delay), ][1:48, ]
worst_48

# Add weather information
worst_48_weather <- left_join(worst_48, weather, 
                              by = c("origin", "year", "month", "day", "hour"))
worst_48_weather
# It's hard to tell if there's a correlation b/w weather and delays.

#b
flights_with_coords <- flights %>%
  left_join(airports %>% rename(origin_lat = lat, origin_lon = lon), by = c("origin" = "faa")) %>%
  left_join(airports %>% rename(dest_lat = lat, dest_lon = lon), by = c("dest" = "faa"))
flights_with_coords
# It's easier to rename the columns before the join.
# Renaming after joining is messier. 

#c
avg_delay_by_dest <- flights %>%
  group_by(dest) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  filter(n > 100)

delay_map_data <- avg_delay_by_dest %>%
  inner_join(airports, by = c("dest" = "faa"))

ggplot(delay_map_data, aes(x = lon, y = lat, color = avg_arr_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  theme_minimal() +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "Avg Delays by Airport",
    x = "Longitude", y = "Latitude",
    color = "Avg Delay (min)"
  )

#d
flights %>%
  filter(!is.na(dep_time), !is.na(arr_time), !is.na(air_time)) %>%
  mutate(dep_minutes = (dep_time %/% 100) * 60 + (dep_time %% 100),
         arr_minutes = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_duration = (arr_minutes - dep_minutes) %% (24 * 60),
         diff = sched_duration - air_time) %>%
  summarise(mean_difference = mean(diff, na.rm = TRUE))
# dep_time and arr_time does not account for potential time differences.
# air_time is the actual duration in flight.

# Problem 2 
library(lubridate)
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)") # This is an interval
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705" # time 17:05
t2 <- "11:15:10 PM"  

# Conversion to ISO format
d1_iso <- mdy(d1) 
d1_iso

d2_iso <- ymd(d2)
d2_iso

d3_iso <- dmy(d3)
d3_iso

d4_interval_start <- mdy(d4[1])
d4_interval_end <- mdy(d4[2])
cat('"', as.character(d4_interval_start), '" to "', as.character(d4_interval_end), '"\n', sep="")

d5_iso <- mdy(d5)
d5_iso

t1_iso <- format(strptime(t1, format = "%H%M"), format = "%H:%M:%S")
t1_iso

t2_iso <- format(strptime(t2, format = "%I:%M:%S %p"), format = "%H:%M:%S")
t2_iso

# Problem 3
calculate_age <- function(birthday) {
  current_date <- Sys.Date()
  
  # Calculate age
  age <- as.numeric(format(current_date, "%Y")) - as.numeric(format(birthday, "%Y"))
  
  # Cases where birthday hasn't occurred yet in the year
  if (format(current_date, "%m-%d") < format(birthday, "%m-%d")) {
    age <- age - 1
  }
  return(age)
}

# Example 
calculate_age(as.Date("2003-03-30"))
calculate_age(as.Date("2003-05-01"))

# Problem 4
boxplot(islands, 
        main="Boxplot of Island Areas (Original Scale)", 
        ylab="Island Area")

boxplot(log(islands), 
        main="Boxplot of Island Areas (Log Scale)", 
        ylab="Log of Island Area")

dotchart(islands, 
         main="Dot Chart of Island Areas (Original Scale)", 
         xlab="Island Area", 
         pch=19)

dotchart(log(islands), 
         main="Dot Chart of Log-Transformed Island Areas", 
         xlab="Log of Island Area", 
         pch=19)

# The dot chart with original scale has extreme outliers.
# Hence, log transformation is needed to have a better idea of the distribution.

# Problem 5
#a
plot(pressure$temperature, pressure$pressure,
     xlab = "Temperature",
     ylab = "Pressure",
     main = "Scatterplot of Pressure vs Temperature",
     pch = 19)       

# As temperature increases, pressure increases as well, but not linearly.

#b
curve((0.168 + 0.007 * x)^(20/3),
      add = TRUE)

residuals <- with(pressure, pressure - (0.168 + 0.007 * temperature)^(20/3))

qqnorm(residuals,
       main = "Normal Q-Q Plot of Residuals",
       pch = 19)
qqline(residuals)

# Residuals follow the normal QQ-line only up until quantile 1.
# After quantile 1, the residuals deviate significantly.
# So, the distribution must be skewed. 

#c
transformed_pressure <- pressure$pressure^(3/20)

plot(pressure$temperature, transformed_pressure,
     xlab = "Temperature",
     ylab = "Transformed Pressure Values",
     main = "Transformed Pressure Values vs Temperature",
     pch = 19)
# The relationship now is linear

#d
y <- 0.168 + 0.007 * pressure$temperature
residuals_transformed <- transformed_pressure - y
residuals_transformed

qqnorm(residuals_transformed,
       main = "Normal Q-Q Plot of Residuals (Transformed Pressure)",
       pch = 19)
qqline(residuals_transformed)

# Points follow the line better than before, but with some outliers. 
# The residuals are approximately normally distributed.