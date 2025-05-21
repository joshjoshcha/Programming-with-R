library(tidyverse)
library(lubridate)

circ <- read_csv("Charm_City_Circulator_Ridership.csv")

# Convert dates
circ <- circ |> mutate(date = mdy(date))

# Change column names for reshaping
colnames(circ) <- colnames(circ) |>
  str_replace("Board", ".Board") |>
  str_replace("Alight", ".Alight") |>
  str_replace("Average", ".Average")

# Make long format by route/type
long <- circ |>
  pivot_longer(
    c(
      starts_with("orange"),
      starts_with("purple"),
      starts_with("green"),
      starts_with("banner")
    ),
    names_to = "var",
    values_to = "number"
  )

# Separate route/type into route and type
long <- long |>
  separate(var, into = c("route", "type"), sep = "[.]")

# Make wide format by type
type_wide <- long |>
  spread(type, value = number)

# Extract just average ridership per day
avg <- long |>
  filter(type == "Average", !is.na(number)) |>
  select(-type) |>
  rename(Average = number)



# Problem 1. Plot average ridership by date using a scatterplot. 
ggplot(avg, aes(x = date, y = Average, color = route)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership",
    color = "Route"
  ) 

ggplot(avg, aes(x = date, y = Average, color = route)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(se = FALSE, color = "black", aes(group = route)) +
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership",
    color = "Route"
  )

# Problem 2. Color the points by day of the week.
ggplot(avg, aes(x = date, y = Average, color = day)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership",
    color = "Day of the Week"
  )

# Problem 3. Color the points by route. Use the following palette
pal <- c("darkorange", "purple", "darkgreen", "blue")

ggplot(avg, aes(x = date, y = Average, color = route)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = pal) +
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership",
    color = "Route"
  ) 

# Problem 4. Plot average ridership by date in separate panels by route.
ggplot(avg, aes(x = date, y = Average)) +
  geom_point(alpha = 0.5) + 
  facet_wrap(~ route) + 
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership")

# Problem 5. Plot average ridership by date in separate panels by day of the week. Color the points by route.
ggplot(avg, aes(x = date, y = Average, color = route)) +
  geom_point(alpha = 0.5) +  
  facet_wrap(~ day) + 
  labs(
    title = "Average Ridership by Date",
    x = "Date",
    y = "Average Ridership",
    color = "Route"
  ) 

# Problem 6. Plot average ridership by date. Color the points by route. Customize the plot:
ggplot(avg, aes(x = date, y = Average, color = route)) +
  geom_point(alpha = 0.5) + 
  labs(
    title = "Average Ridership by Date",
    x = "Year",  
    y = "Number of People",  
    color = "Route"
  ) +
  theme_bw() +  
  theme(text = element_text(size = 20))

# Problem 7.
ggplot(filter(long, route == "orange"), aes(x = date, y = number, linetype = type)) + 
  geom_line(color = "darkorange", size = 0.3) + 
  scale_linetype_manual(values = c("Average" = "solid", "Boardings" = "dashed", "Alightings" = "dashed")) +
  labs(
    title = "Ridership by Date (Orange)",
    x = "Date",
    y = "Number of People",
    linetype = "Line Type"
  )

# Assess whether the boardings and alightings differ from the average values.

# Dashed lines for Boardings and Alightings are hard to see, 
# b/c the solid line for Average overrides them. 
# So, there isn't a big difference b/w Average and Boardings or Alightings.

