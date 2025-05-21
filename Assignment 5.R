# Question 1

# a. 
today <- Sys.Date()
today

# b. 
Date <- as.Date("12/31/2023", format = "%m/%d/%Y")
Date

# c. 
date_sequence <- seq.Date(from = as.Date("2024-01-01"), 
                          to = as.Date("2024-01-10"), by = "day")
date_sequence

# d.
days_difference <- today - as.Date("1986-01-28")
days_difference
class(days_difference)

# e. 
datetime_utc <- as.POSIXct("2023-12-25 15:30:00", tz = "UTC")
datetime_utc

# f. 
new_datetime <- datetime_utc + (7 * 24 + 12) * 3600  
new_datetime

# g.
start_time <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2023-12-31 23:59:59", tz = "UTC")
time_diff <- difftime(end_time, start_time, units = "weeks")
time_diff

# h. 
time_1 <- as.difftime("3:45", format = "%H:%M", units = "mins")
time_1
time_2 <- as.difftime("2:30:15", format = "%H:%M:%S", units = "secs")
time_2

# i.
date_validity <- function(date_str) {
  date_input <- as.Date(date_str, format = "%Y-%m-%d")
  if (is.na(date_input)) {
    return(cat(date_str, "is NOT a valid date.\n"))
  }
  return(cat(date_str, "is a valid date.\n"))
}
  
is_leap_year <- function(date_str) {
  year <- as.numeric(format(as.Date(date_str), "%Y"))
  return ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)
}

# j.
year <- format(today, "%Y")
month <- format(today, "%m")
day <- format(today, "%d")

cat("Year:", year, "Month:", month, "Day:", day)

# Question 2
USArrests

# a. 
print(nrow(USArrests))
print(ncol(USArrests))

# b. 
FL_row <- USArrests["Florida", ]
FL_row

# c.
last_three_rows <- tail(USArrests, 3)
last_three_rows

# d.
state_names <- rownames(USArrests)[c(20, 30, 40)]
state_names

# e. 
USArrests2 <- USArrests[, c("Murder", "Assault", "Rape")]
USArrests2
Urban <- USArrests[, "UrbanPop", drop = FALSE]
Urban

# f.
colnames(Urban)[colnames(Urban) == "UrbanPop"] <- "UrbanPercent"
colnames(Urban)

# g.
column_medians <- apply(USArrests2, 2, median)
column_medians

# h. 
USArrests2$Total <- rowSums(USArrests2)
USArrests2

# i.
ordered_states <- rownames(USArrests2)[order(USArrests2$Murder, decreasing = TRUE)]
ordered_states

# j. 
subset_USArrests2 <- USArrests2[USArrests2$Murder < 10 & USArrests2$Assault < 100 & USArrests2$Rape < 10, ]
subset_USArrests2

# k. 
high_urban_avg_murder <- mean(USArrests$Murder[USArrests$UrbanPop > 77])
low_urban_avg_murder <- mean(USArrests$Murder[USArrests$UrbanPop < 50])
cat("Average Murder Rate (UrbanPop > 77%):", high_urban_avg_murder, "\n")
cat("Average Murder Rate (UrbanPop < 50%):", low_urban_avg_murder, "\n")

# l.
plot(USArrests$Murder, USArrests$Assault, 
     main = "Murder vs Assault (per 100,000 people)", 
     xlab = "Murder Rate", 
     ylab = "Assault Rate", 
     pch = 19, col = "blue")

# States with higher rates of murder also tend to have higher rates of assault.
# More states fall toward the lower end of the murder & assault axes.

# Question 3. 
install.packages("COUNT")
library(COUNT)
data(fishing)
pois.glm <- glm(totabund ~ meandepth, data = fishing, family = poisson)

# a.
typeof(pois.glm)

# b.
length(names(pois.glm))
names(pois.glm)

# c. 
typeof(pois.glm$qr) 
# qr is an example of a component which is a list itself

# d. 
pois.glm$residuals
length(pois.glm$residuals)
max(pois.glm$residuals)
min(pois.glm$residuals)

# e.
new_component <- rpois(10, lambda = 2)
pois.glm$extra <- new_component


