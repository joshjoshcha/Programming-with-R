library(readxl)
data <- read_excel("Group9_EmployeeMentalHealth_JobSatisfaction (1).xlsx")

# Mean, median, standard deviation, minimum, quantiles, maximum of numerical variables

# Only select numerical variables
selected_columns <- data[, c("Age", "Work_Experience", "Weekly_Work_Hours", "Stress_Level", "Work_Life_Balance", "Company_Culture_Score", "Job_Satisfaction" )]

# Compute summary statistics
summary_stats <- data.frame(
  Mean = sapply(selected_columns, mean),
  Median = sapply(selected_columns, median),
  Std_Dev = sapply(selected_columns, sd),
  Min = sapply(selected_columns, min),
  Q1 = sapply(selected_columns, function(x) quantile(x, 0.25)),
  Q3 = sapply(selected_columns, function(x) quantile(x, 0.75)),
  Max = sapply(selected_columns, max)
)

summary_stats

# Boxplots to visualize variability and identify outliers (based on IQR)
library(ggplot2)

# Create boxplots for each numerical variable
ggplot(data, aes(y = Age)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Age", y = "Ages") 

ggplot(data, aes(y = Work_Experience)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Work Experience", y = "Work Experience (years)") 

ggplot(data, aes(y = Weekly_Work_Hours)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Weekly Work Hours", y = "Weekly Work Hours") 

ggplot(data, aes(y = Stress_Level)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Stress Level", y = "Stress Level") 

ggplot(data, aes(y = Work_Life_Balance)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Work Life Balance", y = "Work Life Balance") 

ggplot(data, aes(y = Company_Culture_Score)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Company Culture Score", y = "Company Culture Score") 

ggplot(data, aes(y = Job_Satisfaction)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot for Job Satisfaction", y = "Job Satisfaction") 

# Normality tesing
# Let's inspect visually 
plot_histogram <- function(data, col_name) {
  ggplot(data, aes(x = col_name)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30) +
    labs(title = paste("Histogram and Normal Curve for", col_name), 
         x = col_name, y = "Density") 
}

for (col in names(selected_columns)){print(plot_histogram(data, col))}
# None of the variables seem to be normnally distributed, except Weekly Work Hours

# Normality testing using the Shapiro-Wilk test 
shapiro_test_results <- lapply(selected_columns, shapiro.test)
shapiro_test_results
# All of the variables have a very small p-value, so we reject the null hypothesis
# So, none of the vars. are normally distributed according to the Shapiro-Wilk test

# Pearson’s correlation analysis to determine relationships between numerical variables.
correlation_matrix <- cor(selected_columns, method = "pearson")
correlation_matrix

# If the data are not normally distributed, consider transforming or normalizing them.

