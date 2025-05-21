# Question 1
# a. 
ucb_table <- margin.table(UCBAdmissions, c(1, 2))  
ucb_table_with_totals <- addmargins(ucb_table)
ucb_table_with_totals

# b. 
ucb_proportions <- round(prop.table(ucb_table, margin = 2) * 100, 1)
ucb_proportions
# As shown in table, overall admissions rate is higher for males than for females

# c.
ucb_dept_proportions <- round(prop.table(UCBAdmissions, margin = c(2, 3)) * 100, 1)
ftable(ucb_dept_proportions)
# Even though the overall admission rate was higher for males from 1(b)
# The admissions rate for females is higher in 4 out of the 6 departments

# d. 
gender_dept_table <- margin.table(UCBAdmissions, margin = c(2, 3))
chi_square_test <- chisq.test(gender_dept_table)
chi_square_test
# Since p-value is less than 0.05, we reject the null hypothesis at the 5% significance level
# Hence, there is a statistically significant relationship b/w gender & admission rates across departments


# Question 2

# This equation has one root at x=3. exp(-x) is always greater than zero.  

x0 <- c(3, 3.2, 2.99, 3.01)  # Initial guesses
tolerance <- 0.000001        

for (initial_x in x0) {
  x <- initial_x 
  f <- (x ^ 2 - 6 * x + 9) * exp(-x)  
  iteration <- 0  
  while (abs(f) > tolerance) {
    f.prime <- (-x ^ 2 + 8 * x - 15) * exp(-x)  
    x <- x - f / f.prime  
    f <- (x ^ 2 - 6 * x + 9) * exp(-x)  
    iteration <- iteration + 1
    cat("Iteration:", iteration, "x:", x, "\n") 
  }
  cat("Root found for initial guess", initial_x, "is", x, "\n")  
}

# Recall that x=3 is the root
# For all initial guesses, the iteration stops as the value becomes closer to 3
# Specifically, if the guess is within the specified tolerance level 0.000001
