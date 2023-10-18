


# Create a table
data <- matrix(c(14, 7, 6, 7, 7, 1), nrow = 2)
colnames(data) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")   	 	

rownames(data) <- c("Upper", "Lower")

row_totals <- rowSums(data)
col_totals <- colSums(data)
grand_total <- sum(data)

expected <- matrix(0, nrow = 2, ncol = 3)

for (i in 1:2) {
  for (j in 1:3) {
    expected[i, j] <- (row_totals[i] * col_totals[j]) / grand_total
    
  }
}


chi_squared <- sum((data - expected)^2 / expected)
df = (2 - 1) * (3 - 1)  # df = 2

p_value <- 1 - pchisq(chi_squared, df)



alpha = 0.1  #Given significance level
critical_value <- qchisq(1 - alpha, df)


if (chi_squared > critical_value) {
  cat("Reject the null hypothesis: There is a significant link between class and bribery attempt")
} else {
  cat("Fail to reject the null hypothesis: There is no significant  link between class and bribery attempt")
}


#H0: There is no link between class and bribery attempt.

#H1: There is a link between class and bribery attempt.

#r:row number
#c:column number
#Degre of freedom: (r-1)*(c-1)= 1*2 = 2



#For alpha = 0.1, the critical stats is 4.605. Our calculated Chi2 test value is 3.791168.

#Since Chi2 value is lower than the critical stats, we fail reject null hypothesis where There is no link between class and bribery attempt.

#Question 2


#Data Read
data <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"))
#Model
model<-lm(formula = data$reserved~data$water)
y <- data$reserved
x <- data$water
model <- lm(y ~ x, data = data)
summary(model)
# Hypothesis testing for the coefficient of x
coefficient_x <- coef(model)["x"]
p_value_x <- summary(model)$coefficients["x", "Pr(>|t|)"]


# Desired significance level
alpha <- 0.05

if (p_value_x < alpha) {
  cat("Reject the null hypothesis: There is no link between reservation policy and repaired drinking water facilities.\n")
} else {
  cat("Fail to reject the null hypothesis: There is no link between reservation policy and repaired drinking water facilities.\n")
}
