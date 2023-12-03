install.packages("car")
library("car")
data(Prestige)
help(Prestige)
#Q1.a
#convert binary variable from type

Prestige$professional <- ifelse(Prestige$type =="prof", 1, 0)
Prestige$professional

#Q1.b
#Linear regression model
model <- lm(prestige ~ income + professional, data = Prestige)

#Q1.c
#Run summary to see details
# Display a summary of the regression model
summary(model)
#estimates to be used in the equation 


#Q1.d
#There is a positive correlation between income and prestige of the job.
#However, the impact of the income to the prestige of the job compared to being a professional is very minor.

#Q1.e
#professionals coefficient is 2.276e+01. This means there is a positive correlation 
#between being a professional and prestige of the job. 

#Q1.f
#marginal impact of 1000$ income increase on the prestige score to be calculated by 
#multipliying 1000 with income coefficient
marginal_impact_income <- model$coefficients["income"]*1000
print(marginal_impact_income)

#Q1.g
#Since we are focusing on the marginal effect of changing occupations from professional to non-professional
#the value of income doesn't have any impact. There is no marginal effect when income is the same when occupation type changes.
#The marginal effect to be calculated by using the professional coefficient.

marginal_impact_prof <- model$coefficients["professional"]*(0-1)
print(marginal_impact_prof)
#chaging occupation type has negative marginal effect (-22.757) on prestige score. 
