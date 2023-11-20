#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc_sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

str(inc.sub)

#Question 1

library(stats) 

# Run the regression  
reg_model1 <- lm(voteshare ~ difflog, data = inc_sub)  

# Print the regression summary  
summary(reg_model1)

# Create the scatterplot 
plot(inc_sub$difflog, inc.sub$voteshare, 
     main = "Scatterplot with Regression Line", xlab = "difflog", ylab = "voteshare")  

# Fit the regression line  
reg_line1 <- lm(inc_sub$voteshare ~ inc_sub$difflog)  
abline(reg_line1, col = "red")  


# Save the residuals in a separate object  
residualsq1 <- reg_model1$residuals  

# Print the residuals  
print(residualsq1)  


#The prediction equation for the given model is:
  
#voteshare = 0.579031 + 0.041666 * difflog


#Question2

# Run the regression  
reg_model2 <- lm(presvote ~ difflog, data = inc_sub)  

# Print the regression summary  
summary(reg_model2)  



# Create the scatterplot 
plot(inc_sub$presvote, inc.sub$difflog, main = "Scatterplot with Regression Line", 
     xlab = "presvote", ylab = "difflog")  

# Fit the regression line  
reg_line2 <- lm(inc_sub$presvote ~ inc_sub$difflog)  
abline(reg_line2, col = "blue")

# Save the residuals in a separate object  
residualsq2 <- reg_model2$residuals  

# Print the residuals  
print(residualsq2)

#The prediction equation for the given model is:

#presvote = 0.507583 + 0.023837 * difflog

#Question 3

# Run the regression  
reg_model3 <- lm(voteshare ~ presvote, data = inc.sub)  

# Print the regression summary  
summary(reg_model3)

plot(inc_sub$voteshare, inc.sub$presvote, main = "Scatterplot with Regression Line", 
     xlab = "voteshare", ylab = "presvote")  

# Fit the regression line  
reg_line3 <- lm(inc_sub$voteshare ~ inc_sub$presvote)  
abline(reg_line3, col = "green")

#The prediction equation for the given model is:

#voteshare = 0.441330 + 0.388018 * presvote

#Question 4 

reg_model4 <- lm(residualsq1 ~ residualsq2)

summary(reg_model4)

plot(residualsq1, residualsq2, main = "Scatterplot with Regression Line", 
     xlab = "residualsq1", ylab = "residualsq2")

reg_line4 <- lm(residualsq1 ~ residualsq2)
abline(reg_line4, col = "yellow")

#The prediction equation for the given model is:
  
#residualsq1 = -5.934e-18 + 0.2569 * residualsq2


#Question 5

# Run the regression  
reg_model5 <- lm(inc_sub$voteshare ~ inc_sub$difflog + inc_sub$presvote)  

# Print the regression summary  
summary(reg_model5)  

#The prediction equation for the given model is:
  
#voteshare = 0.4486442 + 0.0355431 * difflog + 0.2568770 * presvote


#Coeffecients of pressvote and residualsq2(based on reg_model2) are identical.
