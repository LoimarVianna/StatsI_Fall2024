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
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")
inc.sub

# Run a regression where the outcome variable is voteshare and the explanatory variable is difflog:
# Run the regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)

# Make a scatterplot of the two variables and add the regression line:

plot(inc.sub$difflog, inc.sub$voteshare,
     main = "Vote Share vs. Difference in Log Spending",
     xlab = "Difference in Log Spending (difflog)",
     ylab = "Vote Share")
abline(model1, col = "blue")

# Save the residuals of the model in a separate object:

residuals_model1 <- residuals(model1)
residuals_model1

# Write the prediction equation:
# voteshare = β0 + β1⋅difflog
# where β0 is the intercept and β1 is the coefficient for difflog, 
# which you’ll find in summary(model1).

# Question 2: Relationship between Spending 
# Difference and Presidential Vote Share

# 1. Run a regression where the outcome variable is presvote 
# and the explanatory variable is difflog:

model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)

# 2.Make a scatterplot of the two variables and add the regression line:

plot(inc.sub$difflog, inc.sub$presvote,
     main = "Presidential Vote Share vs. Difference in Log Spending",
     xlab = "Difference in Log Spending (difflog)",
     ylab = "Presidential Vote Share")
abline(model2, col = "red")

# 3. Save the residuals of the model in a separate object:
residuals_model2 <- residuals(model2)

# Write the prediction equation:
# SEE in LateX

# Question 3: Association between Presidential Vote Share 
# and Incumbent’s Electoral Success
  
# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote:

model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3) 

# 2. Make a scatterplot of the two variables and add the regression line:

plot(inc.sub$presvote, inc.sub$voteshare,
     main = "Vote Share vs. Presidential Vote Share",
     xlab = "Presidential Vote Share",
     ylab = "Vote Share")
abline(model3, col = "green")

# 3. Write the prediction equation:
# # SEE in LateX

# Question 4: Residual Analysis
# 1. Run a regression where the outcome variable is the residuals 
# from Question 1 and the explanatory variable is the residuals 
# from Question 2:

model4 <- lm(residuals_model1 ~ residuals_model2)
summary(model4)

# 2. Make a scatterplot of the two residuals and add the regression line:

plot(residuals_model2, residuals_model1,
     main = "Residuals of Vote Share vs. Residuals of Presidential Vote Share",
     xlab = "Residuals from presvote ~ difflog",
     ylab = "Residuals from voteshare ~ difflog")
abline(model4, col = "purple")

# 3. Write the prediction equation:
# # SEE in LateX

# Question 5: Combined Effect of Spending Difference and 
# Presidential Vote Share on Incumbent’s Vote Share
 
# 1. Run a regression where the outcome variable is the incumbent’s voteshare 
# and the explanatory variables are difflog and presvote:

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5)





