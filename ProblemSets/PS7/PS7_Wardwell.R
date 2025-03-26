library(modelsummary)
library(tidyverse)
library(mice)
library(stargazer)

# Load in the wage data
wage_data = read.csv("C:/Users/lward/Documents/ECON - Data Science for Economists/wages.csv")

# Drop observations where either hgc or tenure is missing
wage_data = wage_data[!is.na(wage_data$hgc) & !is.na(wage_data$tenure),]

# create a college_ind variable where college_ind = 1 if college = "college grad" and college_ind = 0 if college = "not college grad"
wage_data$college_ind = ifelse(wage_data$college == "college grad", 1, 0)

# create a married_ind variable where married_ind = 1 if married = "married" and married_ind = 0 if married = "single"
wage_data$married_ind = ifelse(wage_data$married == "married", 1, 0)

# Use modelsummary package to create a summary statistics table of this data
sum_table<-datasummary_skim(wage_data, 
                            fun_numeric =list("Mean" = mean, 
                                              "Median" = median,
                                              "SD" = sd, 
                                              "Min" = min, 
                                              "Max" = max, 
                                              "N" = length),
                            output="latex")
sum_table

# Is the logwage variable most likely to Missing Completely at Random, Missing at Random, or Missing Not at Random?
# Answer: logwage is most likely missing at random, because the missingness of logwage is likely not related to the value of logwage or any other variables in the dataset.

# Estimate the following regression model: logwage = β0 + β1hgc + β2college + β3tenure + β4tenure^2 + β5age + β6married + ε
lm_wage<-lm(logwage ~ hgc + college_ind + tenure + I(tenure^2) + age + married_ind, data = wage_data)
modelsummary(lm_wage)

# Check the correlation between the hgc and college_ind variables
cor(wage_data$hgc, wage_data$college_ind)

# Drop all observations where logwage is missing
wage_MCAR = wage_data[!is.na(wage_data$logwage),]

lmwage_MCAR<-lm(logwage ~ hgc + college_ind + tenure + I(tenure^2) + age + married_ind, data = wage_MCAR)
modelsummary(lmwage_MCAR)

# This is the same modelsummary as the original, the lm() process drops the missing logwage values in the original dataset

# Perform mean imputation to fill in the missing values of logwage
mean_logwage<-mean(wage_data$logwage, na.rm = TRUE)
# Create a new dataset with the missing values of logwage imputed with the mean logwage
impute_wage<-wage_data
# Replace the missing values of logwage with the mean logwage
impute_wage$logwage[is.na(impute_wage$logwage)]<-mean_logwage

# Estimate the regression model with the imputed logwage values
lm_impute<-lm(logwage ~ hgc + college_ind + tenure + I(tenure^2) + age + married_ind, data = impute_wage)
modelsummary(lm_impute)

# Compare the results with the complete case analysis
modelsummary(list("Complete Cases" = lmwage_MCAR, 
                  "Mean Imputation" = lm_impute))

# Use the mice package to perform a multiple imputation regression model

# Select the variables to be used in the imputation model
imputation_vars <- c("logwage", "hgc", "college_ind", "tenure", "age", "married_ind")
imputation_data <- wage_data[, imputation_vars]

# Run the multiple imputation
# m=5 creates 5 imputed datasets
# maxit=50 specifies the maximum number of iterations
imp <- mice(imputation_data, m=5, maxit=50, printFlag=TRUE, seed=123)

# Fit the regression model to each imputed dataset
imp_models <- with(imp, lm(logwage ~ hgc + college_ind + tenure + I(tenure^2) + age + married_ind))

# Pool the results from the multiple imputed datasets
pooled_results <- pool(imp_models)

# Display the pooled model results
summary(pooled_results)

# Create a model from the first imputed dataset
model_imp1 <- lm(logwage ~ hgc + college_ind + tenure + I(tenure^2) + age + married_ind, 
                 data = complete(imp, 1))

# Load required package for handling pooled results
library(broom.mixed)

# Get tidy results from the pooled model
pooled_tidy <- tidy(pooled_results)

# Create a custom class for the pooled results that will work with modelsummary
pooled_model <- list(
  coefficients = setNames(pooled_tidy$estimate, pooled_tidy$term),
  std.error = pooled_tidy$std.error,
  df.residual = max(pooled_tidy$df),
  nobs = nrow(wage_MCAR)  # Use the same number of observations as complete cases
)
class(pooled_model) <- c("custom_pooled", "lm")

# Create methods for our custom class to work with modelsummary
tidy.custom_pooled <- function(x, ...) {
  data.frame(
    term = names(x$coefficients),
    estimate = x$coefficients,
    std.error = x$std.error,
    statistic = x$coefficients / x$std.error,
    p.value = 2 * pt(abs(x$coefficients / x$std.error), df = x$df.residual, lower.tail = FALSE)
  )
}

glance.custom_pooled <- function(x, ...) {
  data.frame(
    nobs = x$nobs,
    df.residual = x$df.residual
  )
}

# Now create the comparison table with four models (including single imputation but not original regression)
models_comparison <- modelsummary(
  list(
    "Complete Cases" = lmwage_MCAR,
    "Mean Imputation" = lm_impute,
    "Single Imputation" = model_imp1,
    "Multiple Imputation" = pooled_model
  ),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared")
)

# Print the table
models_comparison

modelsummary(
  list(
    "Complete Cases" = lmwage_MCAR,
    "Mean Imputation" = lm_impute,
    "Single Imputation" = model_imp1,
    "Multiple Imputation" = pooled_results
  ),
  statistic = "std.error",
  stars = TRUE,
  output = "latex"
)

