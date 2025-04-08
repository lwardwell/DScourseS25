library(tidymodels)
library(tidyverse)
library(magrittr)
library(glmnet)

# Set the seed noted in PS9
set.seed(123456)

# Load in the data
housing <- read.table("https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv", 
                      header = TRUE, 
                      sep = ",")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Split the data into training and testing sets
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Create a recipe for preprocessing
housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6) %>%
  # prepare the recipe
  prep()

# Run the recipe
housing_train_prepped <- housing_recipe %>% juice()
housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)

# Create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped$medv
housing_test_y  <- housing_test_prepped$medv

# Question 7: Dimensions of the training data
cat("Dimensions of original training data:", dim(housing_train), "\n")
cat("Dimensions of preprocessed training data:", dim(housing_train_prepped), "\n")
num_original_vars <- ncol(housing) - 1  # Subtract 1 for the outcome variable
num_new_vars <- ncol(housing_train_x)
cat("Original number of X variables:", num_original_vars, "\n")
cat("New number of X variables:", num_new_vars, "\n")
cat("Additional X variables:", num_new_vars - num_original_vars, "\n")

# Question 8: LASSO with 6-fold CV
# Create the model specification
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Set up the workflow
lasso_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(lasso_spec)

# Create CV folds
set.seed(123456)
folds <- vfold_cv(housing_train_prepped, v = 6)

# Create the grid of lambda values to try
lambda_grid <- grid_regular(penalty(), levels = 50)

# Tune the model
lasso_tune <- lasso_wf %>%
  tune_grid(
    resamples = folds,
    grid = lambda_grid
  )

# Find the best lambda
best_lambda <- select_best(lasso_tune, metric = "rmse")
print("Best lambda for LASSO:")
print(best_lambda)

# Finalize the workflow with the best lambda
final_lasso <- lasso_wf %>%
  finalize_workflow(best_lambda)

# Fit the model
lasso_fit <- final_lasso %>%
  fit(data = housing_train_prepped)

# In-sample RMSE
in_sample_rmse <- lasso_fit %>% 
  predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred)
print("In-sample RMSE for LASSO:")
print(in_sample_rmse)

# Out-of-sample RMSE
out_sample_rmse <- lasso_fit %>% 
  predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred)
print("Out-of-sample RMSE for LASSO:")
print(out_sample_rmse)

# Question 9: Ridge Regression with 6-fold CV
# Create the model specification
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Set up the workflow
ridge_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(ridge_spec)

# Tune the model
ridge_tune <- ridge_wf %>%
  tune_grid(
    resamples = folds,
    grid = lambda_grid
  )

# Find the best lambda
best_lambda_ridge <- select_best(ridge_tune, metric = "rmse")
print("Best lambda for Ridge:")
print(best_lambda_ridge)

# Finalize the workflow with the best lambda
final_ridge <- ridge_wf %>%
  finalize_workflow(best_lambda_ridge)

# Fit the model
ridge_fit <- final_ridge %>%
  fit(data = housing_train_prepped)

# In-sample RMSE for Ridge
in_sample_rmse_ridge <- ridge_fit %>% 
  predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred)
print("In-sample RMSE for Ridge:")
print(in_sample_rmse_ridge)

# Out-of-sample RMSE for Ridge
out_sample_rmse_ridge <- ridge_fit %>% 
  predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred)
print("Out-of-sample RMSE for Ridge:")
print(out_sample_rmse_ridge)

# For question 10, compare RMSE values
cat("\nModel Comparison:\n")
cat("LASSO out-of-sample RMSE:", out_sample_rmse$.estimate, "\n")
cat("Ridge out-of-sample RMSE:", out_sample_rmse_ridge$.estimate, "\n")