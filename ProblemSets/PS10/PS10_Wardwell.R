library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,education.num,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)


######################
# tidymodels time!
######################
# Splits data into 80% training, 20% testing
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)

# 3-fold cross-validation
rec_folds <- vfold_cv(income_train, v = 3)

#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
# Defines logistic regression model with LASSO regularization
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values (levels=50) of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# Creates workflow combining model and formula
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Performs grid search across all regularization values
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
# Evaluates performance and selects best regularization parameter
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")

# Finalizes model with best parameter
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)

# Runs final model on test data
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso,income_split) %>%
  collect_metrics()

# Displays results
logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
# Stores results for later comparison
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))


#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
# Defines decision tree model with parameters to tune
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter - Maximum node size
  tree_depth = tune(), # tuning parameter - Maximum depth of the tree
  cost_complexity = tune(), # tuning parameter - Complexity parameter (pruning)
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (complexity, depth, etc.)
# Creates parameter grids for tuning
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,50,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
# Combines into full grid of all tuning parameters (100 combinations)
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

# YOU FILL IN THE REST

# TREE MODEL COMPLETED:
# set workflow
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ .) # Uses all variables as predictors

# tune parameters
tune_res <- tune_grid(
  tree_wf, 
  resamples = rec_folds, 
  grid = tree_parm_df, 
  metrics = metric_set(accuracy)
)

# Selects best parameters
top_acc_tree  <- show_best(tune_res, metric = "accuracy")
best_acc_tree <- select_best(tune_res, metric = "accuracy")
final_tree <- finalize_workflow(tree_wf, best_acc_tree)

# Evaluates on test data
print('*********** DECISION TREE **************')
tree_test <- last_fit(final_tree,income_split) %>%
  collect_metrics()

# Displays results
tree_test %>% print(n = 1)
top_acc_tree %>% print(n = 1)

# Stores for later comparison
tree_ans <- top_acc_tree %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "decision_tree") %>% select(-starts_with(".config"))

#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # Number of neurons in hidden layer
  penalty = tune()  # Regularization parameter
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
# Creates parameter grid for tuning
nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
lambda_grid   <- grid_regular(penalty(), levels = 10)
# Combines into full grid (100 combinations)
nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())

# YOU FILL IN THE REST

# Create workflow
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ .)

# Tune Parameters
tune_nnet_res <- tune_grid(
  nnet_wf, 
  resamples = rec_folds, 
  grid = nnet_parm_df
)

# Select best parameters in sample
top_acc_nnet  <- show_best(tune_nnet_res, metric = "accuracy")
best_acc_nnet <- select_best(tune_nnet_res, metric = "accuracy")
final_nnet <- finalize_workflow(nnet_wf, best_acc_nnet)

# Evaluate on test data
print('*********** Neural Network **************')
nnet_test <- last_fit(final_nnet, income_split) %>%
  collect_metrics()

# Displays results
nnet_test %>% print(n = 1)
top_acc_nnet %>% print(n = 1)

# combine results into a nice tibble (for later comparison)
nnet_ans <- top_acc_nnet %>% slice(1)
nnet_ans %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "nnet") %>% select(-starts_with(".config"))

#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
# Defines KNN model
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # Number of neighbors to consider
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
# Creates parameter grid of 30 values
knn_parm_df <- tibble(neighbors = seq(1,30))

# YOU FILL IN THE REST

# Create workflow
knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ .)

# Tune parameters
tune_knn_res <- tune_grid(
  knn_wf, 
  resamples = rec_folds, 
  grid = knn_parm_df
)

# Selects best in-sample parameter
top_acc_knn  <- show_best(tune_knn_res, metric = "accuracy")
best_acc_knn <- select_best(tune_knn_res, metric = "accuracy")
final_knn <- finalize_workflow(knn_wf, best_acc_knn)

# Evaluate on test data
print('*********** K Nearest Neighbors **************')
knn_test <- last_fit(final_knn, income_split)%>%
  collect_metrics()

# Display results
knn_test %>% print(n = 1)
top_acc_knn %>% print(n = 1)

# combine results into a nice tibble (for later use)
knn_ans <- top_acc_knn %>% slice(1)
knn_ans %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "knn") %>% select(-starts_with(".config"))


#####################
# SVM
#####################
print('Starting SVM')
# set up the task and the engine
# Defines SVM with radial basis function kernel
tune_svm_spec <- svm_rbf(
  cost = tune(), # Cost parameter (penalty for misclassification)
  rbf_sigma = tune() # Kernel width parameter
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

# define a set over which to try different values of the regularization parameter (number of neighbors)
# NOTE: Reduced the grid from original 6x6 to 3x3 to make sure it actually runs
# Creates a grid of 3 values for each parameter (9 combinations)
svm_parm_df1 <- tibble(cost      = c(2^(-1),2^0,2^10))
svm_parm_df2 <- tibble(rbf_sigma = c(2^(-1),2^0,2^10))
svm_parm_df  <- full_join(svm_parm_df1,svm_parm_df2,by=character())

# YOU FILL IN THE REST

# Create workflow 
svm_wf <- workflow() %>%
  add_formula(high.earner ~ .) %>% 
  add_model(tune_svm_spec)

# Tunes parameters
tune_svm_res <- tune_grid(
  svm_wf, 
  resamples = rec_folds,
  grid = svm_parm_df
)

# Select best parameters
top_acc_svm  <- show_best(tune_svm_res, metric = "accuracy")
best_acc_svm <- select_best(tune_svm_res, metric = "accuracy")
final_svm <- finalize_workflow(svm_wf, best_acc_svm)

# Evaluate on test data
print('*********** Support Vector Machine **************')
svm_test <- last_fit(final_svm, income_split)%>%
  collect_metrics()

#Display results
svm_test %>% print(n = 1)
top_acc_svm %>% print(n = 1)

# combine results into a nice tibble (for later use)
svm_ans <- top_acc_svm %>% slice(1)
svm_ans %<>% left_join(svm_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "svm") %>% select(-starts_with(".config"))


#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans,tree_ans,nnet_ans,knn_ans,svm_ans)
# Display results in R markdown format
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
# Display results in a LaTeX version
datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="latex", file="PS10_Wardwell_table.tex")

library(xtable)

# Create filtered data frame first
filtered_data <- all_ans %>% select(-.metric, -.estimator, -mean, -n, -std_err)

# Create LaTeX table and write to file
xtable_output <- xtable(filtered_data)
print(xtable_output, file = "PS10_Wardwell.tex", type = "latex")

