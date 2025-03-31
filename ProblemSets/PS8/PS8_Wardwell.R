library(nloptr)
library(modelsummary)

# Set the seed for the random number generator
set.seed(100)

# Define the dimensions of the matrix
N <- 100000 # number of rows
K <- 10 # number of columns

# Create matrix with normally distributed random numbers
# First create the entire matrix of random numbers
X <- matrix(rnorm(N * K), nrow = N, ncol = K)

# Replace the first column with 1's
X[, 1] <- 1

# Generate epsilon (eps) vector with length N, distributed N(0, 0.25)
# Note: sigma = 0.5, so sigma^2 = 0.25
eps <- rnorm(N, mean = 0, sd = 0.5)

# Define beta vector with the given values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y which is a vector equal to X*beta + eps
Y <- X %*% beta + eps

# Check dimensions and preview a few rows
cat("Dimensions of X:", dim(X), "\n")
cat("Length of eps:", length(eps), "\n")
cat("Length of beta:", length(beta), "\n")
cat("Length of Y:", length(Y), "\n")

# Preview a few rows of data
head_data <- data.frame(
  Y = Y[1:5, ],
  X1 = X[1:5, 1],
  X2 = X[1:5, 2],
  X3 = X[1:5, 3],
  X4 = X[1:5, 4],
  X5 = X[1:5, 5],
  eps = eps[1:5]
)
print(head_data)

# ~~~MATRIX ALGEBRA OLS ESTIMATE~~~
# Calculate X'X (transpose of X multiplied by X)
XtX <- t(X) %*% X

# Calculate (X'X)^(-1) (inverse of X'X)
XtX_inv <- solve(XtX)

# Calculate X'Y (transpose of X multiplied by Y)
XtY <- t(X) %*% Y

# Calculate β̂ₒₗₛ = (X'X)^(-1)X'Y
beta_ols <- XtX_inv %*% XtY

# Create a comparison table
comparison <- data.frame(
  Parameter = paste0("β", 0:9),  # β0 is the intercept
  True_Value = beta,
  OLS_Estimate = as.vector(beta_ols),
  Difference = as.vector(beta_ols) - beta
)

print(comparison)

# ~~~LM() OLS ESTIMATE~~~
# Also calculate the OLS estimate using lm() function
lm_estimate <- coef(lm(Y ~ X - 1))
print(lm_estimate)

# How does the estimate comare with the true values?
# ANSWER: The OLS estimate is very close to the true values, as our sample size increases, the OLS estimates should get closer
# to the true values.

# ~~~GRADIENT DESCENT~~~
# Implementing gradient descent to find beta_OLS

# Initialize parameters
learning_rate <- 0.0000003  # As specified in the problem
max_iterations <- 10000     # Maximum number of iterations
tolerance <- 1e-8           # Convergence threshold
beta_gd <- rep(0, K)        # Initialize beta with zeros

# Function to compute the gradient of the cost function
# For OLS, the gradient of (1/2)||Y - Xβ||² is -X'(Y - Xβ)
compute_gradient <- function(X, Y, beta) {
  error <- Y - X %*% beta
  gradient <- -t(X) %*% error
  return(gradient)
}

# Function to compute the cost (MSE)
compute_cost <- function(X, Y, beta) {
  error <- Y - X %*% beta
  cost <- mean(error^2) / 2
  return(cost)
}

# Gradient descent implementation
costs <- numeric(max_iterations)
beta_history <- matrix(0, nrow = max_iterations, ncol = K)

for (i in 1:max_iterations) {
  # Compute gradient
  gradient <- compute_gradient(X, Y, beta_gd)
  
  # Update beta using gradient descent
  beta_gd <- beta_gd - learning_rate * gradient
  
  # Store beta values
  beta_history[i, ] <- beta_gd
  
  # Compute cost
  costs[i] <- compute_cost(X, Y, beta_gd)
  
  # Check for convergence (optional)
  if (i > 1 && abs(costs[i] - costs[i-1]) < tolerance) {
    cat("Converged after", i, "iterations\n")
    break
  }
  
  # Print progress every 1000 iterations
  if (i %% 1000 == 0) {
    cat("Iteration", i, "- Cost:", costs[i], "\n")
  }
}

# Compare gradient descent solution with true beta and OLS solution
comparison <- data.frame(
  Parameter = paste0("β", 0:9),
  True_Value = beta,
  GD_Estimate = beta_gd,
  OLS_Estimate = as.vector(beta_ols),
  GD_vs_True = beta_gd - beta,
  GD_vs_OLS = beta_gd - as.vector(beta_ols)
)

print(comparison)

# Plot the cost function over iterations
plot(1:length(costs[costs > 0]), costs[costs > 0], 
     type = "l", xlab = "Iteration", ylab = "Cost",
     main = "Cost Function During Gradient Descent")

# Plot the convergence of each beta parameter
matplot(beta_history, type = "l", 
        xlab = "Iteration", ylab = "Parameter Value", 
        main = "Convergence of Beta Parameters",
        col = 1:K)
legend("topright", legend = paste0("β", 0:9), col = 1:K, lty = 1:K)

# According the Convergence plot, the beta parameters converge to close to their true values fairly early in the iteration process.
# The parameter value lines have converged fairly early, well before the 2000th iteration.

# ~~~NLOPT~~~

# Define the objective function (sum of squared residuals)
ssr_objective <- function(beta_params, X, Y) {
  # Predict Y using current beta parameters
  Y_pred <- X %*% beta_params
  
  # Calculate residuals
  residuals <- Y - Y_pred
  
  # Return sum of squared residuals
  return(sum(residuals^2))
}

# Define gradient function for L-BFGS-B
ssr_gradient <- function(beta_params, X, Y) {
  # Predict Y using current beta parameters
  Y_pred <- X %*% beta_params
  
  # Calculate residuals
  residuals <- Y - Y_pred
  
  # Calculate gradient
  grad <- -2 * t(X) %*% residuals
  
  return(as.vector(grad))
}

# Set initial values for beta
initial_beta <- rep(0, K)

# Compute β̂ₒₗₛ using L-BFGS-B algorithm
lbfgs_result <- optim(
  par = initial_beta,
  fn = ssr_objective,
  gr = ssr_gradient,
  method = "L-BFGS-B",
  X = X,
  Y = Y,
  control = list(maxit = 1000)
)

# Compute β̂ₒₗₛ using Nelder-Mead algorithm
nelder_mead_result <- optim(
  par = initial_beta,
  fn = ssr_objective,
  method = "Nelder-Mead",
  X = X,
  Y = Y,
  control = list(maxit = 5000)
)

# Extract the optimized beta values
beta_lbfgs <- lbfgs_result$par
beta_nelder_mead <- nelder_mead_result$par

# Compare all methods with the true beta values
comparison <- data.frame(
  Parameter = paste0("β", 0:9),
  True_Value = beta,
  Closed_Form_OLS = as.vector(beta_ols),
  GD_Estimate = beta_gd,
  LBFGS_Estimate = beta_lbfgs,
  NelderMead_Estimate = beta_nelder_mead
)

print(comparison)

# Compare convergence and final values
cat("\nL-BFGS-B Convergence:\n")
cat("Converged:", ifelse(lbfgs_result$convergence == 0, "Yes", "No"), "\n")
cat("Final SSR:", lbfgs_result$value, "\n")
cat("Number of function evaluations:", lbfgs_result$counts[1], "\n")
cat("Number of gradient evaluations:", lbfgs_result$counts[2], "\n")

cat("\nNelder-Mead Convergence:\n")
cat("Converged:", ifelse(nelder_mead_result$convergence == 0, "Yes", "No"), "\n")
cat("Final SSR:", nelder_mead_result$value, "\n")
cat("Number of function evaluations:", nelder_mead_result$counts[1], "\n")

# Calculate differences between methods
cat("\nDifferences Between Methods:\n")
lbfgs_vs_nm <- round(sum((beta_lbfgs - beta_nelder_mead)^2), 8)
lbfgs_vs_ols <- round(sum((beta_lbfgs - as.vector(beta_ols))^2), 8)
nm_vs_ols <- round(sum((beta_nelder_mead - as.vector(beta_ols))^2), 8)

cat("Sum of squared differences (L-BFGS vs Nelder-Mead):", lbfgs_vs_nm, "\n")
cat("Sum of squared differences (L-BFGS vs Closed-Form OLS):", lbfgs_vs_ols, "\n")
cat("Sum of squared differences (Nelder-Mead vs Closed-Form OLS):", nm_vs_ols, "\n")

# Do the answers from L-BFGS-B and Nelder-Mead differ?
# ANSWER: Yes, the answers from L-BFGS-B and Nelder-Mead differ slightly. The Nelder-Mead method seems to give a slightly under estimate for
# positive coefficients and slightly over estimate for negative coefficients compared to the L-BFGS-B method. 

# ~~~MAXIMUM LIKELIHOOD FROM L-BFGS~~~

# Now we'll compute the Maximum Likelihood Estimate (MLE) using L-BFGS

## Our objective function: negative log-likelihood of normal model
objfun <- function(theta, y, X) {
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  loglike <- -sum(-0.5 * (log(2 * pi * (sig^2)) + ((y - X %*% beta) / sig)^2))
  return(loglike)
}

## Gradient of the objective function
gradient <- function(theta, y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  
  grad[1:(length(theta)-1)] <- -t(X) %*% (y - X %*% beta) / (sig^2)
  grad[length(theta)] <- length(y) / sig - crossprod(y - X %*% beta) / (sig^3)
  
  return(grad)
}

y <- Y  # from your simulation
X <- X

## Initial values: use closed-form beta + random sigma
ols_beta <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
theta0 <- c(ols_beta, runif(1))  # append random sigma start

## Optimization options
options <- list("algorithm" = "NLOPT_LD_LBFGS",
                "xtol_rel" = 1.0e-6,
                "maxeval" = 1e4)

## Run optimization with lower bound for sigma
result <- nloptr(x0 = theta0,
                 eval_f = objfun,
                 eval_grad_f = gradient,
                 lb = c(rep(-Inf, length(ols_beta)), 1e-8),  # Lower bound for sigma
                 opts = options,
                 y = y,
                 X = X)

## Extract results
betahat_nloptr <- result$solution[1:(length(result$solution) - 1)]
sigmahat_nloptr <- result$solution[length(result$solution)]

## Print results
cat("NLOPTR Results:\n")
cat("Convergence status:", result$status, "\n")
cat("Final objective value:", result$objective, "\n")
cat("Number of iterations:", result$iterations, "\n")

# Format comparison table
comparison <- data.frame(
  Parameter = c(paste0("β", 0:9), "σ"),
  True_Value = c(beta, 0.5),
  OLS_Estimate = c(ols_beta, NA),
  MLE_Estimate = c(betahat_nloptr, sigmahat_nloptr),
  MLE_vs_True_Diff = c(betahat_nloptr - beta, sigmahat_nloptr - 0.5),
  MLE_vs_OLS_Diff = c(betahat_nloptr - ols_beta, NA)
)

# Print comparison
print(comparison)

# Calculate the OLS estimate of sigma (residual standard error)
ols_resid <- y - X %*% ols_beta
ols_sigma <- sqrt(sum(ols_resid^2) / (length(y) - length(ols_beta)))
cat("\nOLS estimate of sigma (residual standard error):", ols_sigma, "\n")

# Sum of squared differences between MLE and OLS beta estimates
ss_diff <- sum((betahat_nloptr - ols_beta)^2)
cat("\nSum of squared differences between MLE and OLS beta estimates:", ss_diff, "\n")

# Create a plot comparing the estimates
barplot(
  height = rbind(beta, ols_beta, betahat_nloptr),
  beside = TRUE,
  col = c("darkblue", "darkred", "darkgreen"),
  names.arg = paste0("β", 0:9),
  main = "Comparison of Beta Estimates",
  ylab = "Value",
  legend.text = c("True Beta", "OLS Estimate", "MLE Estimate")
)

#~~~LM() OLS MODEL AGAIN~~~
# Fit the linear model using lm() function
# Fit the linear model
lm_model <- lm(Y ~ X - 1)

# Export to a simple .tex file
modelsummary(
  list("OLS Model" = lm_model),
  output = "lm_results.tex",
  title = "Linear Model Results",
  stars = TRUE,
  booktabs = TRUE,
  latex_engine = "default",
  float_env = "table",
  note = "Note: $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$"
)

library(stargazer)

stargazer(
  lm_model,
  type = "latex",
  out = "lm_results_stargazer.tex",
  title = "Linear Model Results",
  digits = 4,
  column.labels = c("OLS Model"),
  dep.var.labels = c("Y"),
  header = FALSE,  # Removes default header
  table.placement = "H",  # Forces the table to be placed exactly where it appears in the text
  float = FALSE,  # Don't wrap in a floating environment (important for input)
  no.space = TRUE  # Reduces extra space
)
