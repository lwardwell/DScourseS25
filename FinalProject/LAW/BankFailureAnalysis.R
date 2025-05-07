library(tidyverse)
library(stargazer)
library(fixest)
library(kableExtra)
library(modelsummary)
library(ggplot2)

## ANALYSIS OF THE EFFECT OF AUDIT ON THE PROBALILTY OF BANK FAILURE ##

## 1. Load the data
lgassets <- read.csv("C:/Users/lward/Documents/ECON - Data Science for Economists/ProjectDocs/CleanedData/large_assets.csv")
smassets <- read.csv("C:/Users/lward/Documents/ECON - Data Science for Economists/ProjectDocs/CleanedData/small_assets.csv")

## 1.1 Data Cleaning
# Add a failure_year variable to both datasets (Failure occurs in year t+1 in these datasets)

# For lgassets dataset
lgassets <- lgassets %>%
  mutate(failure_year = as.numeric(as.character(year)) + 1)

# For smassets dataset
smassets <- smassets %>%
  mutate(failure_year = as.numeric(as.character(year)) + 1)

##1.1.1 Failures Stats
# Create a table showing failures by year

# Define asset size categories precisely for the failure table
small_failures_by_year <- smassets %>%
  filter(Failurep1 == 1) %>%
  count(failure_year) %>%
  rename(`Small Banks (<$500M)` = n)

# For large banks, we need to filter to only include banks with assets >= $500M
large_failures_by_year <- lgassets %>%
  filter(Failurep1 == 1) %>%
  # Filter for banks with assets >= $500M
  filter(ASSET >= 500000) %>%
  count(failure_year) %>%
  rename(`Large Banks (>=$500M)` = n)

# Create a combined failures by year table
# Check what's actually in the failure_year variables
print("Small failures years:")
print(small_failures_by_year$failure_year)

print("Large failures years:")
print(large_failures_by_year$failure_year)

# Create a combined failures by year table with the actual years
# First, identify the correct range of years
min_year <- min(c(small_failures_by_year$failure_year, 
                  large_failures_by_year$failure_year), 
                na.rm = TRUE)

max_year <- max(c(small_failures_by_year$failure_year, 
                  large_failures_by_year$failure_year), 
                na.rm = TRUE)

# Create a sequence of the actual years
all_years <- data.frame(
  failure_year = seq(min_year, max_year, by = 1)
)

# Join with the failure counts
failures_by_year <- all_years %>%
  left_join(small_failures_by_year, by = "failure_year") %>%
  left_join(large_failures_by_year, by = "failure_year")

# Replace NA values with 0
failures_by_year[is.na(failures_by_year)] <- 0

# Add a total column
failures_by_year <- failures_by_year %>%
  mutate(Total = `Small Banks (<$500M)` + `Large Banks (>=$500M)`)

# Convert failure_year to character for better formatting
failures_by_year <- failures_by_year %>%
  mutate(failure_year = as.character(failure_year))

# Now calculate totals and add them as a new row
totals_row <- failures_by_year %>%
  summarise(
    failure_year = "Total",
    `Small Banks (<$500M)` = sum(`Small Banks (<$500M)`),
    `Large Banks (>=$500M)` = sum(`Large Banks (>=$500M)`),
    Total = sum(Total)
  )

# Combine the original data with the totals row
failures_with_totals <- bind_rows(failures_by_year, totals_row)

# Create the formatted table
failures_table <- failures_with_totals %>%
  kable(caption = "Bank Failures by Year",
        col.names = c("Year", "Small Banks (<$500M)", 
                      "Large Banks (>\\$500M and <\\$1B)", 
                      "Total"),
        align = "c",
        escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Number of Failures" = 3)) %>%
  row_spec(nrow(failures_by_year) + 1, bold = TRUE, background = "#f2f2f2")

# Display the table
failures_table

# Save the table to HTML
failures_table %>%
  save_kable(file = "failures_by_year.html")

# Optional: You can also create a simple bar chart visualization

failures_long <- failures_by_year %>%
  filter(failure_year != "Total") %>%
  select(-Total) %>%
  # Use the actual column names from your dataframe for the pivot
  tidyr::pivot_longer(
    cols = c(`Small Banks (<$500M)`, `Large Banks (>=$500M)`),
    names_to = "Bank_Size", 
    values_to = "Failures"
  )

failures_plot <- ggplot(failures_long, aes(x = failure_year, y = Failures, fill = Bank_Size)) +
  geom_bar(stat = "identity", position = "dodge") +
  # Update the values to match your actual column names or create labels 
  scale_fill_manual(
    values = c(`Small Banks (<$500M)` = "#1f77b4", `Large Banks (>=$500M)` = "#ff7f0e"),
    # Optional: You can create shorter labels for the legend
    labels = c(`Small Banks (<$500M)` = "Small Banks (<$500M)", 
               `Large Banks (>=$500M)` = "Large Banks (>$500M)")
  ) +
  labs(title = "Bank Failures by Year",
       x = "Year",
       y = "Number of Failures",
       fill = "Bank Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(failures_plot)

# Save the plot if needed
ggsave("bank_failures_plot.png", failures_plot, width = 8, height = 6)

##1.1.2 Factor Variables
# Make sure Year and FED variables are factors
lgassets$year <- as.factor(lgassets$year)
lgassets$FED <- as.factor(lgassets$FED)
smassets$year <- as.factor(smassets$year)
smassets$FED <- as.factor(smassets$FED)

## 1.2 Load in Forgotten Deposits Variable
# (I forgot to include the total deposits variable when preparing the data, because I am a complete dodo-bird.)
deposits<-read.csv("C:/Users/lward/Documents/ECON - Data Science for Economists/ProjectDocs/CleanedData/deposits.csv")

# Join the deposits data to both datasets, matching on CERT and REPDTE
lgassets <- lgassets %>%
  left_join(deposits, by = c("CERT" = "CERT", "REPDTE" = "REPDTE"))
smassets <- smassets %>%
  left_join(deposits, by = c("CERT" = "CERT", "REPDTE" = "REPDTE"))

## 1.3 Create the br_ratio variable to deflate BRO by DEP
lgassets <- lgassets %>%
  mutate(br_ratio = BRO / DEP)
smassets <- smassets %>%
  mutate(br_ratio = BRO / DEP)

## 1.3.1 Create the lgassets integrated dataset
# Trim the large assets dataset to only include observations from 2017 or later
lgassets_integrated <- lgassets %>% filter(REPDTE >= 20171231)

## 1.4 Create summary statistics tables
# Define the variables of interest for both datasets
vars_of_interest <- c("Failurep1", "audit", "size", "co", "npa", "comre", 
                             "mortgage", "comind", "cons", "capital", "all", 
                             "br_ratio", "SUBCHAPS")

# Create summary statistics for large assets
lgassets_summary <- lgassets %>%
  select(all_of(vars_of_interest)) %>%
  summary()

# Create summary statistics for small assets
smassets_summary <- smassets %>%
  select(all_of(vars_of_interest)) %>%
  summary()

## 1.5 Add information on the year range and FRB regions
# First, get the year range for small assets
min_year_small <- min(as.numeric(as.character(smassets$year)))
max_year_small <- max(as.numeric(as.character(smassets$year)))
year_range_small <- paste0(min_year_small, "-", max_year_small)

# Get the FED district range for small assets
fed_districts_small <- sort(unique(as.numeric(as.character(smassets$FED))))
fed_range_small <- paste(fed_districts_small, collapse = ", ")

# Create the note text for small assets
note_small <- paste0("Sample period: ", year_range_small, ". Federal Reserve Districts: ", fed_range_small, ".")

# Do the same for large assets (integrated)
min_year_large <- min(as.numeric(as.character(lgassets_integrated$year)))
max_year_large <- max(as.numeric(as.character(lgassets_integrated$year)))
year_range_large <- paste0(min_year_large, "-", max_year_large)

fed_districts_large <- sort(unique(as.numeric(as.character(lgassets_integrated$FED))))
fed_range_large <- paste(fed_districts_large, collapse = ", ")

note_large <- paste0("Sample period: ", year_range_large, ". Federal Reserve Districts: ", fed_range_large, ".")

# Then modify your stargazer calls to include these notes
stargazer(as.data.frame(smassets[, vars_of_interest]), 
          title = "Summary Statistics - Bank < $500MM in Assets",
          digits = 3,
          covariate.labels = c("Failure t+1", "Audit", "Size", "Charge Offs", 
                               "Non-performing Assets", "Commercial RE", 
                               "Mortgage", "Comm/Ind", "Consumer", 
                               "Capital", "ALL", "Brokered Deposits", "Subchapter S"),
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max", "n"),
          notes = note_small,
          notes.align = "l",  # align notes to the left
          out = "small_assets_summary.html")

# Do the same for large assets (integrated)
stargazer(as.data.frame(lgassets_integrated[, vars_of_interest]), 
          title = "Summary Statistics - Banks < $1B in Assets",
          digits = 3,
          covariate.labels = c("Failure t+1", "Audit", "Size", "Charge Offs", 
                               "Non-performing Assets", "Commercial RE", 
                               "Mortgage", "Comm/Ind", "Consumer", 
                               "Capital", "ALL", "Brokered Deposits", "Subchapter S"),
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max", "n"),
          notes = note_large,
          notes.align = "l",  # align notes to the left
          out = "large_assets_integrated.html")


## 2. Regression models
## 2.1 Small Assets - Logit Model with Year and FED fixed effects
logit_small_fe<- feglm(Failurep1 ~ audit + size + co + npa + comre + mortgage + 
                         comind + cons + capital + all + br_ratio + SUBCHAPS | year+FED, 
                       family = "logit", 
                       data = smassets,
                       cluster = "CERT")  # 
summary(logit_small_fe)

## 2.2 Small Assets - Logit Model with Year x FED interaction
logit_small_fe_interaction <- feglm(Failurep1 ~ audit + size + co + npa + comre + mortgage + 
                                      comind + cons + capital + all + br_ratio + SUBCHAPS | year^FED, 
                                    family = "logit", 
                                    data = smassets,
                                    cluster = "CERT")  
summary(logit_small_fe_interaction)

## 2.2 Large Assets - Logit Model (all audit)
logit_large_fe <- feglm(Failurep1 ~ audit + size + co + npa + comre + mortgage + 
                             comind + cons + capital + all + br_ratio + SUBCHAPS | year+FED, 
                        family = "logit",
                        data = lgassets, 
                        cluster = "CERT")  
summary(logit_large_fe)

## 2.2.1 Large Assets - Logit Model (post 2017 integrated)
logit_large_2017 <- feglm(Failurep1 ~ integrated + size + co + npa + comre + mortgage + 
                            comind + cons + capital + all + br_ratio + SUBCHAPS | year+FED, 
                          family = "logit",
                          data = lgassets_integrated, 
                          cluster = "CERT")
summary(logit_large_2017)

## Look at large failures from lgassets_trimmed only
lgfailure <-lgassets_integrated %>% 
  filter(Failurep1 == 1) 


## 3. Create tables of regression results
# Create a list of models
models_list <- list(
  "Small Assets" = logit_small_fe,
  "Large Assets (Integrated)" = logit_large_2017
)

# Create custom coefficient labels
coef_labels <- c(
  "audit" = "Audit",
  "integrated" = "Integrated Audit",
  "fsonly" = "F/S Only Audit",
  "size" = "Size",
  "co" = "Charge-offs",
  "npa" = "Non-performing Assets",
  "comre" = "Commercial RE",
  "mortgage" = "Mortgage",
  "comind" = "Comm/Ind",
  "cons" = "Consumer",
  "capital" = "Capital",
  "all" = "ALL",
  "br_ratio" = "Brokered Deposits Ratio",
  "SUBCHAPS" = "Subchapter S"
)

# Create a custom function to format p-values with stars
stars <- function(x) {
  case_when(
    x < 0.01 ~ paste0(round(x, 3), "***"),
    x < 0.05 ~ paste0(round(x, 3), "**"),
    x < 0.1 ~ paste0(round(x, 3), "*"),
    TRUE ~ as.character(round(x, 3))
  )
}

# Generate the table
# Create a list of models with plain text names (avoid special characters)
models_list <- list(
  "Banks under 500MM" = logit_small_fe,
  "Banks 500MM to 1B" = logit_large_2017
)

# Generate the table with these plain names
regression_table <- modelsummary(models_list,
                                 title = "Logistic Regression Results for Bank Failure Probability",
                                 stars = TRUE,
                                 coef_map = coef_labels,
                                 gof_omit = "AIC|BIC|Log|RMSE",
                                 output = "kableExtra",
                                 escape = FALSE, 
                                 note = "Note: *p<0.1; **p<0.05; ***p<0.01. Standard errors clustered by CERT. Year and FED fixed effects included in all models.",
                                 fmt = "%.3f") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                latex_options = c("scale_down", "hold_position"),
                full_width = FALSE)

# Display the table
regression_table



# If you want to save the table to an HTML file:
modelsummary(models_list,
             title = "Logistic Regression Results for Bank Failure Probability",
             stars = TRUE,
             coef_map = coef_labels,
             gof_omit = "AIC|BIC|Log|RMSE",
             output = "html",
             note = "Note: *p<0.1; **p<0.05; ***p<0.01. Standard errors clustered by CERT. Year and FED fixed effects included in all models.",
             fmt = "%.3f") %>%
  save_kable(file = "regression_results.html")

# Save the table to a LaTeX file
modelsummary(models_list,
             title = "Logistic Regression Results for Bank Failure Probability",
             stars = TRUE,
             coef_map = coef_labels,
             gof_omit = "AIC|BIC|Log|RMSE",
             output = "latex",
             note = "Note: *p<0.1; **p<0.05; ***p<0.01. Standard errors clustered by CERT. Year and FED fixed effects included in all models.",
             fmt = "%.3f") %>%
  save_kable(file = "regression_results.tex")
