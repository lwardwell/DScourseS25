## Get bank asset and loan data via API

library(fdicdata)
library(dplyr)

# Get metadata for all institutions
institutions <- getInstitutionsAll()

# Convert ENDEFYMD to Date format if necessary
institutions$ENDEFYMD <- as.Date(institutions$ENDEFYMD, format = "%m/%d/%Y")

# Convert INSDATE to Date format if necessary
institutions$INSDATE <- as.Date(institutions$INSDATE, format = "%m/%d/%Y")

# Set date filters
range_endefymd <- as.Date("2020-12-31")  # Only include institutions that ended after this date
range_insdate <- as.Date("2021-01-01")   # Only include institutions that started before this date

# Filter institutions based on ENDEFYMD (only include those after the start of the range)
filtered_institutions <- institutions %>%
  filter(ENDEFYMD > range_endefymd & INSDATE < range_insdate)

# Extract all unique FED_RSSDs for the filtered institutions
all_IDRSSDs <- unique(filtered_institutions$FED_RSSD)

# Function to pull financial data for a single IDRSSD
pull_data <- function(IDRSSD) {
  print(paste("Pulling data for IDRSSD:", IDRSSD))
  result <- getFinancials(IDRSSD_or_CERT = IDRSSD, 
                          metrics = c("IDRSSD", "ASSET"), 
                          IDRSSD = TRUE,  # TRUE because using IDRSSD
                          range = c("2020-12-01", "2021-01-01"), 
                          limit = 10000)  # Set high limit to retrieve a larger dataset
  
  return(result)  
}


# Loop through all filtered IDRSSDs and pull data for each one
assets_list <- lapply(all_IDRSSDs, pull_data)

# Combine all data into one dataframe (remove NULL entries if any)
all_assets <- bind_rows(assets_list)

# Add log-transformed assets column (handling potential 0 or NA values)
all_assets <- all_assets %>%
  mutate(log_ASSET = ifelse(ASSET > 0, log(ASSET), NA))

# Check the number of rows in the combined data
print(nrow(all_assets))


# Ensure the date column is in Date format
all_assets$DATE <- as.Date(all_assets$DATE)

# Filter for only 12/31/2020 observations
all_assets_12312020 <- all_assets %>%
  filter(DATE == as.Date("2020-12-31"))

# Check the number of rows after filtering
print(nrow(all_assets_12312020))

# Check if it was added successfully
head(all_assets_12312020)

# load ggplot2 to make visualization

library(ggplot2)

ggplot(all_assets_12312020, aes(x = log_ASSET)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Log Bank Assets", x = "Log(Total Assets)", y = "Count") +
  theme_minimal()

# Calculate the mean and standard deviation of log_ASSET
mean_log_asset <- mean(all_assets_12312020$log_ASSET, na.rm = TRUE)
sd_log_asset <- sd(all_assets_12312020$log_ASSET, na.rm = TRUE)


# Calculate the z-scores for each observation
all_assets <- all_assets_12312020 %>%
  mutate(z_score = (log_ASSET - mean_log_asset) / sd_log_asset)

# Check if z-scores were added successfully
head(all_assets_12312020)

# Pull deposits data for the same institutions

pull_dep <- function(IDRSSD) {
  print(paste("Pulling deposits for IDRSSD:", IDRSSD))
  result <- getFinancials(IDRSSD_or_CERT = IDRSSD, 
                          metrics = c("IDRSSD", "DEP"), 
                          IDRSSD = TRUE,  # TRUE because using IDRSSD
                          range = c("2020-12-01", "2021-01-01"), 
                          limit = 10000)  # Set high limit to retrieve a larger dataset
  
  return(result)  
}


# Loop through all filtered IDRSSDs and pull data for each one
deposits_list <- lapply(all_IDRSSDs, pull_dep)

# Combine all data into one dataframe (remove NULL entries if any)
all_deposits <- bind_rows(deposits_list)

# Check the number of rows in the combined data
print(nrow(all_deposits))

# Add log-transformed assets column (handling potential 0 or NA values)
all_deposits <- all_deposits %>%
  mutate(log_DEP = ifelse(DEP > 0, log(DEP), NA))

# Ensure the date column is in Date format
all_deposits$DATE <- as.Date(all_deposits$DATE)

# Filter for only 12/31/2020 observations
all_deposits_12312020 <- all_deposits %>%
  filter(DATE == as.Date("2020-12-31"))

# Check the number of rows after filtering
print(nrow(all_deposits_12312020))

# Make a boxplot of log_DEP
ggplot(all_deposits_12312020, aes(y = log_DEP)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Log Bank Deposits", y = "Log(Total Deposits)") +
  theme_minimal()

# Combine the all_assets_12312020 and all_deposits_12312020 dataframes on IDRSSD
combined_data <- inner_join(all_assets_12312020, all_deposits_12312020, by = "IDRSSD")

# filter combined data to only banks under $5B in assets (community banks)
community_banks <- combined_data %>%
  filter(ASSET < 5e9)

# Make a scatterplot of log_DEP vs. log_ASSET for community banks
ggplot(community_banks, aes(x = log_ASSET, y = log_DEP)) +
  geom_point(color = "blue") +
  labs(title = "Log Total Deposits vs. Log Total Assets for Community Banks",
       x = "Log Total Assets", y = "Log Total Deposits") +
  theme_minimal()

# Calculate the correlation between log_DEP and log_ASSET for community banks
correlation <- cor(community_banks$log_DEP, community_banks$log_ASSET, use = "complete.obs")
print(correlation)

