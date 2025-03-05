library(rvest)
library(dplyr)

## Get Failed Bank data via webscraping
# Define FDIC failed bank list URL
url <- "https://www.fdic.gov/bank-failures/failed-bank-list/"

# Read the webpage
page <- read_html(url)

# Extract the failed bank names (they are in a table)
failed_banks <- page %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>%
  .[[1]]  # Extract the first table

# View the first few rows
head(failed_banks)

# Create a csv file from the data
write.csv(failed_banks, "failed_banks.csv", row.names = FALSE)



## Get bank asset and loan data via API

library(fdicdata)
library(dplyr)

# Get metadata for all institutions
institutions <- getInstitutionsAll()

# Convert ENDEFYMD to Date format if necessary
institutions$ENDEFYMD <- as.Date(institutions$ENDEFYMD, format = "%m/%d/%Y")

# Set the start of your range
range_start <- as.Date("2015-01-01")

# Filter institutions based on ENDEFYMD (only include those after the start of the range)
filtered_institutions <- institutions %>%
  filter(ENDEFYMD > range_start)

# Extract all unique FED_RSSDs for the filtered institutions
all_IDRSSDs <- unique(filtered_institutions$FED_RSSD)

# Function to pull financial data for a single IDRSSD
pull_data <- function(IDRSSD) {
  print(paste("Pulling data for IDRSSD:", IDRSSD))
  result <- getFinancials(IDRSSD_or_CERT = IDRSSD, 
                          metrics = c("IDRSSD", "ASSET", "LNATRES", "LNATRESR", "LNLSNET", 
                                      "LNRENRES", "LNRERES", "LNCI", "LNCON"), 
                          IDRSSD = TRUE,  # TRUE because using IDRSSD
                          range = c("2015-01-01", "2025-01-01"), 
                          limit = 10000)  # Set high limit to retrieve a larger dataset
  
    return(result)  
}

# Loop through all filtered IDRSSDs and pull data for each one
assets_list <- lapply(all_IDRSSDs, pull_data)

# Combine all data into one dataframe (remove NULL entries if any)
all_assets <- bind_rows(assets_list)

# Check the number of rows in the combined data
print(nrow(all_assets))