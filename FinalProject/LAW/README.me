FDIC Bank Data Processing and Analysis Pipeline

This README explains how to run the collection of Python, SAS, and R scripts for processing and analyzing FDIC bank data. The pipeline is designed to be executed in a specific order to ensure proper data flow.
Overview
This project consists of:

Python Notebook for downloading FDIC data
SAS scripts for processing and combining the data
R code for statistical analysis of bank failures

Execution Order
Step 1: Download FDIC Data (Python)
Execute the Jupyter notebook FDICDataDownload.ipynb first to download the necessary FDIC financial data files.
jupyter notebook FDICDataDownload.ipynb
This notebook:

Downloads financial reports from the FDIC SDI API
Handles pagination for years with more than 10,000 records - Please note, the FDIC API limits downloads to 10,000 records at a time. For years prior to 2000 with greater than 10,000 records, the code downloads a second file offset to position 10,001 to capture the remaining records. All years 2000 forward have < 10,000 records. 
Creates separate files for liabilities, non-performing assets, assets, charge-offs, and financial ratios - The FDIC API links used are for different standard reports, this code pulls the full report for each type. 
Saves files to the specified output directory (default: C:/temp/fdic_reports)

Step 2: Process Data (SAS) - SAS Enterprise recommended. SAS Studio may run extremely slowly!

Run the SAS scripts in the following order:

DataCombo.sas - Imports and combines individual FDIC data files

Processes all downloaded CSV files
Standardizes date formats
Combines files across years
Creates a unified FDIC dataset


Audit_Year_Call.sas - Processes audit data

Downloads and normalizes financial reporting audit data
Creates standardized audit type classifications
Maps audit types across different reporting periods


FDICandAuditCombo.sas - Combines FDIC and audit data

Merges the FDIC financial data with audit information
Calculates derived financial variables (ratios, etc.)
Creates datasets for small and large assets
Outputs final datasets for analysis in R



Each SAS script should be run in SAS Enterprise. Be aware that these script may run extremely slowly in SAS Studio! Make sure to update the file paths in the scripts to match your environment.

Step 3: Analyze Data (R)
Finally, run the R script for statistical analysis:
Rscript BankFailureAnalysis.R
The R script:

Loads the processed data from the SAS output
Creates summary statistics
Runs logistic regression models to analyze bank failure probability
Generates tables and visualizations of the results
Outputs HTML tables summarizing the findings

Data Pathway

Python script downloads raw FDIC data (CSV files)
SAS scripts process and combine the data
R script analyzes the combined data

File Locations
Make sure to check and update the following file paths in each script:

Python: Output directory for downloaded files
SAS: Input paths for CSV files and output paths for processed data
R: Input paths for the final SAS-generated datasets

Requirements

Python with pandas, requests libraries
SAS software (SAS Enterprise Guide or SAS Studio)
R with tidyverse, stargazer, fixest, kableExtra, modelsummary packages

Notes

The FDIC API limits downloads to 10,000 observations at a time
Years prior to 2000 contain more than 10,000 observations and require multiple downloads
The SAS scripts handle merging these split files, both for the different report types and the records split across 2 files for the earlier years
The R analysis focuses on small banks (<$500M in assets) and larger banks (<$1B in assets)
