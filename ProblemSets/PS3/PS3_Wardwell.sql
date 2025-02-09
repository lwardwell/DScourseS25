.mode csv

-- Create the table for the data
CREATE TABLE IF NOT EXISTS FL_insurance (
    
policyID INTEGER,
statecode TEXT,
county TEXT,
eq_site_limit DOUBLE,
hu_site_limit DOUBLE,
fl_site_limit DOUBLE,
fr_site_limit DOUBLE,
tiv_2011 DOUBLE,
tiv_2012 DOUBLE,
eq_site_deductible DOUBLE,
hu_site_deductible DOUBLE,
fl_site_deductible DOUBLE,
fr_site_deductible DOUBLE,
point_latitude DOUBLE,
point_longitude DOUBLE,
line TEXT,
construction TEXT,
point_granularity DOUBLE 
);

-- No file path needed if running from inside the correct folder
.import FL_insurance_sample.csv FL_insurance

-- Print the first 10 rows
SELECT * FROM FL_insurance LIMIT 10;

-- List unique counties
SELECT DISTINCT county FROM FL_insurance;

-- Look at the variable names
PRAGMA table_info(FL_insurance)

-- Compute average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation FROM FL_insurance;

-- Frequency table of the construction variable
SELECT 
    construction, 
    COUNT(*) AS count, 
    COUNT(*) * 1.0 / (SELECT COUNT(*) FROM FL_insurance) AS fraction 
FROM FL_insurance 
GROUP BY construction;
