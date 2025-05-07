* This code downloads the correct FRY9 and audit year data for ECON5253 Semester Project;


* Pull in the call report data indicating the level of audit the institution received. This will be joined with the FDIC dataset.;
* Sign on to WRDS;
%macro signon_cloud;
    %let wrds=wrds-cloud.wharton.upenn.edu 4016;
    options comamid=TCP remote=WRDS;
    signon username=lwardwell pw=%STR(AqLZKRwv2V5FgER);
%mend signon_cloud;

%signon_cloud;
rsubmit;

* run the call report data indicating the level of assurance engaged by the bank in the previous year, restricted to those banks that had an audit;
data audit_level;
	set bank.wrds_call_rcon_1;
	year=year(rssd9999);
	month=month(rssd9999);
	keep rssd9999 year rssd9001 rssd9017 rcon6724;
	if 1988 <= year <=2024 and month = 3;
	run; 

* convert rssd id value to character for best join;
data audit_level; 
	set audit_level;
	rssd9001_char=put(rssd9001, z7.);
	rename rcon6724 = audit_type;
run;
 
* create audit year value;
data audit_year;
	set audit_level;
	audit_year = year-1; 

run;
 
* save the audit_year dataset;
libname mydata '/home/ou/lwardwell/sasuser.v94/ECON5253';
 
data mydata.audit_year;
    set audit_year;
run;

/* LOAD IN THE DOWNLOADED DATASET AND START YOUR WORK HERE*/
* Make sure to download the dataset from the WRDS SAS Studio here;

/* Set the library to access the file location */
libname project "C:\Users\lward\Documents\ECON - Data Science for Economists\ProjectDocs\SASCode";

/* Load the dataset */
data work.audit_year;
    set project.audit_year;
run;

/* DATA CHECKING STEPS*/

/* Create a separate data step to view sample records with missing values */
data missing_audit;
    set audit_year;
    where audit_type is missing;
run;

/* Filter by bank ID in the audit_year dataset */
proc print data=audit_year;
    where rssd9001_char = '3267738' or rssd9001_char = '3267738' or rssd9001 = 3267738;
    title "Records for Selected Bank ID";
    var year rssd9001 rssd9001_char audit_type rssd9017;
run;

/* REDO RSSD_char because I'm pretty sure I did it wrong the first time. */
* convert rssd id value to character for best join;
data audit_year; 
	set audit_year;
	rssd9001_char=put(input(strip(rssd9001), BEST7.), z7.);
run;


/* AUDIT NORMALIZATION STEPS*/
/* YOU WILL NEED TO UPDATE THIS WITH A FINAL FIELD THAT APPROPRIATELY ADDRESSES INTEGRATED AUDIT */
/* Create the audit_normalized field based on the specified conditions */
data audit_normalized;
    set work.audit_year;
    
    /* Determine time period: 1989-2000, 2001-2016, or post-2016 */
    period_1989_2000 = (year <= 2000);
    period_2001_2016 = (2001 <= year <= 2016);
    period_post_2017 = (year >= 2017);
    
    /* Create audit_normalized field according to conditions */
    if period_1989_2000 then do;
        /* 1989-2000 rules: use original codes with their historical meaning */
        if strip(audit_type) = '1' then audit_normalized = 1;
        else if strip(audit_type) = '2' then audit_normalized = 2;
        else if strip(audit_type) = '3' then audit_normalized = 3;
        else if strip(audit_type) = '4' then audit_normalized = 4;
        else if strip(audit_type) = '5' then audit_normalized = 5;
        else if strip(audit_type) = '6' then audit_normalized = 6;
        else if strip(audit_type) = '7' then audit_normalized = 7;
        else if strip(audit_type) = '8' then audit_normalized = 8;
        else if strip(audit_type) = '9' then audit_normalized = 9;
    end;
    else if period_2001_2016 then do;
        /* 2001-2016 rules: use codes with meanings for this period */
        if strip(audit_type) = '1' then audit_normalized = 1;
        else if strip(audit_type) = '2' then audit_normalized = 2;
        else if strip(audit_type) = '3' then audit_normalized = 3;
        else if strip(audit_type) = '4' then audit_normalized = 4;
        else if strip(audit_type) = '5' then audit_normalized = 5;
        else if strip(audit_type) = '6' then audit_normalized = 6;
        else if strip(audit_type) = '7' then audit_normalized = 7;
        else if strip(audit_type) = '8' then audit_normalized = 8;
        else if strip(audit_type) = '9' then audit_normalized = 9;
    end;
    else do;
       /* Post-2016 rules: apply the normalization from the first request */
    	if strip(audit_type) in ('1', '1a', '2a') then audit_normalized = 1;
    	else if strip(audit_type) in ('2', '1b', '2b') then audit_normalized = 2;
    	else if strip(audit_type) = '3' then audit_normalized = 3;
    	else if strip(audit_type) = '4' then audit_normalized = 4;
    	else if strip(audit_type) = '5' then audit_normalized = 5;
    	else if strip(audit_type) = '6' then audit_normalized = 6;
    	else if strip(audit_type) = '7' then audit_normalized = 7;
    	else if strip(audit_type) = '8' then audit_normalized = 8;
    	else if strip(audit_type) = '9' then audit_normalized = 9;
end;
        
    /* Create assurance_level field based on audit_normalized */
    if period_1989_2000 then do;
        /* 1989-2000 mapping based on provided guidance */
        select (audit_normalized);
            when (1) assurance_level = 'Integrated';       /* Integrated audit of either bank or HC */
            when (2) assurance_level = 'FS_Only';         /* FS only audit of either bank or HC */
            when (3) assurance_level = 'CPA_DE';        /* Directors' exam by CPA firm */
            when (4) assurance_level = 'Other_DE';      /* Directors' exam by other auditors */
            when (5) assurance_level = 'Review';      /* Review by external auditors */
            when (6) assurance_level = 'Compilation'; /* Compilation by external auditors */
            when (7) assurance_level = 'Other';          /* Other audit procedures */
            when (8) assurance_level = 'None';             /* No external audit work */
            when (9) assurance_level = 'Unknown';              /* Not specified in your guidance */
            otherwise assurance_level = 'Unknown';
        end;
    end;
    else if period_2001_2016 then do;
        /* 2001-2016 mapping based on your provided definitions */
        select (audit_normalized);
            when (1) assurance_level = 'Audit_Bank';
            when (2) assurance_level = 'Audit_HC';
            when (3) assurance_level = 'IC_Attest';        /* New code for this period */
            when (4) assurance_level = 'CPA_DE';
            when (5) assurance_level = 'Other_DE';
            when (6) assurance_level = 'Review';
            when (7) assurance_level = 'Compilation';
            when (8) assurance_level = 'Other';
            when (9) assurance_level = 'None';
            otherwise assurance_level = 'Unknown';
        end;
    end;
    else do;
        /* Post-2016 mapping per your original requirements */
        select (audit_normalized);
            when (1) assurance_level = 'Integrated_Audit';
            when (2) assurance_level = 'FSOnly_Audit';
            when (3) assurance_level = 'ERROR';            /* Not to be used after 2016 */
            when (4) assurance_level = 'CPA_DE';
            when (5) assurance_level = 'Other_DE';
            when (6) assurance_level = 'Review';
            when (7) assurance_level = 'Compilation';
            when (8) assurance_level = 'Other';
            when (9) assurance_level = 'None';
            otherwise assurance_level = 'Unknown';
        end;
    end;
    
    /* Add the new assurance_basic field */
    select (audit_normalized);
        when (1, 2) assurance_basic = 'AUDIT';
        when (3, 4, 5) assurance_basic = 'DE';
        when (6) assurance_basic = 'REVIEW';
        when (7) assurance_basic = 'COMP';
        when (8) assurance_basic = 'OTHER';
        otherwise assurance_basic = 'NONE';
    end;
run;

/* ERROR CHECKING STEPS*/

/* Get detailed distribution of audit_type = '3' by year */
proc freq data=audit_normalized;
    where strip(audit_type) = '3';
    tables year / out=error_by_year;
    title "Distribution of Audit Type '3' by Year";
run;

/* Create a bar chart for audit_type = '3' by year */
/* (Expect to see many 3 values pre-2017, with a declining trend. Should see no 3 values after 2017)*/
proc sgplot data=error_by_year;
    vbar year / response=count;
    xaxis label="Year";
    yaxis label="Count of Audit Type '3'";
    title "Audit Type '3' by Year";
run;

/* Examine the data for missing values of audit_normalized*/

proc sql;
    /* Count total observations with missing audit_normalized */
    title "Count of Observations with Missing audit_normalized";
    select count(*) as Missing_Count 
    from audit_normalized
    where audit_normalized is missing;
    
    /* Get distribution of audit_type values where audit_normalized is missing */
    title "Distribution of audit_type Values where audit_normalized is Missing";
    select audit_type, count(*) as Count
    from audit_normalized
    where audit_normalized is missing
    group by audit_type
    order by Count desc;
    
    /* Look at the distribution by year */
    title "Distribution by Year of Missing audit_normalized Values";
    select year, count(*) as Count
    from audit_normalized
    where audit_normalized is missing
    group by year
    order by year;
    
quit;

/* Create a separate data step to view sample records with missing values */
data missing_samples;
    set audit_normalized;
    where audit_normalized is missing;
run;

/* View the first 20 records with missing values */
proc print data=missing_samples(obs=20);
    title "Sample of Records with Missing audit_normalized (First 20)";
    var rssd9001_char year audit_type pre_2017;
run;

/* Print a log of all possible values of audit_type in the dataset */
proc freq data=audit_normalized;
    title "Distribution of All audit_type Values";
    tables audit_type / nocum nopercent;
run;


/* Filter audit_normalized dataset to only show audit_type='3' observations */
proc sort data=audit_normalized out=audit_type3;
    by year;
    where audit_type = '3';
run;

/* View the results */
proc print data=audit_type3;
    var RSSD9001 RSSD9017 year audit_type pre_2017 audit_normalized assurance_level assurance_basic;
    title "All Observations with audit_type = '3'";
run;

/* Get a frequency count by year to see distribution */
proc freq data=audit_type3;
    tables year / order=freq;
    title "Distribution of audit_type = '3' by Year";
run;



/* USE THIS CODE TO LOOK AT INDIVIDUAL CERT OBSERVATIONS AND DIAGNOSE PROBLEMS*/

/* Filter by bank ID in the audit_normalized dataset */
proc print data=audit_normalized;
    where rssd9001_char = '012311' or rssd9001_char = '12311' or rssd9001 = 12311;
    title "Records for Selected Bank ID";
    var year rssd9001 rssd9001_char audit_type audit_normalized assurance_level assurance_basic;
run;



/* GRAPH OF audit_normalized TREND*/

/* Calculate proportions of each audit_normalized type per year */
proc freq data=audit_normalized;
    tables year*audit_normalized / norow nocol nopercent out=audit_props outpct;
run;

/* Sort the results by year and audit_normalized for trend analysis */
proc sort data=audit_props;
    by year audit_normalized;
run;

/* Display the results */
proc print data=audit_props;
    var year audit_normalized count percent;
    title "Proportion of Each Audit Type by Year";
run;

/* Create a trend analysis with PROC SGPLOT */
proc sgplot data=audit_props;
    series x=year y=percent / group=audit_normalized lineattrs=(thickness=2);
    scatter x=year y=percent / group=audit_normalized markerattrs=(size=9);
    xaxis label="Year" grid;
    yaxis label="Percentage (%)" grid;
    title "Trend of Audit Types Over Time";
run;

/* Save the normalized data back to your project folder if needed */
data project.Audit_normalized_results;
    set audit_normalized;
run;

/*GRAPH OF assurance_basic TREND*/

/* Calculate proportions of each assurance_basic type per year */
proc freq data=audit_normalized;
    tables year*assurance_basic / norow nocol nopercent out=basic_props outpct;
run;

/* Sort the results by year and assurance_basic for trend analysis */
proc sort data=basic_props;
    by year assurance_basic;
run;

/* Display the results */
proc print data=basic_props;
    var year assurance_basic count percent;
    title "Proportion of Each Basic Assurance Type by Year";
run;

/* Create a trend analysis with PROC SGPLOT */
proc sgplot data=basic_props;
    series x=year y=percent / group=assurance_basic lineattrs=(thickness=2);
    scatter x=year y=percent / group=assurance_basic markerattrs=(size=9);
    xaxis label="Year" grid values=(2000 to 2023 by 2);
    yaxis label="Percentage (%)" grid;
    title "Trend of Basic Assurance Types Over Time";
    keylegend / title="Assurance Type" position=right;
run;

/* Export audit_normalized to CSV */
proc export data=work.audit_normalized
    outfile='C:\Users\lward\Documents\ECON - Data Science for Economists\ProjectDocs\CleanedData\audit_normalized.csv'
    dbms=csv
    replace;
run;

/* OUTPUT THE JOINED FILE TO THE SPECIFIED FOLDER */
/* Set the library path to your specified output directory */
libname cleandat "C:\Users\lward\Documents\ECON - Data Science for Economists\ProjectDocs\CleanedData";

/* Output the datasets to the specified folder */
data cleandat.audit_normalized;
    set work.audit_normalized;
run;


/* ERROR CHECKING ON 2017*/
/* Filter the audit_normalized dataset for 2017 observations with assurance_basic = 'NONE' */
proc print data=work.audit_normalized;
    where year = 2017 and assurance_basic = 'NONE';
    title "2017 Observations with assurance_basic = 'NONE'";
    var rssd9001 rssd9001_char year audit_type audit_normalized assurance_level assurance_basic;
run;

/* Get a count of these observations */
proc sql;
    title "Count of 2017 Observations with assurance_basic = 'NONE'";
    select count(*) as Count
    from work.audit_normalized
    where year = 2017 and assurance_basic = 'NONE';
quit;

/* Check the logic that assigns assurance_basic for these observations */
proc print data=work.audit_normalized;
    where year = 2017 and assurance_basic = 'NONE';
    title "Detailed Review of 2017 NONE Assurance Cases";
    var rssd9001_char year audit_type audit_normalized assurance_level assurance_basic;
run;

/* Check the distribution of audit_type values for these observations */
proc freq data=work.audit_normalized;
    where year = 2017 and assurance_basic = 'NONE';
    tables audit_type / nocum nopercent;
    title "Distribution of audit_type Values for 2017 NONE Assurance Cases";
run;
