* This code processes individual FDIC data files into a single dataset for analysis.
* The process starts with importing of FDIC SDI data, downloaded from the SDI Banking Suite;

* Macro for files that end in standard "yyyy" format;
%macro import_files(type,first_year,last_year,keep_vars);
    * Import first dataset;
    %do year = &first_year %to &last_year;
        proc import datafile="C:\Users\lward\Documents\FDIC SDI Datasets\&type._&year..csv" 
            out=&type._&year
            dbms=csv 
            replace;
            getnames=yes;
        run;
        
        data &type._&year;
            set &type._&year;
            keep &keep_vars;
        run;
    %end;
%mend import_files;

* Macro for files that end in "yyyyb" format;
%macro import_files_b(type,first_year,last_year,keep_vars);
    %do year = &first_year %to &last_year;
        proc import datafile="C:\Users\lward\Documents\FDIC SDI Datasets\&type._&year.b.csv" 
            out=&type._&year.b
            dbms=csv 
            replace;
            getnames=yes;
        run;
        
        data &type._&year.b;
            set &type._&year.b;
            keep &keep_vars;
        run;
    %end;
%mend import_files_b;

* Import standard files;
%import_files(assets,1989,2023,ASSET CERT REPDTE LNATRES LNLSNET LNRENRES LNRERES LNCI LNCON FED SUBCHAPS);
%import_files(chgoffs,1989,2023, CERT REPDTE NTLNLS);
%import_files(nonperf,1989,2023, CERT REPDTE P3ASSET P9ASSET NAASSET);
%import_files(intexp,1989,2023,  CERT REPDTE ELNATR);
%import_files(ratios,1989,2023, CERT REPDTE IDT1RWAJR RBCRWAJ);
%import_files(liabilities,1989,2023, CERT REPDTE BRO);

* Import files ending in "b";
%import_files_b(assets,1989,1999,ASSET CERT REPDTE LNATRES LNLSNET LNRENRES LNRERES LNCI LNCON FED SUBCHAPS);
%import_files_b(chgoffs,1989,1999, CERT REPDTE NTLNLS);
%import_files_b(nonperf,1989,1999, CERT REPDTE P3ASSET P9ASSET NAASSET);
%import_files_b(intexp,1989,1999,  CERT REPDTE ELNATR);
%import_files_b(ratios,1989,1999, CERT REPDTE IDT1RWAJR RBCRWAJ);
%import_files_b(liabilities,1989,1999, CERT REPDTE BRO DEP);

* Preprocess both standard and "b" files;
%macro preprocess_files(type,first_year,last_year);
    %do year = &first_year %to &last_year;
        * Process standard files;
        data &type._&year;
            set &type._&year;
            
            /* Date processing code... */
            if vtype(REPDTE) = 'C' then do;
                date_num = MDY(substr(REPDTE, 5, 2), substr(REPDTE, 7, 2), substr(REPDTE, 1, 4));
            end;
            else do;
                date_char = put(REPDTE, 8.);
                date_num = MDY(substr(date_char, 5, 2), substr(date_char, 7, 2), substr(date_char, 1, 4));
                drop date_char;
            end;
            format date_num yymmddN.;
            drop REPDTE;
            rename date_num = REPDTE;
            
            /* Check and standardize P3ASSET only if this is nonperf type */
            %if &type = nonperf %then %do;
                if vtype(P3ASSET) = 'C' then P3ASSET_num = input(P3ASSET, best32.);
                else P3ASSET_num = P3ASSET;
                
                drop P3ASSET;
                rename P3ASSET_num=P3ASSET;
            %end;
        run;
        
        * Only process "b" files if they exist;
        %let dsid = %sysfunc(open(&type._&year.b));
        %if &dsid > 0 %then %do;
            %let rc = %sysfunc(close(&dsid));
            
            data &type._&year.b;
                set &type._&year.b;
                
                /* Date processing code... */
                if vtype(REPDTE) = 'C' then do;
                    date_num = MDY(substr(REPDTE, 5, 2), substr(REPDTE, 7, 2), substr(REPDTE, 1, 4));
                end;
                else do;
                    date_char = put(REPDTE, 8.);
                    date_num = MDY(substr(date_char, 5, 2), substr(date_char, 7, 2), substr(date_char, 1, 4));
                    drop date_char;
                end;
                format date_num yymmddN.;
                drop REPDTE;
                rename date_num = REPDTE;
                
                /* Check and standardize P3ASSET only if this is nonperf type */
                %if &type = nonperf %then %do;
                    if vtype(P3ASSET) = 'C' then P3ASSET_num = input(P3ASSET, best32.);
                    else P3ASSET_num = P3ASSET;
                    
                    drop P3ASSET;
                    rename P3ASSET_num=P3ASSET;
                %end;
            run;
        %end;
    %end;
%mend preprocess_files;

%preprocess_files(assets, 1989, 2023)
%preprocess_files(chgoffs, 1989, 2023)
%preprocess_files(nonperf, 1989, 2023)
%preprocess_files(intexp, 1989, 2023)
%preprocess_files(ratios, 1989, 2023)
%preprocess_files(liabilities, 1989, 2023);


%macro combine_files(type,first_year,last_year);
    data &type._final;
        set
            %do year = &first_year %to &last_year;
                %if %sysfunc(exist(&type._&year)) %then &type._&year;
                %if %sysfunc(exist(&type._&year.b)) %then &type._&year.b;
            %end;
            ;
    run;
%mend;

%combine_files(assets,1989,2023);
%combine_files(chgoffs,1989,2023);
%combine_files(nonperf,1989,2023);
%combine_files(intexp,1989,2023);
%combine_files(ratios,1989,2023);
%combine_files(liabilities,1989,2023);

/* CREATE THE INSTITUTIONS DATASET*/
* import the institutions dataset;
proc import datafile="C:\Users\lward\Documents\FDIC SDI Datasets\institutions.csv" 
    out=institutions
    dbms=csv 
    replace;
    getnames=yes;
    guessingrows=max;
run;

* Create a new dataset with proper conversion;
data institutions;
    set institutions (keep=CERT FED_RSSD STNAME NAME RSSDHCR ENDEFYMD REGAGNT );
    
    /* Since ENDEFYMD is numeric (SAS date), extract year directly */
    if not missing(ENDEFYMD) then 
        year_end = year(ENDEFYMD);
    else 
        year_end = .;
    
    /* Only keep records from 1999 or later */
    if year_end >= 1999;
run;

*create a text field with leading zeros for CERT in intitutions set;
data institutions;
	set institutions;
	if not missing(CERT) then 
	CERT_char=put(CERT, z7.);
run;

/* MERGE THE FDIC DATASETS TOGETHER*/

* Create one master dataset for all of the FDIC SDI data pulled together into the &type_final datasets.;
* First, merge the assets and chgoffs datasets into one fdic_data set.;
proc sql;
   create table fdic_data as
   select a.*, b.NTLNLS
   from assets_final as a
   left join chgoffs_final as b
   on a.CERT = b.CERT and a.REPDTE = b.REPDTE;
quit;

* merge the liabilities dataset.;
proc sql;
   create table fdic_data as
   select a.*, b.BRO, DEP
   from fdic_data as a
   left join liabilities_final as b
   on a.CERT = b.CERT and a.REPDTE = b.REPDTE;
quit;

* Merge fdic_data and intexp datasets;
proc sql;
   create table fdic_data as
   select a.*, b.ELNATR
   from fdic_data as a
   left join intexp_final as b
   on a.CERT = b.CERT and a.REPDTE = b.REPDTE;
quit;

* Merge fdic_data and nonperf datasets;
proc sql;
   create table fdic_data as
   select a.*, b.NAASSET, P3ASSET, P9ASSET
   from fdic_data as a
   left join nonperf_final as b
   on a.CERT = b.CERT and a.REPDTE = b.REPDTE;
quit;

* Merge fdic_data and ratios datasets;
proc sql;
   create table fdic_data as
   select a.*, b.IDT1RWAJR, RBCRWAJ
   from fdic_data as a
   left join ratios_final as b
   on a.CERT = b.CERT and a.REPDTE = b.REPDTE;
quit;

*create a text field with leading zeros for CERT in FDIC_data set;
data fdic_data;
	set fdic_data;
	if not missing(CERT) then 
	CERT_char=put(CERT, z7.);
run;

* add year variable to fdic_data;
data fdic_data; 
	set fdic_data;
	year = year(REPDTE);
run;

* Merge fdic_data and institutions datasets;
proc sql;
   create table fdic_data as
   select a.*, b.FED_RSSD, RSSDHCR
   from fdic_data as a
   left join institutions as b
   on a.CERT_char = b.CERT_char;
quit;

* create FED_RSSD_char variable for best join;
*create a text field with leading zeros for FED_RSSD in intitutions set;
data fdic_data_final;
	set fdic_data;
	if not missing(FED_RSSD) then 
	FED_RSSD_char=put(input(strip(FED_RSSD), BEST7.), z7.);
run;

* convert RSSDHCR value to character for best join with FRY9 data;
data fdic_data_final; 
	set fdic_data_final;
	RSSDHCR_char=put(input(strip(RSSDHCR), BEST7.), z7.);
run;

/* Filter by bank ID in the fdic_data_final dataset */
proc print data=fdic_data_final;
    where FED_RSSD_char = '012311' or FED_RSSD_char = '12311' or FED_RSSD = 12311;
    title "Records for Selected Bank ID";
    var year FED_RSSD_char FED_RSSD ASSET;
run;



/* OUTPUT THE JOINED FILE TO THE SPECIFIED FOLDER */
/* Set the library path to your specified output directory */
libname cleandat "C:\Users\lward\Documents\ECON - Data Science for Economists\ProjectDocs\CleanedData";

/* Output the datasets to the specified folder */
data cleandat.fdic_data;
    set work.fdic_data_final;
run;

data cleandat.institutions;
    set work.institutions;
run;

/* Verify that the datasets were created successfully */
proc contents data=cleandat._all_ nods;
run;
