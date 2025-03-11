## Question Template Types:

For questions, please contact **[Your Name / Contact Info]** or open an issue.

Example of 15 natural language questions generated for the HCT above based on custom templates ("," and ";" in answers, are column and row separators respectively):
(see Food Import-Export template in [NL question templates](./synthetic_data_generator/generator_code/PARAM_NLquestionTemplates.json))
> Q1: *one column, one row selection* "What is the amount of Export of Milk in 2017?"
>
> $\rightarrow$ "57"
> 
> Q2: *one column, many rows selection* "What is the amount of Export of Butter, Coffee, Milk, or Tea in 2017?"
>
> $\rightarrow$ "57; 146 ; 980; 749"
> 
> Q3: *many columns, one row selection* "What is the amount of Export of Tea in 2017 or 2018?"
>
> $\rightarrow$ "749; 622"
> 
> Q4: *Q3 + expression not in the table* "What are the total and minimum amounts of Export or Import of Milk in 2017 or of Export of Milk in 2018?"
>
> $\rightarrow$ "1442,57"
> 
> Q5: (opt): *Q4 + expression from the table* (only if an aggregate exists in the \gls{refNameSingleton}) "What is the average amount of Export or Import of Coffee?"
>
> $\rightarrow$ "533.75"
> 
> Q6: *many rows, many columns selection* "What is the amount of Export or Import of Butter, Cream, or Tea in 2017 or of Export of Butter, Cream, or Tea in 2018?"
>
> $\rightarrow$ "444; 146; 443; 736; 44; 821; 751; 749; 622"
> 
> Q7: *Q2 + aggregation not in the table* "What are the total and minimum amounts of Import of Butter, Coffee, Cream, Milk, or Tea in 2017?"
>
> $\rightarrow$ "2996,266"
> 
> Q8: *Q6 + grouping per column + reporting* "What are the minimum and maximum amounts for each Import-Export and Year, of Export or Import of Butter, Coffee, Cream, Milk, or Tea in 2017 or 2018? Please, report the corresponding Import-Export and Year."
>
> $\rightarrow$ "Export,2017,44,980; Export,2018,121,821; Import,2017,266,799; Import,2018,179,879"
> 
> Q9: *Q2 + grouping per row*  "What is the minimum amount for each Category, of Import of Beverage or Dairy in 2018?"
>
> $\rightarrow$ "673;179"
> 
> Q10: *Q6 + grouping per rows + reporting* "What is the minimum amount for each Category, of Import of Beverage or Dairy in 2018? Please, report the corresponding Category."
>
> $\rightarrow$ "Beverage,673; Dairy,179"
> 
> Q11: *Q6 + grouping per rows and columns + reporting* "What is the minimum amount for each Category, Import-Export, and Year, of Import of Beverage or Dairy in 2017 or of Export or Import of Beverage or Dairy in 2018? Please, report the corresponding Category, Import-Export, and Year."
>
> $\rightarrow$
>   "Beverage,Export,2018,121;
>    Beverage,Import,2017,266;
>    Beverage,Import,2018,673; 
>    Dairy,Export,2018,443;
>    Dairy,Import,2017,444;  
>    Dairy,Import,2018,179"
>   
> Q12: *Q2 + top-K + row filter* "What are the bottom 5 amounts of Export of Beverage or Dairy in 2018?"
>
> $\rightarrow$ "121; 443; 586; 622; 821"
> 
> Q13: *Q2 + row filter + ordering* "What are the amounts ordered by increasing values of Import of Beverage or Dairy in 2017?"
>
> $\rightarrow$ "266; 444; 736; 751; 799"
> 
> Q14: *Q2 + operation on column* "What are the Category and Item for which the amount of Import in 2018 is greater than 513.3?"
>
> $\rightarrow$ "Dairy,Milk; Beverage,Coffee; Beverage,Tea"
> 
> Q15: *Q14 with condition + reporting* "What is the amount of Export in 2018 of Category and of Item for which the amount of Import in 2018 is greater than 513.3? Please, report the corresponding Category, Item, and amount of import-export."
>
> $\rightarrow$ "Dairy,Milk,586; Beverage,Coffee,121; Beverage,Tea,622"
> 


## INSTALLATION
Install R version 4.4.2 and Rstudio (see configuration details below).

> https://posit.co/download/rstudio-desktop/
 
Download all the files from the "generator_code" repository.

Set Rstudio working directory to the folder containing these files. 

Install all necessary R-packages (see Packages to install below). 

## RUN IN THE FOLLOWING ORDER

1. Generate the table instances
 
> 1_geneAllTablesJSONfromPatterns.R

2. Generate relational tables and their HCT variants in the TABLES folder
  
> 2_geneHCTfromJSONtables.R

3. Generate SQL and NL questions for each table in the QandA folder

> 3_geneSQLandNLQfromJSONtemplates.R

4. Generate the non-semantic versions of all the tables and questions

> 4_geneNonSemanticTablesQandA.R

5. Generate the final benchmark folder

> 5_geneCleanBenchmarkData.R

6. Get a summary of the generated data

> 6_countSyntheticData.R

## OPTIONS

config.R # contain PATH to create working and final folders and set other options

## NECESSARY TOOLBOXES used by other codes
toolboxJSONtemplateForAnnotator.R

toolboxSQLandNLQ.R

toolboxTABLESgenerator.R

## NECESSARY PARAMETER FILES
PARAM_semantics.json

PARAM_tableTemplates.json

PARAM_NLquestionTemplates.json


## PACKAGES TO INSTALL

> install.packages("remotes")
> 
> library(remotes)
> 
> install_version("jsonlite", "1.9.0")
> 
> install_version("pivottabler", "1.5.5")
> 
> install_version("htmlTable", "2.4.3")
> 
> install_version("psycModel", "0.5.0")
> 
> install_version("pagedown", "0.22")
> 
> install_version("magick", "2.8.5")
> 
> install_version("rlang", "1.1.5")
> 
> install_version("sqldf", "0.4-11")
> 
> install_version("tidyquery", "0.2.4")
> 
> install_version("stringr", "1.5.1")
> 
> install_version("data.table", "1.16.4")
> 
> install_version("pdftools", "3.4.1")


## SYSTEM CONFIGURATION

> sessionInfo()

R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: Asia/Qatar
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] readr_2.1.5       magick_2.8.5      data.table_1.16.4 stringr_1.5.1     tidyquery_0.2.4  
 [6] sqldf_0.4-11      RSQLite_2.3.9     gsubfn_0.7        proto_1.0.0       rlang_1.1.5      
[11] pagedown_0.22     psycModel_0.5.0   htmlTable_2.4.3   pivottabler_1.5.5 jsonlite_1.9.0   

loaded via a namespace (and not attached):
 [1] generics_0.1.3    tcltk_4.4.2       stringi_1.8.4     hms_1.1.3         digest_0.6.37    
 [6] magrittr_2.0.3    evaluate_1.0.3    timechange_0.3.0  fastmap_1.2.0     blob_1.2.4       
[11] processx_3.8.5    backports_1.5.0   DBI_1.2.3         ps_1.9.0          promises_1.3.2   
[16] pdftools_3.4.1    cli_3.6.4         bit64_4.6.0-1     withr_3.0.2       cachem_1.1.0     
[21] tools_4.4.2       tzdb_0.4.0        memoise_2.0.1     checkmate_2.3.2   dplyr_1.1.4      
[26] httpuv_1.6.15     mime_0.12         vctrs_0.6.5       R6_2.6.1          lifecycle_1.0.4  
[31] lubridate_1.9.4   htmlwidgets_1.6.4 bit_4.5.0.1       pkgconfig_2.0.3   later_1.4.1      
[36] pillar_1.10.1     glue_1.8.0        Rcpp_1.0.14       servr_0.32        xfun_0.51        
[41] tibble_3.2.1      tidyselect_1.2.1  rstudioapi_0.17.1 knitr_1.49        websocket_1.4.2  
[46] htmltools_0.5.8.1 qpdf_1.3.4        compiler_4.4.2    askpass_1.2.1     chron_2.3-62     
[51] queryparser_0.3.2
