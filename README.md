# HCTQA-Benchmark

Project Repository  
``` bash
|
├── inference_evaluate
│   ├── script1.py
│   ├── script2.py
│   └── ...
|
├── realWorld_data_processing
│   ├── tables
│   │   ├── csvs
│   │   └── images
│   └── qaps
│       ├── file1.jsonl
│       ├── file2.jsonl
│       └── ...
|
├── synthetic_data_generator
│   ├── code
│   │   ├── script1.py
│   │   ├── script2.R
│   │   └── ...
│   └── synthetic_data
│       ├── format_variation_tables_subset
│       │   ├── tables
│       │   │   ├── html
│       │   │   ├── latex
│       │   │   └── ...
│       ├── regular_csv
│       ├── dummy_csv
│       ├── all_tables_csvs
│       └── qaps
|
└── prompts_files
    ├── prompt1.jsonl
    ├── prompt2.jsonl
    └── ...
```
1. Add explanation of 15 templates types  
2. Add set up (untarring all data folders)  
3. Infernce code upload (generic llm inference + anonymized cloud inference)  
4. Result parsing & scoring code
5. Synthetic question templates


Here are the 15 natural language questions generated for the table above based on custom templates ("," and ";" in answers, are column and row separators respectively):

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
