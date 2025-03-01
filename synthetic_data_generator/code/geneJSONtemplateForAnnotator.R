

### GENERATING JSON TEMPLATE FOR TABLES + Q and A

# check SQL/HCT template description in: 2024_01_29_SyntheticQueryGeneration_V2_SQL_NLtemplates_Mohamed.pptx

#### NOTE: it seems JSON template makes no difference between multi row and multi column, vs group by  

geneJSONformatQandA <- function(numTemplateQA,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL="",tableAggFun="",firstK=""){
  
  # assume min depth is 1 and depth is the number of columns/rows in HCT headers
  
  query_type <- NULL
  true = "TRUE"
  false = "FALSE"
  EXPRS = c("sum","min","max","avg")
  
  sqlResult = as.character(sqlResult)
  allRowRes = strsplit(sqlResult,ROW_SEP)[[1]]
  nrowRes = length(allRowRes) 
  ncolRes = length(strsplit(allRowRes[1],COL_SEP)[[1]]) 
  
  if (numTemplateQA == 1) {
    # Template 1 -- SINGLE-CELL ONE ROW AND ONE COLUMN SELECTION FROM HCT 1R & 1C
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Non-Qatari')) AND ((Year = '2020/2021' AND Language = 'English'))",
    # "sqlResult": "321",
    # "nlq": "What is the number of graduations of Non-Qatari Male students with a BSc in English in 2020/2021?"},
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  true, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = false,
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  "1", # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 2) {
    # Template 2 -- MULTI-CELL SELECTION FROM ONE COLUMN AND MULTI ROWS IN HCT nR & 1C
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Qatari')) AND ((Year = '2020/2021' AND Language = 'English') OR (Year = '2022/2023' AND Language = 'English') OR (Year = '2020/2021' AND Language = 'French') OR (Year = '2021/2022' AND Language = 'French') OR (Year = '2022/2023' AND Language = 'French'))",
    # "sqlResult": "188; 488; 278; 319; 248",
    # "nlq": "What is the number of graduations of Qatari Female students with a PhD in English or French in 2020/2021 or 2022/2023 or in French in 2021/2022?"},

    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  "1", # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 3) {
    # Template 3 -- MULTI-CELL SELECTION FROM ONE ROW AND MULTI COLUMNS IN HCT 1R & nC
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Qatari') OR (Gender = 'Male' AND Degree = 'MSc' AND Nationality = 'Qatari') OR (Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Non-Qatari') OR (Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Non-Qatari') OR (Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Non-Qatari')) AND ((Year = '2022/2023' AND Language = 'French'))",
    # "sqlResult": "209; 416; 398; 248; 145; 231; 63",
    # "nlq": "What is the number of graduations of Non-Qatari Female or Male students with a BSc or Non-Qatari Male or Qatari Female students with a PhD or Qatari Male students with a BSc, MSc, or PhD in French in 2022/2023?"},
  
    # check result size, because sometime the table is too small to generate multiple column results
    # assume multi-row result

    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  true, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = false, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
    
  } else if (numTemplateQA == 4) {
    # Template 4 -- Multi-cell + expression 1R & nC + expression possibly not in table
    # "sql": "SELECT sum(Value),min(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Non-Qatari') OR (Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Non-Qatari') OR (Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Non-Qatari') OR (Gender = 'Female' AND Degree = 'MSc' AND Nationality = 'Non-Qatari') OR (Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Non-Qatari')) AND ((Year = '2020/2021' AND Language = 'English'))",
    # "sqlResult": "1505, 64",
    # "nlq": "What are the total and minimum numbers of graduations of Non-Qatari Female or Male students with a BSc or Non-Qatari Female students with a MSc or Non-Qatari or Qatari Female or Male students with a PhD in English in 2020/2021?"},
  
    # EXPRS = c("sum","min","max","avg")
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  true, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = false, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = true, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 5) {
    # Template 5 -- Multi-cell + expression 1R & nC + expression from the table
    # "sql": "SELECT sum(Value) FROM DBdata WHERE Gender = 'Female' AND (Degree IN ('BSc','MSc','PhD')) AND ((Year = '2020/2021' AND Language = 'English'))",
    # "sqlResult": "1077",
    # "nlq": "What is the total number of graduations of Female students with a BSc, MSc, or PhD in English in 2020/2021?"},
  
    # always a single expression in our case
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true

    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  true, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = false, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false, # some aggregation on the filtered rows is done (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result 
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = true, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = "1", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
    
    
  } else if (numTemplateQA == 6) {
    # Template 6 -- MULTI-CELL SELECTION FROM MULTI ROWS AND MULTI COLUMNS IN HCT nR & nC
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'MSc' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'MSc' AND Nationality = 'Non-Qatari')) AND ((Year = '2020/2021' AND Language = 'French') OR (Year = '2022/2023' AND Language = 'French'))",
    # "sqlResult": "469; 105; 53; 278; 431; 209; 230; 413; 248; 389",
    # "nlq": "What is the number of graduations of Non-Qatari or Qatari Female students with a MSc or Qatari Female or Male students with a BSc or Qatari Female students with a PhD in French in 2020/2021 or 2022/2023?"},
  
    
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
  } else if (numTemplateQA == 7) {
    # Template 7 -- Multi-cell nR & 1C + expression on a column
    # "sql": "SELECT sum(Value),avg(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Non-Qatari')) AND ((Year = '2020/2021' AND Language = 'English') OR (Year = '2021/2022' AND Language = 'English') OR (Year = '2022/2023' AND Language = 'English') OR (Year = '2020/2021' AND Language = 'French') OR (Year = '2022/2023' AND Language = 'French'))",
    # "sqlResult": "1459, 291.8",
    # "nlq": "What are the total and average numbers of graduations of Non-Qatari Male students with a PhD in English in 2020/2021, 2021/2022, or 2022/2023 or in French in 2020/2021 or 2022/2023?"},
    
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true}
    
    # list of expressions
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  "1", # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = true, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 8) {
    # Template 8 -- nR & nC + local aggregation + GROUP-BY
    # "sql": "SELECT Gender,Degree,Nationality,min(Value),max(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'MSc' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Qatari') OR (Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Non-Qatari') OR (Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Non-Qatari') OR (Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Non-Qatari')) AND ((Year = '2020/2021' AND Language = 'English') OR (Year = '2022/2023' AND Language = 'English') OR (Year = '2020/2021' AND Language = 'French')) GROUP BY Gender,Degree,Nationality",
    # "sqlResult": "Female, BSc, Qatari, 97, 264; Female, PhD, Non-Qatari, 51, 377; Male, BSc, Non-Qatari, 98, 487; Male, MSc, Qatari, 94, 478; Male, PhD, Non-Qatari, 147, 359",
    # "nlq": "What are the minimum and maximum numbers of graduations for each Gender, Degree, and Nationality, of Non-Qatari Female students with a PhD or Non-Qatari Male students with a BSc or PhD or Qatari Female students with a BSc or Qatari Male students with a MSc in English in 2022/2023 or in English or French in 2020/2021? Please, report the corresponding Gender, Degree, and Nationality."},
  
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true}
    
    # list of expressions
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)),  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = true, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
  } else if (numTemplateQA == 9) {
    # Template 9 -- Multi-cell + expression on nR & 1C + GROUP BY HCT row nested level 1
    # "sql": "SELECT min(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Qatari')) AND (Year IN ('2020/2021','2021/2022','2022/2023')) GROUP BY Year",
    # "sqlResult": "316; 380; 209",
    # "nlq": "What is the minimum number of graduations for each Year, of Qatari Male students with a BSc in 2020/2021, 2021/2022, or 2022/2023?"},
    
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
    
    # list of expressions
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = "1", # number of conditions on row columns (row header depth) --> depth 1 grouping condition
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  "1",  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = true,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 10) {
    # Template 10 -- Local aggregation (per group of rows) nR & 1C + GROUP BY per row (level 1) + report group name 
    # "sql": "SELECT Year,min(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'MSc' AND Nationality = 'Non-Qatari')) AND (Year IN ('2020/2021','2021/2022','2022/2023')) GROUP BY Year",
    # "sqlResult": "2020/2021, 339; 2021/2022, 219; 2022/2023, 374",
    # "nlq": "What is the minimum number of graduations for each Year, of Non-Qatari Male students with a MSc in 2020/2021, 2021/2022, or 2022/2023? Please, report the corresponding Year."},

    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
    
    # list of expressions
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = "1", # number of conditions on row columns (row header depth) --> depth 1 grouping condition
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" = as.character(length(indColSel)),  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = true,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
  } else if (numTemplateQA == 11) {
    # Template 11 -- Local aggregation (per group of rows) nR & nC + GROUP BY per row (level 1) + report group name 
    # "sql": "SELECT Year,Gender,Degree,Nationality,min(Value) FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'PhD' AND Nationality = 'Qatari') OR (Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Qatari')) AND (Year IN ('2020/2021','2021/2022','2022/2023')) GROUP BY Year,Gender,Degree,Nationality",
    # "sqlResult": "2020/2021, Female, BSc, Qatari, 97; 2020/2021, Male, PhD, Qatari, 82; 2021/2022, Female, BSc, Qatari, 194; 2021/2022, Male, PhD, Qatari, 243; 2022/2023, Female, BSc, Qatari, 230; 2022/2023, Male, PhD, Qatari, 363",
    # "nlq": "What is the minimum number of graduations for each Year, Gender, Degree, and Nationality, of Qatari Female students with a BSc or Qatari Male students with a PhD in 2020/2021, 2021/2022, or 2022/2023? Please, report the corresponding Year, Gender, Degree, and Nationality."},

    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
    
    # list of expressions
    aggMIN = false
    aggMAX = false
    aggCOUNT = false
    aggSUM = false
    aggAVG = false
    
    if ("min" %in% exprListSQL) aggMIN = true
    if ("max" %in% exprListSQL) aggMAX = true
    if ("count" %in% exprListSQL) aggCOUNT = true
    if ("sum" %in% exprListSQL) aggSUM = true
    if ("avg" %in% exprListSQL) aggAVG = true
    
    aggTABLE_P = false
    aggTABLE_NP = true
    if (tableAggFun %in% exprListSQL){aggTABLE_P = true; aggTABLE_NP = false}
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = "1", # number of conditions on row columns (row header depth) --> depth 1 grouping condition
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" = as.character(length(indColSel)),  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = true, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  true, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  aggMIN, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = aggMAX,
                               "Aggregation Aggregation Type Count" = aggCOUNT,
                               "Aggregation Aggregation Type Sum" = aggSUM,
                               "Aggregation Aggregation Type Avg" = aggAVG,
                               "Aggregation Grouping Local" = true,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = aggTABLE_P, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = aggTABLE_NP, 
                               "Aggregation Num Of Aggregations" = as.character(length(exprListSQL)), # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
  } else if (numTemplateQA == 12) {
    # TEMPLATE 12: nR & 1C + TOP/BOTTOM K + ROW FILTER 
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Female' AND Degree = 'PhD' AND Nationality = 'Qatari')) AND (Year IN ('2020/2021','2021/2022')) ORDER BY VALUE ASC LIMIT 3",
    # "sqlResult": "188; 278; 319",
    # "nlq": "What are the bottom 3 numbers of graduations of Qatari Female students with a PhD in 2020/2021 or 2021/2022?"},
 
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
      
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth) --> depth 1 grouping condition
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" = "1",  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = true, # if some ranking/ordering is used
                               "Rank Rank On Plain" = true,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = as.character(firstK)) # if top or bottom K values is requested, what K is.
    
  } else if (numTemplateQA == 13) {
    # Template 13 -- Local aggregation (per group of rows) nR & 1C + ROW FILTER + ORDER BY 
    # "sql": "SELECT Value FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'MSc' AND Nationality = 'Non-Qatari')) AND (Year IN ('2021/2022','2022/2023')) ORDER BY VALUE DESC",
    # "sqlResult": "457; 374; 356; 219",
    # "nlq": "What are the numbers of graduations ordered by decreasing values of Non-Qatari Male students with a MSc in 2021/2022 or 2022/2023?"},
  
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  true, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = false, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = "1", # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(ncol(HCTrowHeaders)), # number of conditions on row columns (row header depth) --> depth 1 grouping condition
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false,# some aggregation on the filtered row is done  (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" = "1",  # number of columns of the query result
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = true, # if some ranking/ordering is used
                               "Rank Rank On Plain" = true,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the result of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
    
    
  } else if (numTemplateQA == 14) {
    # Template 14 --  nR & 1C + operation on column  to get the n rows
    # "sql": "SELECT Year,Language FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Qatari')) AND Value < 280.94",
    # "sqlResult": "2022/2023, French",
    # "nlq": "What are the Year and Language for which the number of graduations of Qatari Male students with a BSc is lower than 280.94?"},
  
    # check result size, because sometime the table is too small to generate multiple row results
    # assume multi-row result
    RRS = false
    RRM = true
    if (nrowRes==1) { RRM = false; RRS = true} # number of rows selected by the operation on the HCT column
    
    numCond = ncol(HCTcolHeaders)# the conditions to filter the rows is the depth of the column header
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  false, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = true, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = as.character(ncol(HCTrowHeaders)), # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(numCond), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false, # some aggregation on the filtered rows is done (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result 
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
  } else if (numTemplateQA == 15) {
    # Template 15 -- Local aggregation (per group of rows) conditional (reuse template 14) 
    # "sql": "SELECT Year,Language,Value FROM DBdata WHERE ((Gender = 'Female' AND Degree = 'BSc' AND Nationality = 'Non-Qatari')) AND (Year,Language) IN (SELECT Year,Language FROM DBdata WHERE ((Gender = 'Male' AND Degree = 'BSc' AND Nationality = 'Qatari')) AND Value < 280.94)",
    # "sqlResult": "2022/2023, French, 63",
    # "nlq": "What is the number of graduations of Non-Qatari Female students with a BSc in Year and in Language for which the number of graduations of Qatari Male students with a BSc is lower than 280.94? Please, report the corresponding Year, Language, and number of graduations."}]
    
    RRS = false
    RRM = true
    if (length(indRowSel)==1) { RRM = false; RRS = true} # number of rows at depth 1
    numCond = ncol(HCTcolHeaders)# the conditions to filter the rows is the depth of the column header
    
    query_type_df = data.frame("Row Filter" = true,
                               "Row Filter Condition Type Lookup" =  false, # the condition look for attribute in a set of values in the table like year in {2022, 2023} 
                               "Row Filter Condition Type Expression" = true, # the condition is about values in a column greater than a expression (average, sum...) where the expression is not given in the same column
                               "Row Filter Involved Columns Single" =  true,  # true when each condition involves a single column at a time year in {2022, 2023}  and degree in {PhD,BSc} are two single-column conditions
                               "Row Filter Involved Columns Multiple" = false, # rows where qatari is greater than non-qatari would be a filter depending on multiple columns
                               "Row Filter Max Depth Of Involved Columns" = as.character(ncol(HCTrowHeaders)), # depth of the column header of the row columns (not the depth of row header)
                               "Row Filter Retained Rows Single" =  RRS, # number of rows resulting from the filter and of the query result
                               "Row Filter Retained Rows Multiple" = RRM, # number of rows resulting from the filter and of the query result
                               "Row Filter Num Of Conditions" = as.character(numCond), # number of conditions on row columns (row header depth)
                               "Returned Columns" = true,
                               "Returned Columns Project On Plain" = true, # resulting column is part of the table 
                               "Returned Columns Project On Expression" = false, # some aggregation on the filtered rows is done (like the total of all rows in a given column)
                               "Returned Columns Max Depth Of Involved Columns" = as.character(ncol(HCTcolHeaders)), # depth of the columns' header
                               "Returned Columns Expression In Table Present" = false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Expression In Table Not Present" =  false, # necessarily false if "Returned Columns Project On Expression" is false
                               "Returned Columns Num Of Output Columns" =  as.character(length(indColSel)), # number of columns of the query result 
                               "Yes/No" = false, # never used
                               "Yes/No Scope Single" = false, # never used
                               "Yes/No Scope Multiple" = false, # never used
                               "Aggregation" = false, # true if values from several columns are aggregated
                               "Aggregation Aggregation On Plain" =  false, # always true in our case, if there is Aggregation over values in a column
                               "Aggregation Aggregation On Expression" =  false, # always false in our case
                               "Aggregation Aggregation Type Min" =  false, # which agregation is used, true for all that are used
                               "Aggregation Aggregation Type Max" = false,
                               "Aggregation Aggregation Type Count" = false,
                               "Aggregation Aggregation Type Sum" = false,
                               "Aggregation Aggregation Type Avg" = false,
                               "Aggregation Grouping Local" = false,  # if aggregation is per group of rows 
                               "Aggregation Grouping Global" = false, # if aggregation is for all rows at once
                               "Aggregation Aggregation In Table Present" = false, # if aggregation is already in the table like "Total" or average 
                               "Aggregation Aggregation In Table Not Present" = false, 
                               "Aggregation Num Of Aggregations" = "", # number of true in list of aggregation functions above
                               "Rank" = false, # if some ranking/ordering is used
                               "Rank Rank On Plain" = false,   # if rank happens on an existing column
                               "Rank Rank On Expression" = false, # if rank happens on the resul of an expression (artificial column)
                               "Rank Report Top" = "") # if top or bottom K values is requested, what K is.
    
  }

  return(query_type_df)
  
}

aggregateNLQ <- function(query_type_df_list){
  
  df = query_type_df_list[[1]] 
  for (i in 2:length(query_type_df_list)){
    df = rbind(df,query_type_df_list[[i]])
  }
  
  print(nrow(df))
  
  # generate JSON format
  query_type_str1 = jsonlite::toJSON(df) #, pretty=TRUE)
  # clean up special characters
  query_type_str2 = gsub("Yes.No","Yes/No",query_type_str1)
  query_type_str3 = gsub("\\."," ",query_type_str2)
  query_type_str4 = gsub('"FALSE"','false',query_type_str3)
  query_type_str5 = gsub('"TRUE"','true',query_type_str4)
  
  query_type = query_type_str5
  
  return(query_type)
  
}



