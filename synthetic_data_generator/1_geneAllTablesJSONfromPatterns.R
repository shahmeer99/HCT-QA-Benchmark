######################################################################################
### 1 - GENERATE ALL RELATIONAL TABLE INSTANCES TEMPLATES FROM GENERIC TABLE TEMPLATES

library(jsonlite)

rm(list = ls())
set.seed(1) 

source("config.R")

#### INPUT
inputFolder  = PARAMETERS_FOLDER
semanticsJSONfile = PARAM_SEMANTICS_JSON # file containing all semantic data
tablePatternsJSONfile = PARAM_TABLE_TEMPLATES_JSON    # file containing table patterns

#### OUTPUT
outputFolder = PARAMETERS_FOLDER
tableOutputJSONfile = PARAM_TABLE_TO_GEN_JSON    # file containing all detailed tables to generate

#### we generate multiple tables from a specification (see SPECIF below) in R
# all tables are stored in a single json file that can be read by "geneHCTfromJSONtables.R" 
## which contain an array of tables looking like:
# [{
#   ' name' : ' table1' ,
#   ' replica' : 5, 
#   ' shuffle' : "all",  # [none]/ "rows" / "cols / "rowscols" / "all" // row mix only rows, cols mix only cols, rowscols mix rows and cols keeping them as rows or cols, all mix across rows and cols (some cols can become rows and vice-versa)
#   ' row_format': "new" / "indent_str" // "new" use one column for each level, any other creates an empty row for level N and indent rows of level N+1,  
#   ' values' : ' int' ,
#   ' agg_fun' : ' sum' / "mean" / "min" / "max" 
#   ' columns' : {
#     ' groups' : [{ 
#       ' attributes'  : [{ 
#         ' code' : ' Years' , # code name for the seamntic attribute
#         ' pos' : ' none' , # display attribute names none/left/top
#         ' sample' : [3,5], # sample 3, 4, or 5 consecutive attr values
#         ' agg_pos1' : ' left' ,   # none/left/right
#         ' agg_name1' : ' Total' , # used only if agg_pos != none
#         ' agg_fun1' : ' sum'      # aggregation function
#       },
#       { 
#         ' code' : ' Quarters' , # code name for the seamntic attribute
#         ' pos' : ' none' , # display attribute names none/left/top
#         ' keep' : [' Q1' ,' Q3' ], # keep/remove + list of mandatory or forbidden levels
#         ' agg_pos1' : ' left' ,  # none/left/right
#         ' agg_name1' : ' Total' , # used only if agg_pos != none
#         ' agg_fun1' : ' sum' # aggregation function
#       }]
#     }]
#   },
#   ' rows' :{
#     ' groups' : [{ 
#       ' attributes'  : [{
#         ' code' : ' SchoolLevels' ,# code name for the seamntic attribute
#         ' pos' : ' top' , # display attribute names none/left/top
#         ' remove' :  [' Primary' ], # keep/remove + list of mandatory or forbidden levels
#         ' agg_pos1' : [none]/top/botom/aligned ,# aligned with parent level if nested row and sep_col=new, otherwise top, 
#         ' agg_name1' : 'Total' , any string, used only if agg_pos = top or bottom
#         ' agg_fun1' : ' sum' 
#       }]
#     }]
#   }
# },...
# 


#### TABLES SPECIFS ####

# Multi-level columns	9 combinations
# # Levels	1 (flat), 2, 3 --> maxColDepth = 3
# Balanced	Yes,--> YES ONLY
# Symmetric	Yes, No ==> use sampling for no, restrict to maxBranches=4, L1*L2*L3<maxCols=20 value for yes
# 
# Row nesting	3 combination
# # Levels	1 (no nesting), 2 --> maxRowDepth = 2
# Balanced	Yes
# Symmetric	Yes, No ==> use sampling for no, restrict to maxBranches=5, L1*L2<maxRows=15 value for yes
# 
# Aggregations	16 combinations (alway explicit, except when sep_col is indent (not new) )
# Column Global Aggregation	Yes (Explicit), No  ->  (agg at level 1 yes or no)
# Row Global Aggregation	Yes (Explicit), No    -> agg at level 1, yes or no)
# Column Local Aggregation	Yes (Explicit), No  -> agg a all levels > 1 yes or no
# Row Local Aggregation 	Yes (Explicit), No   -> agg at level 2  
# 
# Row Group Label (empty row)	2 combinations
# Row Group Label (empty row)	Yes, No  -> sep_col -> "new"(no) / " " (yes)
# 
# Split Header 	No

# create a generator of table.json data onto we run the table generator
# pick:
# col semantic
# row semantic
# col level [1,2,3] syst/rand
# row level [1,2] syst/rand
# col subset [[2,4],[2,4],[2,4]] syst/rand
# col sym [x,true/false,true/false] syst/rand
# row subset [[2,4],[2,4],[2,4]] syst/rand
# row sym [x,true/false,true/false] syst/rand
# empty row
# col aggregate [[1,2,3],[2,3]] 
# row aggregate [[1,2,3],[2,3]]



# semantic codes [(sub)levels] in semantics.json:

# "UniversityLevels" ["Degree"]
# "AcademicYears" ["Year"]
# "SchoolGovPriv" ["School type"]
# "SchoolCurriculum" ["Nationality"]
# "SchoolLevels" ["Level", "Grade"]

# "Years" ["Year"]
# "Quarters" ["Quarter"]
# "Months" ["Month"]
# "Seasons" ["Season"]
# "QuarterMonths" ["Quarter", "Month"]

# "Languages" ["Language"]
# "LocalNationality" ["Nationality"]
# "Genders" ["Gender"]

# "USA" ["State", "City"]
# "Countries" ["Country", "Region", "City"]

# "ImportExport" ["Import-Export"]
# "Pollutions" ["Polluted element", "Type of pollution"]
# 
# "values": {"realUnit": [0.0,1.0],  # can be [0.0,100.0] for percentages, all generated numbers are distincts in that range
#            "realSignedUnit": [-1.0,1.0], # can be [-100.0,100.0] for percentages, all generated numbers are distincts in that range
#            "int":[0,9], # all generated numbers are distincts positive integers
#            "intSigned":[-100,100]}  # all generated numbers are distincts positive or negative integers

# tablePatterns.json:
# 
# "replica": 10,  # number of replica of each table pattern
# "shuffle": ["none","rowscols"], # "none" keep rows and columns as provided, "rowscols" shuffle rows and cols independently
# "col_row_name_pos": ["none_none","left_top","top_left"], # position of the attribute names of rows and cols
# "col_row_agg_pos": ["none_none","right_none","none_bottom","right_bottom","right_aligned"], # position of aggregate values
# 
# "tables": 
#   [{"valueName":"Number_of_students",
#     "values": [50, 500], # or any code of values in semantics.json
#     "valueUnit": "", # not used yet, could be added after each value like a percentage sign    
#     "rowCodes":["SchoolGovPriv","Genders"], # list of attribute codes for rows (decoded in semantics.json)
#     "colCodes":["LocalNationality","AcademicYears","Quarters"], # list of attribute codes for columns decoded in semantics.json)
#     "agg_name": "Total", # name of aggregate value if displayed (agg_pos != none)
#     "agg_fun": "sum"     # aggregation function
#   }...]

# field from semantic.json (except contentType ad-hoc from set of selected semantic data)
# do not exceed 2 levels for rows and 3 for columns



# read semantic data
allSemanticAttributes <- read_json(paste0(inputFolder,semanticsJSONfile))
# read table instructions data
tablePatterns <- read_json(paste0(inputFolder,tablePatternsJSONfile)) 

### SHARED PATTERN PARAM ###
replica = tablePatterns$replica

### COMBI PARAM ###
allTableCombinations <-NULL
allTableCombinations[["shuffle"]] = tablePatterns$shuffle
allTableCombinations[["col_row_name_pos"]] = tablePatterns$col_row_name_pos
allTableCombinations[["col_row_agg_pos"]] = tablePatterns$col_row_agg_pos
allTableCombinations[["col_row_levels"]] = tablePatterns$col_row_levels
allTableCombinations[["col_row_levels"]] = tablePatterns$col_row_levels
allTableCombinations[["row_format"]] = tablePatterns$row_format


# expand all combination of parameters and semantics
paramCombi = expand.grid(allTableCombinations)


########################################################
############### GENERATOR OF JSON TABLES ###############
########################################################
strJSONstart = '[\n' 
strJSONend = '\n]' 
strJSONcontent = '' 


# create a generator of table.json data onto we run the table generator
# pick:
# col semantic
# row semantic
# col level [1,2,3] syst/rand
# row level [1,2] syst/rand
# col subset [[2,4],[2,4],[2,4]] syst/rand
# col sym [x,true/false,true/false] syst/rand
# row subset [[2,4],[2,4],[2,4]] syst/rand
# row sym [x,true/false,true/false] syst/rand
# empty row
# col aggregate [[1,2,3],[2,3]] 
# row aggregate [[1,2,3],[2,3]]

### Agg_pos can only be both left/top (before) or right/bottom (after) in pivottabler
### top will be used in row only if indent is true

FIRSTPASS=TRUE

for (itbl_pattern in 1:length(tablePatterns$tables)){
  
  #itbl_pattern=1
  curTable = tablePatterns$tables[[itbl_pattern]]
  
  ### SHARED TABLE PARAM ###
  sval = curTable$values 
  if (length(sval)==1) {valuesType = paste0("'",sval,"'"); }
  else { 
    if (is.integer(sval[[1]])) sval1 = sval[[1]] # case [5,10]
    else if (sval[[1]] == round(sval[[1]])) sval1 = sprintf("%.1f",sval[[1]]) # case [5.0, 10.0]
    else sval1 = sval[[1]] # case [5.1, 10.3]
    
    if (is.integer(sval[[2]])) sval2 = sval[[2]]
    else if (sval[[2]] == round(sval[[2]])) sval2 = sprintf("%.1f",sval[[2]])
    else sval2 = sval[[2]]
    
    valuesType = paste0("[",sval1,",",sval2,"]");
  }
  
  valueUnit = curTable$valueUnit
  valueName = curTable$valueName
  
  rowCodes = curTable$rowCodes
  rowSamples = curTable$rowSamples
  
  colCodes = curTable$colCodes
  colSamples = curTable$colSamples
  
  agg_name1 = curTable$agg_name1
  agg_fun1 = curTable$agg_fun1
  
  
  countTables = 0
  
  for (icombi in 1:nrow(paramCombi)){
    
    curParam = paramCombi[icombi,]
    
    print(paste0(itbl_pattern,"/",length(tablePatterns$tables) ," -- ",icombi,"/",nrow(paramCombi)))
    
    
    ### SHARED COMBI PARAM ###
    shuffle = curParam$shuffle
    col_row_levels = curParam$col_row_levels[[1]]
    col_row_name_pos = curParam$col_row_name_pos[[1]]
    col_row_agg_pos = curParam$col_row_agg_pos[[1]]
    row_format = curParam$row_format 
    
    sl = strsplit(col_row_levels,"_")
    col_levels = sl[[1]][[1]]
    row_levels = sl[[1]][[2]]
    
    snp = strsplit(col_row_name_pos,"_")
    col_name_pos = snp[[1]][[1]]
    row_name_pos = snp[[1]][[2]]
    
    sap = strsplit(col_row_agg_pos,"_")
    col_agg_pos = sap[[1]][[1]]
    row_agg_pos = sap[[1]][[2]]
    
    DO_NOT_GENERATE = FALSE
    if (row_format == "new" && row_agg_pos == "top") DO_NOT_GENERATE = TRUE
    
    if (DO_NOT_GENERATE == FALSE){
      
      if (FIRSTPASS == TRUE){
        FIRSTPASS=FALSE
      } else {
        strJSONcontent = paste0(strJSONcontent,sepAttr)
      }
      
      ### JSON ###
      tabName = gsub(" ","_",valueName)
      tableName = paste0(tabName,'_set',icombi);
      
      strAttrOffset   = "        "
      strGroupOffset  = "      "
      strRowColOffset = "    "
      sepAttr = ",\n"
      
      colAttribs = ""
      nColCodes = min(col_levels,length(colCodes))
      for (ilev in 1:nColCodes){
        spl = colSamples[[ilev]]
        if (length(spl)==1) {str_spl = "[0]"; }
        else { str_spl = paste0("[",spl[1],",",spl[2],"]");}
        colAttribs = paste0(colAttribs,
                            strAttrOffset,"{\n", 
                            strAttrOffset,"'code': '",colCodes[ilev],"',\n",
                            strAttrOffset,"'pos': '", col_name_pos,"',\n",
                            strAttrOffset,"'sample': ", str_spl,",\n",
                            strAttrOffset,"'agg_pos1': '", col_agg_pos,"'\n",
                            strAttrOffset,"}")
        # add separator
        if (ilev<nColCodes) colAttribs = paste0(colAttribs,sepAttr)
      }  
      
      rowAttribs = ""
      nRowCodes = min(row_levels,length(rowCodes))
      for (ilev in 1:nRowCodes){
        spl = rowSamples[[ilev]]
        if (length(spl)==1) {str_spl = "[0]"; }
        else { str_spl = paste0("[",spl[1],",",spl[2],"]");}
        
        rowAttribs = paste0(rowAttribs,
                            strAttrOffset,"{\n",
                            strAttrOffset,"'code': '",rowCodes[ilev],"',\n",
                            strAttrOffset,"'pos': '", row_name_pos,"',\n",
                            strAttrOffset,"'sample': ", str_spl,",\n",
                            strAttrOffset,"'agg_pos1': '", row_agg_pos,"'\n",
                            strAttrOffset,"}")
        # add separator
        if (ilev<nRowCodes) rowAttribs = paste0(rowAttribs,sepAttr)
      }
      
      
      newJSONtable = paste0("{\n 'name' : '", tableName,
                            "',\n 'replica' : ", replica,
                            ",\n 'shuffle' : '", shuffle,
                            "',\n 'agg_fun1' : '", agg_fun1,
                            "',\n 'agg_name1' : '", agg_name1,
                            "',\n 'values' : ", valuesType,
                            ",\n 'valueName' : '", valueName,
                            "',\n 'row_format' : '", row_format,
                            "',\n 'columns' : {",
                            "\n",strGroupOffset,"'groups' : [{", 
                            "\n",strAttrOffset,"'attributes': [\n", colAttribs,"]", 
                            "\n",strGroupOffset,"}]",
                            "\n",strRowColOffset,"},",
                            "\n 'rows' :{",
                            "\n",strGroupOffset,"'groups' : [{", 
                            "\n",strAttrOffset,"'attributes': [\n", rowAttribs,"]", 
                            "\n",strGroupOffset,"}]",
                            "\n",strRowColOffset,"}", 
                            "\n}");
      
      
      countTables = countTables + 1
      strJSONcontent = paste0(strJSONcontent,newJSONtable)

    } else {
      print("NOT GENERATED")
    }
  } 
  
}

strJSON = paste0(strJSONstart,strJSONcontent,strJSONend)

# clean json data 
strJSON=gsub("'",'"' ,strJSON)

#filenameJSON = "TEST_JSON.json" #
filenameJSON = paste0(inputFolder,tableOutputJSONfile)
write.table(strJSON, file = filenameJSON, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)

print("NUMBER OF TABLES:")
print(countTables)


