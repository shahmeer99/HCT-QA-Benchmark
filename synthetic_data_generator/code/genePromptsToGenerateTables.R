####################################################################################
####################################################################################
#### 2024_10_09
####################################################################################
####################################################################################

## generate a set of prompts describing a HCT table based on its features


##############################



## GENERATE SQL FROM TEMPLATE

rm(list = ls())

library(jsonlite)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
library(rlang) # to replace names by variables in filter using sym()
#library(RSQLite)
library(sqldf)
library(tidyquery)
library(stringr)

library(data.table)

source("utils.R") ## check this file to set up specific string separators
source("toolboxSQLandNLQ2.R")
source("geneJSONtemplateForAnnotator.R")



################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################

generateNLQ = TRUE

dataFolder   = "./PARAMETERS/" 
#inputFolder  = "./2024_09_16_TABLES/" 
#outputFolder = "./2024_10_10_Prompt_to_generate_2024_09_16_TABLES/"
inputFolder  = "./TABLES_FOR_PROMPT_LLM/" 
outputFolder = "./PROMPTS_FOR_PROMPT_LLM/"
listOfTables = "tablesToGenerate_forPromptLLM.json"

# we only need these table files to generate the Q&A
SIG_FILE_EXT = "_SIG_HCT.json" # use when HCT pivot tables have been generated
FORM_FILE_EXT = "_HCT.json"
DB_FILE_EXT = "_DB.csv"

DATA_TABLE_PATTERNS = "tablePatterns_all.json"
DATA_TABLE_INSTANCES = "tablesToGenerate_all.json"
DATA_TABLE_SEMANTICS = "semantics.json"
NLQ_PATTERN_FILE = "NLquestionPatterns.json"

# COL_DEPTH_MAX = 1
# ROW_DEPTH_MAX = 1
# COL_AGG_NUM = 0
# ROW_AGG_NUM = 0 
# OUTPUT_FILE_NAME = "_ALL_TABLE_PROMPTS_DEPTH1"

COL_DEPTH_MAX = 2
ROW_DEPTH_MAX = 2
COL_AGG_NUM = 0
ROW_AGG_NUM = 0 
OUTPUT_FILE_NAME = "_ALL_TABLE_PROMPTS_DEPTH1_2"

TableListJSON <- read_json(paste0(dataFolder,listOfTables)) 
TableListSTR = NULL
for (i in 1: length(TableListJSON)){
  TableListSTR[i] = strsplit(TableListJSON[[i]]$name,"_set")[[1]][1]
}
LIST_DATA_PREFIX = unique(TableListSTR)

# LIST_DATA_PREFIX = c("Number_of_students",
#                      "Number_of_graduations",
#                      "Evolution_of_pollution_in_percent", # enable simplified nested attributes
#                      "Food_import-export_in_tons", # enable simplified nested attributes
#                      "Weather_statistics",
#                      "Number_of_constructions", # enable simplified nested attributes
#                      "Number_of_accidents")

#LIST_DATA_PREFIX = c("Weather_statistics")

tttt = 0 
ALL_TABLE_TEMPLATE = list()

dumpNLdfALL <- NULL

for (DATA_PREFIX in LIST_DATA_PREFIX){
  
  
  set.seed(1) # set.seed(1) 
  
  dumpName = DATA_PREFIX
  

  TABLE_INSTANCES_JSON = read_json(paste0(dataFolder,DATA_TABLE_INSTANCES))
  TABLE_PATTERNS_JSON = read_json(paste0(dataFolder,DATA_TABLE_PATTERNS))
  SEMANTIC_DATA = read_json(paste0(dataFolder,DATA_TABLE_SEMANTICS))
  NLQ_PATTERNS = read_json(paste0(dataFolder,NLQ_PATTERN_FILE))
  
  
  ### get all table files of specific table pattern
  fileList = list.files(inputFolder)
  ii = 0
  fileListToProcess = NULL
  for (ff in fileList){
    if (grepl(DATA_PREFIX,ff) || nchar(DATA_PREFIX)==0) {
      if (grepl(SIG_FILE_EXT,ff)){
        ii = ii + 1
        fileListToProcess[ii] = unlist(strsplit(ff,split=SIG_FILE_EXT))
      }
    } 
  }
  
  
  print(fileListToProcess)
  
  numDataFiles = length(fileListToProcess)


  
  kkk = 0
  dumpNLprompt1 = character(length = numDataFiles*20)
  dumpNLprompt2 = character(length = numDataFiles*20)
  dumpNLprompt3 = character(length = numDataFiles*20)
  dumpNLprompt4 = character(length = numDataFiles*20)
  dumpNLprompt5 = character(length = numDataFiles*20)
  dumpNLprompt6 = character(length = numDataFiles*20)
  dumpNLprompt7 = character(length = numDataFiles*20)
  dumpNLfileSuffix = character(length = numDataFiles*20)
  
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  
  for (indDataFile in 1:numDataFiles){
    curDataFile = fileListToProcess[indDataFile]
    
    
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    
    DATA_FILE = curDataFile
    
    sqlJSONstr <- NULL
    sqlnlqJSONstr <- NULL
    NLquestions <-NULL
    
    
    
    DB_CSVfile = paste0(DATA_FILE,DB_FILE_EXT) # file containing DB data
    HCT_SIGNATURE_JSONfile = paste0(DATA_FILE,SIG_FILE_EXT) # file containing HCT table description
    HCT_ANNOTATOR_JSONfile = paste0(DATA_FILE,FORM_FILE_EXT) # file containing HCT table JSON annotator format
    
    fileSuffix = unlist(strsplit(DATA_FILE,split="_set"))
    curDataFileSuffix = curDataFile  ###paste0("set",fileSuffix[2])
    
    ## Find the templates of the current table
    tableName = gsub("_"," ",DATA_PREFIX)
    # print("**************************")
    # print(DATA_PREFIX)
    # print(tableName)
     
    for (numTable in 1:length(NLQ_PATTERNS)) {
      # print(NLQ_PATTERNS[[numTable]]$tableName)
      if (NLQ_PATTERNS[[numTable]]$tableName == tableName) break
    }
    NLQ_patterns = NLQ_PATTERNS[[numTable]]
    
    
    
    
    DBcsvFile = paste0(inputFolder,DB_CSVfile)
    if (file.exists(DBcsvFile)){
      # read DB data from csv
      DBdataIni = read.csv(DBcsvFile,stringsAsFactors=FALSE)
      
      DBdata = DBdataIni[2:nrow(DBdataIni),]
      rownames(DBdata) <- NULL
      DBdata$Value <- as.numeric(DBdata$Value)
      
      # read HCT data to get signature, formatValue and table id
      HCTdataSIG <- read_json(paste0(inputFolder,HCT_SIGNATURE_JSONfile)) 
      HCTdataANNOTATOR <- read_json(paste0(inputFolder,HCT_ANNOTATOR_JSONfile))
      HCTdataANNOTATOR_STR = toJSON(HCTdataANNOTATOR)
      
      STRformatValue = HCTdataSIG$formatValue
      STRsignature = HCTdataSIG$signature
      strTMP = strsplit(STRsignature,split="&&&")[[1]]
      colInd = which(strTMP=="COLS")
      rowInd = which(strTMP=="ROWS")
      styleInd = which(strTMP=="STYLE")
      aggNameInd = which(strTMP=="AGG_NAME")
      aggFunInd = which(strTMP=="AGG_FUN")
      aggColInd = which(strTMP=="AGG_COLS")
      aggRowInd = which(strTMP=="AGG_ROWS")
      
      ### READ ALL COLNAMES AND ROWNAMES DISPLAYED IN HCT
      colNames <- NULL
      rowNames <- NULL
      aggColNames <- NULL
      aggRowNames <- NULL
      for (i in (colInd+1):(rowInd-1)) colNames = c(colNames,str_trim(strTMP[i]))
      for (i in (rowInd+1):(styleInd-1)) rowNames =  c(rowNames,str_trim(strTMP[i]))
      
      COLdepth = length(colNames)
      ROWdepth = length(rowNames)
      
      if ((aggColInd+1)<aggRowInd) for (i in (aggColInd+1):(aggRowInd-1)) aggColNames =  c(aggColNames,str_trim(strTMP[i]))
      if ((aggRowInd+1)<length(strTMP)) for (i in (aggRowInd+1):length(strTMP)) aggRowNames =  c(aggRowNames,str_trim(strTMP[i]))
      
      if (is.null(aggColNames)) { aggColNames = "" }
      if (is.null(aggRowNames)) { aggRowNames = ""}
      
      if ((aggColInd+1)>=(aggRowInd-1)) {COLagg = 0 } else { COLagg = length(aggColNames)}
      if ((aggRowInd+1) >= length(strTMP)) {ROWagg = 0 } else { ROWagg = length(aggRowNames)}
      
      
      aggFun = strTMP[aggFunInd+1] # like min, max, sum...
      aggName = strTMP[aggNameInd+1] # like "Total"...
      
      # Cleaning row and col names to match with DB dataframe colnames ("abc de/fg.af-gh" -> "abc_de_fg_af_gh")
      colNames = getSQLattrNames(colNames)
      rowNames = getSQLattrNames(rowNames)
      aggColNames = getSQLattrNames(aggColNames)
      aggRowNames = getSQLattrNames(aggRowNames)
      colnames(DBdata) = getSQLattrNames(colnames(DBdata))
      
      # TEMPLATE REPORT
      nlPattern_report = NLQ_patterns[["template_report"]][[1]]
      
      # VALUE SEMANTIC
      value_meaning = NLQ_patterns[["valueMeaning"]][[1]]
      
      # SIMPLIFICATION OF LISTED NESTED ATTRIBUTES 
      simplifyNestedList = NLQ_patterns[["simplifyNested"]]
      
      # SQL operations -> English
      exprListName = NULL
      exprListName$sum = "total"
      exprListName$min = "minimum"
      exprListName$max = "maximum"
      exprListName$avg = "average"
      exprListName$count = "count"
      
      # Get all NL ATTRIBUTE NAMES from table description: 
      # read all available features in tables of that family (not necessarily in the current DBtable attributes)
      # needed to decode the NLQ patterns of those tables
      # -> ALL_NL_ATTR_NAMES
      tableValueName = gsub("_"," ",unlist(strsplit(HCTdataSIG$id,split = "_set"))[1]) # "Number_of_students_set1_1" -> "Number of students"
      for (itt in 1:length(TABLE_PATTERNS_JSON$tables)){
        curTablePattern = TABLE_PATTERNS_JSON$tables[[itt]]
        if (curTablePattern$valueName == tableValueName) {
          allAttrCodes = unlist(c(curTablePattern$rowCodes,curTablePattern$colCodes))
          ALL_NL_ATTR_NAMES = NULL
          for (curCode in allAttrCodes){
            for (icc in 1:length(SEMANTIC_DATA$data)){
              if (SEMANTIC_DATA$data[[icc]]$code == curCode) {
                ALL_NL_ATTR_NAMES = c(ALL_NL_ATTR_NAMES,unlist(SEMANTIC_DATA$data[[icc]]$names))
              }
            }
          }
        }
      }
      
      
      JSON_QandA <- list()
      NLQ_LIST <- list()
      ANSWER_LIST <- list()
      
      # 1 ############################################
      ################################################
      ################################################
      ###################################################           
      ###################################################           
      ###################################################
      
      if ((COLdepth <= COL_DEPTH_MAX) && (ROWdepth <= ROW_DEPTH_MAX) && (COLagg == COL_AGG_NUM) && (ROWagg == ROW_AGG_NUM)) {
          
        ###################################################           
        
        # get col and row headers of HCT table
        HCTcolHeaders = getHCTcolHeaders(DBdata,colNames,rowNames)
        HCTrowHeaders = getHCTrowHeaders(DBdata,colNames,rowNames)
        
        # FOR TABLE STATISTICS (VALID FOR ALL Q&A)
        COLnum = nrow(HCTcolHeaders)
        ROWnum = nrow(HCTrowHeaders)
        
        ################################################### 
        # example
        #
        # > HCTcolHeaders
        # Gender Year
        # 1   Male 2015
        # 2 Female 2015
        # 3   Male 2016
        # 4 Female 2016
        #
        # > HCTrowHeaders
        # Event Severity
        # 1    Fire   Slight
        # 5 Traffic   Slight
        #
        # > DBdata$Value
        # [1] 13 48 29 25 54 66 84 69
        #
        # > aggName
        # [1] "Average"
        #
        # > STRformatValue
        # [1] "%.2f"
        
        minVal = min(DBdata$Value)
        maxVal = max(DBdata$Value)
        signVal = ""
        if (minVal >=0) signVal = "positive"
        if (maxVal <0) signVal = "negative"
        
        if (STRformatValue == "%.2f"){
          typeVal = "real numbers" 
        } else {
          typeVal = "integers"
        }
        ###########
        # Create a table with $$positive numbers below 1000$$ representing the $$amount of imported and exported food in one year$$. 
        # Generate $$two or three$$ $$food item$$ names in the $$beverage, cereals, and dairy$$ $$categories$$, 
        # and create a $$row$$ for each. 
        # Add $$two$$ columns named $$import$$ and $$export$$, and a column $$averaging$$ their values.  
        
        
        
        # Create a table with $$positive numbers below 1000$$ representing the $$amount of imported and exported food in one year$$. 
        strPrompt1 = paste0("Create a table with numbers representing the ",tableValueName,". ")
        # Generate $$two or three$$ $$food item$$ names in the $$beverage, cereals, and dairy$$ $$categories$$, 
        # and create a $$row$$ for each. 
        if (ncol(HCTcolHeaders)==2){
          cnames = colnames(HCTcolHeaders)
          nameOfColDepth1 = gsub("\\."," ",gsub("_"," ",cnames[1]))
          nameOfColDepth2 = gsub("\\."," ",gsub("_"," ",cnames[2]))
          uniqValColDepth1 = unique(HCTcolHeaders[1])
          uniqValColDepth2 = unique(HCTcolHeaders[2])
          numValInCol1 = length(uniqValColDepth1)
          numValInCol2 = length(uniqValColDepth2)
          
          addPlural1 = ""
          if (numValInCol1>1) addPlural1 = "s"
          addPlural2 = ""
          if (numValInCol2>1) addPlural2 = "s"
          
          # remove col names if the values also contain same names
          listNamesDepth1  = unlist(strsplit(nameOfColDepth1," "))
          for (nm  in listNamesDepth1){
            if (nm %in% unlist(uniqValColDepth1)) {
              nameOfColDepth1 = ""
              addPlural1 = ""
              break
            }
          }
          
          # remove col names if the values also contain same names
          listNamesDepth2  = unlist(strsplit(nameOfColDepth2," "))
          for (nm  in listNamesDepth2){
            if (nm %in% unlist(uniqValColDepth2)) {
              nameOfColDepth2 = ""
              addPlural2 = ""
              break
            }
          }
          
          strDepth1 = paste0(getOxfordStyleGroup(unlist(uniqValColDepth1)," and ")," ",nameOfColDepth1,addPlural1)
          strPromptCOL1 = paste0("Generate columns named with ", getOxfordStyleGroup(unlist(uniqValColDepth2)," and ")," ",nameOfColDepth2,addPlural2," for ",strDepth1,". ")
        } else { # ncol(HCTcolHeaders)==1
          cnames = colnames(HCTcolHeaders)
          nameOfColDepth1 = gsub("\\."," ",gsub("_"," ",cnames[1]))
          
          uniqValColDepth1 = unique(HCTcolHeaders[1])
          numValInCol1 = length(uniqValColDepth1)
          addPlural1 = ""
          if (numValInCol1>1) addPlural1 = "s"
          
          # remove col names if the values also contain same names
          listNamesDepth1  = unlist(strsplit(nameOfColDepth1," "))
          for (nm  in listNamesDepth1){
            if (nm %in% unlist(uniqValColDepth1)) {
              nameOfColDepth1 = ""
              addPlural1 = ""
              break
            }
          }
          
          strDepth1 = paste0(getOxfordStyleGroup(unlist(uniqValColDepth1)," and ")," ",nameOfColDepth1,addPlural1," columns")
          strPromptCOL1 = paste0("Generate ",strDepth1,". ")
        }
        
        # Add $$two$$ columns named $$import$$ and $$export$$, and a column $$averaging$$ their values.  
        if (ncol(HCTrowHeaders)==2){
          rnames = colnames(HCTrowHeaders)
          nameOfRowDepth1 = gsub("\\."," ",gsub("_"," ",rnames[1]))
          nameOfRowDepth2 = gsub("\\."," ",gsub("_"," ",rnames[2]))
          uniqValRowDepth1 = unique(HCTrowHeaders[1])
          uniqValRowDepth2 = unique(HCTrowHeaders[2])
          numValInRow1 = length(uniqValRowDepth1)
          numValInRow2 = length(uniqValRowDepth2)
          
          addPlural1 = ""
          if (numValInRow1>1) addPlural1 = "s"
          addPlural2 = ""
          if (numValInRow2>1) addPlural2 = "s"
          
          # remove col names if the values also contain same names
          listNamesDepth1  = unlist(strsplit(nameOfRowDepth1," "))
          for (nm  in listNamesDepth1){
            if (nm %in% unlist(uniqValRowDepth1)) {
              nameOfRowDepth1 = ""
              addPlural1 = ""
              break
            }
          }
          
          # remove col names if the values also contain same names
          listNamesDepth2  = unlist(strsplit(nameOfRowDepth2," "))
          for (nm  in listNamesDepth2){
            if (nm %in% unlist(uniqValRowDepth2)) {
              nameOfRowDepth2 = ""
              addPlural2 = ""
              break
            }
          }
          
          strDepth1 = paste0(getOxfordStyleGroup(unlist(uniqValRowDepth1)," and ")," ",nameOfRowDepth1,addPlural1)
          strPromptROW1 = paste0("Add rows named with ", getOxfordStyleGroup(unlist(uniqValRowDepth2)," and ") , " ",nameOfRowDepth2,addPlural2," for ",strDepth1,". ")
        } else { # ncol(HCTrowHeaders)==1
          rnames = colnames(HCTrowHeaders)
          nameOfRowDepth1 = gsub("\\."," ",gsub("_"," ",rnames[1]))
          uniqValRowDepth1 = unique(HCTrowHeaders[1])
          numValInRow1 = length(uniqValRowDepth1)
          addPlural1 = ""
          if (numValInRow1>1) addPlural1 = "s"
          
          # remove col names if the values also contain same names
          listNamesDepth1  = unlist(strsplit(nameOfRowDepth1," "))
          for (nm  in listNamesDepth1){
            if (nm %in% unlist(uniqValRowDepth1)) {
              nameOfRowDepth1 = ""
              addPlural1 = ""
              break
            }
          }
          
          strDepth1 = paste0(getOxfordStyleGroup(unlist(uniqValRowDepth1)," and ")," ",nameOfRowDepth1,addPlural1)
          strPromptROW1 = paste0("Add a row for each of ",strDepth1,". ")
        }
        strPrompt2 = paste0("Populate the table with ",signVal," ",typeVal,".")
        
        
        strPromptVariant1 = paste0(strPrompt1,strPromptCOL1,strPromptROW1,strPrompt2)
        
        
        print(strPromptVariant1)
        
        
        
        # Make a table with two columns and five rows containing the amount of food imported or exported in tons in one year. 
        # Invent realistic numbers for a small country. 
        # Put the following items and types of food in rows: oats and rice cereals, as well as butter, cream, and milk dairy. 
        # Name the columns import and export.  
        
        # Create a table of import and export quantities of food in tons. 
        # Put the import and export amounts in two columns. 
        # Put the type of food in rows, picking between two and three items for each of the cereals, dairy, and meat categories. 
        # Add positive real numbers in the hundreds.
        
        # Make a table with five columns and five rows. 
        # Name the columns by USA states and cities, with Boston, Cambridge, Lenox, Detroit and Flint. 
        # Add rows indicating light and thermal air pollution, and industrial, inferior irrigation, and inorganic fertilizers soil pollutions. 
        # Generate random numbers between -100 and 100 to populate the table.
        
        # Create a table with real numbers between -100 and 100. 
        # For the rows, use air and soil polluted elements, and add light and noise as types of air pollution and industrial and inferior irrigation as type of soil pollution. 
        # For the columns, use the cities of Albany and Columbus from Georgia, Champaign and Springfield in Illinois, and Baltimore and Columbia in Maryland. 
        # Give the columns the name of the state and the city, avoid repetition. 
        # Add the average per state as an additional column for each state and add the average across all state as the last column. 
        
        # Generate a table with real numbers between -100 and 100. 
        # Use polluted elements in rows and state and cities in columns. 
        # Pick two cities in Illinois and three cities in Maryland. 
        # Use soil and water for the polluted elements; for each, add one row per type of pollution. 
        # The types of pollution for soil are industrial, inferior irrigation, inorganic fertilizers, and solid waste. 
        # The types of pollution for water are Groundwater, oxygen-depletion, and surface water.
        
        # PROMPT
        
        #     ###############
        kkk = kkk+1
        dumpNLprompt1[kkk] = strPromptVariant1
        dumpNLfileSuffix[kkk] = curDataFileSuffix
        
      }
      
      
      
      ###################################################           
      
      
      
      
      ###################################################           
      
      
      
    }
  } ## END of for each data file with the same prefix
  ###############
  ## SAVE ALL THE Q&A of the same prefix
  dumpNLdf = data.frame(tableFile = dumpNLfileSuffix[1:kkk], 
                        prompt1 = dumpNLprompt1[1:kkk],
                        stringsAsFactors = FALSE)
  if (kkk>0){
    dumpNLdfALL = rbind(dumpNLdfALL,dumpNLdf)
    #write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE)
    write.csv(dumpNLdfALL,paste0(outputFolder,OUTPUT_FILE_NAME,".csv"),row.names=FALSE,quote=TRUE)
  }
  
  
} # END MAIN FOR LOOP ACROSS PREFIX
# JSON_annotator_all = paste0("{",paste0(ALL_TABLE_TEMPLATE,collapse = ","),"}")
# JSON_annotator_all_correct = gsub("'",'"',JSON_annotator_all)
# write.table(JSON_annotator_all_correct, file = "table_questions_answers_annotations.json", row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)




