################## OLD VERSION
################## USE VERSION 2 INSTEAD OF THIS VERSION !!!
##################

#### 1) use "geneAllTablesJSONfromPatterns.R" to generate a list of table templates from a tablePattern.json template and semantics.json
#### 2) use "geneHCTfromJSONtables.R" next to generate all the HCT and DB tables from this list of table templates 
#### 3) use "geneSQLandNLQfromJSONtemplates.R" to generate SQL and NLQ question and answers from the corresponding DB and HCT tables.



#### QUERY GENERATOR

## READ ..._DB.csv data
## READ ..._HCT.json data $signature to get ROWS and COLS of the HCT table

## use sqldf: https://www.playerzero.ai/advanced/r-faqs/how-to-use-the-sqldf-package-in-r-for-sql-on-data-frames 

##############################
### TODO

# BUG TEMP 5 AGG col inexistant "set117_1" and "set127_1"

# check all template on variant of same table (number of graduation)
# check same tamplate on another table content and its variant
# then generate the interface json to ease manual check

# DONE: PATTERNS 15 IS NOT GENERIC, even on number of graduation table, it can only work with year and language. Need adaptation for gender and degree
# DONE: clean up to replace a or b or c by a, b, or c
# DONE: clean up to replace a and b and c by a, b, and c


# DO: 
# in__$Quarter==of==$Year => in Q1 of 2023 / in Q1 / in 2023  
# in__$City==,==$State => in Malibu, California / in Malibu / in California  
# of__$Subtype==$Food_type => of Cow Meat / of Cow / of Meat

# CHECK:  of_((pollution)) of_$Type_of_pollution_pollution of_$Polluted_element  vs  of_((pollution)) of_$Type_of_pollution_pollution__of_$Polluted_element

# NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
# generate some NLP templates for each query
# check that the answer from DB is present in the HCT too in case of NAN)

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
source("toolboxSQLandNLQ.R")



################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################

generateNLQ = TRUE

# LIST_DATA_PREFIX = c("Number_of_students",
#                      "Number_of_graduations",
#                      "Evolution_of_pollution_in_percent",
#                      "Food_import-export_in_tons",
#                      "Weather_statistics",
#                      "Number_of_constructions",
#                      "Number_of_accidents")
LIST_DATA_PREFIX = c("Number_of_graduations")

for (DATA_PREFIX in LIST_DATA_PREFIX){
  
  
  set.seed(1) # set.seed(1) 
  
  
  # DATA_PREFIX = "Number_of_students" ### NLQ pass without BUG -- Quality check STILL REQUIRED  ===> BUG 109_1
  # DATA_PREFIX = "Number_of_graduations"  ### NLQ pass without BUG -- Quality check ok
  # DATA_PREFIX = "Evolution_of_pollution_in_percent" ### NLQ pass without BUG -- Quality check ok
  # DATA_PREFIX = "Food_import-export_in_tons" ### NLQ pass without BUG -- Quality check ok
  
  # DATA_SET = "set129_1" #"set127_1" ## BUG TEMP 5 AGG col inexistant "set117_1"
  
  
  # DATA_FILE = paste0(DATA_PREFIX,"_",DATA_SET)
  # DATA_PREFIX = "Food_import-export_in_tons"
  # DATA_SET = "set357_1"
  # DATA_FILE = paste0(DATA_PREFIX,"_",DATA_SET)
  # DATA_FILE = "Number_of_students_set171_1"
  # DATA_FILE = "Food_import-export_in_tons_set357_1"
  # DATA_FILE = "Evolution_of_pollution_in_percent_set127_1"
  
  dumpName = DATA_PREFIX
  
  dataFolder  = "./PARAMETERS/" 
  inputFolder  = "./TABLES_NEW_ONLY/" 
  outputFolder = "./QandA_DEBUG_2/"
  
  #SIG_FILE_EXT = "_SIG_DB.json"   # use only if DB tables are available, does not contain agg info, does not allow to generate queries
  SIG_FILE_EXT = "_SIG_HCT.json" # use when HCT pivot tables have been generated
  
  DATA_TABLE_PATTERNS = "tablePatterns_all.json"
  DATA_TABLE_INSTANCES = "tablesToGenerate_all.json"
  DATA_TABLE_SEMANTICS = "semantics.json"
  NLQ_PATTERN_FILE = "NLquestionPatterns.json"
  
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

#  }
#if (FALSE){
  
  kkk = 0
  dumpNLquestions = character(numDataFiles*20)
  dumpNLindex = character(length = numDataFiles*20)
  dumpNLtemplate = character(length = numDataFiles*20)
  dumpNLfileSuffix = character(length = numDataFiles*20)
  dumpSQLquestions =  character(numDataFiles*20)
  dumpSQLresults =  character(numDataFiles*20)
  
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ###for (indDataFile in 1:numDataFiles){
       #for (indDataFile in 8) { #numDataFiles){
  for (indDataFile in 1:1){
    
    ###curDataFile = fileListToProcess[indDataFile]
    curDataFile = "Number_of_graduations_set164_5"
    #curDataFile = "Food_import-export_in_tons_set100_1"
    # DEBUG
    
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    print(paste0("**************** PROCESSING FILE : ", curDataFile," -- ", indDataFile ,"/",numDataFiles,"*************************"))
    
    DATA_FILE = curDataFile
    
    sqlJSONstr <- NULL
    sqlnlqJSONstr <- NULL
    NLquestions <-NULL
    
    outputNameQuestions = "_QandA.json"
    
    
    DB_CSVfile = paste0(DATA_FILE,"_DB.csv") # file containing DB data
    HCT_SIGNATURE_JSONfile = paste0(DATA_FILE,SIG_FILE_EXT) # file containing HCT table description
    
    fileSuffix = unlist(strsplit(DATA_FILE,split="_set"))
    curDataFileSuffix = paste0("set",fileSuffix[2])
    
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
    
    # print(NLQ_patterns$tableName)
    # print("**************************")
    
    
    
    DBcsvFile = paste0(inputFolder,DB_CSVfile)
    if (file.exists(DBcsvFile)){
      # read DB data from csv
      DBdataIni = read.csv(DBcsvFile,stringsAsFactors=FALSE)
      
      DBdata = DBdataIni[2:nrow(DBdataIni),]
      rownames(DBdata) <- NULL
      DBdata$Value <- as.numeric(DBdata$Value)
      
      # read HCT data to get signature, formatValue and table id
      HCTdataSIG <- read_json(paste0(inputFolder,HCT_SIGNATURE_JSONfile)) 
      
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
      for (i in (colInd+1):(rowInd-1)) colNames = c(colNames,strTMP[i])
      for (i in (rowInd+1):(styleInd-1)) rowNames =  c(rowNames,strTMP[i])
      
      if ((aggColInd+1)<aggRowInd) for (i in (aggColInd+1):(aggRowInd-1)) aggColNames =  c(aggColNames,strTMP[i])
      if ((aggRowInd+1)<length(strTMP)) for (i in (aggRowInd+1):length(strTMP)) aggRowNames =  c(aggRowNames,strTMP[i])
      
      if (is.null(aggColNames)) aggColNames = ""
      if (is.null(aggRowNames)) aggRowNames = ""
      
      
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
      value_meaning = NLQ_patterns[["valueMeaning"]][[1]]
      
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
      
      
      # 1 ############################################
      ################################################
      ################################################
      ###################################################           
      ### SINGLE-CELL ONE ROW AND ONE COLUMN SELECTION FROM HCT
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 1: 1R & 1C"))
      numTemplate = 1
      
      # get col and row headers of HCT table
      HCTcolHeaders = getHCTcolHeaders(DBdata,colNames,rowNames)
      HCTrowHeaders = getHCTrowHeaders(DBdata,colNames,rowNames)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      print(sqlClause)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 1
        #SELECT Value FROM DBdata WHERE ((Degree = 'BSc' AND Gender = 'Male')) AND ((Year = '2019/2020' AND Language = 'Spanish’))
        # What is the number of graduations of_Male students with_a_BSc_degree in_Spanish in_2019/2020 ?
        HCTheaders = cbind(HCTcolHeaders[indColSel,,drop=FALSE],HCTrowHeaders[indRowSel,,drop=FALSE])
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # single
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # single
        
        print("HCT")
        #print(NLQ_patterns)
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      
      # 2 ############################################
      ################################################
      ################################################
      ################################################
      ###################################################           
      ### MULTI-CELL SELECTION FROM ONE COLUMN AND MULTI ROWS IN HCT
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 2: nR & 1C"))
      numTemplate = 2
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 10)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 2
        # SELECT Value FROM DBdata WHERE ((Degree = 'BSc' AND Gender = 'Female')) 
        # AND ((Year = '2020/2021' AND Language = 'English') OR (Year = '2021/2022' AND Language = 'English') OR (Year = '2020/2021' AND Language = 'French’))
        # What is the number of graduations of_Female_students with_a_BSc_degree in_English in_2020/2021 or in_English in_2021/2022 or in_French in_2020/2021 ?  
        # What is the number of graduations of_$Gender_students with_a_$Degree in_$Language in_$Year ?
        
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # single
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
        }
        
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      # 3 ############################################
      ################################################
      ################################################
      ################################################
      ###################################################           
      ### MULTI-CELL SELECTION FROM ONE ROW AND MULTI COLUMNS IN HCT
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 3: 1R & nC"))
      
      numTemplate = 3
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 10)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 3
        # SELECT Value FROM DBdata WHERE 
        # ((Degree = 'BSc' AND Gender = 'Male') OR (Degree = 'MSc' AND Gender = 'Male') OR (Degree = 'PhD' AND Gender = 'Male’)) 
        # AND ((Year = '2022/2023' AND Language = 'German'))
        # What is the number of graduations 
        # of_Male students with_a_BSc_degree or of_Male_students with_a_MSc_degree or of_Male_students 
        # with_a_PhD_degree in_German in_2022/2023 ? 
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR 
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # single
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      
      # 4 ############################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Multi-cell + expression
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 4: 1R & nC + expression"))
      
      numTemplate = 4
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 10)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      exprListSQL = c("sum","min") #,"max","avg","count") # min-max, avg/std, sum, count
      
      exprClause <- geneExpressionClause(exprListSQL)
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 4
        # SELECT sum(Value),min(Value) FROM DBdata 
        # WHERE ((Degree = 'BSc' AND Gender = 'Male') OR (Degree = 'MSc' AND Gender = 'Male') OR (Degree = 'BSc' AND Gender = 'Female’))
        # What are the Total and Minimum numbers of graduations 
        # of_Male_students with_a_BSc_degree or of_Male_students with_a_MSc_degree or of_Female_students with_a_BSc_degree ?
        
        # What is/are the $EXPR number/numbers of graduations... ?
        # What is the Total number of graduations... ?
        # What are the Total and Min numbers of graduations...  ?
        
        
        # $EXPR -> EXPRval
        # if (length(EXPRval)==1) a/b -> a else a/b -> b
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        
        
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      # 5 ############################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Multi-cell + expression
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 5: 1R & nC + expression from the table"))
      
      numTemplate = 5
      
      # get pattern of the current table 
      
      aggColAttrNames = aggColNames 
      #aggRowAttrNames = aggRowNames 
      
       
      
      # run only if HCT contains an aggregated column
      if (length(aggColAttrNames)>=1 && (nchar(aggColAttrNames[1])>0)){
        
        aggFun1 = aggFun
        aggName1 = aggName
        
        
        # then query all element of that column with that aggregation on the result
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
        # pick one column attribute at random among the ones which have an aggregate displayed, to generate all subcells
        indColAgg = sample(1:length(aggColAttrNames))[1]
        aggColToGen = aggColAttrNames[indColAgg]
        indColSel = which(colnames(HCTcolHeaders) == aggColToGen)
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        res <- geneAggregatedSelectionSQL(HCTcolHeaders,aggColToGen)
        otherColNames = res$otherColNames
        otherColValues = res$otherColValues
        geneColNames = res$geneColNames
        geneColValues = res$geneColValues
        colClause = res$colClause
        
        #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
        exprClause <- geneExpressionClause(aggFun1)
        
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
        sqlResultSTR
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        #### NLQ generation
        if (generateNLQ){
          # TEMPLATE 5
          # SELECT avg(Value) FROM DBdata WHERE (Degree IN ('BSc','MSc','PhD')) AND ((Year = '2021/2022' AND Language = 'French'))
          # What is the Average number of graduations with_a_BSc or MSc or PhD_degree in_French in_2021/2022 ?
          
          # What is/are the $EXPR number/numbers of graduations... ?
          # What is the Total number of graduations... ?
          # What are the Total and Min numbers of graduations...  ?
          
          # $EXPR -> EXPRval
          # if (length(EXPRval)==1) a/b -> a else a/b -> b
          #HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
          HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
          
          HCTcolH = data.frame(x=geneColValues)
          colnames(HCTcolH) = geneColNames
          for (i in 1:length(otherColValues)){
            HCTcolH[[otherColNames[i]]] = rep(otherColValues[i],length(geneColValues))
          }
          
          templateName = paste0("template_",numTemplate)
          
          NLquestions[[numTemplate]]<-list()
          
          
          for (numVariant in 1:length(NLQ_patterns[[templateName]])){
            
            nlPattern = NLQ_patterns[[templateName]][[numVariant]]
            print(nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,aggFun1,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
            
            print(nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            ###############
            
            
          }
          
          #print(sqlClause)
          #print(sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
          
          
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        
      }
      
      
      
      # 6 ############################################
      ################################################
      ################################################
      ################################################
      ###################################################           
      ### MULTI-CELL SELECTION FROM MULTI ROWS AND MULTI COLUMNS IN HCT
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 6: nR & nC"))
      
      numTemplate = 6
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 4)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 5)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 6
        # SELECT Value FROM DBdata 
        # WHERE ((Degree = 'BSc' AND Gender = 'Male') OR (Degree = 'BSc' AND Gender = 'Female')) 
        # AND ((Year = '2020/2021' AND Language = 'English') OR (Year = '2022/2023' AND Language = 'French’))
        # What is the number of graduations of_male students with_a_BSc_degree or of_female students with_a_BSc_degree 
        # in_English in_2020/2021 or in_French in_2022/2023 ? 
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      #break
      
      
      # 7 ############################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Multi-cell + expression
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 7: nR & 1C + aggregation"))
      
      numTemplate = 7
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      exprListSQL = c("sum","min")
      exprClause <- geneExpressionClause(exprListSQL)
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse = COL_SEP)
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 7
        # SELECT sum(Value),min(Value) FROM DBdata 
        # WHERE ((Degree = 'MSc' AND Gender = 'Female')) 
        # AND ((Year = '2022/2023' AND Language = 'French') OR (Year = '2019/2020' AND Language = 'Spanish’))
        # What are the Total and Minimum numbers of graduations of_female students with_a_MSc_degree
        # in_ French in_ 2022/2023 or in_Spanish in_2019/2020 ?
        
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        print(sqlResultSTR)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      # 8 ############################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Multi-cell + expression group by HCT columns
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 8: nR & nC + GROUP BY per column"))
      
      numTemplate = 8
      
      indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 5)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      exprListSQL = c("min","max")
      exprClause <- geneExpressionClause(exprListSQL)
      
      # generate the query with GT for the value
      # group by must be over the column headers 
      groupByAttrNames = colnames(HCTcolHeaders)
      groupByClause = paste0(groupByAttrNames,collapse=",")
      
      sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 8
        # SELECT DEGREE,GENDER,min(Value),max(Value) FROM DBdata 
        # WHERE ((Degree = 'MSc' AND Gender = 'Male') OR (Degree = 'BSc' AND Gender = 'Female')) 
        # AND ((Year = '2022/2023' AND Language = 'English') OR (Year = '2020/2021' AND Language = 'French’)) GROUP BY DEGREE,GENDER
        # What are the Minimum and Maximum numbers of graduations 
        # for_each_DEGREES, and GENDERS, 
        # of_Male_students with_a_MSc_degree or of_Female_students with_a_BSc_degree
        # in_English in_2022/2023 or in_French in_2020/2021 ?
        # Please,_report_the_corresponding _DEGREES and GENDERS 
        
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        nlReportClean = geneReportAttr(getNLattrNames(groupByAttrNames,ALL_NL_ATTR_NAMES),nlPattern_report)
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = paste(nlQuestion,nlReportClean)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      # 9 ############################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 9: nR & 1C + GROUP BY per row (level 1)"))
      
      numTemplate = 9
      
      HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
      #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      exprListSQL = c("min")
      exprClause <- geneExpressionClause(exprListSQL)
      
      # generate the query with GT for the value
      # group by must be over the row headers at level 1
      groupByAttrNames = colnames(HCTrowHeaders)[1]
      groupByClause = paste0(groupByAttrNames,collapse=",")
      
      sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 9
        # SELECT min(Value) FROM DBdata 
        # WHERE ((Degree = 'MSc' AND Gender = 'Female')) AND (Year IN ('2019/2020','2020/2021','2021/2022')) GROUP BY YEAR
        # What is the Minimum number of graduations 
        # for_each_YEAR of_Female_students with_a_MSc_degree in_2019/2020 or 2020/2021 or 2021/2022 ?
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowLev1[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        nlReportClean = geneReportAttr(getNLattrNames(groupByAttrNames,ALL_NL_ATTR_NAMES),nlPattern_report)
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = paste(nlQuestionClean)
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      # 10 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 10: nR & 1C + GROUP BY per row (level 1) + report group name"))
      
      numTemplate = 10
      
      HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
      #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      exprListSQL = c("min")
      exprClause <- geneExpressionClause(exprListSQL)
      
      # generate the query with GT for the value
      # group by must be over the row headers at level 1
      groupByAttrNames = colnames(HCTrowHeaders)[1]
      groupByClause = paste0(groupByAttrNames,collapse=",")
      
      sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 10
        # SELECT YEAR,min(Value) FROM DBdata 
        # WHERE ((Degree = 'BSc' AND Gender = 'Female')) AND (Year IN ('2019/2020','2020/2021','2021/2022')) GROUP BY YEAR
        # What is the Minimum number of graduations of_Female_students with_a_BSc_degree 
        # for_each_YEAR in_2019/2020, 2020/2021 or 2021/2022? Please,_report_the_corresponding _YEAR
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowLev1[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        nlReportClean = geneReportAttr(getNLattrNames(groupByAttrNames,ALL_NL_ATTR_NAMES),nlPattern_report)
      
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = paste(nlQuestion,nlReportClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      # 11 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 11: nR & nC + GROUP BY per row (level 1) and per columns + report group name"))
      
      numTemplate = 11
      
      HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
      #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2,3)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      exprListSQL = c("min")
      exprClause <- geneExpressionClause(exprListSQL)
      
      # group by must be over the row headers at level 1 and over the column headers 
      groupByAttrNames = c(colnames(HCTrowHeaders)[1],colnames(HCTcolHeaders))
      groupByClause = paste0(groupByAttrNames,collapse=",")
      
      # generate the query with GT for the value
      sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      
      #### NLQ generation
      if (generateNLQ){
        
        # TEMPLATE 11
        # SELECT YEAR,DEGREE,GENDER,min(Value) FROM DBdata 
        # WHERE ((Degree = 'BSc' AND Gender = 'Male')  OR (Degree = 'MSc' AND Gender = 'Female'))
        # AND (Year IN ('2019/2020','2020/2021','2021/2022')) GROUP BY YEAR,DEGREE,GENDER
        # What is the Minimum number of graduations for_each_YEAR, DEGREE, and GENDER 
        # of_Female_students with_a_BSc_degree in_2019/2020 or 2020/2021 or 2021/2022? 
        # Please,_report_the_corresponding_ YEAR, DEGREE, and GENDER 
        
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowLev1[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        nlReportClean = geneReportAttr(getNLattrNames(groupByAttrNames,ALL_NL_ATTR_NAMES),nlPattern_report)
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = paste(nlQuestion,nlReportClean)
          
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      # 12 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 12: nR & 1C + TOP/BOTTOM K + ROW FILTER"))
      
      numTemplate = 12
      # NO FILTER
      # rowClause <- "TRUE"
      
      # FILTER ANY ROWS
      # rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,2,4)
      
      # FILTER GROUP OF ROWS BY LEVEL 1
      HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2,2) # use values to select a subset of rows Level 1
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      
      orderByClause <- "ASC" #"DESC" # "ASC"
      firstK <- 5
      
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause," ORDER BY VALUE ",orderByClause," LIMIT ",firstK)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      
      #### NLQ generation
      if (generateNLQ){
        # TEMPLATE 12
        # SELECT Value FROM DBdata
        # WHERE ((Degree = 'BSc' AND Gender = 'Female')) AND (Year IN ('2019/2020','2021/2022')) 
        # ORDER BY Value ASC/DESC LIMIT 5
        # What are the bottom_5 / 5_smallest/ top_5 / 5_largest numbers of graduations 
        # of_Female_students with_a_BSc_degree in_2019/2020 or 2021/2022 ?
        # (If no LIMIT provided) What are the top/bottom_K numbers of graduations 
        # of_Female_students with_a_BSc_degree in_2019/2020 or 2021/2022 ?
        
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowLev1[indRowSel,,drop=FALSE]  # repeated with OR
        
        orderByK <- NULL
        orderByK$K = firstK
        orderByK$orderBy = orderByClause
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,orderByK,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = nlQuestion
          
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      # 13 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 13: nR & 1C + ROW FILTER + ORDER BY"))
      
      numTemplate = 13
      # NO FILTER
      # rowClause <- "TRUE"
      
      # FILTER ANY ROWS
      # rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,2,4)
      
      # FILTER GROUP OF ROWS BY LEVEL 1
      HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
      
      indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2,2) # use values to select a subset of rows Level 1
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
      
      rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
      
      orderByClause <- "ASC" #"DESC" # "ASC"
      firstK <- 0
      
      sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause," ORDER BY VALUE ",orderByClause)
      sqlResult = sqldf(sqlClause)
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR <- formatResults(sqlResult)
      
      sqlResultSTR
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
      
      #### NLQ generation
      if (generateNLQ){
        
        # TEMPLATE 13
        # SELECT Value FROM DBdata
        # WHERE ((Degree = 'BSc' AND Gender = 'Female')) AND (Year IN ('2019/2020','2021/2022')) 
        # ORDER BY Value ASC/DESC
        # What are the bottom_5 / 5_smallest/ top_5 / 5_largest numbers of graduations 
        # of_Female_students with_a_BSc_degree in_2019/2020 or 2021/2022 ?
        # (If no LIMIT provided) What are the numbers of graduations 
        # of_Female_students with_a_BSc_degree in_2019/2020 or 2021/2022 ordered_by_ascending/descending_values ?
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = HCTrowLev1[indRowSel,,drop=FALSE]  # repeated with OR
        
        orderByK <- NULL
        orderByK$K = firstK
        orderByK$orderBy = orderByClause
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1:length(NLQ_patterns[[templateName]])){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,groupByAttrNames,orderByK,NULL,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = nlQuestion
          
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
        }
        print(sqlClause)
        print(sqlResult)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
      }
      
      
      
      
      # 14 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 14: nR & 1C + operation on column"))
      
      numTemplate = 14
      
      # NO FILTER
      rowClause <- "TRUE"
      
      indRowSel <- NULL
      indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
      colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
      
      indColSel14 = indColSel
      
      attrList = colnames(HCTrowHeaders)
      selectionClause = paste0(attrList,collapse=",")
      attrList14 = attrList
      
      #indRandVal = sample(length(DBdata$Value))[1]
      #operationVAL = DBdata$Value[indRandVal] # take one value at random as threshold
      operationVAL = round(mean(DBdata$Value),2) # take the average value in the table as threshold
      #operationVAL = 120 
      
      allOps = c(" > "," < ")
      indRandOps = sample(2)
      indRandOp1 = indRandOps[1]
      indRandOp2 = indRandOps[2]
      operationOP = allOps[indRandOp1]  
      
      operationClause = paste0("Value",operationOP,operationVAL)
      
      sqlClause14 = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",operationClause)
      sqlResult14 = sqldf(sqlClause14)
      sqlResultSTR14 <- formatResults(sqlResult14)
      
      #print(sqlClause14)
      #print(sqlResult14)
      
      if (nrow(sqlResult14) == 0) {
        print("NO RESULT, TRY REVERSE OP")
        # no result. try with the other operation
        operationOP = allOps[indRandOp2]  
        
        operationClause = paste0("Value",operationOP,operationVAL)
        
        sqlClause14 = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",operationClause)
        sqlResult14 = sqldf(sqlClause14)
        
        #print(sqlClause14)
        #print(sqlResult14)
        
      }
      
      # sqlClause
      # result 
      # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
      sqlResultSTR14 <- formatResults(sqlResult14)
      
      sqlResultSTR14
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14)
      
      #### NLQ generation
      if (generateNLQ){
        
        
        # TEMPLATE 14
        # SELECT Year,Language FROM DBdata WHERE ((Degree = 'PhD' AND Gender = 'Male')) AND Value < 120
        # What are the YEAR and LANGUAGE 
        # for_which_(the number of graduations of_Male_students with_a_PhD_degree)_is_lower_than_120 ?
        # "What is/are_the_$OPATTR_for_which the number of graduations 
        # of_$Gender_students with_a_$Degree in_$Language in_$Year is_$OPANDVAL ?"
        
        HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # repeated with OR
        HCTrowH = NULL #HCTrowHeaders[indRowSel,,drop=FALSE]  # repeated with OR
        
        
        forWhich <- NULL
        forWhich$OPATTR <- attrList
        forWhich$OPTYPE <- operationOP
        forWhich$OPVAL <- operationVAL
        
        
        templateName = paste0("template_",numTemplate)
        
        NLquestions[[numTemplate]]<-list()
        for (numVariant in 1){
          
          nlPattern = NLQ_patterns[[templateName]][[numVariant]]
          print(nlPattern)
          
          
          nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,forWhich,nlPattern,ALL_NL_ATTR_NAMES)
          
          nlQuestionClean = nlQuestion
          nlQuestionClean14 = nlQuestionClean
          
          print(nlQuestionClean)
          
          NLquestions[[numTemplate]][numVariant] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause
          dumpSQLresults[kkk] = sqlResultSTR
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          ###############
          
          
          
        }
        print(sqlClause14)
        print(sqlResult14)
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14,NLquestions[[numTemplate]])
        
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14,"NA")
      }
      
      
      
      # 15 ###########################################
      ################################################
      ################################################
      ################################################
      ################################################
      # Local aggregation (per group of rows) 
      print(paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 15: conditional "))
      
      numTemplate = 15
      
      sqlResult14 = sqldf(sqlClause14)
      
      if (nrow(HCTcolHeaders)>1) {
        indColSel15=indColSel14
        while (indColSel15==indColSel14) indColSel15 <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
      } else {
        indColSel15=indColSel14
      }
      
      attrList15SQL = c(attrList14,"Value")
      
      if (is.null(value_meaning)) {
        attrList15 = c(attrList14,"Value")
      } else {
        attrList15 = c(attrList14,value_meaning)
      }
      
      selectionClause =  paste0(attrList15SQL,collapse=",")
      
      if (nrow(sqlResult14)>0){
        
        indRowSel <- geneIndexForRandomClauses(sqlResult14) # use values to select a subset of rows Level 1
        
        
        rowClause <- geneMultiRowOrColSelectionSQL(sqlResult14,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel15)
        
        
        sqlClause = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        
        
        # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
        sqlResultSTR <- formatResults(sqlResult)
        
      } else {
        sqlResult=NULL
        sqlResultSTR = ""
      }
      
      #print(sqlClause)
      #print(sqlResultSTR)
      
      
      sqlClause15 = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause, " AND (",paste0(attrList14,collapse=","),") IN (",sqlClause14,")")
      sqlResult15 = sqldf(sqlClause15)
      sqlResultSTR15 = formatResults(sqlResult15)
      
      #print(sqlClause15)
      #print(sqlResultSTR15)
      
      sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15)
      
      #### NLQ generation
      if (generateNLQ){
        
        # TEMPLATE 15
        # SELECT Value FROM DBdata
        # WHERE ((Degree = 'BSc' AND Gender = 'Female')) 
        # AND (SELECT Year,Language FROM DBdata WHERE ((Degree = 'PhD' AND Gender = 'Male')) AND Value < 120) 
        
        # ------- > SELECT Value FROM DBdata WHERE ((Degree = 'BSc' AND Gender = 'Female')) 
        # AND ((Year = '2021/2022' AND Language = 'English') OR (Year = '2019/2020' AND Language = 'German’))
        
        # What is the [number of graduations of_Female students with_a_BSc_degree in_Spanish in_2021/2022 or in_German in_2019/2020] = EXPR
        # What are the [YEAR and LANGUAGE] for_which_(the number of graduations of_Male_students with_a_PhD_degree)_is_lower_than_120
        # ------- >  What is the [number of graduations of_Female students with_a_BSc_degree in_the_YEAR and LANGUAGE]
        # for_which_(the number of graduations of_Male_students with_a_PhD_degree)_is_lower_than_120 ?
        
        #"What is the number of graduations of_$Gender_students with_a_$Degree in languages and years for which the number of graduations of_$Gender_students with_a_$Degree is_$OPANDVAL ?"
        
        HCTcolH = HCTcolHeaders[indColSel15,,drop=FALSE]  # repeated with OR
        
        
        nlReportClean = geneReportAttr(attrList15,nlPattern_report)
        
         
        templateNamePREM = paste0("template_",1)
        
        nlPattern = NLQ_patterns[[templateNamePREM]][[1]]
        PREM = gsub("\\?","",geneNLQselectExpress(HCTcolH,NULL,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES))
        
        SEC = geneForWhichPremAttr(attrList14,nlPattern,ALL_NL_ATTR_NAMES)
        
        COND = unlist(strsplit(nlQuestionClean14,split="for which"))[2]
        
        nlQuestionClean = stringr::str_squish(paste0(PREM," ",SEC," for which ", COND," ",nlReportClean))
        
        print(nlQuestionClean)
        
        NLquestions[[numTemplate]]<-list()
        NLquestions[[numTemplate]][1] = nlQuestionClean
        
        ###############
        kkk = kkk+1
        dumpSQLquestions[kkk] = sqlClause
        dumpSQLresults[kkk] = sqlResultSTR
        dumpNLquestions[kkk] = nlQuestionClean
        dumpNLtemplate[kkk] = numTemplate
        dumpNLindex[kkk] = indDataFile
        dumpNLfileSuffix[kkk] = curDataFileSuffix
        ###############
        
        
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15,NLquestions[[numTemplate]])
        
      } else {
        sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15,"NA")
      }
      
      
      
      ###################################
      # special: percentage change year on year, quarter on quarter...
      # what is the sum of all the (specific column or row in HCT)
      
      ### write in JSON file
      
      # {
      # "templates": [{name: "template1", description: "select a single HCT cell"},...],
      # "questions": [{
      #             "fileName": root_filename, 
      #             "sql": [{
      #                    "name": "template1",
      #                    "sql": "SELECT...",
      #                    "GTresult": " ", 
      #                    },...]
      #             },...]
      # }
      
      templateList = paste0('[\n',
                            '{"name": "template 1",',
                            '"description": "Select a single cell in HCT"},\n',
                            '{"name": "template 2",',
                            '"description": "Select multiple cells in one column of HCT"},\n',
                            '{"name": "template 3",',
                            '"description": "Select multiple cells in one row of HCT"},\n',
                            '{"name": "template 4",',
                            '"description": "Select multiple cells in one row of HCT and apply an expression"},\n',
                            '{"name": "template 5",',
                            '"description": "Select multiple cells in one row of HCT and apply an expression already displayed in the HCT"},\n',
                            '{"name": "template 6",',
                            '"description": "Select cells from multiple rows and columns of HCT"},\n',
                            '{"name": "template 7",',
                            '"description": "Select multiple cells in one column of HCT and apply an expression"},\n',
                            '{"name": "template 8",',
                            '"description": "Select cells from multiple rows and columns of HCT and apply an expression per group of cells from the same column"},\n',
                            ']')
      
      
      questionList=NULL
      for (iq in 1:length(sqlJSONstr)){
        if (!is.null(sqlnlqJSONstr[[iq]])) questionList = c(questionList,sqlnlqJSONstr[[iq]])
      }
      questionList = paste0(questionList,collapse=",\n")
      questionList = paste0('[',questionList,']')
      
      jsonSTR = paste0('{\n',
                       '"filename": "',DB_CSVfile,'",\n',
                       #'"templates": ',templateList,',\n',
                       '"questions": ',questionList,'\n',
                       '}') 
      
      filenameJSON = paste0(outputFolder,DATA_FILE,outputNameQuestions)
      
      write.table(jsonSTR, file = filenameJSON, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
      
      # for (isql in 1:nRowCodes){
      #   spl = rowSamples[[ilev]]
      #   if (length(spl)==1) {str_spl = "[0]"; }
      #   else { str_spl = paste0("[",spl[1],",",spl[2],"]");}
      #   
      #   rowAttribs = paste0(rowAttribs,
      #                       strAttrOffset,"{\n",
      #                       strAttrOffset,"'code': '",rowCodes[ilev],"',\n",
      #                       strAttrOffset,"'pos': '", row_name_pos,"',\n",
      #                       strAttrOffset,"'sample': ", str_spl,",\n",
      #                       strAttrOffset,"'agg_pos1': '", row_agg_pos,"'\n",
      #                       strAttrOffset,"}")
      #   # add separator
      #   if (ilev<nRowCodes) rowAttribs = paste0(rowAttribs,sepAttr)
      # }
      # 
      
      
      ###############
      if (kkk%%100){
        indLast = max(which(nchar(dumpNLindex)>0))
        dumpNLdf = data.frame(numTemplate = dumpNLtemplate[1:indLast], 
                              NLQ = dumpNLquestions[1:indLast],
                              SQLR = dumpSQLresults[1:indLast],
                              SQLQ = dumpSQLquestions[1:indLast],
                              indData = dumpNLindex[1:indLast], 
                              dataSubset = dumpNLfileSuffix[1:indLast], 
                              stringsAsFactors = FALSE)
        write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE) #,quote=FALSE)
      }
      
    }
  }
  ###############
  if (kkk>100){
    indLast = max(which(nchar(dumpNLindex)>0))
    dumpNLdf = data.frame(numTemplate = dumpNLtemplate[1:indLast], 
                          NLQ = dumpNLquestions[1:indLast],
                          SQLR = dumpSQLresults[1:indLast],
                          SQLQ = dumpSQLquestions[1:indLast],
                          indData = dumpNLindex[1:indLast], 
                          dataSubset = dumpNLfileSuffix[1:indLast], 
                          stringsAsFactors = FALSE)
    write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE)
  }
  
} # END MAIN FOR LOOP ACROSS PREFIX
