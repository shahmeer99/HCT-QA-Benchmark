####################################################################################
####################################################################################
#### 2024_10_09
####################################################################################
####################################################################################

## generate a set of attributes for each table, counting the depth of columns and rows, 
## the number of rows and columns of the table content
## the number of additional rows or columns for aggregates


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

LIST_DATA_PREFIX = c("Number_of_students",
                     "Number_of_graduations",
                     "Evolution_of_pollution_in_percent", # enable simplified nested attributes
                     "Food_import-export_in_tons", # enable simplified nested attributes
                     "Weather_statistics",
                     "Number_of_constructions", # enable simplified nested attributes
                     "Number_of_accidents")

#LIST_DATA_PREFIX = c("Weather_statistics")

tttt = 0 
ALL_TABLE_TEMPLATE = list()

dumpNLdfALL <- NULL

for (DATA_PREFIX in LIST_DATA_PREFIX){
  
  
  set.seed(1) # set.seed(1) 
  
  dumpName = DATA_PREFIX
  
  dataFolder   = "./PARAMETERS/" 
  inputFolder  = "./2024_09_16_TABLES/" 
  outputFolder = "./2024_09_16_TABLES/"
  
  # we only need these table files to generate the Q&A
  SIG_FILE_EXT = "_SIG_HCT.json" # use when HCT pivot tables have been generated
  FORM_FILE_EXT = "_HCT.json"
  DB_FILE_EXT = "_DB.csv"
  
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


  
  kkk = 0
  dumpNLquestions = character(numDataFiles*20)
  dumpNLindex = character(length = numDataFiles*20)
  dumpNLtemplate = character(length = numDataFiles*20)
  dumpNLfileSuffix = character(length = numDataFiles*20)
  dumpSQLquestions =  character(numDataFiles*20)
  dumpSQLresults =  character(numDataFiles*20)
  dumpCOLdepth = 0 
  dumpROWdepth = 0 
  dumpCOLnum = 0 
  dumpROWnum = 0 
  dumpCOLagg = 0 
  dumpROWagg = 0 
  
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ####################################################################################
  
  for (indDataFile in 1:numDataFiles){
    curDataFile = fileListToProcess[indDataFile]
    
    
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
    
    OUTPUT_FILE_NAME = "_ALL_TABLE_FEATURES"
    
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
      
      ###################################################           
      
      # get col and row headers of HCT table
      HCTcolHeaders = getHCTcolHeaders(DBdata,colNames,rowNames)
      HCTrowHeaders = getHCTrowHeaders(DBdata,colNames,rowNames)
      
      # FOR TABLE STATISTICS (VALID FOR ALL Q&A)
      COLnum = nrow(HCTcolHeaders)
      ROWnum = nrow(HCTrowHeaders)
      
      #     ###############
           kkk = kkk+1
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          dumpCOLdepth[kkk] = COLdepth
          dumpROWdepth[kkk] = ROWdepth
          dumpCOLnum[kkk] = COLnum
          dumpROWnum[kkk] = ROWnum
          dumpCOLagg[kkk] = COLagg
          dumpROWagg[kkk] = ROWagg

      
    }
  } ## END of for each data file with the same prefix
  ###############
  ## SAVE ALL THE Q&A of the same prefix
    indLast = max(which(nchar(dumpNLindex)>0))
    dumpNLdf = data.frame(tableFile = dumpNLfileSuffix[1:indLast], 
                          COLdepth = dumpCOLdepth[1:indLast],    
                          ROWdepth = dumpROWdepth[1:indLast],   
                          COLnum = dumpCOLnum[1:indLast],    
                          ROWnum = dumpROWnum[1:indLast],    
                          COLagg = dumpCOLagg[1:indLast],    
                          ROWagg = dumpROWagg[1:indLast],    
                          
                          stringsAsFactors = FALSE)
    
    dumpNLdfALL = rbind(dumpNLdfALL,dumpNLdf)
    #write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE)
    write.csv(dumpNLdfALL,paste0(outputFolder,OUTPUT_FILE_NAME,".csv"),row.names=FALSE,quote=TRUE)
    
  
  
} # END MAIN FOR LOOP ACROSS PREFIX
# JSON_annotator_all = paste0("{",paste0(ALL_TABLE_TEMPLATE,collapse = ","),"}")
# JSON_annotator_all_correct = gsub("'",'"',JSON_annotator_all)
# write.table(JSON_annotator_all_correct, file = "table_questions_answers_annotations.json", row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)




