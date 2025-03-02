################################################################################
### 3 - GENERATE SQL QUERIES AND NL QUESTIONS FROM TEMPLATE AND TABLES

#### QUERY GENERATOR

## use sqldf: https://www.playerzero.ai/advanced/r-faqs/how-to-use-the-sqldf-package-in-r-for-sql-on-data-frames 

##############################

# in__$Quarter==of==$Year => in Q1 of 2023 / in Q1 / in 2023  
# in__$City==,==$State => in Malibu, California / in Malibu / in California  
# of__$Subtype__$Food_type => of Cow Meat / of Cow / of Meat

##############################



## GENERATE SQL FROM TEMPLATE

rm(list = ls())

library(jsonlite)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
library(rlang) # to replace names by variables in filter using sym()
library(sqldf)
library(tidyquery)
library(stringr)
library(data.table)

source("config.R") ## check this file to set up specific string separators
source("toolboxSQLandNLQ.R")
source("toolboxJSONtemplateForAnnotator.R")


verbose = FALSE

############ I/O ############
paramFolder  = PARAMETERS_FOLDER
inputFolder  = SEMANTIC_TABLES_FOLDER 
outputFolder = SEMANTIC_QANDA_FOLDER

# create the output folder if it does not exist
ifelse(!dir.exists(file.path(outputFolder)),
       dir.create(file.path(outputFolder)),
       "")

DATA_TABLE_PATTERNS = PARAM_TABLE_TEMPLATES_JSON
DATA_TABLE_INSTANCES = PARAM_TABLE_TO_GEN_JSON
DATA_TABLE_SEMANTICS = PARAM_SEMANTICS_JSON
NLQ_PATTERN_FILE = PARAM_NLQ_TEMPLATES_JSON


LIST_DATA_PREFIX = c("Evolution_of_pollution_in_percent", 
                     "Food_import-export_in_tons", 
                     "Number_of_accidents",
                     "Number_of_constructions",
                     "Number_of_students",
                     "Number_of_graduations",
                     "Weather_statistics")


################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################
################ I/O #####################

generateNLQ = TRUE

TABLE_INSTANCES_JSON = read_json(paste0(paramFolder,DATA_TABLE_INSTANCES))
TABLE_PATTERNS_JSON = read_json(paste0(paramFolder,DATA_TABLE_PATTERNS))
SEMANTIC_DATA = read_json(paste0(paramFolder,DATA_TABLE_SEMANTICS))
NLQ_PATTERNS = read_json(paste0(paramFolder,NLQ_PATTERN_FILE))

# we only need these table files to generate the Q&A
SIG_FILE_EXT = "_SIG_HCT.json" # use when HCT pivot tables have been generated
FORM_FILE_EXT = "_HCT.json"
DB_FILE_EXT = "_DB.csv"



tttt = 0 
ALL_TABLE_TEMPLATE = list()

dumpNLdfALL <- NULL

for (DATA_PREFIX in LIST_DATA_PREFIX){
  
  
  set.seed(1) # RESET FOR EACH DATA PREFIX
  

  dumpName = DATA_PREFIX
  
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
  
  if (is.null(fileListToProcess)){
    print(paste0(fileListToProcess," DOES NOT EXIST."))
    
  } else {
    
    print(paste0("PROCESSING ",fileListToProcess))
    
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

      DATA_FILE = curDataFile
      
      sqlJSONstr <- NULL
      sqlnlqJSONstr <- NULL
      NLquestions <-NULL
      
      outputNameQuestions = "_QandA.json"
      
      
      DB_CSVfile = paste0(DATA_FILE,DB_FILE_EXT) # file containing DB data
      HCT_SIGNATURE_JSONfile = paste0(DATA_FILE,SIG_FILE_EXT) # file containing HCT table description
      HCT_ANNOTATOR_JSONfile = paste0(DATA_FILE,FORM_FILE_EXT) # file containing HCT table JSON annotator format
      
      fileSuffix = unlist(strsplit(DATA_FILE,split="_set"))
      curDataFileSuffix = curDataFile  
      
      ## Find the templates of the current table
      tableName = gsub("_"," ",DATA_PREFIX)
      
      for (numTable in 1:length(NLQ_PATTERNS)) {
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
        ### SINGLE-CELL ONE ROW AND ONE COLUMN SELECTION FROM HCT
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 1: 1R & 1C"))
        numTemplate = 1
        
        # get col and row headers of HCT table
        HCTcolHeaders = getHCTcolHeaders(DBdata,colNames,rowNames)
        HCTrowHeaders = getHCTrowHeaders(DBdata,colNames,rowNames)
        
        # FOR TABLE STATISTICS (VALID FOR ALL Q&A)
        COLnum = nrow(HCTcolHeaders)
        ROWnum = nrow(HCTrowHeaders)
        
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        printcond(verbose,sqlClause)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult)
        
        #### NLQ generation
        if (generateNLQ){
          # TEMPLATE 1
          #SELECT Value FROM DBdata WHERE ((Degree = 'BSc' AND Gender = 'Male')) AND ((Year = '2019/2020' AND Language = 'Spanish’))
          # What is the number of graduations of_Male students with_a_BSc_degree in_Spanish in_2019/2020 ?
          HCTheaders = cbind(HCTcolHeaders[indColSel,,drop=FALSE],HCTrowHeaders[indRowSel,,drop=FALSE])
          HCTcolH = HCTcolHeaders[indColSel,,drop=FALSE]  # single
          HCTrowH = HCTrowHeaders[indRowSel,,drop=FALSE]  # single
          
          printcond(verbose,"HCT")
          printcond(verbose,NLQ_patterns)
          
          
          templateName = paste0("template_",numTemplate)
          
          NLquestions[[numTemplate]]<-list()
          for (numVariant in 1:length(NLQ_patterns[[templateName]])){
            
            nlPattern = NLQ_patterns[[templateName]][[numVariant]]
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 2 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ###################################################           
        ### MULTI-CELL SELECTION FROM ONE COLUMN AND MULTI ROWS IN HCT
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 2: nR & 1C"))
        numTemplate = 2
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 10)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult)
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
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
          }
          
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 3 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ###################################################           
        ### MULTI-CELL SELECTION FROM ONE ROW AND MULTI COLUMNS IN HCT
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 3: 1R & nC"))
        
        numTemplate = 3
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 10)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult)
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
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 4 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Multi-cell + expression
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 4: 1R & nC + expression"))
        
        numTemplate = 4
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 10)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        #exprListSQL = c("sum","min") #,"max","avg","count") # min-max, avg/std, sum, count
        
        # select two EXPRESSIONS NOT IN THE TABLE IF ANY
        EXPRS = c("sum","min","max","avg")
        # check if HCT contains an explicit aggregation function (aggFun)
        if (length(aggColNames)>=1 && (nchar(aggColNames[1])>0)){
          # remove it from the list
          exprs = c("sum","min","max","avg")
          aggFun1 = aggFun
          exprs = EXPRS[EXPRS!=aggFun]
        } else {
          exprs = EXPRS
        }
        smpl = sample(length(exprs))
        exprListSQL = c(exprs[smpl[1]],exprs[smpl[2]])
        
        exprClause <- geneExpressionClause(exprListSQL)
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
          
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        
        # 5 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Multi-cell + expression
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 5: 1R & nC + expression from the table"))
        
        numTemplate = 5
        
        # get pattern of the current table 
        
        aggColAttrNames = aggColNames 
        #aggRowAttrNames = aggRowNames 
        
        
        
        # run only if HCT contains an aggregated column
        if (length(aggColAttrNames)>=1 && (nchar(aggColAttrNames[1])>0)){
          
          aggFun1 = aggFun
          aggName1 = aggName
          
          # SELECT EXPR WHICH IS IN THE TABLE ONLY
          exprListSQL = aggFun1 # used for Q&A JSON template
          
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
          # use a single expresison present in the table (passed as parameters with HCT table generated)
          exprClause <- geneExpressionClause(aggFun1) 
          
          
          # generate the query with GT for the value
          sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
          sqlResult = sqldf(sqlClause)
          # sqlClause
          # result 
          sqlResultSTR = formatResults(sqlResult)
          sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
          
          JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
              printcond(verbose,nlPattern)
              
              nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,aggFun1,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
              
              printcond(verbose,nlQuestionClean)
              
              NLquestions[[numTemplate]][numVariant] = nlQuestionClean
              
              ###############
              kkk = kkk+1
              dumpSQLquestions[kkk] = sqlClause
              dumpSQLresults[kkk] = sqlResultSTR
              dumpNLquestions[kkk] = nlQuestionClean
              dumpNLtemplate[kkk] = numTemplate
              dumpNLindex[kkk] = indDataFile
              dumpNLfileSuffix[kkk] = curDataFileSuffix
              dumpCOLdepth[kkk] = COLdepth 
              dumpROWdepth[kkk] = ROWdepth 
              dumpCOLnum[kkk] = COLnum 
              dumpROWnum[kkk] = ROWnum 
              dumpCOLagg[kkk] = COLagg 
              dumpROWagg[kkk] = ROWagg 
              ###############
              
              
            }
            
            printcond(verbose,sqlClause)
            printcond(verbose,sqlResultSTR)
            
            sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
            
            
          } else {
            sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
          }
          
          NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
          ANSWER_LIST[[numTemplate]] = sqlResultSTR
          
          
        } else {
          JSON_QandA[[numTemplate]] = "NA"
          NLQ_LIST[[numTemplate]] = "NA"
          ANSWER_LIST[[numTemplate]] = "NA"
          
        }
        
        
        
        # 6 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ###################################################           
        ### MULTI-CELL SELECTION FROM MULTI ROWS AND MULTI COLUMNS IN HCT
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 6: nR & nC"))
        
        numTemplate = 6
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 4)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 5)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult)
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
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 7 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Multi-cell + expression
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 7: nR & 1C + aggregation"))
        
        numTemplate = 7
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
        # SELECT TWO EXPRESSIONS, one of them can be already in the table
        exprs = c("sum","min","max","avg")
        smpl = sample(length(exprs))
        exprListSQL = c(exprs[smpl[1]],exprs[smpl[2]])
        
        exprClause <- geneExpressionClause(exprListSQL)
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        
        sqlResultSTR = formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 8 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Multi-cell + expression group by HCT columns
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 8: nR & nC + GROUP BY per column"))
        
        numTemplate = 8
        
        indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 5)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        
        #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
        # SELECT TWO EXPRESSIONS, one of them can be already in the table
        exprs = c("sum","min","max","avg")
        smpl = sample(length(exprs))
        exprListSQL = c(exprs[smpl[1]],exprs[smpl[2]])
        exprClause <- geneExpressionClause(exprListSQL)
        
        # generate the query with GT for the value
        # group by must be over the column headers 
        groupByAttrNames = colnames(HCTcolHeaders)
        groupByClause = paste0(groupByAttrNames,collapse=",")
        
        sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        
        sqlResultSTR <- formatResults(sqlResult)
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
            nlQuestionClean = paste(nlQuestion,nlReportClean)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
          
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        
        # 9 ############################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 9: nR & 1C + GROUP BY per row (level 1)"))
        
        numTemplate = 9
        
        HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
        
        indRowSel <- geneIndexForRandomClauses(HCTrowLev1) # select all rows at depth level 1 (top of hierarchy)
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
        
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestionClean = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = paste(nlQuestionClean)
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 10 ###########################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 10: nR & 1C + GROUP BY per row (level 1) + report group name"))
        
        numTemplate = 10
        
        HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
        
        indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
        #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
        # SELECT ONE EXPRESSION, it can be already in the table
        exprs = c("sum","min","max","avg")
        smpl = sample(length(exprs))
        exprListSQL = c(exprs[smpl[1]])
        exprClause <- geneExpressionClause(exprListSQL)
        
        # generate the query with GT for the value
        # group by must be over the row headers at level 1
        groupByAttrNames = colnames(HCTrowHeaders)[1]
        groupByClause = paste0(groupByAttrNames,collapse=",")
        
        sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        sqlResultSTR <- formatResults(sqlResult)
        
        sqlResultSTR
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
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
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 11 ###########################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 11: nR & nC + GROUP BY per row (level 1) and per columns + report group name"))
        
        numTemplate = 11
        
        HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
        
        indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
        #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
        indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2,3)
        
        rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
        colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
        
        
        #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
        # SELECT ONE EXPRESSION, it can be already in the table
        exprs = c("sum","min","max","avg")
        smpl = sample(length(exprs))
        exprListSQL = c(exprs[smpl[1]])
        exprClause <- geneExpressionClause(exprListSQL)
        
        # group by must be over the row headers at level 1 and over the column headers 
        groupByAttrNames = c(colnames(HCTrowHeaders)[1],colnames(HCTcolHeaders))
        groupByClause = paste0(groupByAttrNames,collapse=",")
        
        # generate the query with GT for the value
        sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        sqlResultSTR <- formatResults(sqlResult)
        
        sqlResultSTR
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,exprListSQL,tableAggFun=aggFun)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,exprListSQL,groupByAttrNames,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
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
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 12 ###########################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # TEMPLATE 12: nR & 1C + TOP/BOTTOM K + ROW FILTER
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 12: nR & 1C + TOP/BOTTOM K + ROW FILTER"))
        
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
        
        # TMP SQL TO GET THE TOTAL NUMBER OF RESULTS
        sqlClauseTMP = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause)
        sqlResultTMP = sqldf(sqlClauseTMP)
        resNumMax = nrow(sqlResultTMP) # number of resulting values
        
        printcond(verbose," TMP to get number of values for topK")
        printcond(verbose,sqlClauseTMP)
        printcond(verbose,sqlResultTMP)
        
        orderCls = c("ASC","DESC")
        orderByClause <- sample(orderCls)[1] # pick ordering direction at random
        maxTopK <- 5
        maxRnd = min(max(1,resNumMax - 1),maxTopK)
        # Top-K or Bottom-K is with minimum K = 2, otherwise we could use max or min (template 8)
        if (maxRnd <= 2){firstK = 2} else {firstK = sample(2:maxRnd)[1]} # pick TopK at random
        
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause," ORDER BY VALUE ",orderByClause," LIMIT ",firstK)
        sqlResult = sqldf(sqlClause)
        printcond(verbose,sqlClause)
        printcond(verbose,sqlResult)
        
        
        
        # sqlClause
        # result 
        sqlResultSTR <- formatResults(sqlResult)
        
        sqlResultSTR
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult,NULL,NULL,firstK)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,orderByK,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
            nlQuestionClean = nlQuestion
            
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        
        # 13 ###########################################
        ################################################
        ################################################
        ###################################################           
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 13: nR & 1C + ROW FILTER + ORDER BY"))
        
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
        
        #orderByClause <- "ASC" #"DESC" # "ASC"
        orderCls = c("ASC","DESC")
        orderByClause <- sample(orderCls)[1] # pick ordering direction at random
        firstK <- 0
        
        sqlClause = paste0("SELECT Value FROM DBdata WHERE ",colClause," AND ",rowClause," ORDER BY VALUE ",orderByClause)
        sqlResult = sqldf(sqlClause)
        # sqlClause
        # result 
        sqlResultSTR <- formatResults(sqlResult)
        
        sqlResultSTR
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,groupByAttrNames,orderByK,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
            nlQuestionClean = nlQuestion
            
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause
            dumpSQLresults[kkk] = sqlResultSTR
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
          }
          printcond(verbose,sqlClause)
          printcond(verbose,sqlResult)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,NLquestions[[numTemplate]])
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause,sqlResultSTR,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR
        
        # 14 ###########################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 14: nR & 1C + operation on column"))
        
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
        
        #printcond(verbose,sqlClause14)
        #printcond(verbose,sqlResult14)
        
        if (nrow(sqlResult14) == 0) {
          printcond(verbose,"NO RESULT, TRY REVERSE OP")
          # no result. try with the other operation
          operationOP = allOps[indRandOp2]  
          
          operationClause = paste0("Value",operationOP,operationVAL)
          
          sqlClause14 = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",operationClause)
          sqlResult14 = sqldf(sqlClause14)
          
          #printcond(verbose,sqlClause14)
          #printcond(verbose,sqlResult14)
          
        }
        
        # sqlClause
        # result 
        sqlResultSTR14 <- formatResults(sqlResult14)
        
        sqlResultSTR14
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel,sqlResult14)
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
            printcond(verbose,nlPattern)
            
            
            nlQuestion = geneNLQselectExpress(HCTcolH,HCTrowH,NULL,NULL,NULL,forWhich,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList)
            
            nlQuestionClean = nlQuestion
            nlQuestionClean14 = nlQuestionClean
            
            printcond(verbose,nlQuestionClean)
            
            NLquestions[[numTemplate]][numVariant] = nlQuestionClean
            
            ###############
            kkk = kkk+1
            dumpSQLquestions[kkk] = sqlClause14
            dumpSQLresults[kkk] = sqlResultSTR14
            dumpNLquestions[kkk] = nlQuestionClean
            dumpNLtemplate[kkk] = numTemplate
            dumpNLindex[kkk] = indDataFile
            dumpNLfileSuffix[kkk] = curDataFileSuffix
            dumpCOLdepth[kkk] = COLdepth 
            dumpROWdepth[kkk] = ROWdepth 
            dumpCOLnum[kkk] = COLnum 
            dumpROWnum[kkk] = ROWnum 
            dumpCOLagg[kkk] = COLagg 
            dumpROWagg[kkk] = ROWagg 
            ###############
            
            
            
          }
          printcond(verbose,sqlClause14)
          printcond(verbose,sqlResult14)
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14,NLquestions[[numTemplate]])
          
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause14,sqlResultSTR14,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR14
        
        
        # 15 ###########################################
        ################################################
        ################################################
        ###################################################           
        
        ################################################
        # Local aggregation (per group of rows) 
        printcond(verbose,paste0(indDataFile,"/",numDataFiles,"-- ",curDataFile,"---- TEMPLATE 15: conditional "))
        
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
        
        selectionClause15 =  paste0(attrList15SQL,collapse=",")
        
        if (nrow(sqlResult14)>0){
          
          indRowSel <- geneIndexForRandomClauses(sqlResult14) # use values to select a subset of rows Level 1
          
          
          rowClause <- geneMultiRowOrColSelectionSQL(sqlResult14,indRowSel)
          colClause15 <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel15)
          
          
          #sqlClause = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
          #sqlResult = sqldf(sqlClause)
          
          
          #sqlResultSTR <- formatResults(sqlResult)
          
        } else {
          #sqlResult=NULL
          #sqlResultSTR = ""
        }
        
        #printcond(verbose,sqlClause)
        #printcond(verbose,sqlResultSTR)
        
        
        sqlClause15 = paste0("SELECT ",selectionClause15," FROM DBdata WHERE ",colClause15, " AND (",paste0(attrList14,collapse=","),") IN (",sqlClause14,")")
        sqlResult15 = sqldf(sqlClause15)
        sqlResultSTR15 = formatResults(sqlResult15)
        
        #printcond(verbose,sqlClause15)
        #printcond(verbose,sqlResultSTR15)
        
        sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15)
        
        JSON_QandA[[numTemplate]] = geneJSONformatQandA(numTemplate,HCTrowHeaders,HCTcolHeaders,indRowSel,indColSel15,sqlResult15)
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
          PREM = gsub("\\?","",geneNLQselectExpress(HCTcolH,NULL,NULL,NULL,NULL,NULL,nlPattern,ALL_NL_ATTR_NAMES,simplifyNestedList))
          
          SEC = geneForWhichPremAttr(attrList14,nlPattern,ALL_NL_ATTR_NAMES)
          
          COND = unlist(strsplit(nlQuestionClean14,split="for which"))[2]
          
          nlQuestionClean = stringr::str_squish(paste0(PREM," ",SEC," for which ", COND," ",nlReportClean))
          
          printcond(verbose,nlQuestionClean)
          
          NLquestions[[numTemplate]]<-list()
          NLquestions[[numTemplate]][1] = nlQuestionClean
          
          ###############
          kkk = kkk+1
          dumpSQLquestions[kkk] = sqlClause15
          dumpSQLresults[kkk] = sqlResultSTR15
          dumpNLquestions[kkk] = nlQuestionClean
          dumpNLtemplate[kkk] = numTemplate
          dumpNLindex[kkk] = indDataFile
          dumpNLfileSuffix[kkk] = curDataFileSuffix
          
          dumpCOLdepth[kkk] = COLdepth 
          dumpROWdepth[kkk] = ROWdepth 
          dumpCOLnum[kkk] = COLnum 
          dumpROWnum[kkk] = ROWnum 
          dumpCOLagg[kkk] = COLagg 
          dumpROWagg[kkk] = ROWagg 
          ###############
          
          
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15,NLquestions[[numTemplate]])
          
        } else {
          sqlnlqJSONstr[[numTemplate]] = sqlnlqJSONtemplate(numTemplate,sqlClause15,sqlResultSTR15,"NA")
        }
        
        NLQ_LIST[[numTemplate]] = NLquestions[[numTemplate]][[1]]
        ANSWER_LIST[[numTemplate]] = sqlResultSTR15
        
        
        
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
        
        
        questionList=NULL
        for (iq in 1:length(sqlJSONstr)){
          if (!is.null(sqlnlqJSONstr[[iq]])) questionList = c(questionList,sqlnlqJSONstr[[iq]])
        }
        questionList = paste0(questionList,collapse=",\n")
        questionList = paste0('[',questionList,']')
        
        jsonSTR = paste0('{\n',
                         '"filename": "',DB_CSVfile,'",\n',
                         '"questions": ',questionList,'\n',
                         '}') 
        
        ########################################### SAVE QandA JSON ######################################
        
        filenameJSON = paste0(outputFolder,DATA_FILE,outputNameQuestions)
        
        write.table(jsonSTR, file = filenameJSON, row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)
        
        ### JSON template with table + Q&A for annotator
        # {
        # "1967": {
        #   "id": "1967",
        #   "image_source": "static/images/census_8/1967.jpg",
        #   "state": "labelled",
        #   "concern": false,
        #   "notes": "",
        #   "properties": {
        #     "Standard Relational Table": false,
        #     "Multi Level Column": false,
        #     ...
        #     "Split Header Cell": false,
        #     "Row Group Label": false
        #   },
        #   "themes": [],
        #   "template_ids": [
        #     0,
        #     1
        #     ],
        #   "task_list": [
        #     "",
        #     ""
        #     ],
        #   "query_type_list": [
        #     {
        #       "Row Filter": true,
        #       "Row Filter Condition Type Lookup": true,
        #       ...
        #       "Rank Rank On Expression": false,
        #       "Rank Report Top": ""
        #     },
        #     {
        #       "Row Filter": true,
        #       "Row Filter Condition Type Lookup": true,
        #       ...
        #       "Rank Rank On Expression": false,
        #       "Rank Report Top": ""
        #     }
        #     ],
        #   "question_list": [
        #     "What was the total number of employed workers with a wage and salary that were black men?",
        #     "What was the total number of asian people who were waged and salaried workers?"
        #     ],
        #   "answer_list": [
        #     "6302",
        #     "5096"
        #     ]
        # },
        # "2229": {
        #   "id": "2229",
        #   ...
        #   }
        # }
        
        # "1967": {
        #   "id": "1967",
        #   "image_source": "static/images/census_8/1967.jpg",
        #   "state": "labelled",
        #   "concern": false,
        #   "notes": "",
        #   "properties": {
        #     "Standard Relational Table": false,
        #     "Multi Level Column": false,
        #     ...
        #     "Split Header Cell": false,
        #     "Row Group Label": false
        #   },
        #   "themes": [],
        
        # without template 5
        tempIds = "[1,2,3,4,6,7,8,9,10,11,12,13,14,15]"
        taskList = "['','','','','','','','','','','','','','']"
        ADD_TEMP5 = FALSE
        if (ANSWER_LIST[[5]] != "NA") {
          # add template 5
          ADD_TEMP5 = TRUE
          tempIds = "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]"
          taskList = "['','','','','','','','','','','','','','','']"
          
        }
        
        
        JSON_QandA_ALL = aggregateNLQ(JSON_QandA)
        
        JSONheaderSTR1 = paste0("'",curDataFile,"': ",
                                str_sub(gsub('"', "'", HCTdataANNOTATOR_STR),1,-2), # remove last "}"
                                ",'template_ids': ",tempIds,
                                ", 'task_list': ",taskList,
                                ",'query_type_list':",
                                gsub('"', "'", JSON_QandA_ALL), ","
        )
        
        
        #   "template_ids": [
        #     0,
        #     1
        #     ],
        #   "task_list": [
        #     "",
        #     ""
        #     ],
        #   "query_type_list": [
        #     {
        #       "Row Filter": true,
        #       "Row Filter Condition Type Lookup": true,
        #       ...
        #       "Rank Rank On Expression": false,
        #       "Rank Report Top": ""
        #     },
        #     {
        #       "Row Filter": true,
        #       "Row Filter Condition Type Lookup": true,
        #       ...
        #       "Rank Rank On Expression": false,
        #       "Rank Report Top": ""
        #     }
        #     ],
        #   "question_list": [
        #     "What was the total number of employed workers with a wage and salary that were black men?",
        #     "What was the total number of asian people who were waged and salaried workers?"
        #     ],
        #   "answer_list": [
        #     "6302",
        #     "5096"
        #     ]
        # },
        
        JSONheaderSTR2 = paste0(JSONheaderSTR1,"'question_list':['",
                                NLQ_LIST[[1]],"','",
                                NLQ_LIST[[2]],"','",
                                NLQ_LIST[[3]],"','",
                                NLQ_LIST[[4]],"','")
        if (ADD_TEMP5){
          JSONheaderSTR3 = paste0(JSONheaderSTR2,NLQ_LIST[[5]],"','")
        } else {
          JSONheaderSTR3 = JSONheaderSTR2
        }
        JSONheaderSTR4 = paste0(JSONheaderSTR3,
                                NLQ_LIST[[6]],"','",
                                NLQ_LIST[[7]],"','",
                                NLQ_LIST[[8]],"','",
                                NLQ_LIST[[9]],"','",
                                NLQ_LIST[[10]],"','",
                                NLQ_LIST[[11]],"','",
                                NLQ_LIST[[12]],"','",
                                NLQ_LIST[[13]],"','",
                                NLQ_LIST[[14]],"','",
                                NLQ_LIST[[15]],"'],'answer_list': ['",
                                ANSWER_LIST[[1]],"','",
                                ANSWER_LIST[[2]],"','",
                                ANSWER_LIST[[3]],"','",
                                ANSWER_LIST[[4]],"','")
        if (ADD_TEMP5){
          JSONheaderSTR5 = paste0(JSONheaderSTR4,ANSWER_LIST[[5]],"','")
        } else {
          JSONheaderSTR5 = JSONheaderSTR4
        }
        JSONheaderSTR6 = paste0(JSONheaderSTR5,
                                ANSWER_LIST[[6]],"','",
                                ANSWER_LIST[[7]],"','",
                                ANSWER_LIST[[8]],"','",
                                ANSWER_LIST[[9]],"','",
                                ANSWER_LIST[[10]],"','",
                                ANSWER_LIST[[11]],"','",
                                ANSWER_LIST[[12]],"','",
                                ANSWER_LIST[[13]],"','",
                                ANSWER_LIST[[14]],"','",
                                ANSWER_LIST[[15]],"']}") 
        
        tttt = tttt + 1           
        ALL_TABLE_TEMPLATE[[tttt]] = JSONheaderSTR6                      
        
        
        
        ###############
        ## ADD A BLANK ROW TO SEPARATE TABLES IN DUMP FILE
        #kkk = kkk+1
        #dumpSQLquestions[kkk] = " "
        #dumpSQLresults[kkk] = " "
        #dumpNLquestions[kkk] = " "
        #dumpNLtemplate[kkk] = " "
        #dumpNLindex[kkk] = "-1"
        #dumpNLfileSuffix[kkk] = " "
        ###############
        
        ###############
        if (kkk%%100){
          indLast = max(which(nchar(dumpNLindex)>0))
          dumpNLdf = data.frame(dataSubset = dumpNLfileSuffix[1:indLast], 
                                numTemplate = dumpNLtemplate[1:indLast], 
                                NLQ = dumpNLquestions[1:indLast],
                                SQLR = dumpSQLresults[1:indLast],
                                SQLQ = dumpSQLquestions[1:indLast],
                                indData = dumpNLindex[1:indLast], 
                                COLdepth = dumpCOLdepth[1:indLast],    
                                ROWdepth = dumpROWdepth[1:indLast],   
                                COLnum = dumpCOLnum[1:indLast],    
                                ROWnum = dumpROWnum[1:indLast],    
                                COLagg = dumpCOLagg[1:indLast],    
                                ROWagg = dumpROWagg[1:indLast],    
                                
                                stringsAsFactors = FALSE)
          write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE) #,quote=FALSE)
        }
        
      }
    } ## END of for each data file with the same prefix
    ###############
    ## SAVE ALL THE Q&A of the same prefix
    indLast = max(which(nchar(dumpNLindex)>0))
    dumpNLdf = data.frame(tableFile = dumpNLfileSuffix[1:indLast], 
                          questionTemplate = dumpNLtemplate[1:indLast], 
                          NLQ = dumpNLquestions[1:indLast],
                          SQLR = dumpSQLresults[1:indLast],
                          SQLQ = dumpSQLquestions[1:indLast],
                          indData = dumpNLindex[1:indLast], 
                          COLdepth = dumpCOLdepth[1:indLast],    
                          ROWdepth = dumpROWdepth[1:indLast],   
                          COLnum = dumpCOLnum[1:indLast],    
                          ROWnum = dumpROWnum[1:indLast],    
                          COLagg = dumpCOLagg[1:indLast],    
                          ROWagg = dumpROWagg[1:indLast],    
                          
                          stringsAsFactors = FALSE)
    
    dumpNLdfALL = rbind(dumpNLdfALL,dumpNLdf)
    write.csv(dumpNLdf,paste0(outputFolder,"DUMP_NLQ_",dumpName,".csv"),row.names=FALSE,quote=TRUE)
    write.csv(dumpNLdfALL,paste0(outputFolder,"DUMP_NLQ_ALL",".csv"),row.names=FALSE,quote=TRUE)
    
  }
  
} # END MAIN FOR LOOP ACROSS PREFIX
JSON_annotator_all = paste0("{",paste0(ALL_TABLE_TEMPLATE,collapse = ","),"}")
JSON_annotator_all_correct = gsub("'",'"',JSON_annotator_all)
write.table(JSON_annotator_all_correct, file = "table_questions_answers_annotations.json", row.names = FALSE, col.names= FALSE, sep = "",quote=FALSE)




