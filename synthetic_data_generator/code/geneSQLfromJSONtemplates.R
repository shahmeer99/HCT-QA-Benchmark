#### QUERY GENERATOR

## READ ..._DB.csv data
## READ ..._HCT.json data $signature to get ROWS and COLS of the HCT table

## use sqldf: https://www.playerzero.ai/advanced/r-faqs/how-to-use-the-sqldf-package-in-r-for-sql-on-data-frames 


### TODO
# NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
# generate some NLP templates for each query
# check that the answer from DB is present in the HCT too in case of NAN)

## GENERATE SQL FROM TEMPLATE

library(jsonlite)
library(pivottabler)
library(htmlTable)
library(psycModel) # html_to_pdf
library(pagedown) 
library(rlang) # to replace names by variables in filter using sym()
#library(RSQLite)
library(sqldf)
library(tidyquery)


set.seed(1) 


################ SETTINGS #####################
### BE SURE TO CURRENT SET WORKING DIRECTORY TO THE SAME FOLDER AS THIS FILE ###
#> setwd("~/______PSA_TABLE_DETECTION_MohamedElTabakh/CODE/HCTgenerator")

dataFolder  = "./Data/" 
inputFolder  = "./Output/" 
outputFolder = "./QueriesTEST/"

sqlJSONstr <- NULL

outputNameQuestions = "_QandA.json"

DATA_TABLE_PATTERNS = "tablesToGenerate.json"
DATA_TABLE_SEMANTICS = "semantics.json"

DATA_FILE_LIST = c("Number_of_graduations_set127_1", 
                   "Number_of_students_set171_1",
                   "Food_import-export_in_tons_set357_1",
                   "Evolution_of_pollution_in_percent_set127_1")
# DATA_FILE_LIST = "Number_of_graduations_set127_1"
# DATA_FILE_LIST = "Number_of_students_set171_1"
# DATA_FILE_LIST = "Food_import-export_in_tons_set357_1"
# DATA_FILE_LIST = "Evolution_of_pollution_in_percent_set127_1"

# DATA_FILE_LIST = "ALL" # to process all files in the outputFolder folder


################ I/O #####################
# get all row headers of HCT table (without aggregate rows nor DB row colnames)
getHCTrowHeaders <- function(DBdata,colNames,rowNames){
  colVal <- DBdata[1,colNames]
  filtRows <- DBdata
  for (i in 1:length(colNames)){
    filtRows = filtRows[which(filtRows[,colNames[i]] == unlist(colVal[i])),]
  }
  return(filtRows[,rowNames])
}
# get all col headers of HCT table (without aggregate cols nor DB col colnames)
getHCTcolHeaders <- function(DBdata,colNames,rowNames){
  rowVal <- DBdata[1,rowNames]
  filtCols <- DBdata
  for (i in 1:length(rowNames)){
    filtCols = filtCols[which(filtCols[,rowNames[i]] == unlist(rowVal[i])),]
  }
  return(filtCols[,colNames])
}

# HCT COLUMNS or ROWS (multi)-selection at random
# run geneIndexForRandomClauses(HCTrowHeaders,1,1) for selecting a single HCT row
# run geneIndexForRandomClauses(HCTcolHeaders,1,1) for selecting a single HCT column
# run geneIndexForRandomClauses(HCTrowHeaders,mn,mx) for selecting between mn and mx HCT rows
# run geneIndexForRandomClauses(HCTcolHeaders,mn,mx) for selecting between mn and mx HCT cols
# run geneIndexForRandomClauses(HCTrowHeaders) or ...,0,0) for selecting all HCT rows (default)
# run geneIndexForRandomClauses(HCTcolHeaders) or ...,0,0) for selecting all HCT columns (default)
geneIndexForRandomClauses <- function(HCTheaders,minLevVal=0, maxLevVal=0){
  nrowHCT = nrow(HCTheaders)
  if (minLevVal==0 && maxLevVal == 0) { # select all col/row without sampling
    nColSel = nrowHCT
    indColSel = 1:nrowHCT
  } else {
    
    if (minLevVal == maxLevVal){
      nColSel = minLevVal
    } else {
      nColSel = sample(max(1,min(minLevVal,maxLevVal)):min(nrowHCT,max(minLevVal,maxLevVal)))[1] 
    }
    indColSel = sort(sample(1:nrowHCT)[1:nColSel])
  }
  return(indColSel)
}

# GENERATE ROW/COL SELECTION SQL CLAUSE 
# from list of ROW or COLUMN HEADER dataframe and row index
# call indColSel <- geneIndexForRandomClauses(HCTheaders,minLevVal, maxLevVal)
# then call rowColClause = geneMultiRowOrColSelectionSQL(HCTheaders,indColSel)
geneMultiRowOrColSelectionSQL <- function(HCTheaders,indColSel){
  # generate OR or IN clauses
  HCTcol_attrNames = colnames(HCTheaders)
  ncolHCT = ncol(HCTheaders)
  
  nColSel = length(indColSel)
  if (ncolHCT>1){
    # generate the OR clause
    ncolHCT = ncol(HCTheaders)
    colClause = ""
    for (ic in 1:nColSel){
      indCol = indColSel[ic]
      rowClauseLIST <- NULL
      irk = 0
      for (ir in 1:ncolHCT){
        valRowAttr = HCTheaders[indCol,ir] 
        if (!is.na(valRowAttr)){
          irk = irk + 1
          rowClauseLIST[[irk]] = paste0(HCTcol_attrNames[ir]," = '",valRowAttr,"'")
        }
      }
      rowClauseTMP = paste0(rowClauseLIST,collapse=" AND ")
      rowClauseTMP = paste0("(",rowClauseTMP,")")
      
      colClause = paste0(colClause,rowClauseTMP)
      if (ic < nColSel) colClause = paste0(colClause," OR ")
    }
  } else {
    # generate the IN clause
    ick = 0
    colClauseLIST <- NULL
    for (ic in 1:nColSel){
      indCol = indColSel[ic]
      valRowAttr = HCTheaders[indCol,1] 
      if (!is.na(valRowAttr)){
        ick = ick + 1
        colClauseLIST[[ick]] = paste0("'",valRowAttr,"'")
      }
    }
    colClauseTMP = paste0(colClauseLIST,collapse=",")
    colClause = paste0(HCTcol_attrNames," IN (",colClauseTMP,")")
  }
  colClause = paste0("(",colClause,")")
  return(colClause)
}

# Generate all columns at a certain level for which the aggregate is displayed in HCT
geneAggregatedSelectionSQL <- function(HCTcolHeaders,aggColAttrNames){
  # code commented for columns, applies to rows the same way 
  # by using HCTrowHeaders and aggRowAttrNames
  HCTcol_attrNames = colnames(HCTcolHeaders)
  # pick one column attribute at random among the ones which have an aggregate displayed, to generate all subcells
  aggColToGen = sample(aggColAttrNames)[1]
  # pick one value for any of the other column attributes
  indLevelAgg = which(HCTcol_attrNames==aggColToGen)
  if (indLevelAgg>1) { 
    otherCols = HCTcol_attrNames[1:(indLevelAgg-1)]
  } else {
    otherCols <- NULL
  }
  
  # generate clause of the form: "cname1 = A AND cname3 = C AND (cname2 IN (B1,B2,...Bk)) 
  colClause = NULL
  strFilter = NULL
  if (!is.null(otherCols)){
    if (length(otherCols) == 1){
      ucol = unique(HCTcolHeaders[[otherCols]])
      uucol = sample(ucol)[1]
      colClause = c(colClause,paste0(otherCols," = '",uucol,"'"))
      strFilter = c(strFilter,paste0("HCTcolHeaders[['",otherCols,"']] == '",uucol,"'"))
      
    }else{ # need to look only at possible values having these upper level columns
      ucol = HCTcolHeaders[,otherCols] %>% dplyr::distinct()
      iucol = sample(nrow(ucol))[1]
      uucol = ucol[iucol,]
      
      for (cl in colnames(uucol)){
        colClause = c(colClause,paste0(cl," = '",uucol[[cl]],"'"))
        strFilter = c(strFilter,paste0("HCTcolHeaders[['",cl,"']] == '",uucol[[cl]],"'"))
      }
      
    }
  }
  # add the list of values of the aggregated col attribute
  if (!is.null(strFilter)) {
    ucol = paste0(unique(HCTcolHeaders[eval(parse(text=paste0(strFilter,collapse = " & "))),aggColToGen]),collapse = "','")
  }else { 
    ucol = paste0(unique(HCTcolHeaders[[aggColToGen]]),collapse = "','")
  }
  colClause = c(colClause,paste0("(",aggColToGen," IN ('",ucol,"'))"))
  
  colClause = paste(colClause,collapse=" AND ")
  return(colClause)
}

# generate an expression clause on the output value
geneExpressionClause <- function(expression="",valAttrName = "Value"){
  exprClause <- NULL
  for (ixp in 1:length(expression)) {
    expr = expression[ixp]
    if (nchar(expr)!=0){
      exprClause = c(exprClause, paste0(expr,"(",valAttrName,")"));
    }
  }
  exprClause = paste(exprClause,collapse=",")
  return(exprClause)
}

# generate a GROUP BY attribute randomly among the attributes which have different values in the colOrRowClause
# call geneGroupByClause(HCTcolHeaders,colClause) for a grouping per columns
# call geneGroupByClause(HCTrowHeaders,rowClause) for a grouping per rows
# colOrRowClause is like: 
#"((Year = '2016/2017' AND Nationality = 'Non-Qatari' AND Quarter = 'Q3') OR (Year = '2017/2018' AND Nationality = 'Qatari' AND Quarter = 'Q4'))"
geneGroupByClause <- function(HCTcolOrRowHeaders,colOrRowClause){
  candidates <- NULL
  for (cn in colnames(HCTcolOrRowHeaders)){
    # check if it varies across colOrRowClause
    strtmp = unlist(strsplit(colOrRowClause," "))
    indcn = which(grepl(cn, strtmp, fixed = TRUE))
    if (length(unique(strtmp[indcn+2]))>1) candidates = c(candidates,cn)
  }
  if (!is.null(candidates)) {
    return(sample(candidates)[1])
  } else {
    return("")
  }
}  

sqlJSONtemplate <- function(tempNum,sqlClause,sqlResultSTR){
  return(paste0('{"name": "template ',tempNum,'",\n "sql": "',sqlClause,'",\n"sqlResult": "',sqlResultSTR,'"}')) 
}

# get all unique attribute values at a certain level of a dataframe of attributes
getUniqueValuesAtLevel <- function(HCTheaders,valLevel){
  HCTatLevel = as.data.frame(unique(HCTheaders[,valLevel]))
  colnames(HCTatLevel)=colnames(HCTheaders)[valLevel]
  return(HCTatLevel)
}

formatResults <- function(sqlResult){
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  if (nrow(sqlResult)==0) return(NULL)
  
  if (nrow(sqlResult)==1) return(paste0(as.character(sqlResult),collapse=","))
  
  sqlResultSTR<-NULL
  for (ir in 1:nrow(sqlResult)){
    sqlResultSTR = c(sqlResultSTR, paste0(as.character(sqlResult[ir,]),collapse=","))
  }
  sqlResultSTR = paste(sqlResultSTR,collapse = ";")
  
  return(sqlResultSTR)
}


################ READ DATA AND PARAMETERS FROM FILES ####################

if (DATA_FILE_LIST[1] == "ALL"){
  print("Reading all data")
  L1 = list.files("./Output/",pattern="*_HCT.*")
  DATA_FILE_LIST = unique(sapply(strsplit(L1,split = "_HCT."),"[[",1)) # get first part of file names and remove duplicate
} 

for (idata in 1:length(DATA_FILE_LIST)){ 
  DATA_FILE = DATA_FILE_LIST[idata]
  print(paste0("---------------------- ",idata," / ",length(DATA_FILE_LIST)))
  
  
  
  DB_CSVfile = paste0(DATA_FILE,"_DB.csv") # file containing all semantic data
  #tablesJSONfile = "tables.json"       # file containing all table patterns to generate
  HCT_JSONfile = paste0(DATA_FILE,"_HCT.json")       # file containing all table patterns to generate
  
  PATTERN_JSONfile = read_json(paste0(dataFolder,DATA_TABLE_PATTERNS))
  semanticData = read_json(paste0(dataFolder,DATA_TABLE_SEMANTICS))
  
  DBdata = read.csv(paste0(inputFolder,DB_CSVfile),stringsAsFactors=FALSE)
  
  # read HCT data to get signature
  HCTdata <- read_json(paste0(inputFolder,HCT_JSONfile)) 
  
  STRformatValue = HCTdata$formatValue
  STRsignature = HCTdata$signature
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
  for (i in (aggColInd+1):(aggRowInd-1)) aggColNames =  c(aggColNames,strTMP[i])
  for (i in (aggRowInd+1):length(strTMP)) aggRowNames =  c(aggRowNames,strTMP[i])
  aggFun = strTMP[aggFunInd+1]
  aggName = strTMP[aggNameInd+1]
  
  # Cleaning row and col names to match with DB dataframe colnames ("abc de fg" -> "abc.de.fg")
  colNames = gsub(" ","_", colNames)
  rowNames = gsub(" ","_", rowNames)
  colNames = gsub("-","_", colNames)
  rowNames = gsub("-","_", rowNames)
  colNames = gsub("/","_", colNames)
  rowNames = gsub("/","_", rowNames)
  
  
  colnames(DBdata) = gsub("\\.","_",colnames(DBdata))
  colnames(DBdata) = gsub("-","_",colnames(DBdata))
  colnames(DBdata) = gsub("/","_",colnames(DBdata))
  colnames(DBdata) = gsub(" ","_",colnames(DBdata))
  
  
  ###################################################           
  ###################################################           
  ###################################################           
  ### SINGLE-CELL ONE ROW AND ONE COLUMN SELECTION FROM HCT
  print("---- TEMPLATE 1: 1R & 1C")
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
  
  ###################################################           
  ### MULTI-CELL SELECTION FROM ONE COLUMN AND MULTI ROWS IN HCT
  print("---- TEMPLATE 2: nR & 1C")
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
  
  ###################################################           
  ### MULTI-CELL SELECTION FROM ONE ROW AND MULTI COLUMNS IN HCT
  print("---- TEMPLATE 3: 1R & nC")
  
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
  
  ################################################
  # Multi-cell + expression
  print("---- TEMPLATE 4: 1R & nC + expression")
  
  numTemplate = 4
  
  indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 10)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause(c("sum","min"))
  
  # generate the query with GT for the value
  sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  
  ################################################
  # Multi-cell + expression
  print("---- TEMPLATE 5: 1R & nC + expression from the table")
  
  numTemplate = 5
  
  # get pattern of the current table 
  
  aggColAttrNames = aggColNames 
  aggRowAttrNames = aggRowNames 
  aggFun1 = aggFun
  aggName1 = aggName
  
  # then query all element of that column with that aggregation on the result
  indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,1, 1)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
  colClause <- geneAggregatedSelectionSQL(HCTcolHeaders,aggColAttrNames)
  
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
  
  ###################################################           
  ### MULTI-CELL SELECTION FROM MULTI ROWS AND MULTI COLUMNS IN HCT
  print("---- TEMPLATE 6: nR & nC")
  
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
  
  ################################################
  # Multi-cell + expression
  print("---- TEMPLATE 7: nR & 1C + aggregation")
  
  numTemplate = 7
  
  indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause(c("sum","min"))
  
  # generate the query with GT for the value
  sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  sqlResultSTR = paste0(as.character(unlist(sqlResult)),collapse=",")
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  
  
  ################################################
  # Multi-cell + expression group by HCT columns
  print("---- TEMPLATE 8: nR & nC + GROUP BY per column")
  
  numTemplate = 8
  
  indRowSel <- geneIndexForRandomClauses(HCTrowHeaders,2, 5)
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2, 5)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowHeaders,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause(c("min","max"))
  
  # generate the query with GT for the value
  # group by must be over the column headers 
  groupByClause = paste0(colnames(HCTcolHeaders),collapse=",")
  
  sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 9: nR & 1C + GROUP BY per row (level 1)")
  
  numTemplate = 9
  
  HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
  
  indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
  #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1, 1)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause("min")
  
  # generate the query with GT for the value
  # group by must be over the row headers at level 1
  groupByClause = colnames(HCTrowHeaders)[1]
  
  sqlClause = paste0("SELECT ",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 10: nR & 1C + GROUP BY per row (level 1) + report group name")
  
  numTemplate = 10
  
  HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
  
  indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
  #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause("min")
  
  # generate the query with GT for the value
  # group by must be over the row headers at level 1
  groupByClause = colnames(HCTrowHeaders)[1]
  
  sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 11: nR & nC + GROUP BY per row (level 1) and per columns + report group name")
  
  numTemplate = 11
  
  HCTrowLev1 = getUniqueValuesAtLevel(HCTrowHeaders,1)
  
  indRowSel <- geneIndexForRandomClauses(HCTrowLev1)
  #indRowSel <- geneIndexForRandomClauses(HCTrowLev1,2, 4) # use values to select a subset of rows Level 1
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,2,3)
  
  rowClause <- geneMultiRowOrColSelectionSQL(HCTrowLev1,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  
  #expressions = c("sum","min","max","avg","count") # min-max, avg/std, sum, count
  exprClause <- geneExpressionClause("min")
  
  # generate the query with GT for the value
  # group by must be over the row headers at level 1
  groupByClause = paste0("(",colnames(HCTrowHeaders)[1],")")
  # group by must be over the column headers 
  colnamesList = c(colnames(HCTrowHeaders)[1],colnames(HCTcolHeaders))
  groupByClause = paste0(colnamesList,collapse=",")
  
  sqlClause = paste0("SELECT ",groupByClause,",",exprClause," FROM DBdata WHERE ",colClause," AND ",rowClause," GROUP BY ",groupByClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 12: nR & 1C + TOP/BOTTOM K + ROW FILTER")
  
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
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 13: nR & 1C + operation on column")
  
  numTemplate = 13
  
  # NO FILTER
  rowClause <- "TRUE"
  
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  colnamesList = colnames(HCTrowHeaders)
  selectionClause = paste0(colnamesList,collapse=",")
  
  operationVAL = 120
  operationOP = " < "
  operationClause = paste0("Value",operationOP,operationVAL)
  
  sqlClause = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",operationClause)
  sqlResult = sqldf(sqlClause)
  
  sqlClause13 = sqlClause # FOR APPLYING NEXT CLAUSE 14
  
  # sqlClause
  # result 
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
  ################################################
  # Local aggregation (per group of rows) 
  print("---- TEMPLATE 14: nR & 1C + TOP/BOTTOM K")
  
  numTemplate = 14
  
  sqlResult13 = sqldf(sqlClause13)
  
  indRowSel <- geneIndexForRandomClauses(sqlResult13) # use values to select a subset of rows Level 1
  indColSel <- geneIndexForRandomClauses(HCTcolHeaders,1,1)
  
  rowClause <- geneMultiRowOrColSelectionSQL(sqlResult13,indRowSel)
  colClause <- geneMultiRowOrColSelectionSQL(HCTcolHeaders,indColSel)
  
  selectionClause = "Value"
  
  sqlClause = paste0("SELECT ",selectionClause," FROM DBdata WHERE ",colClause," AND ",rowClause)
  sqlResult = sqldf(sqlClause)
  # sqlClause
  # result 
  # NEED TO USE STRformatValue TO USE SAME FORMAT AS GENERATED IN TABLE (sqldf rounding or avg can use different format, decimal...)
  sqlResultSTR <- formatResults(sqlResult)
  
  sqlResultSTR
  sqlJSONstr[[numTemplate]] = sqlJSONtemplate(numTemplate,sqlClause,sqlResultSTR)
  
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
    questionList = c(questionList,sqlJSONstr[[iq]])
  }
  questionList = paste0(questionList,collapse=",\n")
  questionList = paste0('[',questionList,']')
  
  jsonSTR = paste0('{\n',
                   '"filename": "',DB_CSVfile,'",\n',
                   '"templates": ',templateList,',\n',
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
  
} # end of for loop over all DATA_FILE_LIST list